(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *last-time* (get-internal-real-time))

(defvar *1st-pass* nil)
(defvar *1st-pass-sampler* nil)

(defvar *2nd-pass* nil)
(defvar *2nd-pass-sampler* nil)

(defvar *3rd-pass* nil)
(defvar *3rd-pass-sampler* nil)

(defun reset ()
  (setf *things* nil)
  (setf *vcones* nil)
  (unless *1st-pass*
    (setf *1st-pass-sampler*
          (sample (attachment-tex *1st-pass* 0))))
  (unless *2nd-pass*
    (setf *2nd-pass-sampler*
          (sample (attachment-tex *2nd-pass* 0))))
  (unless *3rd-pass*
    (setf *3rd-pass-sampler*
          (sample (attachment-tex *3rd-pass* 0))))
  (make-ground))

(defun game-step ()
  (let* ((now (get-internal-real-time))
         (delta (* (- now *last-time*) 0.001))
         (delta (if (> delta 0.16) 0.00001 delta)))
    (setf *last-time* now)

    ;; update camera
    (update *current-camera* delta)

    ;; set the position of our viewport
    (setf (resolution (current-viewport))
          (surface-resolution (current-surface *cepl-context*)))

    ;; render ALL THE *THINGS*
    (upload-uniforms-for-cam *current-camera*)

    (with-fbo-bound (*1st-pass*)
      (clear)
      (loop :for thing :in *things* :do
         (update thing delta)
         (draw thing)))

    (with-fbo-bound (*2nd-pass*)
      (clear)
      (k-edge *1st-pass-sampler*))

    (let ((src-fbo *2nd-pass*)
          (src-sampler *2nd-pass-sampler*)
          (dst-fbo *1st-pass*)
          (dst-sampler *1st-pass-sampler*))
      (loop :for i :below 4 :do
         (with-fbo-bound (dst-fbo)
           (clear)
           (k-gaussian src-sampler))
         (rotatef src-fbo dst-fbo)
         (rotatef src-sampler dst-sampler)))

    (clear)
    (threshold *2nd-pass-sampler*)
    ;; display what we have drawn
    (swap)
    (decay-events)))


(def-simple-main-loop play (:on-start #'reset)
  (game-step))


;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
