(in-package #:play-with-verts)

(defvar *last-time* (get-internal-real-time))

(defvar *src-tex* nil)
(defvar *src-sampler* nil)

(defvar *fbo0* nil)
(defvar *fbo1* nil)
(defvar *sampler0* nil)
(defvar *sampler1* nil)

(defun reset ()
  (reset-camera)

  (free *src-tex*)
  (free *src-sampler*)

  (free *sampler0*)
  (free *sampler1*)
  (free *fbo0*)
  (free *fbo1*)

  (setf *fbo0* (make-fbo (list 0 :element-type :vec4) :d))
  (setf *fbo1* (make-fbo (list 0 :element-type :vec4) :d))

  (setf *src-tex* (load-tex "./media/blob.png"))
  (setf *src-sampler* (sample *src-tex*))

  (setf *sampler0* (sample (attachment-tex *fbo0* 0)
                           :minify-filter :nearest
                           :magnify-filter :nearest
                           :wrap :clamp-to-edge))
  (setf *sampler1* (sample (attachment-tex *fbo1* 0)
                           :minify-filter :nearest
                           :magnify-filter :nearest
                           :wrap :clamp-to-edge))
  t)

(defun game-step ()
  (let* ((now (get-internal-real-time))
         (delta (* (- now *last-time*) 0.001))
         (delta (if (> delta 0.16) 0.00001 delta)))
    (setf *last-time* now)

    (setf (viewport-resolution (current-viewport))
          (surface-resolution (current-surface)))

    ;; update camera
    (update *current-camera* delta)


    (with-setf (depth-test-function) nil
      (with-fbo-bound (*fbo0*)
        (do-init *src-sampler*))
      (loop
         :for i :below 12
         :do
           (with-fbo-bound (*fbo1*)
             (do-flood-step i *sampler0*))
           (rotatef *fbo0* *fbo1*)
           (rotatef *sampler0* *sampler1*)))

    (as-frame
      (let ((threshold (* 30 (+ 1 (sin (* 4 (now)))))))
        (test-blit *sampler0* threshold)))

    (decay-events)))


(def-simple-main-loop play (:on-start #'reset)
  (game-step))

;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
