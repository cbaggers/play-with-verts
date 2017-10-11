(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *last-time* (get-internal-real-time))

(defvar *scene-fbo* nil)
(defvar *scene-sampler* nil)
(defvar *scene-depth-sampler* nil)

(defun reset ()
  (setf *things* nil)
  (make-ground)
  (loop :for i :below 10 :do
     (make-box))
  (loop :for i :below 10 :do
     (make-ball)))

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

    ;; draw stuff
    (with-fbo-bound (*scene-fbo*)
      (clear-fbo *scene-fbo*)
      (loop :for thing :in *things* :do
         (update thing delta)
         (draw #'some-pipeline *current-camera* thing)))

    (clear)
    (radial-blur2 *scene-sampler*)

    (draw-tex *scene-depth-sampler*)

    ;; display what we have drawn
    (swap)
    (decay-events)))


(def-simple-main-loop play (:on-start #'reset)
  (game-step))


;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
