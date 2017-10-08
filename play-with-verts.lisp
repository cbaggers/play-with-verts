(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *some-fbo* nil)
(defvar *some-sampler* nil)

(defvar *last-time* (get-internal-real-time))

(defun reset ()
  (setf *things* nil)
  (make-ground)
  (dotimes (x 10) (make-box))
  (dotimes (x 10) (make-ball)))

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
    (with-fbo-bound (*some-fbo*)
      (clear-fbo *some-fbo*)
      (loop :for thing :in *things* :do
         (update thing delta)
         (draw #'some-pipeline *current-camera* thing)))

    (clear)
    (map-g #'first-blur (get-quad-stream-v2) :sam *some-sampler*)
    ;;(draw-tex *some-sampler*)

    ;; display what we have drawn
    (swap)
    (decay-events)))


(def-simple-main-loop play (:on-start #'reset)
  (game-step))


;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
