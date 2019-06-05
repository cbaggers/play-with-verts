(in-package #:play-with-verts)

(defvar *last-time* (get-internal-real-time))

(defun reset (&key force)
  (reset-camera))

(defun game-step ()
  (let* ((now (get-internal-real-time))
         (delta (* (- now *last-time*) 0.001))
         (delta (if (> delta 0.16) 0.00001 delta)))
    (setf *last-time* now)

    ;; update camera
    (update *current-camera* delta)

    (with-setf (clear-color) (v! 0 1 0 0)
      (as-frame
       ;; todo
       ))
    (decay-events)))

(def-simple-main-loop play (:on-start #'reset)
  (game-step))

;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
