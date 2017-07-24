(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *delta* 1)
(defvar *last-time* (get-internal-real-time))

(defun reset ()
  (setf *things* nil)
  (make-terrain))

(defun game-step ()
  (let* ((now (get-internal-real-time))
         (delta (* (- now *last-time*) 0.001))
         (delta (if (> delta 0.16) 0.00001 delta)))
    (setf *delta* delta)
    (setf *last-time* now))

  ;; update camera
  (update-camera *camera* *delta*)

  ;; set the position of our viewport
  (setf (resolution (current-viewport))
        (surface-resolution (current-surface *cepl-context*)))

  ;; clear the default fbo
  (clear)

  ;; render ALL THE *THINGS*
  (upload-uniforms-for-cam *camera*)

  (loop :for thing :in *things* :do
     (update thing)
     (draw thing))

  ;; display what we have drawn
  (swap)
  (decay-events))


(def-simple-main-loop play (:on-start #'reset)
  (game-step))


;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
