(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *last-time* (get-internal-real-time))

(defun reset ()
  (setf *things* nil)
  (setf *vcones* nil)
  (init-cones)
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

    ;; clear the default fbo
    (clear)

    ;; render ALL THE *THINGS*
    (upload-uniforms-for-cam #'inst-pipeline *current-camera*)

    (loop :for thing :in *things* :do
       (update thing delta)
       (draw thing))

    (with-gpu-array-as-c-array
        (carr *per-cone-gpu-data*)
      (loop :for cone :in *vcones* :for i :from 0 :do
         (update cone delta)
         (let ((elem (aref-c carr i))
               (pos (pos cone)))
           (setf (pos elem) (v! (x pos) (z pos))
                 (color elem) (color cone)))))

    (draw-cones *current-camera*)

    ;; display what we have drawn
    (swap)
    (decay-events)))


(def-simple-main-loop play (:on-start #'reset)
  (game-step))


;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
