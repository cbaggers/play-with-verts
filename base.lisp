(in-package #:play-with-verts)

(defvar *last-time* (get-internal-real-time))
(defvar *mesh* nil)
(defvar *sampler* nil)

(defun reset ()
  (when *mesh* (free *mesh*))
  (setf *mesh* (make-terrain))
  (setf
   *sampler*
   (sample
    (dirt:load-image-to-texture
     (asdf:system-relative-pathname
      :play-with-verts "./media/cobble0.jpg"))))
  (reset-camera))

(defun game-step ()
  (let* ((now (get-internal-real-time))
         (delta (* (- now *last-time*) 0.001))
         (delta (if (> delta 0.16) 0.00001 delta)))
    (setf *last-time* now)

    (setf (viewport-resolution (current-viewport))
          (surface-resolution (current-surface)))

    ;; update camera
    (update *current-camera* delta)

    (as-frame
      (when *mesh*
        (with-slots (bstream) *mesh*
          (render *current-camera*
                  bstream
                  *sampler*
                  6f0))))
    (decay-events)))

(def-simple-main-loop play (:on-start #'reset)
  (game-step))

;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
