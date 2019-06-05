(in-package #:play-with-verts)

(defvar *last-time* (get-internal-real-time))
(defvar *mesh* nil)
(defvar *bstream* nil)
(defvar *sampler* nil)

(defun reset ()
  (when *mesh* (free *mesh*))
  (when *bstream* (free *bstream*))
  (setf *mesh* (do-it))
  (setf *bstream* (make-buffer-stream *mesh*))
  (setf
   *sampler*
   (sample
    (dirt:load-image-to-texture
     (asdf:system-relative-pathname
      :play-with-verts "./media/rust.jpg"))))
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
      (when *bstream*
        (render *current-camera*
                *bstream*
                *sampler*)))
    (decay-events)))

(def-simple-main-loop play (:on-start #'reset)
  (game-step))

;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
