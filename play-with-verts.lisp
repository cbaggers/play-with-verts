(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *last-time* (get-internal-real-time))

(defun reset ()
  (unless *ground* (make-ground))
  (make-ball)
  (reset-particles)
  (make-particle))

(defun game-step ()
  (let* ((now (get-internal-real-time))
         (delta (* (- now *last-time*) 0.001))
         (delta (if (> delta 0.16) 0.00001 delta)))
    (setf *last-time* now)

    ;; update camera
    (update *current-camera* delta)

    ;; set the position of our viewport
    (setf (resolution (current-viewport))
          (surface-resolution (current-surface (cepl-context))))

    ;; clear the default fbo
    (clear)

    (update-particle-state *stream-src* *tfs-dst*)

    ;; rendering time
    (upload-uniforms-for-cam #'first-pass *current-camera*)
    ;;(draw #'first-pass *ground*)
    (update *ball* delta)
    (draw #'first-pass *ball*)

    (with-instances (first (dimensions *pbuffer-src*))
      (draw #'first-pass *particle*))

    ;; display what we have drawn
    (swap)
    (particle-swap)
    (decay-events)))


(def-simple-main-loop play (:on-start #'reset)
  (game-step))


;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
