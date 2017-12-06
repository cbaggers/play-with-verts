(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *last-time* (get-internal-real-time))
(defvar *empty-buffer-stream* nil)

(defun reset ()
  (unless *ground* (make-ground))
  (make-ball)
  (reset-particles)
  (make-particle)
  (make-defer-fbo)
  (setf *sphere-data* (sphere))
  (setf *empty-buffer-stream*
        (make-buffer-stream nil :primitive :points)))

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
    (update *ball* delta)

    (clear-fbo *first-pass-fbo*)
    (with-fbo-bound (*first-pass-fbo*)
      (draw #'first-pass *ball* *current-camera*)
      (with-instances (first (dimensions *pbuffer-src*))
        (draw #'first-pass *particle* *current-camera*)))

    (map-g #'second-pass *empty-buffer-stream*
           :vp-size (viewport-resolution (current-viewport))
           :pos-sampler *position-sam*
           :normal-sampler *normal-sam*
           :albedo-sampler *albedo-sam*
           :light-pos (pos *ball*)
           :cam-pos (pos *current-camera*))

    (map-g #'volume-pass *sphere-data*
           ;;vert
           :scale 1f0
           :model->world (m4:translation (v! 0 0 0 0))
           :world->view (get-world->view-space *current-camera*)
           :view->clip (projection *current-camera*)
           ;;frag
           :vp-size (viewport-resolution (current-viewport))
           :pos-sampler *position-sam*
           :normal-sampler *normal-sam*
           :albedo-sampler *albedo-sam*
           :light-pos (pos *ball*)
           :cam-pos (pos *current-camera*))

    ;; display what we have drawn
    (swap)
    (particle-swap)
    (decay-events)))


(def-simple-main-loop play (:on-start #'reset)
  (game-step))


;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
