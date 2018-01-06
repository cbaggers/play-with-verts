(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *last-time* (get-internal-real-time))
(defvar *empty-buffer-stream* nil)
(defvar *light-data-array* nil)
(defvar *light-data* nil)

(defun reset ()
  (unless *ground* (make-ground))
  (make-ball)
  (reset-particles)
  (make-particle)
  (make-defer-fbo)
  (setf *sphere-data* (sphere))
  (setf *empty-buffer-stream*
        (make-buffer-stream nil :primitive :points))
  (setf *light-data-array*
   (make-gpu-array
     (list
      (list
       (loop :for i below 1000 collect
          (v! (- (random 100f0) 50f0)
              (random 80f0)
              (- (random 100f0) 50f0)))))
     :dimensions 1
     :element-type 'light-data))

  (setf *light-data*
        (make-ubo *light-data-array*
                  'light-data)))

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

    (with-blending *blend-params*
      (with-setf (depth-test-function) nil
        (map-g #'albedo-pass *empty-buffer-stream*
               :pos-sampler *position-sam*
               :normal-sampler *normal-sam*
               :albedo-sampler *albedo-sam*)

        (with-instances 1000
          (map-g #'volume-pass *sphere-data*
                 ;;vert
                 :ldata *light-data*
                 :world->view (get-world->view-space *current-camera*)
                 :view->clip (projection *current-camera*)
                 ;;frag
                 :vp-size (viewport-resolution (current-viewport))
                 :pos-sampler *position-sam*
                 :normal-sampler *normal-sam*
                 :albedo-sampler *albedo-sam*
                 :cam-pos (pos *current-camera*)))))

    ;; display what we have drawn
    (swap)
    (particle-swap)
    (decay-events)))

(defvar *blend-params* nil)

(def-simple-main-loop play (:on-start #'reset)
  (game-step))


;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
