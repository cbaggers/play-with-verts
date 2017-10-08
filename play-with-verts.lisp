(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *bokeh-rt* nil)

(defvar *scene-fbo* nil)
(defvar *scene-sampler* nil)

(defvar *coc-fbo* nil)
(defvar *coc-sampler* nil)

(defvar *last-time* (get-internal-real-time))

(defun reset ()
  (setf *things* nil)
  (make-ground)
  (dotimes (x 10) (make-box))
  (dotimes (x 10) (make-ball))
  ;; todo wrap
  (unless *bokeh-rt*
    (setf *bokeh-rt*
          (make-render-target
           t
           `(0 :dimensions (1024 1024) :element-type :vec3)
           `(:d :dimensions (1024 1024))))
    (setf *coc-fbo*
          (make-fbo
           `(0 :dimensions (1024 1024) :element-type :float)))
    (setf *coc-sampler*
          (sample (attachment-tex *coc-fbo* 0)))
    (setf (wrap *coc-sampler*) :clamp-to-edge)
    (setf *scene-fbo*
          (make-fbo
           `(0 :dimensions (1024 1024) :element-type :vec4)
           `(:d :dimensions (1024 1024))))
    (setf *scene-sampler*
          (sample (attachment-tex *scene-fbo* 0)))))

(defparameter *stepper*
  (tlambda ()
    (repeat
      (before (seconds 1)
        nil)
      (before (seconds 1)
        t))))

(defun draw-scene (delta)
  (let ((fbo *scene-fbo*))
    (with-fbo-bound (fbo)
      (clear-fbo fbo)
      (loop :for thing :in *things* :do
         (update thing delta)
         (draw #'some-pipeline *current-camera* thing)))))

(defun run-pipeline-into (pipeline render-target
                          &optional alt-sampler)
  (let ((fbo (rt-fbo render-target)))
    (with-fbo-bound (fbo)
      (clear-fbo fbo)
      (map-g pipeline (get-quad-stream-v2)
             :sam (or alt-sampler (rt-sampler render-target 0))))
    (rt-swap render-target)))

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
    (draw-scene *bokeh-rt*)
    (run-pipeline-into #'first-blur *bokeh-rt* *scene-sampler*)
    (run-pipeline-into #'fill-blur *bokeh-rt*)

    (with-fbo-bound (*coc-fbo*)
      (clear-fbo *coc-fbo*)
      (map-g #'coc-pass (get-quad-stream-v2)
             :sam *scene-sampler*))

    (clear)
    (map-g #'fuckery (get-quad-stream-v2) :sam *coc-sampler*)

    ;; display what we have drawn
    (swap)
    (decay-events)))


(def-simple-main-loop play (:on-start #'reset)
  (game-step))


;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
