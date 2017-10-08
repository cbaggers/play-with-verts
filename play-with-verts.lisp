(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *bokeh-rt* nil)

(defvar *last-time* (get-internal-real-time))

(defun reset ()
  (setf *things* nil)
  (make-ground)
  (dotimes (x 10) (make-box))
  (dotimes (x 10) (make-ball))
  (unless *bokeh-rt*
    (setf *bokeh-rt*
          (make-render-target
           t
           `(0 :dimensions (1024 1024) :element-type :vec3)
           `(:d :dimensions (1024 1024))))))

(defparameter *stepper*
  (tlambda ()
    (repeat
      (before (seconds 1)
        nil)
      (before (seconds 1)
        t))))

(defun draw-scene (render-target delta)
  (let ((fbo (rt-fbo render-target)))
    (with-fbo-bound (fbo)
      (clear-fbo fbo)
      (loop :for thing :in *things* :do
         (update thing delta)
         (draw #'some-pipeline *current-camera* thing)))
    (rt-swap render-target)))

(defun run-pipeline-into (pipeline render-target)
  (let ((fbo (rt-fbo render-target)))
    (with-fbo-bound (fbo)
      (clear-fbo fbo)
      (map-g pipeline (get-quad-stream-v2)
             :sam (rt-sampler render-target 0)))
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
    (draw-scene *bokeh-rt* delta)
    (run-pipeline-into #'first-blur *bokeh-rt*)
    (run-pipeline-into #'fill-blur *bokeh-rt*)
    (clear)

    (map-g #'blit (get-quad-stream-v2)
           :sam (if (funcall *stepper*)
                    (rt-sampler-front *bokeh-rt* 0)
                    (rt-sampler-back *bokeh-rt* 0)))

    ;; display what we have drawn
    (swap)
    (decay-events)))


(def-simple-main-loop play (:on-start #'reset)
  (game-step))


;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
