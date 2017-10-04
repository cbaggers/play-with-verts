(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *last-time* (get-internal-real-time))

(defun reset ()
  (setf *things* nil)
  ;;NewtonDestroyAllBodies
  (when *world*
    (world-destroy-all-bodies *world*))
  (make-ground)
  (unless *light-fbo*
    (setf *light-fbo*
          (make-fbo `(:d :dimensions (1024 1024))))
    (setf *light-sampler*
          (sample (attachment-tex *light-fbo* :d)))
    (setf (frame-size *camera-1*)
          (v:* (viewport-resolution (current-viewport))
               0.1f0))
    (setf (pos *camera-1*) (v! -17.38 37.5 20.15)
          (rot *camera-1*) (v! -0.82 0.32 0.45 0.11))))

(defun render-all-the-things (pipeline camera delta)
  (upload-uniforms-for-cam pipeline camera)
  (loop :for thing :in *things* :do
     (update thing delta)
     (draw pipeline thing)))

(defvar *world* (make-world))

(defparameter *phys-step* (make-stepper (seconds 0.01)))

(defun game-step ()
  (let* ((now (get-internal-real-time))
         (delta (* (- now *last-time*) 0.001))
         (delta (if (> delta 0.16) 0.00001 delta)))
    (setf *last-time* now)

    ;; run physics
    (loop :while (funcall *phys-step*) :do
       (world-step *world* 0.01))

    ;; update camera
    (update *current-camera* delta)

    ;; set the position of our viewport
    (setf (resolution (current-viewport))
          (surface-resolution (current-surface *cepl-context*)))

    ;; render ALL THE *THINGS*
    (with-fbo-bound (*light-fbo* :attachment-for-size :d)
      (clear *light-fbo*)
      (render-all-the-things #'light-pipeline *camera-1* delta))

    ;; clear the default fbo
    (clear)

    (render-all-the-things #'some-pipeline *camera* delta)

    ;;(draw-tex-br *light-sampler*)

    ;; display what we have drawn
    (swap)
    (decay-events)))


(def-simple-main-loop play (:on-start #'reset)
  (game-step))


;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
