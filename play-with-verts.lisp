(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *last-time* (get-internal-real-time))

(defun reset ()
  (setf *things* nil)
  (unless *vector-field*
    (let ((tex (make-texture nil :dimensions '(256 256)
                             :element-type :vec4)))
      (setf *vector-field* (make-fbo (list 0 tex)))
      (setf *vector-field-sampler* (sample tex))))
  (unless *particles*
    (setf *particles* (make-particles-data)))
  (make-ball))

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

    (blit-noise)

    (update-particles delta)

    ;;(clear)
    ;; (draw-tex-tr *vector-field-sampler*
    ;;              :color-scale (v! 1 1 0 0))
    ;; (draw-tex-tl (src-sampler *particles*)
    ;;              :color-scale (v! 1 0 0 0))
    (upload-uniforms-for-cam *current-camera*)
    (loop :for thing :in *things* :do
       (update thing delta)
       (draw thing))

    ;; display what we have drawn
    (swap)
    (decay-events)))


(def-simple-main-loop play (:on-start #'reset)
  (game-step))


;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
