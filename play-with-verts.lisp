(in-package #:play-with-verts)

(defstruct-g (los-results :layout std-430)
  (data (:float 255)))

(defvar *last-time* (get-internal-real-time))

(defvar *scene-fbo* nil)
(defvar *scene-sampler* nil)

(defvar *los-scene* nil)

(defclass los-scene ()
  ((cube-map :initarg :cube-map)
   (fbos :initarg :fbos)
   (sampler :initarg :sampler)
   (ssbo :initarg :ssbo)))

(defun make-los-scene ()
  (let ((scene (make-instance 'los-scene)))
    (with-slots (cube-map fbos sampler ssbo) scene
      (setf cube-map (make-texture
                      nil
                      :element-type :uint8
                      :dimensions '(512 512)
                      :cubes t)
            fbos (loop :for i :below 6 :collect
                      (make-fbo
                       (list 0 (texref cube-map :cube-face i))
                       (list :d :dimensions '(512 512))))
            sampler (sample cube-map)
            ssbo (make-ssbo nil 'los-results))
      scene)))

(defmethod free ((obj los-scene))
  (with-slots (cube-map fbos sampler) obj
    (free cube-map)
    (map nil #'free fbos)
    (free sampler)
    nil))

(defvar *fallback-normal-map* nil)

(defun reset (&key force)
  ;;
  ;; on first startup
  (when (or force (not *scene-fbo*))
    (setf (clear-color) (v! 0 0 0 0))
    (setf *things* nil)
    (setf *fallback-normal-map*
          (sample
           (make-texture (list (list (v! 0.5 0.5 1)))
                         :dimensions '(1 1)
                         :mipmap nil)))
    (setf *los-scene* (make-los-scene))
    ;;
    ;; Add some things to the scene
    (load-assimp-things "sponza_obj/sponza.obj" 0.2f0)
    (make-ball (v! 0 10 20) 3.0)
    (reset-lights))
  ;;
  ;; every time
  (reset-fbos)
  (reset-camera))

(defun reset-fbos ()
  (when *scene-fbo*
    (free *scene-fbo*))
  (setf *scene-fbo*
        (make-fbo (list 0 :element-type :rgba16f)
                  (list 1 :element-type :rgba16f)
                  :d))
  (setf *scene-sampler*
        (sample (attachment-tex *scene-fbo* 0))))

(defun reset-lights ()
  (when *lights*
    (free *lights*)
    (free *lights-arr*))

  (let* ((light-data (make-c-array nil :dimensions 1
                                   :element-type 'light-set))
         (set (aref-c light-data 0))
         (plights (light-set-plights set)))

    (setf (light-set-count set) 3)

    (setf (aref-c plights 0) (list (v! 0 5 -18)
                                   (v! 1 1 1)
                                   1300.0)
          (aref-c plights 1) (list (v! 0 120 24)
                                   (v! 1 1 1)
                                   3200.0)
          (aref-c plights 2) (list (v! 200 15 -10)
                                   (v! 1 1 1)
                                   3200.0))

    (setf *lights-arr* (make-gpu-array light-data)
          *lights* (make-ubo *lights-arr* 'light-set))))

(defun game-step ()
  (let* ((now (get-internal-real-time))
         (delta (* (- now *last-time*) 0.001))
         (delta (if (> delta 0.16) 0.00001 delta)))
    (setf *last-time* now)

    ;; update camera
    (update *current-camera* delta)

    ;; set the position of our viewport
    (setf (resolution (current-viewport))
          (surface-resolution (current-surface)))

    ;; draw stuff
    (with-fbo-bound (*scene-fbo*)
      (clear-fbo *scene-fbo*)
      (loop :for thing :in *things* :do
           (update thing delta)
           (draw *current-camera* thing)))

    (as-frame
      (fxaa3-pass *scene-sampler*)
      ;;(draw-tex *scene-sampler*)
      (draw-tex (slot-value *los-scene* 'sampler))
      )
    (los-pass *los-scene* *camera-0*)
    (decay-events)))

(defparameter *rotations*
  (make-array 6 :initial-contents
              (list
               (q:from-direction (v! 0 1 0) (v! 1 0 0))
               (q:from-direction (v! 0 1 0) (v! 0 1 0))
               (q:from-direction (v! 0 1 0) (v! 0 0 1))
               (q:from-direction (v! 0 1 0) (v! -1 0 0))
               (q:from-direction (v! 0 1 0) (v! 0 -1 0))
               (q:from-direction (v! 0 1 0) (v! 0 0 -1)))))

(defparameter *empty-results*
  (list
   (make-array 255 :element-type 'single-float
               :initial-contents
               (loop :for i :below 255 :collect 0f0))))

(defun draw-to-los-scene (los-scene camera)
  (when *current-creature*
    (setf (pos camera) (pos *current-creature*))
    (with-slots (fbos) los-scene
      (loop
         :for i :below 6
         :for fbo :in fbos
         :do
           (setf (rot camera) (aref *rotations* i))
           (with-fbo-bound (fbo)
             (clear-fbo fbo)
             (loop :for thing :in *things* :do
                  (los-draw camera thing)))))))

(defun accum-los-results (los-scene)
  (with-slots (ssbo sampler) los-scene
    (map-g #'los-accumulate (get-quad-stream-v2)
           :results ssbo
           :cube-map sampler)
    (let ((fence (make-gpu-fence)))
      (wait-on-gpu-fence fence)
      (free fence)
      (first (first (pull-g (ssbo-data ssbo)))))))

(defun los-pass (los-scene camera)
  (push-g *empty-results*
          (slot-value *los-scene* 'ssbo))
  (draw-to-los-scene los-scene camera)
  (let ((foo (accum-los-results los-scene)))
    ;; (loop :for x :in foo
    ;;    :when (> x 0f0)
    ;;    :do (print "yay"))
    foo))


(def-simple-main-loop play (:on-start #'reset)
  (game-step))

;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
