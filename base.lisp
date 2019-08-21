(in-package #:play-with-verts)

(defvar *last-time* (get-internal-real-time))
(defvar *mesh* nil)
(defvar *sampler* nil)
(defvar *per-inst-data* nil)
(defvar *vp-size* (v! 512 512))
(defvar *vp-size-list* '(512 512))
(defvar *occlusion-buffer-fbo* nil)
(defvar *occlusion-buffer* nil)
(defvar *occlusion-buffer-sampler* nil)
(defparameter *chain-sizes* '((256 256)
                              (128 128)
                              (64 64)
                              (32 32)
                              (16 16)
                              (8 8)
                              (4 4)
                              (2 2)
                              (1 1)))
(defvar *chain-textures* nil)
(defvar *chain-fbos* nil)
(defvar *chain-samplers* nil)

(defun reset ()
  (when *mesh* (free *mesh*))
  (reset-occlusion-buffer)
  (reset-per-inst-data)
  (setf *mesh* (make-sphere *per-inst-data*))
  (setf
   *sampler*
   (sample
    (dirt:load-image-to-texture
     (asdf:system-relative-pathname
      :play-with-verts "./media/wat.png"))))
  (reset-camera))

(defun reset-occlusion-buffer ()
  (when *occlusion-buffer-fbo*
    (free *occlusion-buffer-fbo*)
    (free (gpu-array-texture *occlusion-buffer*))
    (free *occlusion-buffer-sampler*)
    (map nil #'free *chain-textures*)
    (map nil #'free *chain-fbos*))
  (setf *occlusion-buffer-fbo*
        (make-fbo `(:d :dimensions ,*vp-size-list*)))
  (setf *occlusion-buffer*
        (attachment *occlusion-buffer-fbo* :d))
  (setf *occlusion-buffer-sampler*
        (sample (gpu-array-texture *occlusion-buffer*)))
  (setf *chain-textures*
        (loop
           :for size :in *chain-sizes*
           :collect (make-texture nil :dimensions size
                                  :element-type :float)))
  (setf *chain-fbos*
        (loop
           :for tex :in *chain-textures*
           :collect (make-fbo (list 0 tex))))
  (setf *chain-samplers*
        (loop
           :for tex :in *chain-textures*
           :collect (sample tex :wrap :clamp-to-edge))))

(defun reset-per-inst-data ()
  (when *per-inst-data*
    (free *per-inst-data*))
  (let* ((count 1000)
         (pia (make-gpu-array nil :dimensions count
                             :element-type 'per-inst-data)))
    (with-gpu-array-as-c-array (c-arr pia)
      (loop
         :for i :below count
         :for elem := (aref-c c-arr i)
         :do
           (setf (per-inst-data-pos elem)
                 (v! (- (random 200f0) 100f0)
                     (- (random 200f0) 100f0)
                     (- (random 200f0) 100f0)))
           (setf (per-inst-data-scale elem)
                 (+ 0.5 (random 9f0)))))
    (setf *per-inst-data* pia)))

(defun game-step ()
  (let* ((now (get-internal-real-time))
         (delta (* (- now *last-time*) 0.001))
         (delta (if (> delta 0.16) 0.00001 delta)))
    (setf *last-time* now)

    (setf (viewport-resolution (current-viewport))
          ;;(surface-resolution (current-surface))
          *vp-size*)

    ;; update camera
    (update *current-camera* delta)

    (as-frame
      (when *mesh*
        (with-slots (bstream) *mesh*
          (with-fbo-bound (*occlusion-buffer-fbo*
                           :attachment-for-size :d)
            (clear-fbo *occlusion-buffer-fbo*)
            (with-instances 1000
              (populate-occlusion-buffer *current-camera*
                                         bstream
                                         *sampler*)))))
      (gen-mip-chain)
      (blit-it (elt *chain-samplers* 0) 20f0))
    (decay-events)))

(defun gen-mip-chain ()
  (loop
     :for sampler :in (cons *occlusion-buffer-sampler*
                            *chain-samplers*)
     :for fbo :in *chain-fbos*
     :do (with-fbo-bound (fbo)
           (downscale-it sampler))))

(def-simple-main-loop play (:on-start #'reset)
  (game-step))

;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
