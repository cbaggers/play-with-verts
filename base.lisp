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
      :play-with-verts "./media/cobble0.jpg"))))
  (reset-camera))

(defun reset-occlusion-buffer ()
  (when *occlusion-buffer-fbo*
    (free *occlusion-buffer-fbo*)
    (free *occlusion-buffer*)
    (free *occlusion-buffer-sampler*))
  (setf *occlusion-buffer-fbo*
        (make-fbo `(:d :dimensions ,*vp-size-list*)))
  (setf *occlusion-buffer*
        (attachment *occlusion-buffer-fbo* :d))
  (setf *occlusion-buffer-sampler*
        (sample (gpu-array-texture *occlusion-buffer*))))

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
            (with-instances 1000
              (render *current-camera*
                      bstream
                      *sampler*)))))
      (blit-it *occlusion-buffer-sampler*))
    (decay-events)))

(def-simple-main-loop play (:on-start #'reset)
  (game-step))

;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
