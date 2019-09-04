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
(defvar *occlusion-chain-texture* nil)
(defvar *occlusion-chain-sampler* nil)
(defvar *occlusion-chain-fbo* nil)
(defvar *per-obj-occluders* nil)
(defvar *inst-counts* nil)
(defvar *inst-counts2* nil)
(defvar *occluder-output* nil)

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
    (free *occlusion-chain-texture*)
    (free *occlusion-chain-sampler*)
    (free *occlusion-chain-fbo*)
    (map nil #'free *chain-textures*)
    (map nil #'free *chain-fbos*))
  (setf *occlusion-buffer-fbo*
        (make-fbo `(:d :dimensions ,*vp-size-list*)))
  (setf *occlusion-buffer*
        (attachment *occlusion-buffer-fbo* :d))
  (setf *occlusion-buffer-sampler*
        (sample (gpu-array-texture *occlusion-buffer*)))
  (setf *occlusion-chain-texture*
   (make-texture nil
                 :dimensions *vp-size-list*
                 :element-type :float
                 :mipmap t))
  (setf *occlusion-chain-sampler*
        (sample *occlusion-chain-texture*
                :minify-filter :nearest
                :magnify-filter :nearest))
  (setf *occlusion-chain-fbo*
        (make-fbo (list 0 (texref *occlusion-chain-texture*))))

  (setf *chain-textures*
        (loop
           :for size :in *chain-sizes*
           :collect (make-texture nil :dimensions size
                                  :element-type :float)))
  (setf *chain-fbos*
        (loop
           :for tex :in *chain-textures*
           :for i :from 1
           :collect (make-fbo
                     (list 0 tex)
                     (list 1 (texref *occlusion-chain-texture*
                                     :mipmap-level i)))))
  (setf *chain-samplers*
        (loop
           :for tex :in *chain-textures*
           :collect (sample tex :wrap :clamp-to-edge))))

(defun reset-per-inst-data ()
  (when *per-inst-data*
    (free *per-inst-data*)
    (free *per-obj-occluders*)
    (free *inst-counts*)
    (free *inst-counts2*)
    (free *occluder-output*))
  (setf *per-obj-occluders*
        (make-ssbo nil 'obj-occ-data))
  (setf *inst-counts*
        (make-ssbo nil 'inst-counts))
  (setf *inst-counts2*
        (make-ssbo nil 'inst-counts2))
  (setf *occluder-output*
        (make-ssbo nil 'occ-output))
  (let* ((count 1000)
         (pia (make-gpu-array nil :dimensions count
                              :element-type 'per-inst-data))
         (occ-data (ssbo-data *per-obj-occluders*))
         (tmp-data (make-array count)))
    (loop
       :for i :below count
       :do
         (setf (aref tmp-data i)
               (cons (v! (- (random 200f0) 100f0)
                         (- (random 200f0) 100f0)
                         (- (random 200f0) 100f0))
                     (+ 0.5 (random 9f0)))))
    (with-gpu-array-as-c-array (c-arr pia)
      (loop
         :for (pos . scale) :across tmp-data
         :for i :from 0
         :for elem := (aref-c c-arr i)
         :do
           (setf (per-inst-data-pos elem) pos
                 (per-inst-data-scale elem) scale)))
    (setf *per-inst-data* pia)

    ;; populate per-obj occluder data
    (with-gpu-array-as-c-array (c-arr occ-data)
      (let ((pia (obj-occ-data-arr (aref-c c-arr 0))))
        (loop
           :for (pos . scale) :across tmp-data
           :for i :from 0
           :for elem := (aref-c pia i)
           :for offset := (v! scale scale scale)
           :do
             (setf (occ-data-bbox-min elem)
                   (v3:- pos offset))
             (setf (occ-data-bbox-max elem)
                   (v3:+ pos offset))
             (setf (occ-data-world elem)
                   (m4:translation pos)))))))

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

    #+nil
    (as-frame
      (when *mesh*
        (with-slots (bstream) *mesh*
          (with-instances 1000
            (fart *current-camera*
                  bstream
                  *sampler*)))))
    (as-frame
      ;; (when *mesh*
      ;;   (with-slots (bstream) *mesh*
      ;;     (with-fbo-bound (*occlusion-buffer-fbo*
      ;;                      :attachment-for-size :d)
      ;;       (clear-fbo *occlusion-buffer-fbo*)
      ;;       (with-instances 1000
      ;;         (populate-occlusion-buffer *current-camera*
      ;;                                    bstream
      ;;                                    *sampler*)))))
      ;; (gen-mip-chain)
      ;;(blit-it (elt *chain-samplers* 0) 20f0)
      ;; (blit-it *occlusion-chain-sampler* 20f0 0)
      (blort)
      )
    (decay-events)))

(defun gen-mip-chain ()
  (with-fbo-bound (*occlusion-chain-fbo*)
    (blit-it *occlusion-buffer-sampler*))
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
