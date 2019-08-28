(in-package #:play-with-verts)

;;------------------------------------------------------------

(defstruct-g (occ-data :layout std-430)
  (bbox-min :vec3)
  (bbox-max :vec3)
  (world :mat4))

(defstruct-g (obj-occ-data :layout std-430)
  (arr (occ-data 1000)))

(defstruct-g (inst-counts :layout std-430)
  (arr (:uint 10)))

(defstruct-g (occ-output :layout std-430)
  (arr (:mat4 1000)))

(defun-g uv2 ((v :vec2))
  (ivec2 (int (x v))
         (int (y v))))

(defun-g occlusion-check (&uniform
                          (inst-data obj-occ-data :ssbo)
                          (instance-counts inst-counts :ssbo)
                          (output occ-output :ssbo)
                          (view-projection :mat4)
                          (max-mip-level :int)
                          (input-rt :sampler-2d))
  (declare (local-size :x 1 :y 1 :z 1))
  ;;if (threadID.x < NoofInstances) ? neccesary ?
  (let ((data (aref (obj-occ-data-arr inst-data)
                    (x gl-global-invocation-id))))
    (with-slots (bbox-min bbox-max world) data
      (let* ((box-size (- bbox-max bbox-min))
             (box-corners
              (vector
               bbox-min
               (+ bbox-min (vec3 (x box-size) 0 0))
               (+ bbox-min (vec3 0 (y box-size) 0))
               (+ bbox-min (vec3 0 0 (z box-size)))
               (+ bbox-min (vec3 (s~ box-size :xy) 0))
               (+ bbox-min (vec3 0 (s~ box-size :yz)))
               (+ bbox-min (vec3 (x box-size) 0 (z box-size)))
               (+ bbox-min box-size)))
             (min-z 1f0)
             (min-xy (v! 1 1))
             (max-xy (v! 0 0)))
        (for (i 0) (< i 8) (++ i)
             ;; transform world space aaBox to NDC
             (let* ((clip-pos
                     (* view-projection
                        (v! (aref box-corners i) 1))))
               (setf
                (z clip-pos) (max (z clip-pos) 0)
                (s~ clip-pos :xyz) (/ (s~ clip-pos :xyz)
                                      (w clip-pos))
                (s~ clip-pos :xy) (clamp (s~ clip-pos :xy)
                                         -1
                                         1)
                (s~ clip-pos :xy) (+ (* (s~ clip-pos :xy)
                                        (v! 0.5 -0.5))
                                     (v! 0.5 0.5))
                min-xy (min (s~ clip-pos :xy) min-xy)
                min-xy (max (s~ clip-pos :xy) max-xy)
                min-z (saturate (min min-z (z clip-pos))))))
        (let* ((box-uvs (v! min-xy max-xy))
               ;; Calculate hi-Z buffer mip
               (size (* (- max-xy min-xy)
                        (v! 512 512) ;; {TODO} check this
                        ))
               (mip (ceil (log2 (max (x size) (y size)))))
               (mip (clamp mip 0 max-mip-level))
               ;; Texel footprint for the lower (finer-grained)
               ;; level
               (level-lower (max (- mip 1) 0))
               (scale (exp2 (- level-lower)))
               (a (floor (* (s~ box-uvs :xy) scale)))
               (b (ceil (* (s~ box-uvs :zw) scale)))
               (dims (- b a))
               ;; Use the lower level if we only touch <= 2
               ;; texels in both dimensions
               (mip (int (if (and (<= (x dims) 2)
                                  (<= (y dims) 2))
                             level-lower
                             mip)))
               ;; load depths from high z buffer
               (depth
                (v!
                 (x (texel-fetch
                     input-rt (uv2 (s~ box-uvs :xy)) mip))
                 (x (texel-fetch
                     input-rt (uv2 (s~ box-uvs :zy)) mip))
                 (x (texel-fetch
                     input-rt (uv2 (s~ box-uvs :xw)) mip))
                 (x (texel-fetch
                     input-rt (uv2 (s~ box-uvs :zw)) mip))))
               ;; find the max depth
               (max-depth (max (max (max (x depth) (y depth))
                                    (z depth))
                               (w depth))))
          (when (<= min-z max-depth)
            (with-slots ((count-arr arr)) instance-counts
              (let ((i (atomic-add (aref count-arr 0) 1)))
                (setf (aref (occ-output-arr output) i)
                      world))
              ;; WRITE OUT TO PER INST BUFFERS \o/
              ))))))
  (values))

(defpipeline-g occlude-pass ()
  :compute occlusion-check)

(defun blort (&optional (camera *current-camera*))
  (let ((proj (m4:* (get-world->view-space camera)
                    (projection camera)))
        (mips (- (texture-mipmap-levels
                  *occlusion-chain-texture*)
                 1)))
    (reallocate-buffer
     (gpu-array-buffer
      (ssbo-data *inst-counts*)))
    (push-g '((0 0 0 0 0 0 0 0 0 0))
            *inst-counts*)
    (map-g #'occlude-pass (make-compute-space 1000)
           :inst-data *per-obj-occluders*
           :instance-counts *inst-counts*
           :view-projection proj
           :max-mip-level mips
           :output *occluder-output*
           :input-rt *occlusion-chain-sampler*)
    (wait-on-gpu-fence (make-gpu-fence))
    (pull-g *inst-counts*)))

;; (&uniform
;;  (inst-data obj-occ-data :ssbo)
;;  (instance-counts inst-counts :ssbo)
;;  (view-projection :mat4)
;;  (max-mip-level :int)
;;  (input-rt :sampler-2d))

;; A disadvantage of this approach is that since Gather does not
;; support mip level selection you will have to create a
;; different shader/rendertarget view per mip level and bind
;; them successively during downsampling.

;;------------------------------------------------------------
