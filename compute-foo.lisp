(in-package #:play-with-verts)

(defstruct-g (ssbo-test-data :layout std-430)
  (vals (:int 100)))

(defstruct-g (some-more-data :layout std-430)
  (vals (:vec4 100)))

;;------------------------------------------------------------

;; write some nonsence here :)

(defstruct-g (occ-data :layout std-430)
  (bbox-min :vec3)
  (bbox-max :vec3)
  (world :mat4))

(defstruct-g (obj-occ-data :layout std-430)
  (arr (occ-data 1000)))

(defun-g occlusion-check (&uniform
                          (inst-data obj-occ-data :ssbo)
                          (view-projection :mat4)
                          (max-mip-level :int)
                          (input-rt :sampler-2d))
  (declare (local-size :x 1 :y 1 :z 1))
  ;;if (threadID.x < NoofInstances) ? neccesary ?
  (let ((data (aref (obj-occ-data-arr inst-data)
                    gl-invocation-id)))
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
               (mip (if (and (<= (x dims) 2)
                             (<= (y dims) 2))
                        level-lower
                        mip))
               ;; load depths from high z buffer
               (depth
                (v!
                 (texel-fetch input-rt (s~ box-uvs :xy) mip)
                 (texel-fetch input-rt (s~ box-uvs :zy) mip)
                 (texel-fetch input-rt (s~ box-uvs :xw) mip)
                 (texel-fetch input-rt (s~ box-uvs :zw) mip)))
               ;; find the max depth
               (max-depth (max (max (max (x depth) (y depth))
                                    (z depth))
                               (w depth))))
          (when (<= min-z max-depth)
            ;; WRITE OUT TO PER INST BUFFERS \o/
            (+ 1 2))))))
  (values))

;; A disadvantage of this approach is that since Gather does not
;; support mip level selection you will have to create a
;; different shader/rendertarget view per mip level and bind
;; them successively during downsampling.

;;------------------------------------------------------------


(defun-g test-compute-func (&uniform
                            (woop ssbo-test-data :ssbo))
  (declare (local-size :x 10 :y 20 :z 3))
  (with-slots (vals) woop
    (atomic-add (aref vals 0) 1))
  (values))


(defun-g test-compute-func2 (&uniform
                             (woop some-more-data :ssbo)
                             &shared
                             (foo :int))
  (declare (local-size :x 10 :y 1 :z 1))
  (with-slots (vals) woop
    (setf (aref vals 0)
          (v! gl-global-invocation-id 0)))
  (values))

(defpipeline-g test-compute-pline ()
  :compute test-compute-func)

(defpipeline-g test-compute-pline2 ()
  :compute test-compute-func2)

(defun test-compute ()
  (let* ((data (make-gpu-array
                nil :dimensions 1
                :element-type 'ssbo-test-data))
         (ssbo (make-ssbo data)))
    (unwind-protect
         (progn
           (map-g #'test-compute-pline
                  (make-compute-space 100)
                  :woop ssbo)
           (wait-on-gpu-fence (make-gpu-fence))
           (with-gpu-array-as-c-array (c-arr data)
             (aref-c
              (ssbo-test-data-vals (aref-c c-arr 0))
              0)))
      (free ssbo)
      (free data))))

(defun test-compute2 ()
  (let* ((data (make-gpu-array
                nil :dimensions 1
                :element-type 'some-more-data))
         (ssbo (make-ssbo data)))
    (unwind-protect
         (progn
           (map-g #'test-compute-pline2
                  (make-compute-space 100)
                  :woop ssbo)
           (wait-on-gpu-fence (make-gpu-fence))
           (with-gpu-array-as-c-array (c-arr data)
             (aref-c
              (some-more-data-vals (aref-c c-arr 0))
              0)))
      (free ssbo)
      (free data))))
