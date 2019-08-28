(in-package #:play-with-verts)

;;------------------------------------------------------------

(defstruct-g per-inst-data
  (pos :vec3)
  (scale :float)
  (color :vec4))

;;------------------------------------------------------------

(defun-g blit-vert ((vert :vec2))
  (values
   (v! vert 0 1)
   (+ (* vert 0.5) 0.5)))

(defun-g blit-frag ((uv :vec2)
                    &uniform
                    (sam :sampler-2d)
                    (power :float))
  (v! (expt (x (texture sam uv)) power) 0 0 1))

(defpipeline-g blit ()
  (blit-vert :vec2)
  (blit-frag :vec2))

(defun-g blit-lod-frag ((uv :vec2)
                        &uniform
                        (size :vec2)
                        (sam :sampler-2d)
                        (power :float)
                        (lod :int))
  (let ((pos (ivec2 (int (* (x uv) (x size)))
                    (int (* (y uv) (y size))))))
    (v! (expt (x (texel-fetch sam pos lod)) power) 0 0 1)))

(defpipeline-g blit-lod ()
  (blit-vert :vec2)
  (blit-lod-frag :vec2))

(defun blit-it (sampler &optional (power 1f0) mip-level)
  (if mip-level
      (destructuring-bind (x y)
          (gpu-array-dimensions
           (texref (sampler-texture sampler)
                   :mipmap-level mip-level))
        (map-g #'blit-lod (get-quad-stream-v2)
               :sam sampler
               :power power
               :size (v! x y)
               :lod mip-level))
      (map-g #'blit (get-quad-stream-v2)
             :sam sampler
             :power power)))

;;------------------------------------------------------------

(defun-g downscale ((uv :vec2)
                    &uniform
                    (sam :sampler-2d))
  (let* ((depths (texture-gather sam uv))
         (val (max (max (x depths) (y depths))
                   (max (z depths) (w depths)))))
    (values val val)))

(defpipeline-g downscale-pline ()
  (blit-vert :vec2)
  (downscale :vec2))

(defun downscale-it (sampler)
  (map-g #'downscale-pline
         (get-quad-stream-v2)
         :sam sampler))

;;------------------------------------------------------------

(defun-g thing-vert-stage ((vert g-pnt)
                           (inst-data per-inst-data)
                           &uniform
                           (model->world :mat4)
                           (world->view :mat4)
                           (view->clip :mat4)
                           (time :float))
  (with-slots ((offset pos) scale) inst-data
    (let* ((model-pos (v! (* (pos vert) scale) 1))
           (normal (norm vert))
           ;;
           (world-pos (+ model-pos (v! offset 0)))
           (view-pos (* world->view world-pos))
           (clip-pos (* view->clip view-pos)))
      (values
       clip-pos
       (s~ world-pos :xyz)
       normal
       (tex vert)))))

(defun-g old-frag-stage ((world-pos :vec3)
                         (norm :vec3)
                         (uv :vec2)
                         &uniform
                         (sam :sampler-2d)
                         (tex-scale :float))
  (let* ((norm (normalize norm))
         (light-dir (normalize (v! 1 1 1)))
         (light-ammount (clamp (dot norm light-dir) 0 1)))
    (* (+ 0.1 light-ammount)
       (texture sam (* 6 uv)))))

(defpipeline-g occlusion-pipeline ()
  :vertex (thing-vert-stage g-pnt per-inst-data)
  :fragment nil)

(defpipeline-g fart-pipeline ()
  :vertex (thing-vert-stage g-pnt per-inst-data)
  :fragment (old-frag-stage :vec3 :vec3 :vec2))

;;------------------------------------------------------------

(defun populate-occlusion-buffer (camera buffer-stream sampler)
  (declare (ignore sampler))
  (map-g #'occlusion-pipeline buffer-stream
         :model->world (m4:identity)
         :world->view (get-world->view-space camera)
         :view->clip (projection camera)
         ;; :tex-scale 0.5f0
         ;; :sam sampler
         ;;:time (now)
         ))

(defun fart (camera buffer-stream sampler)
  (map-g #'fart-pipeline buffer-stream
         :model->world (m4:identity)
         :world->view (get-world->view-space camera)
         :view->clip (projection camera)
         :tex-scale 0.5f0
         :sam sampler
         :time (now)))
