(in-package #:play-with-verts)

;;------------------------------------------------------------

(defun-g thing-vert-stage ((vert g-pnt)
                           &uniform
                           (model->world :mat4)
                           (world->view :mat4)
                           (view->clip :mat4)
                           (scale :float))
  (labels ((get-disp ((pos :vec2))
             (* (min
                 (expt (* 4
                          (nineveh.noise:perlin-noise
                           (* pos 0.1)))
                       2)
                 4f0)
                2f0)))
    (let* ((model-pos (v! (* (pos vert) scale) 1))
           (2d-pos (v! (x model-pos) (z model-pos)))
           (ydisp (get-disp 2d-pos))
           (model-pos (v! (x model-pos)
                          ydisp
                          (z model-pos)
                          1))
           (world-pos (* model->world model-pos))
           (view-pos (* world->view world-pos))
           (clip-pos (* view->clip view-pos))
           (tex-norm
            (nineveh.normals:simple-sample-normals
             #'get-disp 2d-pos 0.5f0)))
      (values
       clip-pos
       (s~ world-pos :xyz)
       tex-norm
       (tex vert)))))



(defun-g triplanar-blend ((world-normal :vec3))
  (let* ((abs-norm (abs world-normal))
         ;; Force weights to sum to 1.0
         (max-norm (normalize (max abs-norm 0.00001)))
         (b (+ (x max-norm)
               (y max-norm)
               (z max-norm))))
    (/ max-norm (v! b b b))))

;; good version
(defun-g thing-frag-stage ((world-pos :vec3)
                           (norm :vec3)
                           (uv :vec2)
                           &uniform
                           (sam :sampler-2d)
                           (tex-scale :float))
  (let* ((norm (normalize norm))
         (light-dir (normalize (v! 1 1 1)))
         (light-ammount (clamp (dot norm light-dir) 0 1))
         ;;
         (blending (triplanar-blend norm))
         (x-axis (s~ (texture sam (* (s~ world-pos :yz) tex-scale)) :xyz))
         (y-axis (s~ (texture sam (* (s~ world-pos :xz) tex-scale)) :xyz))
         (z-axis (s~ (texture sam (* (s~ world-pos :xy) tex-scale)) :xyz))
         (col (+ (* x-axis (x blending))
                 (* y-axis (y blending))
                 (* z-axis (z blending)))))
    (vec4 (y blending))
    (* (+ 0.1 light-ammount) col)))

;; bad version
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
       (texture sam (* 6 uv))
       ;;(v! 1 0 0 1)
       )))

(defpipeline-g some-pipeline ()
  (thing-vert-stage g-pnt)
  (thing-frag-stage :vec3 :vec3 :vec2)
  ;;(old-frag-stage :vec3 :vec3 :vec2)
  )

(defun render (camera buffer-stream sampler scale)
  (map-g #'some-pipeline buffer-stream
         :model->world (m4:identity)
         :world->view (get-world->view-space camera)
         :view->clip (projection camera)
         :scale scale
         :tex-scale 0.1f0
         :sam sampler))

;;------------------------------------------------------------
