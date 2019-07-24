(in-package #:play-with-verts)

;;------------------------------------------------------------

(defun-g thing-vert-stage ((vert g-pnt)
                           &uniform
                           (model->world :mat4)
                           (world->view :mat4)
                           (view->clip :mat4)
                           (time :float))
  (labels ((get-disp ((pos :vec2))
             ;;(sin (+ time (x pos)))
             (clamp (* 5.5 (perlin-noise (* 0.5 pos)))
                    -1
                    1.5)))
    (let* ((model-pos3 (pos vert))
           ;;
           (2d-pos (s~ model-pos3 :xz))
           (ydisp (get-disp 2d-pos))
           (sample-offset (/ 1f0 200f0))
           (tex-norm
            (nineveh.normals:simple-sample-normals
             #'get-disp 2d-pos sample-offset 200f0))
           ;;
           (model-pos (v! (x model-pos3)
                          ydisp
                          (z model-pos3)
                          1))
           (world-pos (* model->world model-pos))
           (view-pos (* world->view world-pos))
           (clip-pos (* view->clip view-pos)))
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
  :vertex (thing-vert-stage g-pnt)
  :fragment (thing-frag-stage :vec3 :vec3 :vec2)
  ;; :fragment (old-frag-stage :vec3 :vec3 :vec2)
  )

;;------------------------------------------------------------

(defun-g normals-geom ((world-pos (:vec3 3))
                       (normals (:vec3 3))
                       (uvs (:vec2 3)))
  (declare (output-primitive :kind :line-strip :max-vertices 6))
  (labels ((gen-line ((index :int))
             (let* ((magnitude 0.9)
                    (n (normalize (aref normals index)))
                    (n-scaled (* n magnitude))
                    (p (gl-position (aref gl-in index)))
                    (p0 p)
                    (p1 (+ p (v! n-scaled 0))))
               (emit ()
                     p0
                     (w p))
               (emit ()
                     p1
                     (w p))
               (end-primitive)
               (values))))
    (gen-line 0)
    (gen-line 1)
    (gen-line 2)
    (values)))

(defun-g normals-frag ((hmm :float))
  (v! 1 1 0 1)
  (vec4 hmm))

(defpipeline-g draw-normals ()
  :vertex (thing-vert-stage g-pnt)
  :geometry (normals-geom (:vec3 3) (:vec3 3) (:vec2 3))
  :fragment (normals-frag :float))

;;------------------------------------------------------------

(defun render (camera buffer-stream sampler)
  (map-g #'some-pipeline buffer-stream
         :model->world (m4:identity)
         :world->view (get-world->view-space camera)
         :view->clip (projection camera)
         :tex-scale 0.5f0
         :sam sampler
         ;;:time (now)
         ))

(defun render-norms (camera buffer-stream)
  (map-g #'draw-normals buffer-stream
         :model->world (m4:identity)
         :world->view (get-world->view-space camera)
         :view->clip (projection camera)
         ;;:time (now)
         ))

;;------------------------------------------------------------
