(in-package #:play-with-verts)

;;------------------------------------------------------------

(defstruct-g (plight :layout :std-140)
  (pos :vec3)
  (color :vec3)
  (strength :float))

(defstruct-g (light-set :layout :std-140)
  (plights (plight 30))
  (count :int))

(defvar *lights* nil)
(defvar *lights-arr* nil)
(defparameter *exposure* 0.3f0)
(defparameter *ambient* 0.0f0)

;;------------------------------------------------------------

(defun-g treat-uvs ((uv :vec2))
  (v! (x uv) (- 1.0 (y uv))))

(defun-g attenuate ((dist :float))
  (/ 1f0 (* dist dist)))

(defun-g gamma-correct ((color :vec3))
  (expt color (vec3 2.2)))

(defun-g gamma-encode ((color :vec3))
  (expt color (vec3 (/ 1.0 2.2))))

(defun-g calc-light ((frag-pos :vec3)
                     (frag-normal :vec3)
                     (light plight))
  (let* ((pos (plight-pos light))
         (vec-to-light (- pos frag-pos))
         (dir-to-light (normalize vec-to-light))
         (point-light-strength
          (* (saturate (dot dir-to-light frag-normal))
             (plight-strength light))))
    (clamp
     (* point-light-strength
        (attenuate (length vec-to-light))
        (plight-color light))
     0 1)))

(defun-g norm-from-map ((normal-map :sampler-2d) (uv :vec2))
  ;; // obtain normal from normal map in range [0,1]
  ;; normal = texture(normalMap, fs_in.TexCoords).rgb;
  ;; // transform normal vector to range [-1,1]
  ;; normal = normalize(normal * 2.0 - 1.0);
  (let* ((norm-from-map (s~ (texture normal-map uv) :xyz))
         (norm-from-map (normalize
                         (- (* norm-from-map 2.0) 1.0))))
    (v! (x norm-from-map)
        (- (y norm-from-map))
        (z norm-from-map))))

;;------------------------------------------------------------

(defun-g vert-stage-common ((pos :vec3)
                            (scale :float)
                            (normal :vec3)
                            (tangent :vec3)
                            (bitangent :vec3)
                            (uv :vec2)
                            (model->world :mat4)
                            (world->view :mat4)
                            (view->clip :mat4))
  (let* ((model-pos (v! (* pos scale) 1))
         (world-pos (* model->world model-pos))
         (view-pos (* world->view world-pos))

         (world-norm (* (m4:to-mat3 model->world) normal))
         (view-norm (* (m4:to-mat3 world->view) world-norm))

         (clip-pos (* view->clip view-pos))
         ;;
         (t0 (normalize
              (s~ (* world->view
                     (* model->world
                        (v! tangent 0)))
                  :xyz)))
         (b0 (normalize
              (s~ (* world->view
                     (* model->world
                        (v! bitangent 0)))
                  :xyz)))
         (n0 (normalize (s~ view-norm :xyz)))
         (tbn (mat3 t0 b0 n0)))
    (values clip-pos
            (s~ view-pos :xyz)
            tbn
            (treat-uvs uv))))

(defun-g thing-vert-stage ((vert g-pnt)
                           (tb tb-data)
                          &uniform
                          (model->world :mat4)
                          (world->view :mat4)
                          (view->clip :mat4)
                          (scale :float))
  (vert-stage-common (pos vert) scale (norm vert)
                     (tb-data-tangent tb) (tb-data-bitangent tb)
                     (treat-uvs (tex vert))
                     model->world world->view view->clip))

(defun-g assimp-vert-stage ((vert assimp-mesh)
                            &uniform
                            (model->world :mat4)
                            (world->view :mat4)
                            (view->clip :mat4)
                            (scale :float))
  (with-slots (pos normal uv tangent bitangent) vert
    (vert-stage-common pos scale normal tangent bitangent uv
                       model->world world->view view->clip)))

(defun-g frag-stage ((view-pos :vec3)
                     (tbn :mat3)
                     (uv :vec2)
                     &uniform
                     (albedo :sampler-2d)
                     (normal-map :sampler-2d)
                     (now :float)
                     (lights light-set :ubo)
                     (mult :float))
  (let* (;;
         (albedo (gamma-correct (s~ (texture albedo uv) :xyz)))
         ;;
         (norm-from-map (norm-from-map normal-map uv))
         (view-norm (normalize (* tbn norm-from-map))))
    (values
     view-pos
     view-norm
     albedo)))

(defun-g prep-final-color ((color :vec3))
  (let* ((final-color (tone-map-uncharted2
                       color *exposure* 2f0))
         (luma (rgb->luma-bt601 final-color)))
    (v! final-color luma)))

;;------------------------------------------------------------

(defpipeline-g thing-pipeline ()
  (thing-vert-stage g-pnt tb-data)
  (frag-stage :vec3 :mat3 :vec2))

(defpipeline-g assimp-thing-pipeline ()
  (assimp-vert-stage assimp-mesh)
  (frag-stage :vec3 :mat3 :vec2))

;;------------------------------------------------------------

(defstruct-g hemi-samples
  (data (:vec3 64)))

(defun-g quad-vert ((vert :vec2))
  (values
   (v! vert 0 1)
   (+ (* vert 0.5) 0.5)))

(defun-g ssao-frag ((tex-coords :vec2)
                    &uniform
                    (pos-tex :sampler-2d)
                    (norm-tex :sampler-2d)
                    (noise-tex :sampler-2d)
                    (samples hemi-samples :ubo)
                    (view->clip :mat4)
                    (screen-res :vec2))
  ;; tile noise texture over screen based on screen dimensions
  ;; divided by noise size
  (let* ((noise-scale (/ screen-res 4))
         ;;
         (view-pos (s~ (texture pos-tex tex-coords) :xyz))
         (normal (s~ (texture norm-tex tex-coords) :xyz))
         (rand-vec (s~ (texture noise-tex
                                (* tex-coords noise-scale))
                       :xyz))
         ;;
         (tangent (normalize
                   (- rand-vec (* normal (dot rand-vec normal)))))
         (bitangent (cross normal tangent))
         (tbn (m3:from-columns tangent bitangent normal))
         ;;
         (kernel-size 64)
         (radius 0.5)
         (bias 0.025)
         (occlusion 0f0))
    (for (i 0) (< i kernel-size) (incf i)
         (let* ((arr (hemi-samples-data samples))
                (sample (+ view-pos
                           (* (* tbn (aref arr i)) radius)))
                (offset (* view->clip (v! sample 1.0)))
                (offset (/ offset (w offset)))
                (screen-space-pos
                 (+ (* 0.5 (s~ offset :xyz)) 0.5))
                (sample-depth
                 (z (texture pos-tex (s~ screen-space-pos :xy))))
                (rdiff (/ radius (abs (- (z view-pos)
                                         sample-depth))))
                (range-check (smoothstep 0 1 rdiff)))
           (incf occlusion
                 (* (if (>= sample-depth (+ (z sample) bias))
                        1f0
                        0f0)
                    range-check))))
    (vec4 (expt (- 1f0 (/ occlusion kernel-size)) 1))))

(defpipeline-g ssao-pipeline ()
  (quad-vert :vec2)
  (ssao-frag :vec2))

(defun-g blur-frag ((tex-coords :vec2)
                    &uniform
                    (ssao-input :sampler-2d))
  (let* ((tex-size (texture-size ssao-input 0))
         (texel-size (/ 1f0 (vec2 (x tex-size) (y tex-size))))
         (result 0f0))
    (for (x -2) (< x 2) (++ x)
         (for (y -2) (< y 2) (++ y)
              (let* ((offset (* (v! x y) texel-size))
                     (tc (+ tex-coords offset)))
                (incf result (x (texture ssao-input tc))))))
    (vec4 (/ result (* 4f0 4f0)) )))

(defpipeline-g blur-pipeline ()
  (quad-vert :vec2)
  (blur-frag :vec2))

;;------------------------------------------------------------

(defun-g compose-frag ((tex-coords :vec2)
                       &uniform
                       (pos-sam :sampler-2d)
                       (albedo-sam :sampler-2d)
                       (norm-sam :sampler-2d)
                       (ssao-sam :sampler-2d)
                       (now :float)
                       (lights light-set :ubo)
                       (world->view :mat4))
  (let* (;; process inputs
         (pos (s~ (texture pos-sam tex-coords)
                  :xyz))
         (normal (s~ (normalize (texture norm-sam tex-coords))
                     :xyz))
         (albedo (s~ (texture albedo-sam tex-coords) :xyz))
         (occlusion (expt (s~ (texture ssao-sam tex-coords) :x)
                          4))
         ;;
         (ambient (vec3 (* *ambient* occlusion)))
         (diffuse-power (vec3 0.0)))

    (with-slots (plights count) lights
      (dotimes (i count)
        (let* ((light (aref plights i))
               (lpos (plight-pos light))
               (tlpos (s~ (* world->view (v! lpos 1)) :xyz))
               (fixed (make-plight
                       tlpos
                       (plight-color light)
                       (plight-strength light))))
          (incf diffuse-power
                (calc-light pos normal fixed)))))

    (let* ((light-amount (+ ambient diffuse-power))
           (color (* light-amount albedo)))
      (prep-final-color color))))

(defpipeline-g compose-pipeline ()
  (quad-vert :vec2)
  (compose-frag :vec2))


(defun-g normals-vert ((vert assimp-mesh)
                       &uniform
                       (model->world :mat4)
                       (world->view :mat4)
                       (view->clip :mat4)
                       (scale :float)
                       (normal-map :sampler-2d))
  (with-slots (pos normal uv tangent bitangent) vert
    (multiple-value-bind (clip-pos
                          view-pos
                          tbn
                          uvs)
        (vert-stage-common pos scale normal tangent bitangent uv
                           model->world world->view view->clip)
      (let* ((norm-from-map (norm-from-map normal-map uv))
             (view-norm (normalize (* tbn norm-from-map))))
        (values
         clip-pos
         (s~ view-norm :xyz))))))

(defun-g normals-geom ((normals (:vec3 3)))
  (declare (output-primitive :kind :line-strip :max-vertices 6))
  (labels ((gen-line ((index :int))
             (let* ((magnitude 3f0)
                    (p0 (gl-position (aref gl-in index)))
                    (p1 (+ p0 (* (v! (aref normals index) 0) magnitude))))
               (setf gl-position p0)
               (emit-vertex)
               (setf gl-position p1)
               (emit-vertex)
               (end-primitive)
               (values))))
    (gen-line 0)
    (gen-line 1)
    (gen-line 2)
    (values)))

(defun-g normals-frag ()
  (v! 1 1 0 1))


(defpipeline-g draw-normals ()
  :vertex (normals-vert assimp-mesh)
  :geometry (normals-geom (:vec3 3))
  :fragment (normals-frag))
