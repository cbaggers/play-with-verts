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
(defparameter *exposure* 0.1f0)
(defparameter *ambient* 0.0f0)

;;------------------------------------------------------------

(defun-g treat-uvs ((uv :vec2))
  (v! (x uv) (- 1.0 (y uv))))

;; We will use this function as our vertex shader
(defun-g some-vert-stage ((vert g-pnt)
                          (tb tb-data)
                          &uniform
                          (model->world :mat4)
                          (world->view :mat4)
                          (view->clip :mat4)
                          (scale :float))
  (let* ((pos (* (pos vert) scale))
         (normal (norm vert))
         (uv (treat-uvs (tex vert)))
         (model-pos (v! pos 1))
         (world-pos (* model->world model-pos))
         (view-pos (* world->view world-pos))
         (world-norm (* (m4:to-mat3 model->world) normal))
         (clip-pos (* view->clip view-pos))
         ;;
         (t0 (normalize
              (s~ (* model->world
                     (v! (tb-data-tangent tb) 0))
                  :xyz)))
         (b0 (normalize
              (s~ (* model->world
                     (v! (tb-data-bitangent tb) 0))
                  :xyz)))
         (n0 (normalize
              (s~ (* model->world
                     (v! normal 0))
                  :xyz)))
         (tbn (mat3 t0 b0 n0)))
    (values clip-pos
            world-norm
            (s~ world-pos :xyz)
            uv
            tbn)))

(defun-g assimp-vert-stage ((vert assimp-mesh)
                            &uniform
                            (model->world :mat4)
                            (world->view :mat4)
                            (view->clip :mat4)
                            (scale :float))
  (with-slots (pos normal uv tangent bitangent) vert
    (let* ((model-pos (v! (* pos scale) 1))
           (world-pos (* model->world model-pos))
           (view-pos (* world->view world-pos))
           (world-norm (* (m4:to-mat3 model->world) normal))
           (clip-pos (* view->clip view-pos))
           ;;
           (t0 (normalize
                (s~ (* model->world
                       (v! tangent 0))
                    :xyz)))
           (b0 (normalize
                (s~ (* model->world
                       (v! bitangent 0))
                    :xyz)))
           (n0 (normalize
                (s~ (* model->world
                       (v! normal 0))
                    :xyz)))
           (tbn (mat3 t0 b0 n0)))
      (values clip-pos
              world-norm
              (s~ world-pos :xyz)
              (treat-uvs uv)
              tbn))))

(defun-g assimp-norm-vert ((vert assimp-mesh)
                           &uniform
                           (model->world :mat4)
                           (world->view :mat4)
                           (view->clip :mat4)
                           (scale :float))
  (with-slots (pos normal uv) vert
    (let* ((model-pos (v! (* pos scale) 1))
           (world-pos (* model->world model-pos))
           (world-norm (* (m4:to-mat3 model->world) normal))
           (view-pos (* world->view world-pos))
           (view-norm (* (mat4 (m4:to-mat3 world->view)) (v! world-norm 0)))
           (clip-pos (* view->clip view-pos))
           (clip-norm (* view->clip view-norm)))
      (values clip-pos
              (s~ clip-norm :xyz)))))

(defun-g assimp-norm-geom ((normals (:vec3 3)))
  (declare (output-primitive :kind :line-strip :max-vertices 6))
  (labels ((gen-line ((index :int))
             (let* ((magnitude 2)
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

(defun-g assimp-norm-frag ()
  (v! 1 1 0 1))

(defun-g lin-attenuate ((dist :float))
  (/ 1f0 dist))

(defun-g attenuate ((dist :float))
  (/ 1f0 (* dist dist)))

(defun-g gamma-correct ((color :vec3))
  (expt color (vec3 2.2)))

(defun-g gamma-encode ((color :vec3))
  (expt color (vec3 (/ 1.0 2.2))))

(defun-g calc-light ((frag-pos :vec3)
                     (frag-normal :vec3)
                     (light plight)
                     (offset :vec3))
  (let* ((pos (+ (plight-pos light)
                 offset))
         (vec-to-light (- pos frag-pos))
         (dir-to-light (normalize vec-to-light))
         (point-light-strength
          (* (saturate (dot dir-to-light frag-normal))
             (plight-strength light))))
    (* point-light-strength
       (attenuate (length vec-to-light))
       (plight-color light))))

(defun-g frag-stage-with-norms ((frag-normal :vec3)
                                (pos :vec3)
                                (uv :vec2)
                                (tbn :mat3)
                                &uniform
                                (albedo :sampler-2d)
                                (normal-map :sampler-2d)
                                (now :float)
                                (lights light-set :ubo))
  ;; // obtain normal from normal map in range [0,1]
  ;; normal = texture(normalMap, fs_in.TexCoords).rgb;
  ;; // transform normal vector to range [-1,1]
  ;; normal = normalize(normal * 2.0 - 1.0);

  (let* (;; process inputs
         (normal (normalize frag-normal))
         (norm-from-map (s~ (texture normal-map uv) :xyz))
         (norm-from-map (normalize
                         (- (* norm-from-map 2.0) 1.0)))
         (norm-from-map (v! (x norm-from-map)
                            (- (y norm-from-map))
                            (z norm-from-map)))
         ;;
         (albedo (gamma-correct (s~ (texture albedo uv) :xyz)))
         ;;
         (ambient (vec3 *ambient*))
         (diffuse-power (vec3 0.0))
         ;;
         (normal (* tbn norm-from-map))
         (now (* 2.5 now)))
    ;;
    (with-slots (plights count) lights
      (incf diffuse-power
            (calc-light pos
                        normal
                        (aref plights 0)
                        (v! 0
                            (+ 8 (* 5 (sin now)))
                            0)))
      (incf diffuse-power
            (calc-light pos
                        normal
                        (aref plights 1)
                        (v! (+ 8 (* 10 (sin (* 1.5 now))))
                            1
                            0))))
    ;;
    (let* ((light-amount (+ ambient diffuse-power))
           (color (* albedo light-amount))
           (final-color (tone-map-uncharted2
                         color *exposure* 2f0))
           (luma (rgb->luma-bt601 final-color)))
      (v! final-color luma))))

(defpipeline-g some-pipeline-with-norms ()
  (some-vert-stage g-pnt tb-data)
  (frag-stage-with-norms :vec3 :vec3 :vec2 :mat3))

(defpipeline-g assimp-pipeline ()
  (assimp-vert-stage assimp-mesh)
  (frag-stage-with-norms :vec3 :vec3 :vec2 :mat3))

(defpipeline-g assimp-norm-pipeline ()
  :vertex (assimp-norm-vert assimp-mesh)
  :geometry (assimp-norm-geom (:vec3 3))
  :fragment (assimp-norm-frag))
