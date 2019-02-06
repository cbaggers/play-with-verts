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
    (* point-light-strength
       (attenuate (length vec-to-light))
       (plight-color light))))

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
            tbn)))

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

(defun-g frag-stage ((frag-normal :vec3)
                     (pos :vec3)
                     (uv :vec2)
                     (tbn :mat3)
                     &uniform
                     (albedo :sampler-2d)
                     (normal-map :sampler-2d)
                     (now :float)
                     (lights light-set :ubo)
                     (mult :float))
  (let* (;; process inputs
         (normal (normalize frag-normal))
         (norm-from-map (norm-from-map normal-map uv))
         ;;
         (albedo (gamma-correct (s~ (texture albedo uv) :xyz)))
         ;;
         (ambient (vec3 *ambient*))
         (diffuse-power (vec3 0.0))
         ;;
         (normal (* tbn norm-from-map))
         (now (* 2.5 now)))
    ;;
    (if (> mult 1.01)
        (setf diffuse-power (vec3 1))
        (with-slots (plights count) lights
          (dotimes (i count)
            (incf diffuse-power
                  (clamp
                   (calc-light pos
                               normal
                               (aref plights i))
                   0 1)))))
    ;;
    (let* ((light-amount (+ ambient diffuse-power))
           (color (* albedo light-amount)))
      (prep-final-color color))))

(defun-g prep-final-color ((color :vec3))
  (let* ((final-color (tone-map-uncharted2
                       color *exposure* 2f0))
         (luma (rgb->luma-bt601 final-color)))
    (v! final-color luma)))

;;------------------------------------------------------------

(defpipeline-g thing-pipeline ()
  (thing-vert-stage g-pnt tb-data)
  (frag-stage :vec3 :vec3 :vec2 :mat3))

(defpipeline-g assimp-thing-pipeline ()
  (assimp-vert-stage assimp-mesh)
  (frag-stage :vec3 :vec3 :vec2 :mat3))


;;------------------------------------------------------------

(defun-g tile-frag-stage ((frag-normal :vec3)
                          (pos :vec3)
                          (uv :vec2)
                          (tbn :mat3)
                          &uniform
                          (albedo :sampler-2d)
                          (normal-map :sampler-2d)
                          (now :float)
                          (lights light-set :ubo)
                          (mult :float)
                          (cutaway-height :float))
  (let* (;; process inputs
         (normal (normalize frag-normal))
         (norm-from-map (norm-from-map normal-map uv))
         ;;
         (albedo (gamma-correct (s~ (texture albedo uv) :xyz)))
         ;;
         (ambient (vec3 *ambient*))
         (diffuse-power (vec3 0.0))
         ;;
         (normal (* tbn norm-from-map))
         (now (* 2.5 now)))
    ;;
    (cond
      ((> (y pos) cutaway-height)
       (discard))
      ((> mult 1.01)
       (setf diffuse-power (vec3 1)))
      (t
       (with-slots (plights count) lights
         (dotimes (i count)
           (incf diffuse-power
                 (clamp
                  (calc-light pos
                              normal
                              (aref plights i))
                  0 1))))))
    ;;
    (let* ((light-amount (+ ambient diffuse-power))
           (color (* albedo light-amount)))
      (prep-final-color color))))

(defpipeline-g tile-pipeline ()
  (thing-vert-stage g-pnt tb-data)
  (tile-frag-stage :vec3 :vec3 :vec2 :mat3))


(defun-g tile-fake-top-frag-stage
    ((frag-normal :vec3)
     (pos :vec3)
     (uv :vec2)
     (tbn :mat3)
     &uniform
     (albedo :sampler-2d)
     (normal-map :sampler-2d)
     (now :float)
     (lights light-set :ubo)
     (mult :float)
     (cam-pos-world :vec3)
     (cutaway-height :float)
     (fake-top-sampler :sampler-2d))
  ;;
  (if (> (y pos) cutaway-height)
      (discard)
      (let* ((y-diff (- (y cam-pos-world) (y pos)))
             (full-ray (- pos cam-pos-world))
             (y-to-cutaway (- (y cam-pos-world)
                              cutaway-height))
             (scaled-ray (* (/ full-ray y-diff)
                            y-to-cutaway))
             (top-pos (+ cam-pos-world scaled-ray))
             (uv (* 0.1 (s~ top-pos :xz)))
             ;;
             (albedo (gamma-correct
                      (s~ (texture fake-top-sampler uv) :xyz)))
             (ambient (vec3 *ambient*))
             (diffuse-power (vec3 0.0))
             (normal (v! 0 1 0)))
        (with-slots (plights count) lights
          (dotimes (i count)
            (incf diffuse-power
                  (clamp
                   (calc-light top-pos
                               normal
                               (aref plights i))
                   0 1))))
        (let* ((light-amount (+ ambient diffuse-power))
               (color (* albedo light-amount)))
          (prep-final-color color)))))

(defpipeline-g tile-fake-top-pipeline ()
  (thing-vert-stage g-pnt tb-data)
  (tile-fake-top-frag-stage :vec3 :vec3 :vec2 :mat3))
