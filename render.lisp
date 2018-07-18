(in-package #:play-with-verts)

;;------------------------------------------------------------

;; We will use this function as our vertex shader
(defun-g some-vert-stage ((vert g-pnt)
                          &uniform
                          (model->world :mat4)
                          (world->view :mat4)
                          (view->clip :mat4))
  (let* ((pos (pos vert))
         (normal (norm vert))
         (uv (tex vert))
         (model-pos (v! pos 1))
         (world-pos (* model->world model-pos))
         (view-pos (* world->view world-pos))
         (world-norm (* (m4:to-mat3 model->world) normal))
         (clip-pos (* view->clip view-pos)))

    (values clip-pos
            world-norm
            (s~ world-pos :xyz)
            uv)))

(defun-g lin-attenuate ((dist :float))
  (/ 1f0 dist))

(defun-g attenuate ((dist :float))
  (/ 1f0 (* dist dist)))

(defun-g gamma-correct ((color :vec3))
  (expt color (vec3 2.2)))

(defun-g gamma-encode ((color :vec3))
  (expt color (vec3 (/ 1.0 2.2))))

(defun-g some-frag-stage ((frag-normal :vec3)
                          (world-pos :vec3)
                          (uv :vec2)
                          &uniform
                          (albedo :sampler-2d)
                          (alt-sam :sampler-2d)
                          (now :float))
  (let* ((frag-normal (normalize frag-normal))
         (albedo (gamma-correct (s~ (texture albedo uv) :xyz)))
         (ambient 0.0)
         (vec-to-light (- (v! 0 4 0) world-pos))
         (dir-to-light (normalize vec-to-light))
         (point-light-strength 400.8)
         (final-light-strength (* point-light-strength
                                  (attenuate (length vec-to-light))))
         (diffuse (* (saturate (dot dir-to-light frag-normal))
                     final-light-strength))
         (light-amount (+ ambient diffuse))
         (color (s~ (* albedo light-amount) :xyz) 0 ))
    (v! (gamma-encode color) 0)))

(defpipeline-g some-pipeline ()
  (some-vert-stage g-pnt)
  (some-frag-stage :vec3 :vec3 :vec2))

(defvar *alt-sampler* nil)

;;------------------------------------------------------------

(defstruct-g plight
  (pos :vec3)
  (color :vec3)
  (strength :float))

(defstruct-g light-set
  (plights (plight 3)))

(defun make-lights ())
