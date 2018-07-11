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
            uv
            (/ (- (z view-pos)) 100f0))))


(defun-g some-frag-stage ((frag-normal :vec3)
                          (world-pos :vec3)
                          (uv :vec2)
                          (lin-depth :float)
                          &uniform
                          (albedo :sampler-2d)
                          (alt-sam :sampler-2d)
                          (now :float))
  (let* ((albedo (texture albedo uv))
         (ambient 0.2)
         (dir-to-light (normalize (v! 1 1 1)))
         (diffuse (saturate
                   (dot dir-to-light
                        (normalize frag-normal))))
         (light-amount (+ ambient diffuse)))
    (v! (s~ (* albedo light-amount) :xyz)
        lin-depth)))

(defpipeline-g some-pipeline ()
  (some-vert-stage g-pnt)
  (some-frag-stage :vec3 :vec3 :vec2 :float))

(defvar *alt-sampler* nil)

;;------------------------------------------------------------
