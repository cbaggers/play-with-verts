(in-package #:play-with-verts)

;;------------------------------------------------------------

;; We will use this function as our vertex shader
(defun-g some-vert-stage ((vert g-pnt)
                          &uniform
                          (model->world :mat4)
                          (world->clip :mat4))
  (let* ((pos (pos vert))
         (normal (norm vert))
         (uv (tex vert))
         (model-pos (v! pos 1))
         (world-pos (* model->world model-pos))
         (world-norm (* (m4:to-mat3 model->world) normal))
         (clip-pos (* world->clip world-pos)))

    (values clip-pos world-norm uv)))


(defun-g some-frag-stage ((frag-normal :vec3)
                          (uv :vec2)
                          &uniform
                          (albedo :sampler-2d))
  (let* ((albedo (texture albedo uv))
         (ambient 0.2)
         (dir-to-light (normalize (v! 1 1 1)))
         (diffuse (saturate (dot dir-to-light (normalize frag-normal))))
         (light-amount (+ ambient diffuse)))
    (* albedo light-amount)))


(defpipeline-g some-pipeline ()
  (some-vert-stage g-pnt)
  (some-frag-stage :vec3 :vec2))

;;------------------------------------------------------------
