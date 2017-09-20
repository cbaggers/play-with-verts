(in-package #:play-with-verts)

;;------------------------------------------------------------

;; We will use this function as our vertex shader
(defun-g dpart-vert ((vert g-pnt)
                     &uniform (now :float)
                     (scale :float)
                     (model->world :mat4)
                     (world->view :mat4)
                     (view->clip :mat4))
  (let* (;; Unpack the data from our vert
         ;; (pos & normal are in model space)
         (pos (* (pos vert) scale))
         (normal (norm vert))
         (uv (tex vert))

         ;; model space to world space.
         ;; We don't want to translate the normal, so we
         ;; turn the mat4 to a mat3
         (model-pos (v! pos 1))
         (world-pos (* model->world model-pos))
         (world-norm (* (m4:to-mat3 model->world)
                        normal))

         ;; world space to view space
         (view-pos (* world->view world-pos))

         ;; view space to clip space
         (clip-pos (* view->clip view-pos)))

    ;; return the clip-space position and the 3 other values
    ;; that will be passed to the fragment shader
    (values
     clip-pos
     (s~ world-pos :xyz)
     world-norm
     uv)))

;; We will use this function as our fragment shader
(defun-g dpart-frag ((frag-pos :vec3)
                     (frag-normal :vec3)
                     (uv :vec2)
                     &uniform (light-pos :vec3)
                     (cam-pos :vec3)
                     (albedo :sampler-2d)
                     (spec-map :sampler-2d))
  (v! 1 0 0 0))

;; The pipeline itself, we map-g over this to draw stuff
(defpipeline-g dpart-pipeline ()
  (dpart-vert g-pnt)
  (dpart-frag :vec3 :vec3 :vec2))

;;------------------------------------------------------------
