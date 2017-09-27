(in-package #:play-with-verts)

(defvar *light-fbo* nil)
(defvar *light-sampler* nil)

;;------------------------------------------------------------

;; We will use this function as our vertex shader
(defun-g some-vert-stage ((vert g-pnt)
                          &uniform (now :float)
                          (scale :float)
                          (model->world :mat4)
                          (world->view :mat4)
                          (view->clip :mat4)
                          (world->light :mat4)
                          (light->clip :mat4))
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
         (clip-pos (* view->clip view-pos))

         ;;
         (pos-in-light-space
          (* light->clip (* world->light world-pos))))

    ;; return the clip-space position and the 3 other values
    ;; that will be passed to the fragment shader
    (values
     clip-pos
     (s~ world-pos :xyz)
     world-norm
     uv
     pos-in-light-space)))

(defun-g shadow-factor ((light-sampler :sampler-2d)
                        (pos-in-light-space :vec4))
  (let* ((proj-coords (/ (s~ pos-in-light-space :xyz)
                         (w pos-in-light-space)))
         (proj-coords (+ (* proj-coords 0.5) (vec3 0.5)))
         (light-depth (x (texture light-sampler (s~ proj-coords :xy))))
         (our-depth (z proj-coords))
         (factor (if (> (- our-depth 0.005) light-depth) 0f0 1f0))
         (factor (if (> our-depth 1f0) 0f0 factor)))
    factor))

;; We will use this function as our fragment shader
(defun-g some-frag-stage ((frag-pos :vec3)
                          (frag-normal :vec3)
                          (uv :vec2)
                          (pos-in-light-space :vec4)
                          &uniform (light-pos :vec3)
                          (cam-pos :vec3)
                          (albedo :sampler-2d)
                          (spec-map :sampler-2d)
                          (light-sampler :sampler-2d))
  (let* (;; we will multiply with color with the light-amount
         ;; to get our final color
         (object-color (texture albedo uv))

         ;; We need to normalize the normal because the linear
         ;; interpolation from the vertex shader will have shortened it
         (frag-normal (normalize frag-normal))

         ;; ambient color is the same from all directions
         (ambient 0.2)

         ;; diffuse color is the cosine of the angle between the light
         ;; and the normal. As both the vectors are normalized we can
         ;; use the dot-product to get this.
         (vec-to-light (- light-pos frag-pos))
         (dir-to-light (normalize vec-to-light))
         (diffuse (saturate (dot dir-to-light frag-normal)))

         ;; The specular is similar but we do it between the direction
         ;; our camera is looking and the direction the light will reflect.
         ;; We also raise it to a big power so it's a much smaller spot
         ;; with a quick falloff
         (vec-to-cam (- cam-pos frag-pos))
         (dir-to-cam (normalize vec-to-cam))
         (reflection (normalize (reflect (- dir-to-light) frag-normal)))
         (specular-power (* 4 (x (texture spec-map uv))))
         (specular (* (expt (saturate (dot reflection dir-to-cam))
                            32f0)
                      specular-power))
         (shadow-factor (shadow-factor light-sampler pos-in-light-space))

         ;; The final light amount is the sum of the different components
         (light-amount (+ ambient
                          (* (+ diffuse
                                specular)
                             shadow-factor))))

    ;; And we multipy with the object color. This means that 0 light results
    ;; in no color, and 1 light results in full color. Cool!
    (* object-color light-amount)))

(defun-g light-frag-stage ((frag-pos :vec3)
                           (frag-normal :vec3)
                           (uv :vec2)
                           (pos-in-light-space :vec4)
                           &uniform (light-pos :vec3)
                           (cam-pos :vec3)
                           (albedo :sampler-2d)
                           (spec-map :sampler-2d))
  (values))

;; The pipeline itself, we map-g over this to draw stuff
(defpipeline-g some-pipeline ()
  (some-vert-stage g-pnt)
  (some-frag-stage :vec3 :vec3 :vec2 :vec4))

(defpipeline-g light-pipeline ()
  (some-vert-stage g-pnt)
  (light-frag-stage :vec3 :vec3 :vec2 :vec4))

;;------------------------------------------------------------

(defun upload-uniforms-for-cam (pipeline camera)
  (declare (type function pipeline))
  (map-g pipeline nil
         :light-pos *light-pos*
         :cam-pos (pos camera)
         :now (now)
         :world->view (get-world->view-space camera)
         :view->clip (projection camera)
         :world->light (get-world->view-space *camera-1*)
         :light->clip (projection *camera-1*)))

;;------------------------------------------------------------
