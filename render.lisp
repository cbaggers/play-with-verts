(in-package #:play-with-verts)

;;------------------------------------------------------------

(defstruct-g pdata
  (pos :vec3)
  (vel :vec3)
  (target :vec3)
  (life :float))

;;------------------------------------------------------------

;; We will use this function as our vertex shader
(defun-g some-vert-stage ((vert g-pnt)
                          (inst-data pdata)
                          &uniform (now :float)
                          (scale :float)
                          (model->world :mat4)
                          (world->view :mat4)
                          (view->clip :mat4))
  (let* (;; Unpack the data from our vert
         ;; (pos & normal are in model space)
         (pos (+ (* (pos vert) scale)
                 (pdata-pos inst-data)))
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
(defun-g some-frag-stage ((frag-pos :vec3)
                          (frag-normal :vec3)
                          (uv :vec2)
                          &uniform (light-pos :vec3)
                          (cam-pos :vec3)
                          (albedo :sampler-2d)
                          (spec-map :sampler-2d))
  (let* (;; we will multiply with color with the light-amount
         ;; to get our final color
         (object-color (texture albedo uv))

         ;; We need to normalize the normal because the linear
         ;; interpolation from the vertex shader will have
         ;; shortened it
         (frag-normal (normalize frag-normal))

         ;; ambient color is the same from all directions
         (ambient (vec3 0.2))

         ;; diffuse color is the cosine of the angle between the
         ;; light and the normal. As both the vectors are
         ;; normalized we can use the dot-product to get this.
         (vec-to-light (- light-pos frag-pos))
         (dir-to-light (normalize vec-to-light))
         (light-col (v! 1 0 0))
         (diffuse (saturate (dot dir-to-light frag-normal)))
         ;;
         (fudge 0.001)
         (light-dist (length (- light-pos frag-pos)))
         (attenuation-fudged (/ 1 (* light-dist light-dist fudge)))
         (diffuse-col (* (vec3 (* diffuse attenuation-fudged))
                         light-col))

         ;; The final light amount is the sum of the different
         ;; components
         (light-amount (+ ambient diffuse-col)))

    ;; And we multipy with the object color. This means that 0
    ;; light results in no color, and 1 light results in full
    ;; color. Cool!
    (* object-color (v! light-amount 1))))

;; The pipeline itself, we map-g over this to draw stuff
(defpipeline-g first-pass ()
  (some-vert-stage g-pnt pdata)
  (some-frag-stage :vec3 :vec3 :vec2))

;;------------------------------------------------------------

(defun upload-uniforms-for-cam (pipeline camera)
  (declare (type function pipeline))
  (map-g pipeline nil
         :light-pos (pos *ball*)
         :cam-pos (pos camera)
         :now (now)
         :world->view (get-world->view-space camera)
         :view->clip (projection camera)))

;;------------------------------------------------------------
