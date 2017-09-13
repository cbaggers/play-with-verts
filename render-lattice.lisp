(in-package #:play-with-verts)

;;------------------------------------------------------------


(defun-g simple-sample-normals ((func (function (:vec2) :float))
                                (pos :vec2)
                                (offset :vec2))
  (let ((scale 1f0)
        (s0 (funcall func (+ pos (v! (- (x offset))  (- (y offset))))))
        (s1 (funcall func (+ pos (v!             .0  (- (y offset))))))
        (s2 (funcall func (+ pos (v!     (x offset)  (- (y offset))))))
        (s3 (funcall func (+ pos (v! (- (x offset))  .0))))
        (s5 (funcall func (+ pos (v!     (x offset)  .0))))
        (s6 (funcall func (+ pos (v! (- (x offset))  (y offset)))))
        (s7 (funcall func (+ pos (v!             .0  (y offset)))))
        (s8 (funcall func (+ pos (v!     (x offset)  (y offset))))))
    (normalize
     (v! (* scale (- (- s2 (- (+ s0 (+ (* 2 (- s5 s3)) s8)) s6))))
         1f0
         (* scale (- (- s6 (- (+ s0 (+ (* 2 (- s7 s1)) s8)) s2))))))))

(defun-g lattice-vert-stage ((vert g-pnt)
                             &uniform (now :float)
                             (scale :float)
                             (model->world :mat4)
                             (world->view :mat4)
                             (view->clip :mat4)
                             (albedo :sampler-2d)
                             (step :vec2))
  (labels ((bump ((uv :vec2))
             (* 0.5 (perlin-noise (* uv 180))))
           (func ((uv :vec2))
             (+ (* 10 (w (texture albedo uv)))
                (bump uv))))
    (let* (;; Unpack the data from our vert
           ;; (pos & normal are in model space)
           (pos (* (pos vert) scale))
           (uv (tex vert))
           (normal (simple-sample-normals #'func uv step))

           (noise (* 0.05 (bump uv)))
           (height (+ noise (w (texture albedo uv))))
           (pos (+ pos (v! 0 (* 10 height) 0)))

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
       uv))))

;;------------------------------------------------------------

(defun-g lattice-frag-stage ((frag-pos :vec3)
                             (frag-normal :vec3)
                             (uv :vec2)
                             &uniform (light-pos :vec3)
                             (cam-pos :vec3)
                             (albedo :sampler-2d))
  (let* (;; we will multiply with color with the light-amount
         ;; to get our final color
         (object-color (texture albedo uv))

         ;; We need to normalize the normal because the linear
         ;; interpolation from the vertex shader will have shortened it
         (frag-normal (normalize frag-normal))

         ;; ambient color is the same from all directions
         (ambient 0.4)

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

         ;; The final light amount is the sum of the different components
         (light-amount (+ ambient diffuse)))

    ;; And we multipy with the object color. This means that 0 light results
    ;; in no color, and 1 light results in full color. Cool!
    (* object-color light-amount)))

;;------------------------------------------------------------

(defpipeline-g lattice-pipeline ()
  (lattice-vert-stage g-pnt)
  (lattice-frag-stage :vec3 :vec3 :vec2))

;;------------------------------------------------------------
