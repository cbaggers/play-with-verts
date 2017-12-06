(in-package #:play-with-verts)

;;------------------------------------------------------------

(defstruct-g pdata
  (pos :vec3)
  (vel :vec3)
  (target :vec3)
  (life :float))

;;------------------------------------------------------------

(defvar *sphere-data* nil)

;;------------------------------------------------------------

(defvar *first-pass-fbo* nil)
(defvar *position-tex* nil)
(defvar *normal-tex* nil)
(defvar *albedo-tex* nil)
(defvar *position-sam* nil)
(defvar *normal-sam* nil)
(defvar *albedo-sam* nil)

(defun make-defer-fbo ()
  (progn;;unless *first-pass-fbo*
    (let ((dim (viewport-dimensions (current-viewport))))
      (setf *position-tex*
            (make-texture nil :dimensions dim :element-type :vec3))
      (setf *normal-tex*
            (make-texture nil :dimensions dim :element-type :vec3))
      (setf *albedo-tex*
            (make-texture nil :dimensions dim :element-type :vec3))
      (setf *position-sam*
            (sample *position-tex* :wrap :clamp-to-edge))
      (setf *normal-sam*
            (sample *normal-tex* :wrap :clamp-to-edge))
      (setf *albedo-sam*
            (sample *albedo-tex* :wrap :clamp-to-edge))
      (setf *first-pass-fbo*
            (make-fbo (list 0 *position-tex*)
                      (list 1 *normal-tex*)
                      (list 2 *albedo-tex*)
                      (list :d :dimensions dim))))))

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
                          &uniform
                          (albedo :sampler-2d))
  (values
   frag-pos
   frag-normal
   (texture albedo uv)))

;; The pipeline itself, we map-g over this to draw stuff
(defpipeline-g first-pass ()
  (some-vert-stage g-pnt pdata)
  (some-frag-stage :vec3 :vec3 :vec2))

;;------------------------------------------------------------

(defun-g attenuation-fudged ((frag-pos :vec3)
                             (light-pos :vec3)
                             (max :float))
  (let* ((light-dist (length (- light-pos frag-pos))))
    (/ 1.5 (* light-dist light-dist 0.04))))

(defun-g attenuation-fudged ((frag-pos :vec3)
                             (light-pos :vec3)
                             (max :float))
  (let* ((light-dist (length (- light-pos frag-pos))))
    (smoothstep 1 0 (/ light-dist max))))

(defun-g calc-diffuse ((frag-pos :vec3)
                       (frag-normal :vec3)
                       (light-pos :vec3)
                       (light-color :vec3)
                       (light-size :float))
  (let* (;; diffuse color is the cosine of the angle between the
         ;; light and the normal. As both the vectors are
         ;; normalized we can use the dot-product to get this.
         (vec-to-light (- light-pos frag-pos))
         (dir-to-light (normalize vec-to-light))
         (diffuse (saturate (dot dir-to-light frag-normal)))
         ;;
         (attenuation-fudged (attenuation-fudged frag-pos
                                                 light-pos
                                                 light-size))
         (col (* (vec3 (* diffuse attenuation-fudged))
                 light-color
                 1.4)))
    col))

(defun-g light-sphere-vert ((vert g-pnt)
                            &uniform
                            (world->view :mat4)
                            (view->clip :mat4)
                            (ldata light-data :ubo))
  (let* ((data (aref (light-data-pos ldata)
                     gl-instance-id))
         (light-pos (v! (s~ data :xyz) 0))
         (light-size (* 1.5 (w data)))
         (light-color (v! (* 0.3 (sin (+ 0.1 gl-instance-id)))
                          (cos (+ 0.2 gl-instance-id))
                          (sin (+ 0.3 gl-instance-id))))
         ;; Unpack the data from our vert
         ;; (pos & normal are in model space)
         (pos (* (pos vert) light-size))
         (normal (norm vert))
         (uv (tex vert))

         ;; model space to world space.
         ;; We don't want to translate the normal, so we
         ;; turn the mat4 to a mat3
         (model-pos (v! pos 1))
         (world-pos (+ model-pos light-pos))
         (world-norm normal)

         ;; world space to view space
         (view-pos (* world->view world-pos))

         ;; view space to clip space
         (clip-pos (* view->clip view-pos)))

    ;; return the clip-space position and the 3 other values
    ;; that will be passed to the fragment shader
    (values clip-pos
            (s~ light-pos :xyz)
            light-color
            light-size)))


;; float specularStrength = 0.5;
;; vec3 viewDir = normalize(viewPos - FragPos);
;; vec3 reflectDir = reflect(-lightDir, norm);
;; float spec = pow(max(dot(viewDir, reflectDir), 0.0), 32);
;; vec3 specular = specularStrength * spec * lightColor;


(defun-g deferred-shading-frag ((light-pos :vec3)
                                (light-color :vec3)
                                (light-size :float)
                                &uniform
                                (vp-size :vec2)
                                (pos-sampler :sampler-2d)
                                (normal-sampler :sampler-2d)
                                (albedo-sampler :sampler-2d)
                                (cam-pos :vec3))
  (let* ((uv (/ (s~ gl-frag-coord :xy) vp-size))
         ;; we will multiply with color with the light-amount
         ;; to get our final color
         (frag-pos (s~ (texture pos-sampler uv) :xyz))
         (frag-normal (s~ (texture normal-sampler uv) :xyz))
         (object-color (texture albedo-sampler uv))

         ;; We need to normalize the normal because the linear
         ;; interpolation from the vertex shader will have
         ;; shortened it
         (frag-normal (normalize frag-normal))

         ;; ambient color is the same from all directions
         (ambient (vec3 0.0))

         (diffuse-col (calc-diffuse frag-pos
                                    frag-normal
                                    light-pos
                                    light-color
                                    light-size))

         ;; (view-dir (normalize (- cam-pos frag-pos)))
         ;; (light-dir (normalize (- light-pos frag-pos)))
         ;; (reflect-dir (reflect (- light-dir) frag-normal))
         ;; (spec (pow (max (dot view-dir reflect-dir) 0.0) 32))
         ;; (specular (* 0.02 spec (- (vec3 1f0) light-color)))

         ;; The final light amount is the sum of the different
         ;; components
         (light-amount (+ ambient diffuse-col)))

    ;; And we multipy with the object color. This means that 0
    ;; light results in no color, and 1 light results in full
    ;; color. Cool!
    (* object-color (v! light-amount 1))))

(defpipeline-g volume-pass ()
  :vertex (light-sphere-vert g-pnt)
  :fragment (deferred-shading-frag :vec3 :vec3 :float))

(defun-g albedo-frag ((uv :vec2)
                      &uniform
                      (pos-sampler :sampler-2d)
                      (normal-sampler :sampler-2d)
                      (albedo-sampler :sampler-2d))
  (let* ((frag-pos (s~ (texture pos-sampler uv) :xyz))
         (frag-normal (s~ (texture normal-sampler uv) :xyz))
         (object-color (texture albedo-sampler uv))

         (frag-normal (normalize frag-normal))
         (ambient (vec3 0.03)))
    (s~ (* object-color (v! ambient 1)) :xyz)))

(defpipeline-g albedo-pass (:points)
  :fragment (albedo-frag :vec2))

;;------------------------------------------------------------
