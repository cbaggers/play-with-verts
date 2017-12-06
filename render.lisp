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
  (unless *first-pass-fbo*
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

(defun-g calc-diffuse ((frag-pos :vec3)
                       (frag-normal :vec3)
                       (light-pos :vec3))
  (let* (;; diffuse color is the cosine of the angle between the
         ;; light and the normal. As both the vectors are
         ;; normalized we can use the dot-product to get this.
         (vec-to-light (- light-pos frag-pos))
         (dir-to-light (normalize vec-to-light))
         (light-col (v! 1 0 0))
         (diffuse (saturate (dot dir-to-light frag-normal)))
         ;;
         (fudge 0.001)
         (light-dist (length (- light-pos frag-pos)))
         (attenuation-fudged (/ 1 (* light-dist light-dist fudge))))
    (* (vec3 (* diffuse attenuation-fudged))
       light-col)))

(defun-g deferred-shading-frag ((uv :vec2)
                                &uniform
                                (vp-size :vec2)
                                (pos-sampler :sampler-2d)
                                (normal-sampler :sampler-2d)
                                (albedo-sampler :sampler-2d)
                                (light-pos :vec3)
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
         (ambient (vec3 0.2))

         (diffuse-col (calc-diffuse frag-pos
                                    frag-normal
                                    light-pos))

         ;; The final light amount is the sum of the different
         ;; components
         (light-amount (+ ambient diffuse-col)))

    ;; And we multipy with the object color. This means that 0
    ;; light results in no color, and 1 light results in full
    ;; color. Cool!
    (* object-color (v! light-amount 1))))

(defpipeline-g second-pass (:points)
  :fragment (deferred-shading-frag :vec2))

(defun-g light-sphere-vert ((vert g-pnt)
                            &uniform
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
    (values clip-pos
            (v! 0 0))))

(defpipeline-g volume-pass (:points)
  :vertex (light-sphere-vert g-pnt)
  :fragment (deferred-shading-frag :vec2))

;;------------------------------------------------------------
