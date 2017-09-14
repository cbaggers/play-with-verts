(in-package #:play-with-verts)

;;------------------------------------------------------------

;; We will use this function as our vertex shader
(defun-g some-vert-stage ((vert g-pnt)
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
(defun-g some-frag-stage ((frag-pos :vec3)
                          (frag-normal :vec3)
                          (uv :vec2)
                          &uniform
                          (color :vec3))
  color)

;; The pipeline itself, we map-g over this to draw stuff
(defpipeline-g some-pipeline ()
  (some-vert-stage g-pnt)
  (some-frag-stage :vec3 :vec3 :vec2))

;;------------------------------------------------------------

(defun upload-uniforms-for-cam (pipeline camera)
  (map-g pipeline nil
         :now (now)
         :world->view (get-world->view-space camera)
         :view->clip (projection
                      camera
                      (* 0.3 (x (viewport-resolution
                                 (current-viewport))))
                      (* 0.3 (y (viewport-resolution
                                 (current-viewport)))))))

;;------------------------------------------------------------

(defstruct-g cone-data
  (pos :vec2 :accessor pos)
  (color :vec3 :accessor color))

;; We will use this function as our vertex shader
(defun-g inst-vert-stage ((vert g-pnt)
                          (per-inst-data cone-data)
                          &uniform (now :float)
                          (model->world :mat4)
                          (world->view :mat4)
                          (view->clip :mat4))
  (let* (;; Unpack the data from our vert
         ;; (pos & normal are in model space)
         (per-inst-pos (pos per-inst-data))
         (pos (pos vert))
         (normal (norm vert))
         (uv (tex vert))

         ;; model space to world space.
         ;; We don't want to translate the normal, so we
         ;; turn the mat4 to a mat3
         (model-pos (v! pos 1))
         (world-pos (+ model-pos
                       (v! (x per-inst-pos)
                           0
                           (y per-inst-pos)
                           0)))
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
     (color per-inst-data))))

;; We will use this function as our fragment shader
(defun-g inst-frag-stage ((color :vec3))
  color)

;; The pipeline itself, we map-g over this to draw stuff
(defpipeline-g inst-pipeline ()
  (inst-vert-stage g-pnt cone-data)
  (inst-frag-stage :vec3))

;;------------------------------------------------------------
