(in-package #:play-with-verts)

;;------------------------------------------------------------

;; We will use this function as our vertex shader
(defun-g dpart-vert ((vert g-pnt)
                     &uniform (now :float)
                     (scale :float)
                     (model->world :mat4)
                     (world->view :mat4)
                     (view->clip :mat4)
                     (particles :sampler-2d)
                     (ptex-size :vec2))
  (let* (;; Unpack the data from our vert
         ;; (pos & normal are in model space)
         (idx (ivec2 (int (mod gl-instance-id (x ptex-size)))
                     (int (floor (/ gl-instance-id (x ptex-size))))))
         (particle-data (texel-fetch particles idx 0))
         (particle-pos (s~ particle-data :xy))
         (particle-pos4 (v! (x particle-pos) 0 (y particle-pos) 0))
         ;;
         (pos (* (pos vert) scale))
         (normal (norm vert))
         (uv (tex vert))

         ;; model space to world space.
         ;; We don't want to translate the normal, so we
         ;; turn the mat4 to a mat3
         (model-pos (v! pos 1))
         (world-pos (+ (* particle-pos4 30)
                       (* model->world model-pos)))
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
  (v! 1 1 1 0.1))

;; The pipeline itself, we map-g over this to draw stuff
(defpipeline-g dpart-pipeline ()
  (dpart-vert g-pnt)
  (dpart-frag :vec3 :vec3 :vec2))

;;------------------------------------------------------------

(defclass particles ()
  ((src :initarg :src :accessor front)
   (dst :initarg :dst :accessor back)
   (src-sampler :initarg :src-sampler
                :accessor src-sampler)
   (dst-sampler :initarg :dst-sampler
                :accessor dst-sampler)))

(defun make-particles-data ()
  (let* ((src (make-texture nil :dimensions '(64 64)
                            :element-type :vec4))
         (dst (make-texture nil :dimensions '(64 64)
                            :element-type :vec4))
         (src-fbo (make-fbo (list 0 src)))
         (dst-fbo (make-fbo (list 0 dst)))
         (src-sampler (sample (attachment-tex src-fbo 0)))
         (dst-sampler (sample (attachment-tex dst-fbo 0))))
    (make-instance 'particles
                   :src src-fbo
                   :dst dst-fbo
                   :src-sampler src-sampler
                   :dst-sampler dst-sampler)))

(defvar *particles* nil)

(defun-g limit ((v :vec2) (max :float))
  (let ((len (length v))
        (n (normalize v)))
    (* n (clamp len 0 max))))

(defun-g part-frag ((uv :vec2)
                    &uniform (dt :float)
                    (particles :sampler-2d)
                    (vec-field :sampler-2d))
  (let* ((particle (texture particles uv))
         (pos (s~ particle :xy))
         (vel (s~ particle :zw))
         (foo (texture vec-field pos))
         (force (* (s~ foo :xy) 0.1))
         (new-pos (+ pos (* vel dt)))
         (new-pos (v! (mod (x new-pos) 1f0)
                      (mod (y new-pos) 1f0)))
         (new-vel (limit (+ vel (* force dt)) 0.1f0)))
    (v! new-pos new-vel)))

(defpipeline-g particle-update-pass ()
  (vert :vec2)
  (part-frag :vec2))

(defun update-particles (delta-time)
  (with-slots (src dst src-sampler dst-sampler) *particles*
    (with-fbo-bound (dst)
      (clear dst)
      (map-g #'particle-update-pass (get-quad-stream-v2)
             :dt delta-time
             :particles src-sampler
             :vec-field *vector-field-sampler*))
    (rotatef src dst)
    (rotatef src-sampler dst-sampler)))

(defun-g reset-frag ((uv :vec2))
  (v! (nineveh.random:rand uv)
      (nineveh.random:rand (* 2 uv))
      0
      0))

(defpipeline-g particle-reset-pass ()
  (vert :vec2)
  (reset-frag :vec2))

(defun reset-particles ()
  (with-slots (src src-sampler) *particles*
    (with-fbo-bound (src)
      (clear src)
      (map-g #'particle-reset-pass (get-quad-stream-v2)))))

;;------------------------------------------------------------
