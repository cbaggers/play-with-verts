(in-package #:play-with-verts)

(defvar *buf-stream* nil)
(defvar *cube-stream* nil)
(defvar *gpu-arr* nil)
(defvar *light-pos* (v! 0 30 -5))
(defvar *albedo-sampler* nil)
(defvar *specular-sampler* nil)

;;------------------------------------------------------------

(defclass camera ()
  ((pos :initform (v! 0 20 0) :accessor pos)
   (rot :initform (q:identity) :accessor rot)))

(defvar *camera* (make-instance 'camera))
(defvar *camera-1* (make-instance 'camera))

(defun get-world->view-space (camera)
  (m4:* (m4:translation (v3:negate (pos camera)))
        (q:to-mat4 (q:inverse (rot camera)))))

;;------------------------------------------------------------

(defclass thing ()
  ((pos :initarg :pos :initform (v! 0 0 0) :accessor pos)
   (rot :initarg :rot :initform (q:identity) :accessor rot)))

;; Make a whole bunch of 'thing' instances.
;; Give em random positions & rotations
(defvar *things*
  (loop for i below 40 collect
       (make-instance
        'thing
        :pos (v3:+ (v! 0 0 -25)
                   (v! (- (random 20) 10)
                       (random 40)
                       (- (random 20) 10)))
        :rot (q:from-fixed-angles-v3
              (v! (- (random 20f0) 10)
                  (random 40f0)
                  (- (random 20f0) 10))))))

(defun get-model->world-space (thing)
  (m4:* (m4:translation (pos thing))
        (q:to-mat4 (rot thing))))

(defun update-thing (thing)
  (with-slots (pos) thing
    (setf (y pos)
          (mod (- (y pos) 0.000) 40f0))))

;;------------------------------------------------------------

;; We will use this function as our vertex shader
(defun-g some-vert-stage ((vert g-pnt)
                          &uniform (now :float)
                          (model->world :mat4)
                          (world->view :mat4)
                          (view->clip :mat4))
  (let* (;; Unpack the data from our vert
         ;; (pos & normal are in model space)
         (pos (pos vert))
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
         ;; interpolation from the vertex shader will have shortened it
         (frag-normal (normalize frag-normal))

         ;; ambient color is the same from all directions
         (ambient 0.1)

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

         ;; The final light amount is the sum of the different components
         (light-amount (+ ambient
                          diffuse
                          specular)))

    ;; And we multipy with the object color. This means that 0 light results
    ;; in no color, and 1 light results in full color. Cool!
    (* object-color light-amount)))

;; The pipeline itself, we map-g over this to draw stuff
(defpipeline-g some-pipeline ()
  (some-vert-stage g-pnt)
  (some-frag-stage :vec3 :vec3 :vec2))

;;------------------------------------------------------------

(defun now ()
  ;; Just some number that increases over time that we use
  ;; in a bunch of places
  (/ (float (get-internal-real-time))
     5000))

(defun draw-thing (thing camera)
  ;; Here we just call our pipeline with all the data, we
  ;; should really put some of this in our 'thing' objects
  (map-g #'some-pipeline *buf-stream*
         :light-pos *light-pos*
         :cam-pos (pos camera)
         :now (now)
         :model->world (get-model->world-space thing)
         :world->view (get-world->view-space camera)
         :view->clip (rtg-math.projection:perspective
                      (x (resolution (current-viewport)))
                      (y (resolution (current-viewport)))
                      0.1
                      30f0
                      60f0)
         :albedo *albedo-sampler*
         :spec-map *specular-sampler*))

(defun draw ()
  ;; tell the host to pump all the events
  (step-host)

  ;; Update the position of our light
  (let ((val (* 10 (now))))
    (setf *light-pos* (v! (* 20 (sin val))
                          20
                          (- (* 20 (cos val)) 14))))

  ;; set the position of our viewport
  (setf (resolution (current-viewport))
        (surface-resolution (current-surface *cepl-context*)))

  ;; clear the default fbo
  (clear)

  ;; render ALL THE *THINGS*
  (loop :for thing :in *things* :do
     (update-thing thing)
     (draw-thing thing *camera*))

  ;; display what we have drawn
  (swap))

(defun init ()
  ;;
  ;; The data for a cube
  (unless *cube-stream*
    (destructuring-bind (vert index)
        (nineveh.mesh.data.primitives:cube-gpu-arrays)
      (setf *cube-stream*
            (make-buffer-stream vert :index-array index))))
  ;;
  ;; The data for a sphere
  (unless *buf-stream*
    (destructuring-bind (vert index)
        (nineveh.mesh.data.primitives:sphere-gpu-arrays)
      (setf *buf-stream*
            (make-buffer-stream vert :index-array index))))
  ;;
  ;; The color for our object
  (unless *albedo-sampler*
    (setf *albedo-sampler*
          (sample
           (dirt:load-image-to-texture
            (asdf:system-relative-pathname
             :play-with-verts "container-albedo.png")))))
  ;;
  ;; Contains the specular power
  (unless *specular-sampler*
    (setf *specular-sampler*
          (sample
           (dirt:load-image-to-texture
            (asdf:system-relative-pathname
             :play-with-verts "container-specular.png"))))))

(def-simple-main-loop play (:on-start #'init)
  (draw))
