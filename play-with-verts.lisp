(in-package #:play-with-verts)

(defvar *buf-stream* nil)
(defvar *cube-stream* nil)
(defvar *floor-stream* nil)
(defvar *floor-sampler* nil)
(defvar *alien-sampler* nil)
(defvar *gpu-arr* nil)
(defvar *light-pos* (v! 0 30 -5))
(defvar *albedo-sampler* nil)
(defvar *specular-sampler* nil)

;;------------------------------------------------------------

(defclass camera ()
  ((pos :initform (v! 0 15 20) :accessor pos)
   (rot :initform (q! 0.98384374 -0.17902958 0.0 0.0)
        :accessor rot)))

(defvar *camera* (make-instance 'camera))
(defvar *camera-1* (make-instance 'camera))

(defun get-world->view-space (camera)
  (m4:* (q:to-mat4 (q:inverse (rot camera)))
        (m4:translation (v3:negate (pos camera)))))

(defun update-camera (camera)
  (when (keyboard-button (keyboard) key.w)
    (v3:incf (pos camera)
             (v3:*s (q:to-direction (rot camera))
                    (* 10 *delta*))))

  (when (keyboard-button (keyboard) key.s)
    (v3:decf (pos camera)
             (v3:*s (q:to-direction (rot camera))
                    (* 10 *delta*))))

  (when (mouse-button (mouse) mouse.left)
    (let ((move (v2:*s (mouse-move (mouse))
                       0.03)))
      (setf (rot camera)
            (q:normalize
             (q:* (rot camera)
                  (q:normalize
                   (q:from-axis-angle (v! 1 0 0) (- (y move)))
                        ;;(q:from-axis-angle (v! 0 1 0) (- (x move)))
                        )))))))

;;------------------------------------------------------------

(defclass thing ()
  ((stream :initarg :stream :initform nil :accessor buf-stream)
   (sampler :initarg :sampler :initform nil :accessor sampler)
   (pos :initarg :pos :initform (v! 0 0 0) :accessor pos)
   (rot :initarg :rot :initform (q:identity) :accessor rot)))

(defclass bullet (thing)
  ((stream :initform *cube-stream*)))

;; Make a whole bunch of 'thing' instances.
;; Give em random positions & rotations
(defvar *player* nil)
(defvar *things* nil)
(defvar *floor* nil)
(defvar *bullets* nil)

(defun get-model->world-space (thing)
  (m4:* (m4:translation (pos thing))
        (q:to-mat4 (rot thing))))

(defun update-thing (thing)
  (with-slots (pos) thing
    (decf (y pos) (* 8 *delta*))
    (when (< (y pos) -2f0)
      (setf (y pos) 40f0))
    (unless (loop :for bullet :in *bullets* :never
               (< (v3:length (v3:- (pos bullet) (pos thing)))
                  1.2))
      (setf *things* (remove thing *things*)))))

(defun update-bullet (bullet)
  (with-slots (pos) bullet
    (incf (y pos) (* 16 *delta*))
    (when (> (y pos) 50f0)
      (setf *bullets*
            (remove bullet *bullets*)))))

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

(defun draw-thing (thing camera &optional (scale 1f0))
  ;; Here we just call our pipeline with all the data, we
  ;; should really put some of this in our 'thing' objects
  (map-g #'some-pipeline (buf-stream thing)
         :light-pos *light-pos*
         :cam-pos (pos camera)
         :now (now)
         :scale scale
         :model->world (get-model->world-space thing)
         :world->view (get-world->view-space camera)
         :view->clip (rtg-math.projection:perspective
                      (x (resolution (current-viewport)))
                      (y (resolution (current-viewport)))
                      0.1
                      200f0
                      60f0)
         :albedo (sampler thing)
         :spec-map *specular-sampler*))

(defvar *fps* 0)
(defvar *fps-wip* 0)
(defvar *stepper* (make-stepper (seconds 1)))
(defvar *delta* 1)
(defvar *can-fire* t)

(defun draw ()
  (incf *fps-wip*)
  (when (funcall *stepper*)
    (setf *fps* *fps-wip*
          *fps-wip* 0))
  (setf *delta* (min 0.5 (/ 1.0 *fps*)))

  ;; tell the host to pump all the events
  (step-host)

  ;; foooooo
  (let ((pos (gamepad-2d (gamepad) 0)))
    (setf (pos *player*)
          (v3:*s (v! (x pos) 0 (- (y pos)))
                 10.0))
    (if (gamepad-button (gamepad) 0)
        (when *can-fire*
          (setf *can-fire* nil)
          (push (make-instance 'bullet
                               :pos (pos *player*)
                               :sampler *alien-sampler*)
                *bullets*))
        (setf *can-fire* t)))

  ;; update camera

  (update-camera *camera*)

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

  (loop :for bullet :in *bullets* :do
     (update-bullet bullet)
     (draw-thing bullet *camera* 0.4))

  (draw-thing *player* *camera*)
  (draw-thing *floor* *camera*)

  ;; display what we have drawn
  (swap)
  (decay-events))

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
  ;; The data for the floor
  (unless *floor-stream*
    (destructuring-bind (vert index)
        (nineveh.mesh.data.primitives:box-gpu-arrays
         :width 40 :depth 40)
      (setf *floor-stream*
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
             :play-with-verts "container-specular.png")))))

  (unless *things*
    (loop for i below 40 collect
         (make-instance
          'thing
          :sampler *albedo-sampler*
          :stream *buf-stream*
          :pos (v! (- (random 20) 10)
                   (+ 10 (random 40))
                   (- (random 20) 10))
          :rot (q:from-fixed-angles-v3
                (v! (- (random 20f0) 10)
                    (random 40f0)
                    (- (random 20f0) 10))))))

  (unless *player*
    (setf *player*
          (make-instance
           'thing
           :sampler *albedo-sampler*
           :stream *cube-stream*
           :pos (v! 0 0 0)
           :rot (q:identity))))

  (unless *floor*
    (setf *floor*
          (make-instance
           'thing
           :sampler *floor-sampler*
           :stream *floor-stream*
           :pos (v! 0 0 0)
           :rot (q:identity)))))

(def-simple-main-loop play (:on-start #'init)
  (draw))
