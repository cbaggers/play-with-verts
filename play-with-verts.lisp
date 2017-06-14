(in-package #:play-with-verts)

(defvar *buf-stream* nil)
(defvar *gpu-arr* nil)

(defclass camera ()
  ((pos :initform (v! 0 0 0) :accessor pos)
   (rot :initform (q:identity) :accessor rot)))

(defclass thing ()
  ((pos :initform (v! 0 0 0) :accessor pos)
   (rot :initform (q:identity) :accessor rot)))

(defvar *camera* (make-instance 'camera))
(defvar *camera-1* (make-instance 'camera))

(defvar *things* (loop for i below 40 collect
                      (make-instance 'thing)))

(defun-g some-vert-stage ((vert g-pnt)
                          &uniform (now :float)
                          (model->world :mat4)
                          (world->view :mat4)
                          (view->clip :mat4))
  (let* ((id gl-instance-id)
         (now (+ now id))

         (pos (pos vert))
         (color (+ (pos vert) (v! 0.5 0.5 0.5)))

         ;; model space to world space
         (pos (v! pos 1))
         (pos (* model->world pos))

         ;; world space to view space
         (pos (* world->view pos)))

    (values
     ;; view space to clip space
     (* view->clip pos)
     color)))

(defun-g some-frag-stage ((color :vec3))
  color)

(defpipeline-g some-pipeline ()
  (some-vert-stage g-pnt)
  (some-frag-stage :vec3))

(defun now ()
  (/ (float (get-internal-real-time))
     5000))

(defun get-world->view-space (camera)
  (m4:* (m4:translation (v3:negate (pos camera)))
        (q:to-mat4 (q:inverse (rot camera)))))

(defun get-model->world-space (thing)
  (m4:* (m4:translation (pos thing))
        (q:to-mat4 (rot thing))))

(defun update-thing (thing)
  (with-slots (pos) thing
    (setf (y pos)
          (mod (- (y pos) 0.001) 40f0))))

(defun draw ()
  (step-host)

  (setf (resolution (current-viewport))
        (surface-resolution (current-surface *cepl-context*)))

  (clear)

  (loop :for thing :in *things* :do
     (update-thing thing)
     (map-g #'some-pipeline *buf-stream*
            :now (now)
            :model->world (get-model->world-space thing)
            :world->view (get-world->view-space *camera-1*)
            :view->clip (rtg-math.projection:perspective
                         (x (resolution (current-viewport)))
                         (y (resolution (current-viewport)))
                         0.1
                         30f0
                         60f0)))
  (swap))

(defun init ()
  (unless *buf-stream*
    (destructuring-bind (vert index)
        (nineveh.mesh.data.primitives:cube-gpu-arrays)
      (setf *buf-stream*
            (make-buffer-stream vert :index-array index)))))

(def-simple-main-loop play (:on-start #'init)
  (draw))
