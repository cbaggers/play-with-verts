(in-package #:play-with-verts)

;;------------------------------------------------------------
;; Light

(defvar *light-pos* (v! 0 30 -5))

;;------------------------------------------------------------
;; Things

(defclass thing ()
  ((stream
    :initarg :stream :initform nil :accessor buf-stream)
   (sampler
    :initarg :sampler :initform nil :accessor sampler)
   (specular-sampler
    :initarg :specular :initform nil :accessor specular-sampler)
   (pos
    :initarg :pos :initform (v! 0 0 0) :accessor pos)
   (rot
    :initarg :rot :initform (q:identity) :accessor rot)
   (scale
    :initarg :scale :initform 1f0 :accessor scale)
   (color
    :initarg :color :initform (v! (random 1f0)
                                  (random 1f0)
                                  (random 1f0))
    :accessor color)))

(defvar *things* nil)

(defmethod get-model->world-space ((thing thing))
  (m4:* (m4:translation (pos thing))
        (q:to-mat4 (rot thing))))

(defmethod draw ((thing thing))
  (map-g #'some-pipeline (buf-stream thing)
         :scale (scale thing)
         :model->world (get-model->world-space thing)
         :color (color thing)))

;;------------------------------------------------------------
;; Floor

(defclass ground (thing)
  ((stream :initform (box 40 1 40))
   (sampler :initform (get-tex "dirt.jpg"))))

(defun make-ground ()
  (push (make-instance 'ground) *things*))

(defmethod update ((thing ground) dt)
  nil)

;;------------------------------------------------------------
;; Foo!

(defvar *vcones* nil)
(defvar *per-cone-gpu-data* nil)
(defvar *cone-stream* nil)

(defun init-cones ()
  (unless *vcones*
    (setf *per-cone-gpu-data*
          (make-gpu-array nil :dimensions 1000
                          :element-type 'cone-data)))
  (unless *cone-stream*
    (destructuring-bind (vert index)
        (nineveh.mesh.data.primitives:cone-gpu-arrays
         :radius 20f0
         :height 20f0)
      (setf *cone-stream*
            (make-buffer-stream
             (list vert (cons *per-cone-gpu-data* 1))
             :index-array index)))))

(defclass vcone (thing)
  ((stream :initform *cone-stream*)
   (sampler :initform (get-tex "dirt.jpg"))
   (start-pos :initform (rand-pos) :accessor start-pos)
   (end-pos :initform (rand-pos) :accessor end-pos)))

(defun rand-pos ()
  (v! (- (random 140f0) 70f0)
      0
      (- (random 140f0) 70)))

(defun make-vcone ()
  (let ((res (make-instance 'vcone))
        (pos (rand-pos)))
    (setf (pos res) pos)
    (push res *vcones*)
    res))

(defparameter *tfunc*
  (tlambda (thing)
    (repeat
      (before (seconds 9f0)
        (setf (pos thing)
              (v3:lerp (start-pos thing)
                       (end-pos thing)
                       (easing-f:in-out-back %progress%))))
      (once
       (loop :for cone :in *vcones* :do
          (setf (start-pos cone) (end-pos cone)
                (end-pos cone) (rand-pos)))))))

(defmethod update ((thing vcone) dt)
  (funcall *tfunc* thing))

(defun draw-cones (camera)
  (with-instances (length *vcones*)
    (map-g #'inst-pipeline *cone-stream*
           :world->view (get-world->view-space camera)
           :view->clip (projection
                        camera
                        (* 0.3 (x (viewport-resolution
                                   (current-viewport))))
                        (* 0.3 (y (viewport-resolution
                                   (current-viewport))))))))

;;------------------------------------------------------------
