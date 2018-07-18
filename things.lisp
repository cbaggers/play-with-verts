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
   (pos
    :initarg :pos :initform (v! 0 0 0) :accessor pos)
   (rot
    :initarg :rot :initform (q:identity) :accessor rot)))

(defvar *things* nil)

(defmethod get-model->world-space ((thing thing))
  (m4:* (m4:translation (pos thing))
        (q:to-mat4 (rot thing))))

(defmethod draw ((pipeline function) (camera camera) (thing thing))
  (map-g pipeline (buf-stream thing)
         :model->world (get-model->world-space thing)
         :world->view (get-world->view-space camera)
         :view->clip (projection camera)
         :albedo (sampler thing)
         :now (now)))

(defmethod update ((thing thing) dt) nil)

;;------------------------------------------------------------
;; Floor

(defclass ground (thing)
  ((stream :initform (box 40 1 40))
   (sampler :initform (tex "floor.jpg"))))

(defun make-ground ()
  (push (make-instance 'ground) *things*))

;;------------------------------------------------------------
;; Box

(defclass box (thing)
  ((stream :initarg :stream)
   (sampler :initform (tex "scratched.jpg"))))

(defun make-box (pos &optional (size (v! 2 2 2)))
  (check-type pos vec3)
  (check-type size vec3)
  (let ((obj (make-instance 'box :stream (box (x size) (y size) (z size)))))
    (setf (pos obj) pos)
    (push obj *things*)))

;;------------------------------------------------------------
;; Ball

(defclass ball (thing)
  ((stream :initarg :stream )
   (sampler :initform (tex "blue.jpg"))))

(defun make-ball (pos &optional (radius 3))
  (let ((obj (make-instance 'ball :stream (sphere radius))))
    (setf (pos obj) pos)
    (push obj *things*)))

;;------------------------------------------------------------
