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
         :albedo (sampler thing)))

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
  ((stream :initform (box 2 2 2))
   (sampler :initform (tex "scratched.jpg"))))

(defun make-box ()
  (let ((obj (make-instance 'box)))
    (setf (pos obj) (v! (- (random 30f0) 15f0)
                        (+ 3 (random 20f0))
                        (- (random 30f0) 15f0)))
    (push obj *things*)))

;;------------------------------------------------------------
;; Ball

(defclass ball (thing)
  ((stream :initform (sphere 3))
   (sampler :initform (tex "blue.jpg"))))

(defun make-ball ()
  (let ((obj (make-instance 'ball)))
    (setf (pos obj) (v! (- (random 30f0) 15f0)
                        (+ 3 (random 20f0))
                        (- (random 30f0) 15f0)))
    (push obj *things*)))

;;------------------------------------------------------------
