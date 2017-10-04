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
    :initarg :scale :initform 1f0 :accessor scale)))

(defvar *things* nil)

(defmethod get-model->world-space ((thing thing))
  (m4:* (m4:translation (pos thing))
        (q:to-mat4 (rot thing))))

(defmethod draw ((pipeline function) (thing thing))
  (map-g pipeline (buf-stream thing)
         :scale (scale thing)
         :model->world (get-model->world-space thing)
         :albedo (sampler thing)
         :spec-map (specular-sampler thing)))

;;------------------------------------------------------------
;; Floor

(defclass ground (thing)
  ((stream :initform (box 40 1 40))
   (sampler :initform (tex "floor.jpg"))))

(defun make-ground ()
  (push (make-instance 'ground) *things*))

(defmethod update ((thing ground) dt)
  nil)

;;------------------------------------------------------------
;; Foo!

(defclass box (thing)
  ((stream :initform (box 2 2 2))
   (sampler :initform (tex "scratched.jpg"))))

(defun make-box (&optional (pos (v! 0 0 0)))
  (let ((box (make-instance 'box)))
    (setf (pos box) pos)
    (push box *things*)
    box))

(defmethod update ((thing box) dt)
  (setf (y (pos thing)) (+ 9f0 (* 5f0 (sin (+ (position thing *things*) (* 0.1 (now))))))))

;;------------------------------------------------------------

(defclass sphere (thing)
  ((stream :initform (sphere 3))
   (sampler :initform (tex "blue.jpg"))))

(defun make-sphere (&optional (pos (v! 0 0 0)))
  (let ((sphere (make-instance 'sphere)))
    (setf (pos sphere) pos)
    (push sphere *things*)
    sphere))

(defmethod update ((thing sphere) dt)
  (setf (y (pos thing)) (+ 9f0 (* 5f0 (sin (+ (position thing *things*) (* 0.1 (now))))))))
