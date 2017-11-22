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

(defvar *ground* nil)

(defclass ground (thing)
  ((stream :initform (box 40 1 40))
   (sampler :initform (tex "dirt.jpg"))))

(defun make-ground ()
  (let ((ground (make-instance 'ground)))
    (setf *ground* ground)))

(defmethod update ((thing ground) dt)
  nil)

;;------------------------------------------------------------
;; Foo!



;;------------------------------------------------------------
