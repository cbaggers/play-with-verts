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

(defclass phys-thing (thing)
  ((phys-body :accessor phys-body)))

(defmethod update ((thing phys-thing) dt)
  (setf (pos thing)
        (body-position (phys-body thing)))
  (let ((mat (body-matrix4 (phys-body thing))))
    (setf (rot thing)
          (q:from-mat3 (m4:to-mat3 mat)))))

(defun apply-gravity (body timestep)
  (declare (ignore timestep))
  (let ((mass (body-mass body)))
    (setf (body-force body) (v! 0f0 (* -9.8 mass) 0f0 0f0))))

;;------------------------------------------------------------
;; Floor

(defclass ground (phys-thing)
  ((stream :initform (box 40 1 40))
   (sampler :initform (tex "floor.jpg"))))

(defun make-ground ()
  (with-geometry (geom (make-box-geometry
                        *world* :dimensions (v! 40 1 40)))
    (let* ((obj (make-instance 'ground))
           (body (make-body *world* geom :linear-damping 0f0
                            :mass 0f0)))
      (setf (body-matrix4 body) (m4:translation (pos obj)))
      (setf (phys-body obj) body)
      (push obj *things*)
      obj)))

(defmethod update ((thing ground) dt)
  nil)

;;------------------------------------------------------------
;; Foo!

(defclass box (phys-thing)
  ((stream :initform (box 2 2 2))
   (sampler :initform (tex "scratched.jpg"))))

(defun make-box (&optional (pos (v! 0 0 0)))
  (with-geometry (geom (make-box-geometry *world* :dimensions (v! 2 2 2)))
    (let* ((box (make-instance 'box))
           (body (make-body *world* geom :mass 0.1f0)))
      (setf (pos box) pos)
      (setf (body-force-torque-callback body) #'apply-gravity)
      (setf (body-matrix4 body) (m4:translation (pos box)))
      (setf (phys-body box) body)
      (push box *things*)
      box)))

;;------------------------------------------------------------

(defclass sphere (phys-thing)
  ((stream :initform (sphere 3))
   (sampler :initform (tex "blue.jpg"))))

(defun make-sphere (&optional (pos (v! 0 0 0)))
  (with-geometry (geom (make-sphere-geometry *world* :radius 3f0))
    (let* ((sphere (make-instance 'sphere))
           (body (make-body *world* geom :mass 1f0)))
      (setf (pos sphere) pos)
      (setf (body-force-torque-callback body) #'apply-gravity)
      (setf (body-matrix4 body) (m4:translation (pos sphere)))
      (setf (phys-body sphere) body)
      (push sphere *things*)
      sphere)))

;;------------------------------------------------------------

(defvar *lazer* nil)

(defclass lazer (thing)
  ((stream :initform (cylinder 0.1f0 10f0))
   (sampler :initform (tex "blue.jpg"))))

(defun make-lazer ()
  (let* ((lazer (make-instance 'lazer)))
    (push lazer *things*)
    (setf *lazer* lazer)
    lazer))

(defmethod update ((thing lazer) delta)
  )
