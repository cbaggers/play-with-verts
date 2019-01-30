(in-package #:play-with-verts)

;;------------------------------------------------------------
;; Light

(defvar *light-pos* (v! 0 30 -5))

(defvar *current-creature* nil)

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
    :initarg :rot :initform (q:identity) :accessor rot)
   (normals
    :initarg :normals :initform nil :accessor normals)
   (scale
    :initarg :scale :initform 1f0 :accessor scale)))

(defvar *things* nil)

(defmethod get-model->world-space ((thing thing))
  (m4:* (m4:translation (pos thing))
        (q:to-mat4 (rot thing))))

(defmethod draw ((camera camera)
                 (thing thing))
  (map-g #'thing-pipeline (buf-stream thing)
         :model->world (get-model->world-space thing)
         :world->view (get-world->view-space camera)
         :view->clip (projection camera)
         :albedo (sampler thing)
         :now (now)
         :lights *lights*
         :normal-map (or (normals thing)
                         *fallback-normal-map*)
         :scale (scale thing)
         :mult 1.0))

(defmethod los-draw ((camera camera)
                 (thing thing))
  (map-g #'thing-pipeline (buf-stream thing)
         :model->world (get-model->world-space thing)
         :world->view (get-world->view-space camera)
         :view->clip (projection camera)
         :albedo (sampler thing)
         :now (now)
         :lights *lights*
         :normal-map (or (normals thing)
                         *fallback-normal-map*)
         :scale (scale thing)
         :mult 1.0))

(defmethod update ((thing thing) dt) nil)

(defun free-thing (thing)
  (with-slots (stream) thing
    (destructuring-bind (arrs &optional iarr)
        (buffer-stream-gpu-arrays stream)
      (map nil #'free arrs)
      (free iarr)
      (free stream))
    nil))

;;------------------------------------------------------------
;; Box

(defclass box (thing)
  ((stream :initarg :stream)
   (sampler :initform (get-tex "brickwall.jpg"))
   (normals :initform (get-tex "brickwall_normal.jpg"))))

(defun make-box (pos &optional (size (v! 2 2 2)))
  (check-type pos vec3)
  (check-type size vec3)
  (let ((obj (make-instance 'box :stream (box (x size) (y size) (z size)))))
    (setf (pos obj) pos)
    (push obj *things*)))

;;------------------------------------------------------------
;; Ball

(defclass ball (thing)
  ((stream :initarg :stream)
   (sampler :initform (get-tex "brickwall.jpg"))
   (normals :initform (get-tex "brickwall_normal.jpg"))))


(defun make-ball (pos &optional (radius 3))
  (let ((obj (make-instance 'ball :stream (sphere radius))))
    (setf (pos obj) pos)
    (push obj *things*)
    obj))

;;------------------------------------------------------------
;; Assimp thing

(defclass assimp-thing (thing)
  ((stream :initarg :stream)
   (sampler :initform (get-tex "rust.jpg"))))

(defun free-all-assimp-things ()
  (setf *things*
        (loop :for i :in *things*
           :if (typep i 'assimp-thing)
           :do (free-thing i)
           :else :collect i)))

(defmethod draw ((camera camera)
                 (thing assimp-thing))
  (map-g #'assimp-thing-pipeline (buf-stream thing)
         :model->world (get-model->world-space thing)
         :world->view (get-world->view-space camera)
         :view->clip (projection camera)
         :albedo (sampler thing)
         :normal-map (normals thing)
         :now (now)
         :lights *lights*
         :scale (scale thing)
         :mult 1.0))

(defmethod los-draw ((camera camera)
                 (thing assimp-thing))
  (map-g #'assimp-thing-pipeline (buf-stream thing)
         :model->world (get-model->world-space thing)
         :world->view (get-world->view-space camera)
         :view->clip (projection camera)
         :albedo (sampler thing)
         :normal-map (normals thing)
         :now (now)
         :lights *lights*
         :scale (scale thing)
         :mult 1.0))

;;------------------------------------------------------------
