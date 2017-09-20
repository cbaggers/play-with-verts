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

(defmethod draw ((thing thing))
  (map-g #'some-pipeline (buf-stream thing)
         :scale (scale thing)
         :model->world (get-model->world-space thing)
         :albedo (sampler thing)
         :spec-map (specular-sampler thing)))

;;------------------------------------------------------------
;; Floor

(defclass ground (thing)
  ((stream :initform (box 40 1 40))
   (sampler :initform (tex "dirt.jpg"))))

(defun make-ground ()
  (push (make-instance 'ground) *things*))

(defmethod update ((thing ground) dt)
  nil)

;;------------------------------------------------------------
;; Foo!

(defvar *particle-blend-params*
  (make-blending-params))

(defclass ball (thing)
  ((stream :initform (sphere 1f0))
   (sampler :initform (tex "dirt.jpg"))))

(defun make-ball ()
  (push (make-instance 'ball) *things*))

(defmethod update ((thing ball) dt)
  nil)

(defmethod draw ((thing ball))
  (let ((camera *current-camera*)
        (pdata (src-sampler *particles*)))
    (with-blending *particle-blend-params*
      (with-instances 1000
        (map-g #'dpart-pipeline (buf-stream thing)
               :scale 0.03
               :model->world (get-model->world-space thing)
               :albedo (sampler thing)
               :spec-map (specular-sampler thing)
               :light-pos *light-pos*
               :cam-pos (pos camera)
               :now (now)
               :world->view (get-world->view-space camera)
               :view->clip (rtg-math.projection:perspective
                            (x (viewport-resolution (current-viewport)))
                            (y (viewport-resolution (current-viewport)))
                            1f0
                            400f0
                            45f0)
               :particles pdata
               :ptex-size (v! (dimensions (sampler-texture pdata))))))))

;;------------------------------------------------------------
