(in-package #:play-with-verts)

;;------------------------------------------------------------

(defclass thing ()
  (
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

(defun get-model->world-space (thing)
  (m4:* (m4:translation (pos thing))
        (q:to-mat4 (rot thing))))

;;------------------------------------------------------------

(defun upload-uniforms-for-cam (camera)
  (map-g #'some-pipeline nil
         :light-pos *light-pos*
         :cam-pos (pos camera)
         :now (now)
         :world->view (get-world->view-space camera)
         :view->clip (rtg-math.projection:perspective
                      (x (viewport-resolution (current-viewport)))
                      (y (viewport-resolution (current-viewport)))
                      0.1
                      200f0
                      60f0)))

(defun draw-thing (thing)
  ;; Here we just call our pipeline with all the data, we
  ;; should really put some of this in our 'thing' objects
  (map-g #'some-pipeline (buf-stream thing)
         :scale (scale thing)
         :model->world (get-model->world-space thing)
         :albedo (sampler thing)
         :spec-map (specular-sampler thing)))

;;------------------------------------------------------------
