(in-package #:play-with-verts)

;;------------------------------------------------------------

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

(defun get-model->world-space (thing)
  (m4:* (m4:translation (pos thing))
        (q:to-mat4 (rot thing))))

;;------------------------------------------------------------

(defun draw-thing (thing camera)
  ;; Here we just call our pipeline with all the data, we
  ;; should really put some of this in our 'thing' objects
  (map-g #'some-pipeline (buf-stream thing)
         :light-pos *light-pos*
         :cam-pos (pos camera)
         :now (now)
         :scale (scale thing)
         :model->world (get-model->world-space thing)
         :world->view (get-world->view-space camera)
         :view->clip (rtg-math.projection:perspective
                      (x (resolution (current-viewport)))
                      (y (resolution (current-viewport)))
                      0.1
                      200f0
                      60f0)
         :albedo (sampler thing)
         :spec-map (specular-sampler thing)))

;;------------------------------------------------------------
