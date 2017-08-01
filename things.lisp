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
;; Terrain

(defclass terrain (thing)
  ((stream :initform (latice 512 512 512 512))
   (sampler :initform (tex "dirt-and-water.png"))
   (scale :initform 1f0)
   (state-0 :initform (make-terrain-state) :accessor state-0)
   (state-1 :initform (make-terrain-state) :accessor state-1)))

(defun make-terrain ()
  (push (make-instance 'terrain) *things*))

(defmethod update ((thing terrain) time-delta)
  (erode (first *things*) time-delta))

(defmethod draw ((thing terrain))
  (let ((state (state-0 thing)))
    (map-g #'terrain-pipeline (buf-stream thing)
           :scale (scale thing)
           :model->world (get-model->world-space thing)
           :albedo (sampler thing)
           :height-water-sediment-map (height-water-sediment-map state))
    (draw-state-dbg thing)))

(defun draw-state-dbg (terrain)
  (let ((state (state-0 terrain)))
    (draw-tex-bl (height-water-sediment-map state))
    (draw-tex-br (water-flux-map state))
    ;;(draw-tex-tr (water-velocity-map state))
    ))

(defun draw-dbg ()
  (as-frame
    (draw-state-dbg (first *things*))))

;;------------------------------------------------------------
