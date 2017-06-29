(in-package #:play-with-verts)

;;------------------------------------------------------------
;; Bullets

(defclass bullet (thing)
  ((stream :initform (box))
   (scale :initform 0.4)))

(defvar *bullets* nil)

(defun update-bullet (bullet)
  (with-slots (pos) bullet
    (incf (y pos) (* 16 *delta*))
    (when (> (y pos) 50f0)
      (setf *bullets*
            (remove bullet *bullets*)))))

;;------------------------------------------------------------
;; Falling things

(defclass falling-thing (thing) ())

(defvar *things* nil)

(defun make-falling-thing ()
  (make-instance
   'thing
   :sampler (tex "container-albedo.png")
   :specular (tex "container-specular.png")
   :stream (sphere)
   :pos (v! (- (random 20) 10)
            (+ 10 (random 40))
            (- (random 20) 10))
   :rot (q:from-fixed-angles-v3
         (v! (- (random 20f0) 10)
             (random 40f0)
             (- (random 20f0) 10)))))

(defun update-falling-thing (thing)
  (with-slots (pos) thing
    (decf (y pos) (* 8 *delta*))
    (when (< (y pos) -2f0)
      (setf (y pos) 40f0))
    (unless (loop :for bullet :in *bullets* :never
               (< (v3:length (v3:- (pos bullet) (pos thing)))
                  1.2))
      (setf *things* (remove thing *things*)))))

;;------------------------------------------------------------
;; Player

(defclass player (thing) ())

(defvar *player* nil)

(defun make-player ()
  (make-instance
   'thing
   :sampler (tex "container-albedo.png")
   :specular (tex "container-specular.png")
   :stream (box)
   :pos (v! 0 0 0)
   :rot (q:identity)))

(defun update-player (player)
  (let ((pos (gamepad-2d (gamepad) 0)))
    (setf (pos player)
          (v3:*s (v! (x pos) 0 (- (y pos)))
                 10.0))
    (if (gamepad-button (gamepad) 0)
        (when *can-fire*
          (setf *can-fire* nil)
          (push (make-instance 'bullet
                               :pos (pos player)
                               :sampler (tex "green.png"))
                *bullets*))
        (setf *can-fire* t))))

;;------------------------------------------------------------
;; Floor

(defclass scenery (thing) ())

(defvar *floor* nil)

(defun make-floor ()
  (setf *floor*
        (make-instance
         'scenery
         :sampler (tex "lava.png")
         :specular (tex "container-specular.png")
         :stream (box 40 1 40)
         :pos (v! 0 -1 0)
         :rot (q:identity))))

;;------------------------------------------------------------
;; Light

(defvar *light-pos* (v! 0 30 -5))

;;------------------------------------------------------------
