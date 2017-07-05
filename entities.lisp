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

(defclass falling-thing (thing)
  ((shadow-stream :initform (cylinder 2f0 0.1))))

(defclass pepperoni (falling-thing) ())
(defclass olive (falling-thing) ())

(defvar *falling-things* nil)
(defvar *stuck-things* nil)

(defun make-pepperoni (&optional (random-y t))
  (let ((angle (random 2pi-f))
        (dist (random 23)))
    (make-instance
     'pepperoni
     :sampler (tex "container-albedo.png")
     :specular (tex "container-specular.png")
     :stream (cylinder 2.0 0.1)
     :pos (v! (* (sin angle) dist)
              (if random-y
                  (+ 10 (random 40))
                  40)
              (* (cos angle) dist))
     :rot (q:identity))))

(defun make-olive (&optional (random-y t))
  (let ((angle (random 2pi-f))
        (dist (random 23)))
    (make-instance
     'olive
     :sampler (tex "container-albedo.png")
     :specular (tex "container-specular.png")
     :stream (sphere 1)
     :pos (v! (* (sin angle) dist)
              (if random-y
                  (+ 10 (random 40))
                  40)
              (* (cos angle) dist))
     :rot (q:identity))))

(defun get-shadow-model->world-space (thing)
  (let* ((p (pos thing))
        (p (v! (x p) 0.09 (z p))))
    (m4:* (m4:translation p)
          (q:to-mat4 (rot thing)))))

(defun draw-shadow (thing)
  ;; Here we just call our pipeline with all the data, we
  ;; should really put some of this in our 'thing' objects
  (map-g #'some-pipeline (slot-value thing 'shadow-stream)
         :scale 1f0
         :model->world (get-shadow-model->world-space thing)
         :albedo (tex "shadow.png")
         :spec-map (specular-sampler thing)))

(defun update-falling-thing (thing)
  (with-slots (pos) thing
    (decf (y pos) (* 8 *delta*))
    (when (< (y pos) 0f0)
      (setf (y pos) 0f0)
      (setf *falling-things* (remove thing *falling-things*))

      (push thing *stuck-things*)
      (etypecase thing
        (pepperoni
         (push (make-pepperoni nil) *falling-things*)
         (incf *score*))
        (olive
         (push (make-olive nil) *falling-things*)
         (decf *score* 2)))
      (when (= 0 (mod *score* 10))
        (format t "~%SCORE IS NOW ~a" *score*)))

    (unless (loop :for bullet :in *bullets* :never
               (< (v3:length (v3:- (pos bullet) (pos thing)))
                  1.2))
      (setf *falling-things*
            (remove thing *falling-things*))
      (etypecase thing
        (pepperoni
         (push (make-pepperoni nil) *falling-things*)
         (decf *score* 4)
         (print "NNOOOOO THE MEAT!!"))
        (olive
         (push (make-olive nil) *falling-things*)
         (incf *score* 5)
         (print "DIE OLIVE SCUM!"))))))

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
  (let* ((pos (gamepad-2d (gamepad) 0))
         (gpos (v! (x pos) 0 (- (y pos))))
         (glen (min (v3:length gpos) 1f0))
         (gpos (v3:*s (v3:normalize gpos)
                      glen)))
    (setf (pos player)
          (v3:*s gpos
                 22.0))
    (if (gamepad-button (gamepad) 0)
        (when *can-fire*
          (setf *can-fire* nil)
          (push (make-instance 'bullet
                               :pos (pos player)
                               :sampler (tex "green.png"))
                *bullets*))
        (setf *can-fire* t))))

;;------------------------------------------------------------
;; Pizza

(defvar *pizza-base* nil)

(defclass pizza (thing)
  ((crust-bit
    :initarg :crust-bit :initform nil :accessor crust-bit)))

(defclass crust-bit (thing) ())

(defun make-crust-bit ()
  (make-instance
   'crust-bit
   :scale 2f0
   :stream (sphere)
   :sampler (tex "bread.jpg")))

(defun make-pizza-base ()
  (setf *pizza-base*
        (make-instance
         'pizza
         :pos (v! 0 -1.5 0)
         :stream (cylinder 30f0 1f0)
         :sampler (tex "sauce.png")
         :crust-bit (make-crust-bit))))


(defun draw-pizza (pizza)
  (draw-thing pizza)
  (let* ((bits 50)
         (step (/ 2pi-f bits)))
    (loop :for i :below bits :do
       (setf (pos (crust-bit pizza))
             (v! (* 30 (sin (* i step)))
                 2
                 (* 30 (cos (* i step)))))
       (draw-thing (crust-bit pizza)))))

;;------------------------------------------------------------
;; Light

(defvar *light-pos* (v! 0 30 -5))

;;------------------------------------------------------------
