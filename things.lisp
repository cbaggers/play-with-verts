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
   (sampler :initform (tex "dirt.jpg"))))

(defun make-ground ()
  (push (make-instance 'ground) *things*))

(defmethod update ((thing ground) dt)
  nil)

;;------------------------------------------------------------
;; Foo!

(defvar *self*)
(defvar *god* (make-instance 'actor))

(defclass actor (thing)
  ((stream :initform (box 1 1 1))
   (scale :initform 1f0)))

(define-actor bullet ((:visual "bullet.png")
                      (:sprite-size (20 20))
                      (speed 1))
  (move-forward 0.1)
  (setf x (mouse-x))
  (let ((a (actors-in-range 10)))
    (when (touching-p a)
      (play-sound :bang)
      (die))))

(defmacro as-god (&body body)
  `(let ((*self* *god*))
     ,@body))

(define-actor ship ((:visual "ship.png")
                    (health 10)
                    (bullet nil))
  (setf x (mouse-x))
  ;;(spawn :bullet (v! 0 0) :speed 10)
  (when (gamepad-button-a)
    (setf bullet (spawn :bullet (v! 4 7) :speed 2)))
  (when (touching-p (actors-in-range 10 :alien-bullet))
    (die)))

(defmacro define-actor (name values &body body)
  (let* ((local-vars (remove-if #'keywordp values
                                :key #'first))
         (local-var-names (mapcar #'first local-vars)))
    `(progn
       (defclass ,name (actor)
         ,(loop :for (var-name var-val) :in local-vars :collect
             `(,var-name :initform ,var-val
                         :initarg ,(intern (symbol-name var-name)
                                           :keyword))))
       (defmethod update ((self ,name) dt)
         (symbol-macrolet ((x (x (slot-value self 'pos)))
                           (y (y (slot-value self 'pos))))
           (with-slots ,local-var-names self
             (let ((*self* self))
               ,@body)))))))

;;------------------------------------------------------------

(defun spawn (actor-kind-name pos
              &rest args &key &allow-other-keys)
  (let* ((hack-name (intern (symbol-name actor-kind-name)
                            :play-with-verts))
         (actor (apply #'make-instance hack-name
                       args)))
    (setf (pos actor)
          (v3:+ (pos *self*) (v! (x pos) 1 (y pos))))
    (push actor *things*)
    actor))

(defun die ()
  (setf *things* (remove *self* *things*)))

(defun play-sound (sound-name)
  (declare (ignore sound-name))
  nil)

(defun touching-p (actor/s)
  (declare (ignore actor/s))
  nil)

(defun mouse-x ()
  0f0)

(defun move-forward (distance)
  (let ((distance (float distance 1f0))
        (dir (v3:normalize (q:to-direction (rot *self*)))))
    (v3:incf (pos *self*) (v3:*s dir distance))))

(defun gamepad-button-a ())

(defun actors-in-range (distance &optional actor-kind)
  (declare (ignore distance actor-kind)))

;;------------------------------------------------------------
