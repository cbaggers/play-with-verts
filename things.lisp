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

(defmethod update (thing))

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
   (sampler :initform (tex "dirt.jpg"))
   (scale :initform 0.4)))

(defun make-ground ()
  (push (make-instance 'ground) *things*))

(defmethod update ((thing ground))
  (setf (scale thing) 0.4))

;;------------------------------------------------------------
;; LSystemy stuff

(defclass branch (thing)
  ((stream :initform (cylinder 0.1 5.0))
   (sampler :initform (tex "dirt.jpg"))
   (length :initform 5.0 :initarg :length
           :accessor len)))

(defclass draw-head ()
  ((pos :initarg :pos :accessor pos)
   (rot :initarg :rot :accessor rot)
   (mem :initarg :mem :accessor mem)
   (gen :initarg :gen :accessor gen)))

(defun make-branch (&key
                      (pos (v! 0 0 0))
                      (rot (q:identity))
                      (angle 0.0)
                      (length 5.0)
                      (thickness 3.0))
  (let* ((rot (q:normalize
               (q:* rot
                    (q:from-axis-angle
                     (v! 0 0 1) (radians angle)))))
         (rot (q:* rot
                   (q:from-axis-angle
                    (v! 0 1 0) (random 2pi-f))))
         (branch (make-instance
                  'branch
                  :stream (cylinder (* 0.5 thickness)
                                    length)
                  :pos pos
                  :rot rot
                  :length length)))
    (push branch *things*)
    branch))

(defun make-head (pos rot mem gen)
  (make-instance 'draw-head
                 :pos pos
                 :rot rot
                 :mem mem
                 :gen gen))

(defun forward (draw-head)
  (let* ((len-factor 0.9)
         (len (* 8.0 (expt len-factor (gen draw-head))))
         (thick-factor 0.6)
         (thickness (* 3.0 (expt thick-factor (gen draw-head))))
         (branch (make-branch :pos (pos draw-head)
                              :rot (rot draw-head)
                              :angle 0f0
                              :length len
                              :thickness thickness))
         (rot (rot branch))
         (dir (q:rotate (v! 0 1 0) rot)))
    (make-head (v3:+ (pos branch)
                     (v3:*s dir (len branch)))
               rot
               (mem draw-head)
               (+ 1 (gen draw-head)))))

(defun turn-left (draw-head)
  (let* ((rot (q:* (rot draw-head)
                   (q:from-axis-angle (v! 0 0 1) (radians 10f0)))))
    (make-head (pos draw-head)
               rot
               (mem draw-head)
               (gen draw-head))))

(defun turn-right (draw-head)
  (let* ((rot (q:* (rot draw-head)
                   (q:from-axis-angle (v! 0 0 1) (radians -10f0)))))
    (make-head (pos draw-head)
               rot
               (mem draw-head)
               (gen draw-head))))

(defun push-draw-head (draw-head)
  (make-head (pos draw-head)
             (rot draw-head)
             (cons draw-head (mem draw-head))
             (gen draw-head)))

(defun pop-draw-head (draw-head)
  (pop (mem draw-head)))

(defun draw-lsystem (list)
  (reset)
  (let ((state (make-head (v! 0 0 0)
                          (q:identity)
                          nil
                          1)))
    (loop :for symbol :in list :do
       (setf state
             (funcall (or (cdr (assoc symbol *draw-map*))
                          #'identity)
                      state)))))

(defparameter *axion* '(a))

(defparameter *rules*
  '((f f ^ l l l f f v ^ r r r f v)))

;; (f f ^ l l f v f ^ r r f v f)
;; (f f ^ l l l f f v ^ r r r f v)

(defparameter *draw-map*
  (list (cons 'f  'forward)
        (cons 'l  'turn-left)
        (cons 'r  'turn-right)
        (cons '^ 'push-draw-head)
        (cons 'v 'pop-draw-head)))

(defun lsys-substitute (rules symbol)
  (let ((res (or (rest (assoc symbol rules))
                 (list symbol))))
    ;;(format t "~%~a -> ~a" symbol res)
    res))

(defun lsys-run (rules list &optional (n 1))
  (let ((result (loop :for i :in list :append
                   (lsys-substitute rules i))))
    (if (> n 1)
        (lsys-run rules result (- n 1))
        result)))


;;------------------------------------------------------------
