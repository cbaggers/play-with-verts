(in-package #:play-with-verts)

;;------------------------------------------------------------

(defclass camera ()
  ((pos :initform (v! -0.43 25.33 43.20)
        :accessor pos)
   (rot :initform (v! 0.97 -0.20 -0.01 0.0)
        :accessor rot)
   (near :initform 1f0
         :accessor near)
   (far :initform 2400f0
        :accessor far)
   (frame-size :initform nil
               :accessor frame-size)))

(defclass orthographic-camera (camera) ())

(defclass perspective-camera (camera)
  ((fov :initform 45f0 :accessor fov :initarg :fov)))

(defparameter *camera-0* (make-instance 'perspective-camera))
(defparameter *camera-1* (make-instance 'orthographic-camera))
(defparameter *camera-2* (make-instance 'perspective-camera
                                        :fov 90f0))
(defparameter *current-camera* *camera-0*)

(defun get-world->view-space (camera)
  (m4:* (q:to-mat4 (q:inverse (rot camera)))
        (m4:translation (v3:negate (pos camera)))))

(defmethod update ((camera camera) dt)
  (let ((factor 90))
    (when (keyboard-button (keyboard) key.lshift)
      (setf factor 150))

    (when (keyboard-button (keyboard) key.w)
      (v3:incf (pos camera)
               (v3:*s (q:to-direction (rot camera))
                      (* factor dt))))

    (when (keyboard-button (keyboard) key.s)
      (v3:decf (pos camera)
               (v3:*s (q:to-direction (rot camera))
                      (* factor dt)))))

  (when (mouse-button (mouse) mouse.left)
    (let ((move (v2:*s (mouse-move (mouse))
                       0.03)))
      (setf (rot camera)
            (q:normalize
             (q:* (rot camera)
                  (q:normalize
                   (q:* (q:from-axis-angle (v! 1 0 0) (- (y move)))
                        (q:from-axis-angle (v! 0 1 0) (- (x move)))))))))))

(defun reset-camera (&optional (cam *current-camera*))
  ;; (setf (pos *current-camera*) (v! 0.0 0.0 32.70001)
  ;;       (rot *current-camera*) (q:identity))
  (setf (pos *current-camera*) (v! 37.338387 45.32499 70.408005)
        (rot *current-camera*) (q! 0.9508852 -0.22257783 0.21247639 0.033620883))
  cam)

(defmethod projection ((camera perspective-camera))
  (let ((fs (or (frame-size camera)
                (viewport-resolution (current-viewport)))))
    (rtg-math.projection:perspective
     (x fs)
     (y fs)
     (near camera)
     (far camera)
     (fov camera))))

(defmethod projection ((camera orthographic-camera))
  (let ((fs (or (frame-size camera)
                (viewport-resolution (current-viewport)))))
    (rtg-math.projection:orthographic
     (x fs)
     (y fs)
     (near camera)
     (far camera))))

;;------------------------------------------------------------
