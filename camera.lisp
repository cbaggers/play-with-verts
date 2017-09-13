(in-package #:play-with-verts)

;;------------------------------------------------------------

(defclass camera ()
  ((pos :initform (v! -0.43 25.33 43.20)
        :accessor pos)
   (rot :initform (v! 0.97 -0.20 -0.01 0.0)
        :accessor rot)
   (near :initform 1f0
         :accessor near)
   (far :initform 400f0
         :accessor far)))

(defclass orthographic-camera (camera) ())

(defclass perspective-camera (camera)
  ((fov :initform 45f0 :accessor fov)))

(defparameter *camera* (make-instance 'orthographic-camera))
(defparameter *camera-1* (make-instance 'perspective-camera))
(defparameter *current-camera* *camera*)

(defun get-world->view-space (camera)
  (m4:* (q:to-mat4 (q:inverse (rot camera)))
        (m4:translation (v3:negate (pos camera)))))

(defmethod update ((camera camera) dt)
  (let ((factor 10))
    (when (keyboard-button (keyboard) key.lshift)
      (setf factor 20))

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
  (setf (pos cam) (v! -0.43 25.33 43.20)
        (rot cam) (v! 0.97 -0.20 -0.01 0.0))
  cam)

(defmethod projection ((camera perspective-camera) width height)
  (rtg-math.projection:perspective
   width
   height
   (near camera)
   (far camera)
   (fov camera)))

(defmethod projection ((camera orthographic-camera) width height)
  (rtg-math.projection:orthographic
   width
   height
   (near camera)
   (far camera)))

;;------------------------------------------------------------
