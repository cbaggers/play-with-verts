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
        :accessor far)
   (frame-size :initform nil
               :accessor frame-size)))

(defclass orthographic-camera (camera) ())

(defclass perspective-camera (camera)
  ((fov :initform 45f0 :accessor fov)))

(defparameter *camera* (make-instance 'perspective-camera))
(defparameter *camera-1* (make-instance 'orthographic-camera))
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
                        (q:from-axis-angle (v! 0 1 0) (- (x move))))))))))
  (when (mouse-button (mouse) mouse.right)
    (place-lazer)))

(defun reset-camera (&optional (cam *current-camera*))
  (setf (pos cam) (v! -0.43 25.33 43.20)
        (rot cam) (v! 0.97 -0.20 -0.01 0.0))
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

(defun set-light-cam-to-cam ()
  (setf (pos *camera-1*) (v! (pos *camera*))
        (rot *camera-1*) (v! (rot *camera*)))
  *camera-1*)

;;------------------------------------------------------------

(defun bodged-ray (camera mpos)
  (let* ((fov (fov camera))
         (vsize (viewport-resolution (current-viewport)))
         (x (/ (x mpos) (x vsize)))
         (y (/ (y mpos) (y vsize)))
         (nx (- (- (* x 2f0) 1f0)))
         (ny (- (- (* y 2f0) 1f0)))
         (rot (q:normalize
               (q:* (rot camera)
                     (q:* (q:from-axis-angle (v! 0 0 1)
                                             (* nx (radians fov)))
                          (q:from-axis-angle (v! 1 0 0)
                                             (* ny (radians fov))))))))
    rot))

(defun place-lazer ()
  (unless *lazer*
    (make-lazer))
  (let* ((rot (bodged-ray *camera* (mouse-pos (mouse 0))))
         (dir (q:to-direction rot)))
    (setf (rot *lazer*)
          (q:* (q:from-axis-angle (v! 1 0 0) (radians -90f0))
               rot))
    (setf (pos *lazer*)
          (v3:+ (v! (pos *camera*))
                (v! 0 -1 0)))
    (let ((hit nil))
      (world-ray-cast *world*
                      (pos *camera*)
                      (v3:+ (pos *camera*)
                            (v3:*s dir 50f0))
                      (lambda (body geometry a b c d)
                        (declare (ignore geometry a b c d))
                        (setf hit body)
                        0f0))
      (when (and hit (> (body-mass hit) 0f0))
        (body-add-impluse hit (v! 0 5 0))))))
