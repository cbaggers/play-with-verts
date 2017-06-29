(in-package #:play-with-verts)

;;------------------------------------------------------------

(defclass camera ()
  ((pos :initform (v! 0 15 20) :accessor pos)
   (rot :initform (q! 0.98384374 -0.17902958 0.0 0.0)
        :accessor rot)))

(defvar *camera* (make-instance 'camera))
(defvar *camera-1* (make-instance 'camera))

(defun get-world->view-space (camera)
  (m4:* (q:to-mat4 (q:inverse (rot camera)))
        (m4:translation (v3:negate (pos camera)))))

(defun update-camera (camera delta)
  (when (keyboard-button (keyboard) key.w)
    (v3:incf (pos camera)
             (v3:*s (q:to-direction (rot camera))
                    (* 10 delta))))

  (when (keyboard-button (keyboard) key.s)
    (v3:decf (pos camera)
             (v3:*s (q:to-direction (rot camera))
                    (* 10 delta))))

  (when (mouse-button (mouse) mouse.left)
    (let ((move (v2:*s (mouse-move (mouse))
                       0.03)))
      (setf (rot camera)
            (q:normalize
             (q:* (rot camera)
                  (q:normalize
                   (q:* (q:from-axis-angle (v! 1 0 0) (- (y move)))
                        (q:from-axis-angle (v! 0 1 0) (- (x move)))))))))))

;;------------------------------------------------------------
