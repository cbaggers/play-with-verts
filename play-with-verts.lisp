(in-package #:play-with-verts)

;;------------------------------------------------------------

(defun reset ()
  (print "==== reset! ===="))

;;------------------------------------------------------------

(defun game-step ()
  (setf (viewport-resolution (current-viewport))
        (surface-resolution (current-surface (cepl-context))))
  (as-frame))

(def-simple-main-loop play (:on-start #'reset)
  (game-step))

;;------------------------------------------------------------
