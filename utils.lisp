(in-package #:play-with-verts)

(defun now ()
  ;; Just some number that increases over time that we use
  ;; in a bunch of places
  (/ (float (get-internal-real-time))
     1000f0))

(defmethod cepl:free ((obj null))
  nil)
