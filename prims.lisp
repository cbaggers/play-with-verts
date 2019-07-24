(in-package :play-with-verts)

(defclass mesh ()
  ((verts :initarg :verts)
   (indices :initarg :indices)
   (bstream :initarg :bstream)))

(defun make-sphere (&optional (radius 1f0))
  (destructuring-bind (v i)
      (nineveh.mesh.data.primitives:sphere-gpu-arrays
       :radius radius
       :lines-of-latitude 60
       :lines-of-longitude 60)
    (make-instance
     'mesh
     :verts v
     :indices i
     :bstream (make-buffer-stream v :index-array i))))

(defun make-terrain (&optional (size 10f0))
  (destructuring-bind (v i)
      (nineveh.mesh.data.primitives:lattice-gpu-arrays
       :width size
       :height size
       :x-segments 200
       :y-segments 200)
    (make-instance
     'mesh
     :verts v
     :indices i
     :bstream (make-buffer-stream v :index-array i))))

(defmethod free ((o mesh))
  (with-slots (verts indices bstream) o
    (free verts)
    (free indices)
    (free bstream)
    nil))
