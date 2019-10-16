(in-package :play-with-verts)

(defclass mesh ()
  ((verts :initarg :verts)
   (indices :initarg :indices)
   (bstream :initarg :bstream)))

(defun make-sphere (per-inst-data)
  (destructuring-bind (v i)
      (nineveh.mesh.data.primitives:sphere-gpu-arrays
       :radius 1f0
       :lines-of-latitude 60
       :lines-of-longitude 60)
    (make-instance
     'mesh
     :verts v
     :indices i
     :bstream (make-buffer-stream
               (list v (cons per-inst-data 1))
               :index-array i))))

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

(defun load-tex (local-path)
  (dirt:load-image-to-texture
   (asdf:system-relative-pathname
    :play-with-verts local-path)))))
