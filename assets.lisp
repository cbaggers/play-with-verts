(in-package #:play-with-verts)

;;------------------------------------------------------------
;; Textures & Samplers
;;
;; We cache the data based on the the path so we don't
;; get lots of instances in memory

(defvar *samplers* (make-hash-table :test #'equal))

(defun get-tex (path &optional (force nil) (mipmap t))
  (when force
    (let ((s (gethash path *samplers*)))
      (when s
        (free (sampler-texture s)))
      (remhash path *samplers*)))
  (or (gethash path *samplers*)
      (setf (gethash path *samplers*)
            (sample
             (dirt:load-image-to-texture
              (asdf:system-relative-pathname
               :play-with-verts path)
              :rgba8
              mipmap
              t)))))

;;------------------------------------------------------------
