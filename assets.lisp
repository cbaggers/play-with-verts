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

(defun get-cube-tex (paths &optional (force nil) (mipmap t))
  (assert (= (length paths) 6))
  (let ((key (format nil "~{~a~}" paths)))
    (when force
      (let ((s (gethash key *samplers*)))
        (when s
          (free (sampler-texture s)))
        (remhash key *samplers*)))
    (or (gethash key *samplers*)
        (with-c-arrays-freed (ca (mapcar (lambda (p)
                                           (dirt:load-image-to-c-array
                                            (asdf:system-relative-pathname
                                             :play-with-verts p)
                                            :rgba))
                                         paths))
          (let ((s (sample
                    (make-texture ca :element-type :rgba8 :cubes t
                                  :mipmap mipmap
                                  :generate-mipmaps t))))
            (setf (wrap s) :clamp-to-edge)
            (setf (gethash key *samplers*) s))))))

;;------------------------------------------------------------
