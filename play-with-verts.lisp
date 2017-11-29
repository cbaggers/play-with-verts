(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *some-sampler* nil)
(defvar *bs* nil)

(defun reset ()
  (setf *bs* (make-buffer-stream nil))
  (setf *some-sampler* (tex "capital-a.png"))
  (setf (wrap *some-sampler*) :clamp-to-edge))

(defun-g goodluck-f ((uv :vec2) &uniform (sam :sampler-2d))
  (texture sam (v! (x uv) (- 1 (y uv)))))

(defpipeline-g nada (:points)
  :fragment (goodluck-f :vec2))

(defun game-step ()
  (setf (viewport-resolution (current-viewport))
        (surface-resolution (current-surface (cepl-context))))
  (as-frame
    (map-g #'nada *bs* :sam *some-sampler*)))

(def-simple-main-loop play (:on-start #'reset)
  (game-step))
