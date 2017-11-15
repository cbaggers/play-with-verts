(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *some-sampler* nil)
(defvar *bs* nil)

(defun reset ()
  (setf *some-sampler* (tex "wat0.png"))
  (setf *bs* (make-buffer-stream nil :primitive :points)))

(defun-g blit ((uv :vec2) &uniform (sam :sampler-2d))
  (texture sam (* uv (v! 1 -1))))

(defpipeline-g blitter (:points)
  :fragment (blit :vec2))

(defun game-step ()
  (setf (viewport-resolution (current-viewport))
        (surface-resolution (current-surface (cepl-context))))
  (as-frame
    (map-g #'blitter *bs* :sam *some-sampler*)))

(def-simple-main-loop play (:on-start #'reset)
  (game-step))
