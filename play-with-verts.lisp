(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *some-sampler* nil)

(defun reset ()
  (setf *some-sampler* (tex "dirt.jpg")))

(defun-g simple-v ((vert :vec2))
  (values (v! vert 0 1)
          (+ (vec2 0.5) (* vert 0.5))))

(defun-g simple-f ((uv :vec2) &uniform (sam :sampler-2d))
  (texture sam uv))

(defpipeline-g simple ()
  (simple-v :vec2)
  (simple-f :vec2))

(defun game-step ()
  (setf (viewport-resolution (current-viewport))
        (surface-resolution (current-surface (cepl-context))))
  (as-frame
    (map-g #'simple (get-quad-stream-v2) :sam *some-sampler*)))

(def-simple-main-loop play (:on-start #'reset)
  (game-step))
