(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *verts* nil)

(defun reset ()
  (unless *verts*
    (destructuring-bind (v i)
        (nineveh.mesh.data.primitives:sphere-gpu-arrays)
      (setf *verts*
            (make-buffer-stream v :index-array i))))
  (print "==== reset! ===="))

;;------------------------------------------------------------

(defun-g foo-vs ((vert g-pnt)
                 &uniform (view->clip :mat4))
  (let* ((pos3 (pos vert))
         (pos3 (* pos3 (v! 1 1.3 1)))
         (vpos3 (+ pos3 (v! 0 0 -10)))
         (cpos4 (* view->clip (v! vpos3 1))))
    (values
     cpos4
     (tex vert))))

(defun-g foo-fs ((uv :vec2))
  (v! 1 0 0 0))

(defpipeline-g foo-pline ()
  (foo-vs g-pnt)
  (foo-fs :vec2))

;;------------------------------------------------------------

(defun game-step ()
  (let* ((res (surface-resolution (current-surface)))
         (vc (rtg-math.projection:perspective-v2
              res 0.1 100f0 45f0)))
    (setf (viewport-resolution (current-viewport)) res)
    (as-frame
      (map-g #'foo-pline *verts*
             :view->clip vc))))

(def-simple-main-loop play (:on-start #'reset)
  (game-step))

;;------------------------------------------------------------
