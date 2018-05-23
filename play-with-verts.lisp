(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *verts* nil)

(defun reset ()
  (setf (cepl:clear-color) (v! 0.03 0.03 0.05 1f0))
  (init-sky)
  (unless *verts*
    (destructuring-bind (v i)
        (nineveh.mesh.data.primitives:sphere-gpu-arrays)
      (setf *verts*
            (make-buffer-stream v :index-array i))))
  (print "==== reset! ===="))

;;------------------------------------------------------------

(defun-g foo-vs ((vert g-pnt)
                 &uniform
                 (model->view :mat4)
                 (view->clip :mat4))
  (let* ((pos3 (pos vert))
         (pos4 (v! pos3 1))
         (vpos4 (* model->view pos4))
         (cpos4 (* view->clip vpos4)))
    (values
     cpos4
     (tex vert))))

(defun-g foo-fs ((uv :vec2))
  (v! 1 0 0 0))

(defpipeline-g foo-pline ()
  (foo-vs g-pnt)
  (foo-fs :vec2))

;;------------------------------------------------------------

(defun control ()
  (when (key-down-p key.up)
    (cam-move 0.1f0))
  (when (key-down-p key.down)
    (cam-move -0.1f0))
  (when (key-down-p key.left)
    (cam-turn (radians 0.8f0)))
  (when (key-down-p key.right)
    (cam-turn (radians -0.8f0))))

(defun game-step ()
  (let* ((res (surface-resolution (current-surface)))
         (view->clip (rtg-math.projection:perspective-v2
                      res 0.1 100f0 45f0))
         (model->view
          (with-slots (pos rot) *camera*
            (m4:* (m4:look-at (v! 0 1 0)
                              pos
                              (v3:+ pos (q:to-direction rot)))
                  (m4:translation (v! 0 0 -10))))))
    (setf (viewport-resolution (current-viewport)) res)
    (control)
    (as-frame
      (draw-sky view->clip)
      (map-g #'foo-pline *verts*
             :model->view model->view
             :view->clip view->clip))
    (decay-events)))

(def-simple-main-loop play (:on-start #'reset)
  (game-step))

;;------------------------------------------------------------
