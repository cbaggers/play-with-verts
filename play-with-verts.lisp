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
                 (view->clip :mat4)
                 (scale :float))
  (let* ((ldir (normalize (v! 1 1 1)))
         (pos3 (* (pos vert) scale))
         (pos4 (v! pos3 1))
         (vpos4 (* model->view pos4))
         (cpos4 (* view->clip vpos4)))
    (values
     cpos4
     (tex vert)
     (norm vert)
     ldir)))

(defun-g foo-fs ((uv :vec2)
                 (norm :vec3)
                 (ldir :vec3)
                 &uniform
                 (color :vec3)
                 (shadow :float))
  (* (v! color 1)
     (+ 0.1 (saturate (dot norm ldir)))))

(defpipeline-g foo-pline ()
  (foo-vs g-pnt)
  (foo-fs :vec2 :vec3 :vec3))

;;------------------------------------------------------------

(defun control ()
  (when (key-down-p key.up)
    (cam-move 0.3f0))
  (when (key-down-p key.down)
    (cam-move -0.3f0))
  (when (key-down-p key.left)
    (cam-turn (radians 1.8f0)))
  (when (key-down-p key.right)
    (cam-turn (radians -1.8f0))))

(defun draw-ball (pos color scale world->view view->clip)
  (let ((model->view
         (m4:* world->view
               (m4:translation pos))))
    (map-g #'foo-pline *verts*
           :model->view model->view
           :view->clip view->clip
           :scale scale
           :color color)))

(defun game-step ()
  (let* ((res (surface-resolution (current-surface)))
         (view->clip (rtg-math.projection:perspective-v2
                      res 0.1 100f0 45f0))
         (world->view
          (with-slots (pos rot) *camera*
            (m4:look-at (v! 0 1 0)
                        pos
                        (v3:+ pos (q:to-direction rot))))))
    (setf (viewport-resolution (current-viewport)) res)
    (control)
    (as-frame
      (draw-sky view->clip)
      (draw-ball (v! 0 0 -10)
                 (v! 1 0 0)
                 2f0
                 world->view
                 view->clip)
      (draw-ball (v! 0 3 -12)
                 (v! 0 1 0)
                 2f0
                 world->view
                 view->clip)
      (draw-ball (v! 2 1 -12)
                 (v! 0 0 1)
                 2f0
                 world->view
                 view->clip)
      (draw-ball (v! 0 2 -15)
                 (v! 1 1 1)
                 0.5f0
                 world->view
                 view->clip))
    (decay-events)))

(def-simple-main-loop play (:on-start #'reset)
  (game-step))

;;------------------------------------------------------------
