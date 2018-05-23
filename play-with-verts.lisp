(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *verts* nil)
(defvar *fbo* nil)
(defvar *fbo-sam* nil)
(defvar *empty-stream* nil)

(defun reset ()
  (setf (cepl:clear-color) (v! 0.03 0.03 0.05 1f0))
  (init-sky)
  (unless *empty-stream*
    (setf *empty-stream*
          (make-buffer-stream nil :primitive :points)))
  (unless *fbo*
    (setf *fbo* (make-fbo 0 :d))
    (setf *fbo-sam* (sample (attachment-tex *fbo* 0))))
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
                 (color :vec3))
  (* (v! color 1)
     (+ 0.1 (saturate (dot norm ldir)))))

(defun-g flat-fs ((uv :vec2)
                  (norm :vec3)
                  (ldir :vec3)
                  &uniform
                  (color :vec3))
  (v! color 1))

(defpipeline-g foo-pline ()
  (foo-vs g-pnt)
  (foo-fs :vec2 :vec3 :vec3))

(defpipeline-g flat-pline ()
  (foo-vs g-pnt)
  (flat-fs :vec2 :vec3 :vec3))

;;------------------------------------------------------------

(defun-g compos-fs ((uv :vec2)
                    &uniform
                    (exposure :float)
                    (decay :float)
                    (density :float)
                    (weight :float)
                    (screen-light-pos :vec2)
                    (first-pass :sampler-2d)
                    (vp-size :vec2))
  (let* ((num-samples 200)
         (tex-coo (s~ gl-frag-coord :xy))
         (delta (* (- tex-coo screen-light-pos)
                   (/ 1f0 (* (float num-samples) density))))
         (illumination-decay 1.0)
         (final-col (v! 0 0 0 0)))
    (dotimes (i num-samples)
      (decf tex-coo delta)
      (let* ((col (texture first-pass (/ tex-coo vp-size)))
             (col (* col illumination-decay weight)))
        (incf final-col (v! (s~ col :xyz) 1f0))
        (setf illumination-decay
              (* illumination-decay decay))))
    (* (/ final-col num-samples)
       exposure)))

(defpipeline-g compos-pline (:points)
  :fragment (compos-fs :vec2))

(defparameter *blend-params*
  (make-blending-params
   :source-rgb :one
   :destination-rgb :one))

(defparameter *blend-params2*
  (make-blending-params))

(defun blah (pos3 res model->view view->clip)
  (let* ((pos4 (v! pos3 1))
         (vpos4 (m4:*v model->view pos4))
         (cpos4 (m4:*v view->clip vpos4))
         (w (w cpos4)))
    (v2:* (v2:+ (v2:/s (s~ cpos4 :xy) w)
                (v! 1 1))
          (v2:*s res 0.5))))

(defun composite (light-pos res world->view view->clip)
  (with-blending *blend-params*
    (map-g #'compos-pline *empty-stream*
           :exposure 0.64
           :decay 1f0
           :density 0.86
           :weight 4.65
           :first-pass *fbo-sam*
           :vp-size res
           :screen-light-pos
           (blah light-pos
                 res
                 (m4:* world->view (m4:translation (v! 0 0 0)))
                 view->clip))))

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

(defun flat-ball (pos color scale world->view view->clip)
  (let ((model->view
         (m4:* world->view
               (m4:translation pos))))
    (map-g #'flat-pline *verts*
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
                        (v3:+ pos (q:to-direction rot)))))
         (light-pos (v! -1 2.3 -15)))
    (setf (viewport-resolution (current-viewport)) res)
    (control)
    (with-fbo-bound (*fbo*)
      (with-setf (cepl:clear-color) (v! 0.02 0.05 0.1 1f0)
        (clear-fbo *fbo*)
        (flat-ball (v! 0 0 -10)
                   (v! 0 0 0)
                   2f0
                   world->view
                   view->clip)
        (flat-ball (v! 0 3 -12)
                   (v! 0 0 0)
                   2f0
                   world->view
                   view->clip)
        (flat-ball (v! 2 1 -12)
                   (v! 0 0 0)
                   2f0
                   world->view
                   view->clip)
        (flat-ball light-pos
                   (v! 1 1 1)
                   0.5f0
                   world->view
                   view->clip)))
    (as-frame
      (with-setf (cepl:clear-color) (v! 0.03 0.2 0.4 1f0)
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
        (draw-ball light-pos
                   (v! 1 1 1)
                   0.5f0
                   world->view
                   view->clip))
      (composite light-pos res world->view view->clip))
    (decay-events)))



(def-simple-main-loop play (:on-start #'reset)
  (game-step))

;;------------------------------------------------------------
