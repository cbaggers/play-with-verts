(in-package #:play-with-verts)

;;------------------------------------------------------------

(defclass camera ()
  ((pos :initform (v! 0 0 0))
   (rot :initform (q:identity))))

(defvar *camera* (make-instance 'camera))

(defun reset-cam ()
  (with-slots (pos rot) *camera*
    (setf pos (v! 0 0 0)
          rot (q:identity))))

(defun cam-move (dist)
  (with-slots (pos rot) *camera*
    (v3:incf pos (v3:*s (q:to-direction rot) dist))))

(defun cam-turn (ang)
  (with-slots (rot) *camera*
    (setf rot (q:normalize
               (q:* rot (q:from-axis-angle (v! 0 1 0) ang))))))

;;------------------------------------------------------------

(defvar *sky-verts* nil)
(defvar *sky-sampler* nil)

(defun init-sky ()
  (unless *sky-sampler*
    (setf *sky-sampler*
          (get-cube-tex (list "ThickCloudsWater/left.png"
                              "ThickCloudsWater/right.png"
                              "ThickCloudsWater/up.png"
                              "ThickCloudsWater/down.png"
                              "ThickCloudsWater/front.png"
                              "ThickCloudsWater/back.png"))))
  (unless *sky-verts*
    (destructuring-bind (v i)
        (nineveh.mesh.data.primitives:cube-gpu-arrays :size 1f0)
      (setf *sky-verts*
            (make-buffer-stream v :index-array i)))))

;;------------------------------------------------------------

(defun-g cube-vert ((vert g-pnt)
                    &uniform
                    (model->view :mat4)
                    (view->clip :mat4))
  (let* ((pos3 (pos vert))
         (pos4 (v! pos3 1))
         (vpos4 (* model->view pos4))
         (cpos4 (* view->clip vpos4)))
    (values
     (s~ cpos4 :xyww)
     pos3)))

(defun-g cube-frag ((tc :vec3)
                    &uniform
                    (tex :sampler-cube))
  (v! 0 1 0 1)
  (texture tex tc))

(defpipeline-g skybox ()
  (cube-vert g-pnt)
  (cube-frag :vec3))

;;------------------------------------------------------------

(defun draw-sky (proj-m4)
  (with-setf* ((cepl:front-face) :cw
               (depth-test-function) #'<=)
    (with-slots (rot) *camera*
      (map-g #'skybox *sky-verts*
             :model->view (q:to-mat4 (q:inverse rot))
             :view->clip proj-m4
             :tex *sky-sampler*))))
