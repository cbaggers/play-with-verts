(in-package #:play-with-verts)

;;------------------------------------------------------------

(defun-g thing-vert-stage ((vert g-pnt)
                           &uniform
                           (model->world :mat4)
                           (world->view :mat4)
                           (view->clip :mat4)
                           (scale :float))
  (let* ((model-pos (v! (* (pos vert) scale) 1))
         (world-pos (* model->world model-pos))
         (view-pos (* world->view world-pos))
         (clip-pos (* view->clip view-pos)))
    (values
     clip-pos
     (norm vert)
     (tex vert))))

(defun-g thing-frag-stage ((norm :vec3)
                           (uv :vec2)
                           &uniform (sam :sampler-2d))
  (let* ((norm (normalize norm))
         (light-dir (normalize (v! 1 1 1)))
         (light-ammount (clamp (dot norm light-dir) 0 1)))
    (* (+ 0.1 light-ammount)
       (texture sam uv)
       ;;(v! 1 0 0 1)
       )))

(defpipeline-g some-pipeline ()
  (thing-vert-stage g-pnt)
  (thing-frag-stage :vec3 :vec2))

(defun render (camera buffer-stream sampler scale)
  (map-g #'some-pipeline buffer-stream
         :model->world (m4:identity)
         :world->view (get-world->view-space camera)
         :view->clip (projection camera)
         :scale scale
         :sam sampler))

;;------------------------------------------------------------
