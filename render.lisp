(in-package #:play-with-verts)

;;------------------------------------------------------------

(defstruct-g per-inst-data
  (pos :vec3)
  (scale :float)
  (color :vec4))

;;------------------------------------------------------------

(defun-g thing-vert-stage ((vert g-pnt)
                           (inst-data per-inst-data)
                           &uniform
                           (model->world :mat4)
                           (world->view :mat4)
                           (view->clip :mat4)
                           (time :float))
  (with-slots ((offset pos) scale) inst-data
    (let* ((model-pos (v! (* (pos vert) scale) 1))
           (normal (norm vert))
           ;;
           (world-pos (+ model-pos (v! offset 0)))
           (view-pos (* world->view world-pos))
           (clip-pos (* view->clip view-pos)))
      (values
       clip-pos
       (s~ world-pos :xyz)
       normal
       (tex vert)))))

(defun-g old-frag-stage ((world-pos :vec3)
                         (norm :vec3)
                         (uv :vec2)
                         &uniform
                         (sam :sampler-2d)
                         (tex-scale :float))
  (let* ((norm (normalize norm))
         (light-dir (normalize (v! 1 1 1)))
         (light-ammount (clamp (dot norm light-dir) 0 1)))
    (* (+ 0.1 light-ammount)
       (texture sam (* 6 uv)))))

(defpipeline-g some-pipeline ()
  :vertex (thing-vert-stage g-pnt per-inst-data)
  :fragment (old-frag-stage :vec3 :vec3 :vec2))

;;------------------------------------------------------------

(defun render (camera buffer-stream sampler)
  (map-g #'some-pipeline buffer-stream
         :model->world (m4:identity)
         :world->view (get-world->view-space camera)
         :view->clip (projection camera)
         :tex-scale 0.5f0
         :sam sampler
         ;;:time (now)
         ))

;;------------------------------------------------------------
