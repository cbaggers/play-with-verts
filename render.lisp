(in-package #:play-with-verts)

;;------------------------------------------------------------

(defstruct-g (plight :layout :std-430)
  (pos :vec3)
  (color :vec3)
  (strength :float))

;;------------------------------------------------------------

;; We will use this function as our vertex shader
(defun-g some-vert-stage ((vert g-pnt)
                          &uniform
                          (model->world :mat4)
                          (world->view :mat4)
                          (view->clip :mat4))
  (let* ((pos (pos vert))
         (normal (norm vert))
         (uv (tex vert))
         (model-pos (v! pos 1))
         (world-pos (* model->world model-pos))
         (view-pos (* world->view world-pos))
         (world-norm (* (m4:to-mat3 model->world) normal))
         (clip-pos (* view->clip view-pos)))

    (values clip-pos
            world-norm
            (s~ world-pos :xyz)
            uv)))

(defun-g lin-attenuate ((dist :float))
  (/ 1f0 dist))

(defun-g attenuate ((dist :float))
  (/ 1f0 (* dist dist)))

(defun-g gamma-correct ((color :vec3))
  (expt color (vec3 2.2)))

(defun-g gamma-encode ((color :vec3))
  (expt color (vec3 (/ 1.0 2.2))))

(defun-g calc-light ((frag-pos :vec3)
                     (frag-normal :vec3)
                     (light plight))
  (let* ((vec-to-light (- (plight-pos light) frag-pos))
         (dir-to-light (normalize vec-to-light))
         (point-light-strength
          (* (saturate (dot dir-to-light frag-normal))
             (plight-strength light))))
    (* point-light-strength
       (attenuate (length vec-to-light))
       (plight-color light))))

(defun-g some-frag-stage ((frag-normal :vec3)
                          (pos :vec3)
                          (uv :vec2)
                          &uniform
                          (albedo :sampler-2d)
                          (now :float))

  (let* (;; process inputs
         (normal (normalize frag-normal))
         ;;
         (albedo (gamma-correct (s~ (texture albedo uv) :xyz)))
         ;;
         (ambient (vec3 0.015))
         (light0 (make-plight
                  (v! 0 4 0)
                  (v! 1 1 1)
                  (* 8000 (+ 1.2 (sin (* 2.8 now))))))
         (light1 (make-plight (v! -10 50 -10)
                              (v! 0 1 0)
                              1000))
         (diffuse-power0 (calc-light pos normal light0))
         (diffuse-power1 (calc-light pos normal light1)))

    ;;
    (let* ((light-amount (+ ambient
                            diffuse-power0
                            diffuse-power1))
           (color (* albedo light-amount) 0))
      (v! color 0))))

(defpipeline-g some-pipeline ()
  (some-vert-stage g-pnt)
  (some-frag-stage :vec3 :vec3 :vec2))

;;------------------------------------------------------------

(defun-g reinhard-tonemap ((hdr-color :vec3))
  (/ hdr-color (+ hdr-color (vec3 1f0))))

(defun-g expose-tonemap ((hdr-color :vec3)
                         (exposure :float))
  (- (vec3 1) (exp (* (- hdr-color) exposure))))

(defun-g expose-tonemap ((hdr-color :vec3))
  (- (vec3 1) (exp (- hdr-color))))

(defun-g quad-v ((vert :vec2))
  (values
   (v! vert 0 1)
   (+ (* vert 0.5) 0.5)))

(defun-g quad-f ((uv :vec2)
                 &uniform
                 (sam :sampler-2d)
                 (now :float))
  (let* ((col (s~ (texture sam uv) :xyz)))
    (tone-map-uncharted2 col 0.1 2f0)))

(defpipeline-g quad-pline ()
  (quad-v :vec2)
  (quad-f :vec2))

(defun splat (sampler)
  (map-g #'quad-pline (get-quad-stream-v2)
         :sam sampler
         :now (now)))

;;------------------------------------------------------------

(defun-g some-vert-stage ((vert g-pnt)
                          &uniform
                          (model->world :mat4)
                          (world->view :mat4)
                          (view->clip :mat4)))

(defun-g line-v ((pos :vec3)
                 &uniform
                 (model->world :mat4)
                 (world->view :mat4)
                 (view->clip :mat4))
  (let* ((model-pos (v! pos 1))
         (world-pos (* model->world model-pos))
         (view-pos (* world->view world-pos))
         (clip-pos (* view->clip view-pos)))

    clip-pos))

(defun-g line-f ()
  (v! 1 1 1 1))

(defpipeline-g line-pline (:lines)
  (line-v :vec3)
  (line-f))

(defun draw-line ()
  (let ((camera *current-camera*))
    (map-g #'line-pline tmp3
           :model->world (m4:identity)
           :world->view (get-world->view-space camera)
           :view->clip (projection camera))))
