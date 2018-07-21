(in-package #:play-with-verts)

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
            uv
            (/ (- (z view-pos)) 100f0))))

(defun-g ferris-bad-grain ((uv :vec2)
                           (time :float)
                           (grain-amount :float))
  "Expected to be used as follows:
   (* color (ferris-bad-grain uv time grain-amount))"
  (let* ((grain-strength (* 50.0 grain-amount))
         (v (* (+ (x uv) 4.0)
               (+ (y uv) 4.0)
               (+ time 10.0)
               10.0))
         (grain (* (saturate (- (mod (* (+ (mod v 13.0) 1.0)
                                        (+ (mod v 123.0) 1.0))
                                     0.01)
                                0.005))
                   grain-strength)))
    (- 1.0 grain)))

(defun-g some-frag-stage ((frag-normal :vec3)
                          (world-pos :vec3)
                          (uv :vec2)
                          (lin-depth :float)
                          &uniform
                          (albedo :sampler-2d)
                          (alt-sam :sampler-2d)
                          (now :float))
  (labels ((albedo-edge ((edge :float)
                         (effect-dist :float))
             (mix (texture alt-sam uv)
                  (texture albedo uv)
                  (smoothstep edge (+ edge 2) effect-dist)))
           (albedo-band ((edge :float)
                         (effect-dist :float))
             (let* ((thickness 1)
                    (band (/ (max (+ thickness
                                     (- (expt (- effect-dist edge) 2)))
                                  0)
                             thickness)))
               (mix (texture albedo uv)
                    (texture alt-sam uv)
                    band))))
    (let* (;; calculate the region of influence of the effect
           (focus (v! (* (sin now) 10) 0 0))
           (pos-diff (- world-pos focus))
           (pos-diff-norm (normalize (s~ pos-diff :xz)))
           (effect-dist (length pos-diff))
           (edge (+ 5 (* (+ 1 (sin now)) 10)))

           ;; make the edge wavy
           (angle (v2:angle-from (v! 0 1) pos-diff-norm))
           (edge (+ edge (sin (+ (* 10 (+ now angle))))))

           ;; uncomment one of these effects
           (albedo (albedo-edge edge effect-dist))
           ;;(albedo (albedo-band edge effect-dist))

           (ambient 0.2)
           (dir-to-light (normalize (v! 1 1 1)))
           (diffuse (saturate (dot dir-to-light (normalize frag-normal))))
           (light-amount (+ ambient diffuse))
           (col (* albedo light-amount))
           (col+grain (* col (ferris-bad-grain uv now 0.7)))
           (col3 (s~ col+grain :xyz)))
      (v! col3 (rgb->luma-bt601 col3)))))

(defpipeline-g some-pipeline ()
  (some-vert-stage g-pnt)
  (some-frag-stage :vec3 :vec3 :vec2 :float))

(defvar *alt-sampler* nil)

;;------------------------------------------------------------
