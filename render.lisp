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
         (world-norm (* (m4:to-mat3 model->world) normal))
         (view-pos (* world->view world-pos))
         (clip-pos (* view->clip view-pos)))

    (values clip-pos
            world-norm
            uv
            (z view-pos))))


(defun-g some-frag-stage ((frag-normal :vec3)
                          (uv :vec2)
                          (view-z :float)
                          &uniform
                          (albedo :sampler-2d))
  (let* ((albedo (texture albedo uv))
         (ambient 0.2)
         (dir-to-light (normalize (v! 1 1 1)))
         (diffuse (saturate (dot dir-to-light (normalize frag-normal))))
         (light-amount (+ ambient diffuse)))
    (v! (s~ (* albedo light-amount) :xyz) view-z)))


(defpipeline-g some-pipeline ()
  (some-vert-stage g-pnt)
  (some-frag-stage :vec3 :vec2 :float))

;;------------------------------------------------------------

(defun-g unit-square-to-ngon ((p :vec2) (n :float) (amount :float))
  (let* ((a (- (* (x p) 2.0) 1.0))
         (b (- (* (y p) 2.0) 1.0))
         ((r :float))
         ((theta :float)))
    ;; Which quadrant of the square are we in
    (if (> a (- b))
        (if (> a b)
            (progn
              (setf r a)
              (setf theta (* (/ pi-f 4.0) (/ b a))))
            (progn
              (setf r b)
              (setf theta (* (/ pi-f 4.0) (- 2.0 (/ a b))))))
        (if (< a b)
            (progn
              (setf r (- a))
              (setf theta (* (/ pi-f 4.0) (+ 4.0 (/ b a)))))
            (progn
              (setf r (- b))
              (if (/= b 0.0)
                  (setf theta (* (/ pi-f 4.0) (- 6.0 (/ a b))))
                  (setf theta 0.0)))))
    (let* ((circle-radius r))
      (setf r (* r
                 (mix 1.0
                      (/ (cos (/ pi-f n))
                         (cos
                          (- theta
                             (* (/ (* 2.0 pi-f) n)
                                (floor (/ (+ (* n theta) pi-f) (* 2.0 pi-f)))))))
                      amount)))
      ;; This is just so that the shape isn't aligned to an axis,
      ;; which looks a bit nicer
      (incf theta 0.6)
      (let* ((u (* r (cos theta))) (v (* r (sin theta))))
        (v3! u v circle-radius)))))


(defun-g quad-passthrough ((vert :vec2))
  (values
   (v! vert 0 1)
   (+ (* vert 0.5) (vec2 0.5))))

(defun-g some-blur ((uv :vec2)
                    &uniform
                    (sam :sampler-2d))
  (let* ((num-of-sides 6)
         (amount 1)
         (scale 0.02)
         (size 7)
         ;;
         (size-v2 (vec2 size))
         (accum (v! 0 0 0 0)))
    ;; make a square of samples..
    (for (y 0) (< y size) (++ y)
         (for (x 0) (< x size) (++ x)
              ;; ..morphed into shape
              (let* ((size-inclusive (- size-v2 (vec2 1)))
                     (unit-vec (/ (v! x y) size-inclusive))
                     (ngon-uv (s~ (unit-square-to-ngon
                                   unit-vec num-of-sides amount)
                                  :xy))
                     (ngon-uv (* ngon-uv scale))
                     (sample-pos (+ uv ngon-uv)))
                (incf accum (/ (texture sam sample-pos) (* size size))))))
    accum))

(defun-g fill-pass ((uv :vec2)
                    &uniform
                    (sam :sampler-2d))
  (let* ((num-of-sides 6)
         (amount 1)
         (scale 0.01)
         (size 4)
         ;;
         (size-v2 (vec2 size))
         (accum (texture sam uv)))
    ;; make a square of samples..
    (for (y 0) (< y size) (++ y)
         (for (x 0) (< x size) (++ x)
              ;; ..morphed into shape
              (let* ((size-inclusive (- size-v2 (vec2 1)))
                     (unit-vec (/ (v! x y) size-inclusive))
                     (ngon-uv (s~ (unit-square-to-ngon
                                   unit-vec num-of-sides amount)
                                  :xy))
                     (ngon-uv (* ngon-uv scale))
                     (sample-pos (+ uv ngon-uv)))
                (setf accum (max accum (texture sam sample-pos))))))
    accum))

(defun-g frag-passthough ((uv :vec2)
                          &uniform
                          (sam :sampler-2d))
  (texture sam uv))

(defpipeline-g first-blur ()
  :vertex (quad-passthrough :vec2)
  :fragment (some-blur :vec2))

(defpipeline-g fill-blur ()
  :vertex (quad-passthrough :vec2)
  :fragment (fill-pass :vec2))

(defpipeline-g blit ()
  :vertex (quad-passthrough :vec2)
  :fragment (frag-passthough :vec2))


(defun-g coc-frag ((uv :vec2)
                   &uniform
                   (sam :sampler-2d))
  (let* ((focus-depth 25f0)
         ;; from depth to far/near
         (focus-range 10f0)
         (depth (w (texture sam uv))))
    (clamp (/ (- (- depth) focus-depth)
              focus-range)
           -1 1)))

(defpipeline-g coc-pass ()
  :vertex (quad-passthrough :vec2)
  :fragment (coc-frag :vec2))




(defun-g fuckery-frag ((uv :vec2)
                       &uniform
                       (sam :sampler-2d))
  (- (texture sam uv)))

(defpipeline-g fuckery ()
  :vertex (quad-passthrough :vec2)
  :fragment (fuckery-frag :vec2))
