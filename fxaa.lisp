(in-package #:play-with-verts)

(defun-g some-v ((vert :vec2))
  (values (v! vert 0 1)
          (+ (* vert 0.5) 0.5)))

;; vec2 oversiz = radialdistort(vec2(1.0), dist2);
;; uv = remap( uv, 1.0-oversiz, oversiz );

(defun-g some-f ((uv :vec2)
                 &uniform
                 (some-sampler :sampler-2d)
                 (res :vec2))
  (let* ((recip-res (/ 1.0 res))
         (fxaa-subpix-shift (/ 1.0 4.0))
         (dist 2f0)
         (uv (brown-conrady-distortion uv dist))
         (osize (brown-conrady-distortion (vec2 1) dist))
         (uv (remap2 uv (- 1 osize) osize))
         (uv2 (vec4 uv (- uv (* recip-res (+ 0.5 fxaa-subpix-shift))))))
    (* (fxaa2 uv2 some-sampler recip-res)
       (vignette uv))))

(defpipeline-g some-pline ()
  (some-v :vec2)
  (some-f :vec2))

(defun blat (some-sampler)
  (let ((res (viewport-resolution (current-viewport))))
    (map-g #'some-pline (get-quad-stream-v2)
           :some-sampler some-sampler
           :res res)))

(defun-g radial-distort ((coord :vec2) (amt :vec2))
 (let* ((cc (* (- coord 0.5) 2.0)))
   (+ coord (* cc amt))))

(defun-g barrel-distortion ((p :vec2) (amt :vec2))
 (let* ((p (- (* 2.0 p) 1.0))
        (max-barrel-power (sqrt 5.0))
        (radius (dot p p))
        (p (* p (pow (v2! radius) (* max-barrel-power amt)))))
   (+ (* p 0.5) 0.5)))

(defun-g brown-conrady-distortion ((uv :vec2) (dist :float))
 (let* ((uv (- (* uv 2.0) 1.0))
        (barrel-distortion1 (* 0.1 dist))
        (barrel-distortion2 (* (- 0.025) dist))
        (r2 (dot uv uv))
        (uv (* uv (+ 1.0 (+ (* barrel-distortion1 r2)
                            (* barrel-distortion2 (* r2 r2)))))))
   (+ (* uv 0.5) 0.5)))

(defun-g remap2 ((s :vec2) (a :vec2) (b :vec2))
  (clamp (/ (- s a) (- b a)) 0.0 1.0))
