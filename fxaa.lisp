(in-package #:play-with-verts)

(defun-g some-v ((vert :vec2))
  (values (v! vert 0 1)
          (+ (* vert 0.5) 0.5)))

(defconstant +gamma+ 2.2f0)

(defun-g correct-gamma ((color :vec3))
  (expt color (vec3 (/ 1.0 +gamma+))))

(defun-g some-f ((uv :vec2)
                 &uniform
                 (some-sampler :sampler-2d)
                 (res :vec2))
  (let* ((recip-res (/ 1.0 res))
         (fxaa-subpix-shift (/ 1.0 4.0))
         (dist 2f0)
         (uv2 (vec4 uv (- uv (* recip-res
                                (+ 0.5 fxaa-subpix-shift))))))
    (correct-gamma (fxaa2 uv2 some-sampler recip-res))))

(defpipeline-g some-pline ()
  (some-v :vec2)
  (some-f :vec2))

(defun blat (some-sampler)
  (let ((res (viewport-resolution (current-viewport))))
    (map-g #'some-pline (get-quad-stream-v2)
           :some-sampler some-sampler
           :res res)))
