(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *wat-sampler* nil)
(defvar *lut-sampler* nil)

(defun-g rblur-vert ((vert :vec2))
  (values
   (v! vert 0 1)
   (+ (* vert 0.5) (vec2 0.5))))

(defun-g rblur-frag ((uv :vec2)
                     &uniform
                     (sam :sampler-2d)
                     (aspect-ratio :float))
  (let* ((fpos uv)
         (focus (v! 0.5 0.5))
         (dir (normalize (- focus uv)))
         (steps 64.0)
         (full (texture sam uv))
         (color (s~ full :xyz))
         (depth (w full))
         (scaled-dir (* dir 0.0003)))
    (for (i 1) (< i steps) (++ i)
         (incf fpos scaled-dir)
         (let ((factor (aberration-color-ramp-stateless
                        (/ i steps))))
           (incf color (* factor (s~ (texture sam fpos) :xyz)))))
    (* (v! (/ (* 3 color) steps) 0)
       (vignette uv))))

(defpipeline-g rblur ()
  (rblur-vert :vec2)
  (rblur-frag :vec2))

(defun radial-blur (sampler)
  (let ((res (viewport-resolution (current-viewport))))
    (map-g #'rblur (get-quad-stream-v2)
           :sam sampler
           :aspect-ratio (/ (y res) (x res)))))

;;------------------------------------------------------------

(defun-g aberration-color-ramp-stateless ((f :float))
  (let* ((f (* f 3f0))
         (r (- 1f0 (- f 0.5)))
         (g (- 1f0 (abs (- f 1.5))))
         (b (- f 1.5)))
    (saturate (v! r g b))))
