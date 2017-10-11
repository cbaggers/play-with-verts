(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *wat-sampler* nil)

(defun-g rblur-vert ((vert :vec2))
  (values
   (v! vert 0 1)
   (+ (* vert 0.5) (vec2 0.5))))

(defun-g rblur-frag ((uv :vec2)
                     &uniform (sam :sampler-2d))
  (let* ((fpos uv)
         (focus (v! 0.5 0.5))
         (dir (- focus uv))
         (steps 64.0)
         (color (s~ (texture sam uv) :xyz))
         (scaled-dir (* 0.2 (/ dir steps))))
    (for (i 1) (< i steps) (++ i)
         (incf fpos scaled-dir)
         (let ((factor (aberration-color-ramp-stateless
                        (/ i steps))))
           (incf color (* factor (s~ (texture sam fpos) :xyz)))))
    (v! (/ color steps) 0)))

(defpipeline-g rblur ()
  (rblur-vert :vec2)
  (rblur-frag :vec2))

(defun radial-blur (sampler)
  (map-g #'rblur (get-quad-stream-v2)
         :sam sampler))

;;------------------------------------------------------------


(defun-g aberration-color-ramp-stateless ((f :float))
  (let* ((f (* f 3f0))
         (r (- 1f0 (- f 0.5)))
         (g (- 1f0 (abs (- f 1.5))))
         (b (- f 1.5)))
    (saturate (v! r g b))))
