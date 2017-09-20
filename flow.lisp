(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *vector-field* nil)
(defvar *vector-field-sampler* nil)

(defun-g vert ((vert :vec2))
  (values (v! vert 0 1)
          (+ (vec2 0.5) (* vert 0.5))))

(defun-g frag ((uv :vec2) &uniform (now :float))
  (let* ((noise (perlin-noise (v! (* uv 8)
                                  (* now 0.1))))
         (angle (* noise 2 pi-f))
         (vec (v! (sin angle)
                  (cos angle))))
    (v! vec 0 0)))

(defpipeline-g noise-pass ()
  (vert :vec2)
  (frag :vec2))

(defun blit-noise ()
  (with-fbo-bound (*vector-field*)
    (clear *vector-field*)
    (map-g #'noise-pass (get-quad-stream-v2)
           :now (now))))

;;------------------------------------------------------------
