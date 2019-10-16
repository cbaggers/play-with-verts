(in-package #:play-with-verts)

;;------------------------------------------------------------

(defstruct-g per-inst-data
  (pos :vec3)
  (scale :float)
  (color :vec4))

;;------------------------------------------------------------

(defun-g blit-vert ((vert :vec2))
  (values
   (v! vert 0 1)
   (+ (* vert 0.5) 0.5)))

(defun-g blit-frag ((uv :vec2)
                    &uniform
                    (sam :sampler-2d)
                    (threshold :float))
  (let* ((pos (s~ (texture sam uv) :xy))
         (len (length (- pos (s~ gl-frag-coord :xy)))))
    (v! (if (< len threshold) 1.0 0.0)
        0
        0
        1)))

(defpipeline-g blit ()
  (blit-vert :vec2)
  (blit-frag :vec2))

(defun test-blit (sampler threshold)
  (map-g #'blit (get-quad-stream-v2)
         :sam sampler
         :threshold (float threshold 0f0)))

;;------------------------------------------------------------

(defun-g flood-vert ((vert :vec2))
  (values
   (v! vert 0 1)
   (+ (* vert 0.5) 0.5)))

;;------------------------------------------------------------

(defun-g flood-init ((uv :vec2)
                     &uniform
                     (sam :sampler-2d))
  (let* ((color (texture sam uv)))
    (if (> (x color) 0.01)
        (v! (s~ gl-frag-coord :xy) 0.0 0.0)
        (vec4 0))))

(defpipeline-g init-pline ()
  (flood-vert :vec2)
  (flood-init :vec2))

(defun do-init (src-sampler)
  (map-g #'init-pline (get-quad-stream-v2)
         :sam src-sampler))

;;------------------------------------------------------------

(defun-g flood-step ((uv :vec2)
                     &uniform
                     (iter :float)
                     (sam :sampler-2d)
                     (res :vec2))
  (labels ((load0 ((p :ivec2))
             (let ((uv (/ (- (vec2 (x p) (y p)) 0.5)
                          res)))
               (s~ (texture sam uv) :xy))))
    (let* ((level (clamp iter 0.0 11.0))
           (stepwidth (int (+ (exp2 (- 11.0 level)) 0.5)))

           (center (+ (s~ gl-frag-coord :xy) 0.5))
           (tc (ivec2 (int (x center))
                      (int (y center))))

           (best-dist 999999.0)
           (best-coord (vec2 0)))

      (for (y -1) (<= y 1) (++ y)
           (for (x -1) (<= x 1) (++ x)
                (let* ((fc (+ tc (* (ivec2 x y) stepwidth)))
                       (ntc (load0 fc))
                       (d (length (- ntc center))))
                  (when (and (/= (x ntc) 0.0)
                             (/= (y ntc) 0.0)
                             (< d best-dist))
                    (setf best-dist d)
                    (setf best-coord ntc)))))

      (v! best-coord 0.0 0.0))))


(defpipeline-g step-pline ()
  (flood-vert :vec2)
  (flood-step :vec2))

(defun do-flood-step (step-num input-sampler)
  (map-g #'step-pline (get-quad-stream-v2)
         :iter (float step-num 0f0)
         :sam input-sampler
         :res (viewport-resolution (current-viewport))))
