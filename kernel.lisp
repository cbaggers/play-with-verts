(in-package #:play-with-verts)

;;------------------------------------------------------------

(def-kernel k-blit ()
  1)

(def-kernel k-edge ()
  -1 -1 -1
  -1  8 -1
  -1 -1 -1)

(def-kernel k-gaussian (:normalize t)
  1  4  6  4 1
  4 16 24 16 4
  6 24 36 24 6
  4 16 24 16 4
  1  4  6  4 1)

(def-frag-pipeline threshold (uv sam step (voronoi :sampler-2d))
  (let* ((col (texture sam uv))
         (val (+ (x col) (y col) (z col)))
         (grout (v! 0.4 0.4 0.4)))
    (* (step 0.1 val)
       grout)))
