(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *verts* nil)

(defun reset ()
  (unless *verts*
    (destructuring-bind (v i)
        (nineveh.mesh.data.primitives:sphere-gpu-arrays)
      (setf *verts*
            (make-buffer-stream v :index-array i))))
  (print "==== reset! ===="))

;;------------------------------------------------------------

(defun-g foo-vs ((vert g-pnt)
                 &uniform (view->clip :mat4))
  (let* ((pos3 (pos vert))
         (pos3 (* (m3:rotation-x (radians 180))
                  pos3))
         (pos3 (* pos3 (v! 1 1.3 1)))
         (vpos3 (+ pos3 (v! 0 0 -4)))
         (cpos4 (* view->clip (v! vpos3 1))))
    (values
     cpos4
     (tex vert))))

(defun-g quad-vs ((vert :vec2))
  (values (v! vert 0 1)
          (+ (* vert 0.5) 0.5)))

;; (graph (lambda ((x :float))
;;          (expt (- 1 x) 4))
;;        uv)

(defun-g dist-noise ((pos :vec2))
  (perlin-noise pos))

;;;; noise flame
;; float4 flame = saturate(noise.a * _Hard)

;;;; noise flame edge
;; float4 flamerim = saturate((noise.a + _Edge) * _Hard) - flame
;;;; coloured flame edge
;; float4 flamecolored2 = flamerim * gradientTint

(defun-g foo-fs ((uv :vec2)
                 &uniform
                 (now :float))
  (let* ((p (* uv 10))
         (distort (dist-noise p))
         (distort1 (dist-noise (+ p (v! 5 0))))
         (duv (v! (+ (x p) distort)
                  (+ (y p) distort1 (- now))))
         (result (expt (+ (* (cellular-noise duv) 0.8) 1)
                       10))
         (inv-y (- 1 (y uv)))
         (grad (* 2 inv-y))
         (hard 1)
         (foo (- 1 (smoothstep 0 0.7 (y uv))))
         (result (* (+ result grad)
                    hard
                    foo))
         (col (mix (v! 0.0352 0.952 1.0)
                   (v! 0.0274 0.517 0.894)
                   inv-y))
         (flame-col (* result col))
         (edge 0.1)
         ;; (rim (- (saturate (+ result edge))
         ;;         result))
         ;; (rim (- result2 result))
         ;; (rim-col (* rim (v! 1 0 0)))
         )
    (vec4 (+ flame-col ;;rim-col
             )
          1)))

(defpipeline-g foo-pline ()
  (foo-vs g-pnt)
  (foo-fs :vec2))

(defpipeline-g quad-pline ()
  (quad-vs :vec2)
  (foo-fs :vec2))

;;------------------------------------------------------------

(defun game-step ()
  (let* ((res (surface-resolution (current-surface)))
         (vc (rtg-math.projection:perspective-v2
              res 0.1 100f0 45f0))
         (now (* (now) 2)))
    (setf (viewport-resolution (current-viewport)) res)
    (as-frame
      (map-g #'foo-pline *verts* :view->clip vc :now now)
      ;;(map-g #'quad-pline (get-quad-stream-v2) :now now)
      )))

(def-simple-main-loop play (:on-start #'reset)
  (game-step))

;;------------------------------------------------------------
