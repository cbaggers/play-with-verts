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

(defun-g foo-fs ((uv :vec2)
                 &uniform
                 (now :float))
  (let* ((p (* uv 10))
         (distort (perlin-noise p))
         (distort1 (perlin-noise (+ p (v! 5 (- now)))))
         (duv (v! (+ (x p) distort)
                  (+ (x p) distort1)))
         (result (saturate
                  (* (+ (perlin-noise duv) 0.7) 0.5)))
         (grad (saturate (expt (- 1 (y uv)) 4)))
         (col (mix (v! 0.0352 0.952 1.0)
                   (v! 0.0274 0.517 0.894)
                   grad)))
    (* (vec4 (* result grad col) 1) 4)
    ;; (graph (lambda ((x :float))
    ;;          (smoothstep 0 1 x))
    ;;    uv)
    ))

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
      (map-g #'quad-pline (get-quad-stream-v2) :now now)
      )))

(def-simple-main-loop play (:on-start #'reset)
  (game-step))

;;------------------------------------------------------------
