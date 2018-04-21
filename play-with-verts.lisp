(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *some-sampler* nil)
(defvar *bs* nil)

(defun reset ()
  (setf *bs* (make-buffer-stream nil :primitive :points))
  (setf *some-sampler* (tex "capital-a.png"))
  (setf (wrap *some-sampler*) :clamp-to-edge)
  (setf *atlas-sampler* (tex "atlas.png"))
  ;; (setf *atlas-glyphs*
  ;;       (make-gpu-array nil :element-type 'font-atlas
  ;;                       :dimensions 1))
  )

;;--

(defun-g goodluck-f ((uv :vec2) &uniform (sam :sampler-2d))
  (texture sam (v! (x uv) (- 1 (y uv)))))

(defpipeline-g nada (:points)
  :fragment (goodluck-f :vec2))

(defun-g median ((r :float) (g :float) (b :float))
  (max (min r g)
       (min (max r g) b)))

;;--

(defvar *atlas-sampler* nil)

(defun-g msdf-frag ((pos :vec2)
                    &uniform
                    (msdf :sampler-2d)
                    (px-range :float)
                    (bg-color :vec4)
                    (fg-color :vec4))
  (let* ((pos (v! (x pos) (- 1 (y pos))))
         (sam (s~ (texture msdf pos) :xyz))
         (size4 (texture-size msdf 0))
         (msdf-unit (/ px-range (vec2 (x size4) (y size4))))
         (sig-dist (- (median (x sam) (y sam) (z sam))
                      0.5)))
    (multf sig-dist (dot msdf-unit (/ 0.5 (fwidth pos))))
    (let* ((opacity (clamp (+ sig-dist 0.5) 0.0 1.0)))
      (mix bg-color fg-color opacity))))

(defpipeline-g msdf (:points)
  :fragment (msdf-frag :vec2))

(defun use-msdf ()
  (map-g #'msdf *bs*
         :msdf *some-sampler*
         :px-range 2f0
         :bg-color (v! 0 0 0 0)
         :fg-color (v! 1 1 1 1)))

;;--

(defun game-step ()
  (setf (viewport-resolution (current-viewport))
        (surface-resolution (current-surface (cepl-context))))
  (as-frame
    ;;(map-g #'nada *bs* :sam *some-sampler*)
    (use-msdf)
    ))

(def-simple-main-loop play (:on-start #'reset)
  (game-step))

;;------------------------------------------------------------
