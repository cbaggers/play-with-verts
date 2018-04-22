(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *atlas-glyphs* nil)
(defvar *some-sampler* nil)
(defvar *bs* nil)

(defun reset ()
  (setf *bs* (make-buffer-stream nil :primitive :points))
  ;;
  (setf *some-sampler* (tex "capital-a.png"))
  (setf (wrap *some-sampler*) :clamp-to-edge)
  ;;
  (setf *atlas-sampler* (tex "atlas2.png"))
  (setf (wrap *atlas-sampler*) :clamp-to-edge)
  ;;
  (setf *atlas-glyphs* (make-ubo nil 'font-atlas))
  (with-gpu-array-as-c-array (c-arr (ubo-data *atlas-glyphs*))
    (foo c-arr)))

;;--

(defun-g goodluck-f ((uv :vec2) &uniform (sam :sampler-2d))
  (texture sam (v! (x uv) (- 1 (y uv)))))

(defpipeline-g nada (:points)
  :fragment (goodluck-f :vec2))

;;--

(defvar *atlas-sampler* nil)

(defun-g median ((r :float) (g :float) (b :float))
  (max (min r g)
       (min (max r g) b)))

(defun-g msdf-sample ((uv :vec2)
                      (tile-offset-in-pixels :vec2)
                      (tile-dims-in-pixels :vec2)
                      (msdf :sampler-2d)
                      (sampler-dims :vec2)
                      (px-range :float)
                      (bg-color :vec4)
                      (fg-color :vec4))
  (let* ((nuv (/ (+ tile-offset-in-pixels
                    (* tile-dims-in-pixels uv))
                 sampler-dims))
         (sam (s~ (texture msdf nuv) :xyz))
         (msdf-unit (/ px-range tile-dims-in-pixels))
         (sig-dist (- (median (x sam) (y sam) (z sam))
                      0.5)))
    (multf sig-dist (dot msdf-unit (/ 0.5 (fwidth uv))))
    (let* ((opacity (clamp (+ sig-dist 0.5) 0.0 1.0)))
      (mix bg-color fg-color opacity))))

(defun-g msdf-frag ((pos :vec2)
                    &uniform
                    (msdf :sampler-2d)
                    (px-range :float)
                    (bg-color :vec4)
                    (fg-color :vec4))
  (let* ((pos (v! (x pos) (- 1 (y pos))))
         (size (s~ (texture-size msdf 0) :xy)))
    (msdf-sample pos
                 (v! 0 0)
                 size
                 msdf
                 size
                 px-range
                 bg-color
                 fg-color)))

(defpipeline-g msdf (:points)
  :fragment (msdf-frag :vec2))

(defun-g msdf-any-char-frag ((pos :vec2)
                             &uniform
                             (msdf :sampler-2d)
                             (px-range :float)
                             (bg-color :vec4)
                             (fg-color :vec4)
                             (atlas font-atlas :ubo))
  (let* ((code #.(char-code #\@))
         (glyph (aref (font-atlas-glyphs atlas) code))
         (size2 (s~ (texture-size msdf 0) :xy)))
    (with-slots (atlas-x atlas-y atlas-w atlas-h) glyph
      (msdf-sample (v! (x pos) (- (y pos)))
                   (vec2 atlas-x
                         (- (y size2) atlas-y))
                   (vec2 atlas-w
                         atlas-h)
                   msdf
                   size2
                   px-range
                   bg-color
                   fg-color))))

(defpipeline-g msdf-any-char (:points)
  :fragment (msdf-any-char-frag :vec2))

(defun use-msdf ()
  (map-g #'msdf-any-char *bs*
         :msdf *atlas-sampler*
         :px-range 1f0
         :bg-color (v! 0 0 0 0)
         :fg-color (v! 1 1 1 1)
         :atlas *atlas-glyphs*))

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
