(in-package :play-with-verts)

;;------------------------------------------------------------

(defvar *virtual-pipe-area* 20f0)
(defvar *virtual-pipe-length* 1f0)

;;------------------------------------------------------------

(defstruct-g erosion-uvs
  (center :vec2 :accessor center)
  (left :vec2 :accessor left)
  (right :vec2 :accessor right)
  (top :vec2 :accessor top)
  (bottom :vec2 :accessor bottom)
  (top-left :vec2 :accessor top-left)
  (top-right :vec2 :accessor top-right)
  (bottom-left :vec2 :accessor bottom-left)
  (bottom-right :vec2 :accessor bottom-right))

(defun-g get-erosion-uvs ((uv :vec2)
                          (tex-size :float))
  (let ((tex-step (/ 1.0 tex-size)))
    (make-erosion-uvs
     uv
     (+ uv (v! (- tex-step) 0))
     (+ uv (v! tex-step 0))
     (+ uv (v! 0 tex-step))
     (+ uv (v! 0 (- tex-step)))
     (+ uv (v! (- tex-step) tex-step))
     (+ uv (v! tex-step tex-step))
     (+ uv (v! (- tex-step) (- tex-step)))
     (+ uv (v! tex-step (- tex-step))))))

(defstruct-g height-water-sediment
  (center :vec4 :accessor center)
  (left :vec4 :accessor left)
  (right :vec4 :accessor right)
  (top :vec4 :accessor top)
  (bottom :vec4 :accessor bottom)
  (top-left :vec4 :accessor top-left)
  (top-right :vec4 :accessor top-right)
  (bottom-left :vec4 :accessor bottom-left)
  (bottom-right :vec4 :accessor bottom-right))

(defun-g get-height-water-sediment ((uvs erosion-uvs)
                                    (height-water-sediment-map :sampler-2d))
  (make-height-water-sediment
   (texture height-water-sediment-map (center uvs))
   (texture height-water-sediment-map (left uvs))
   (texture height-water-sediment-map (right uvs))
   (texture height-water-sediment-map (top uvs))
   (texture height-water-sediment-map (bottom uvs))
   (texture height-water-sediment-map (top-left uvs))
   (texture height-water-sediment-map (top-right uvs))
   (texture height-water-sediment-map (bottom-left uvs))
   (texture height-water-sediment-map (bottom-right uvs))))

(defstruct-g water-flux
  (center :vec4 :accessor center)
  (left :vec4 :accessor left)
  (right :vec4 :accessor right)
  (top :vec4 :accessor top)
  (bottom :vec4 :accessor bottom))

(defun-g get-water-flux ((uvs erosion-uvs)
                         (flux-map :sampler-2d))
  (make-water-flux
   (texture flux-map (center uvs))
   (texture flux-map (left uvs))
   (texture flux-map (right uvs))
   (texture flux-map (top uvs))
   (texture flux-map (bottom uvs))))
