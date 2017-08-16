(in-package :play-with-verts)

(defclass terrain-state ()
  ((height-water-sediment-map
     :initarg :height-water-sediment-map
     :accessor height-water-sediment-map)
   (water-flux-map
    :initarg :water-flux-map
    :accessor water-flux-map)
   (water-velocity-map
    :initarg :water-velocity-map
    :accessor water-velocity-map)
   (thermal-map-0
    :initarg :thermal-map-0
    :accessor thermal-map-0)
   (thermal-map-1
    :initarg :thermal-map-1
    :accessor thermal-map-1)
   (debug-map
    :initarg :debug-map
    :accessor debug-map)
   (fbo
    :initarg :fbo
    :accessor terrain-fbo)))

(defun make-terrain-state ()
  (let ((hws-map
         (make-texture nil :dimensions '(512 512) :element-type :vec4))
        (wf-map
         (make-texture nil :dimensions '(512 512) :element-type :vec4))
        (wv-map
         (make-texture nil :dimensions '(512 512) :element-type :vec4))
        (t0-map
         (make-texture nil :dimensions '(512 512) :element-type :vec4))
        (t1-map
         (make-texture nil :dimensions '(512 512) :element-type :vec4))
        (dbg-map
         (make-texture nil :dimensions '(512 512) :element-type :vec4)))
    (make-instance
     'terrain-state
     :height-water-sediment-map (sample hws-map)
     :water-flux-map (sample wf-map)
     :water-velocity-map (sample wv-map)
     :thermal-map-0 (sample t0-map)
     :thermal-map-1 (sample t1-map)
     :debug-map (sample dbg-map)
     :fbo (make-fbo (list 0 hws-map) (list 1 wf-map) (list 2 wv-map)
                    (list 3 t0-map) (list 4 t1-map) (list 5 dbg-map)))))

;;------------------------------------------------------------

(defun free-terrain-state (terrain-state)
  (free (terrain-fbo terrain-state))
  (free (sampler-texture (height-water-sediment-map terrain-state)))
  (free (height-water-sediment-map terrain-state))
  (free (sampler-texture (water-flux-map terrain-state)))
  (free (water-flux-map terrain-state))
  (free (sampler-texture (water-velocity-map terrain-state)))
  (free (water-velocity-map terrain-state))
  (free (sampler-texture (thermal-map-0 terrain-state)))
  (free (thermal-map-0 terrain-state))
  (free (sampler-texture (thermal-map-1 terrain-state)))
  (free (thermal-map-1 terrain-state))
  (free (sampler-texture (debug-map terrain-state)))
  (free (debug-map terrain-state))
  nil)

(defmethod free ((terrain-state terrain-state))
  (free-terrain-state terrain-state))

;;------------------------------------------------------------

(defun-g quad-vert ((vert :vec2))
  (values (v! vert 0 1)
          (+ (vec2 0.5) (* 0.5 vert))))

(defun-g noise-frag ((uv :vec2))
  (values (v! (* 40 (perlin-noise (* 6 uv))) 0 0 0)
          (v! 0 0 0 0)
          (v! 0 0 0 0)
          (v! 0 0 0 0)
          (v! 0 0 0 0)
          (v! 0 0 0 0)))

(defpipeline-g blit-noise-pipeline ()
  (quad-vert :vec2)
  (noise-frag :vec2))

(defun blit-noise ()
  (map-g #'blit-noise-pipeline (get-quad-stream-v2)))

(defun reset-terrain-state (terrain)
  (with-fbo-bound ((terrain-fbo (state-src terrain)))
    (clear)
    (blit-noise))
  (with-fbo-bound ((terrain-fbo (state-dst terrain)))
    (clear)
    (blit-noise)))

;;------------------------------------------------------------

(defun swap-state (terrain)
  (rotatef (state-src terrain) (state-dst terrain)))

;------------------------------------------------------------
