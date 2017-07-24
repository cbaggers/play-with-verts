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
   (fbo
    :initarg :fbo
    :accessor terrain-fbo)))

(defun make-terrain-state ()
  (let ((hws-map
         (make-texture nil :dimensions '(512 512) :element-type :vec4))
        (wf-map
         (make-texture nil :dimensions '(512 512) :element-type :vec4))
        (wv-map
         (make-texture nil :dimensions '(512 512) :element-type :vec4)))
    (make-instance
     'terrain-state
     :height-water-sediment-map (sample hws-map)
     :water-flux-map (sample wf-map)
     :water-velocity-map (sample wv-map)
     :fbo (make-fbo (list 0 hws-map) (list 1 wf-map) (list 2 wv-map)))))

;;------------------------------------------------------------

(defun free-terrain-state (terrain-state)
  (free (terrain-fbo terrain-state))
  (free (sampler-texture (height-water-sediment-map terrain-state)))
  (free (height-water-sediment-map terrain-state))
  (free (sampler-texture (water-flux-map terrain-state)))
  (free (water-flux-map terrain-state))
  (free (sampler-texture (water-velocity-map terrain-state)))
  (free (water-velocity-map terrain-state))
  nil)

(defmethod free ((terrain-state terrain-state))
  (free-terrain-state terrain-state))

;;------------------------------------------------------------

(defun-g noise-vert ((vert :vec2))
  (values (v! vert 0 1) (+ (vec2 0.5) (* 0.5 vert))))

(defun-g noise-frag ((pos :vec2))
  (values (v! (perlin-noise (* 6 pos)) 0 0 0)
          (v! 0 0 0 0)
          (v! 0 0 0 0)))

(defpipeline-g blit-noise-pipeline ()
  (noise-vert :vec2)
  (noise-frag :vec2))

(defun blit-noise ()
  (map-g #'blit-noise-pipeline (get-quad-stream-v2)))

(defun reset-terrain-state (terrain-state)
  (with-fbo-bound ((terrain-fbo terrain-state))
    (clear)
    (blit-noise)))

;;------------------------------------------------------------

;; • terrain height b
;; • water height d
;; • suspended sediment amount s
;; • water outflow flux f = ( fL , fR , fT , fB ),
;; • velocity vector →v

;; Symbol and Description
;; ∆t Time increment [0; 0.05]  0.02
;; Kr Rain rate scale factor  [0; 0.05] 0.012
;; Ke Water evaporation rate  [0; 0.05]  0.015
;; A Virtual pipe cross section area  [0.1; 60]  20
;; g Gravity  [0.1; 20]  9.81
;; Kc Sediment capacity  [0.1; 3]  1
;; Kt Thermal erosion rate  [0; 3]  0.15
;; Ks  Soil suspension rate  [0.1; 2]  0.5
;; Kd  Sediment deposition rate  [0.1; 3]  1
;; Kh  Sediment softening rate  [0; 10]  5
;; Kdmax Maximal erosion depth  [0; 40]  10
;; Ka  Talus angle tangent coeff.  [0; 1]  0.8
;; Ki  Talus angle tangent bias  [0; 1]  0.1

(defun-g rain ((pos :vec2))
  0.1)

;; d1(x, y) = dt(x, y) + ∆t · rt(x, y) · Kr

(defvar *rain-rate-scale-factor* 0.012)
(defvar *virtual-pipe-area* 20f0)
(defvar *virtual-pipe-length* 1f0)

(defun-g calc-new-rain ((pos :vec2)
                        (time-delta :float))
  (+ (water-at pos)
     (* time-delta
        (rain pos)
        *rain-rate-scale-factor*)))

;; ∆hL(x, y) = bt(x, y) + d1(x, y) − bt(x−1, y) − d1 (x−1, y)

(defun-g height-diff ((pos-terrain-height :float)
                      (pos-water-depth :float)
                      (offset-terrain-height :float)
                      (offset-water-depth :float))
  (- (+ pos-terrain-height pos-water-depth)
     (+ offset-terrain-height offset-water-depth)))

(defvar *gravity* -9.81)

(defun-g flux-to-offset ((pos :vec2)
                         (flux-index :int)
                         (offset :vec2)
                         (time-delta :float)
                         (pos-terrain-height :float)
                         (pos-water-depth :float)
                         (height-water-sediment-map :sampler-2d)
                         (water-flux-map :sampler-2d))
  (let* ((offset-pos (+ pos offset))
         (offset-data (texture height-water-sediment-map offset-pos))
         (offset-terrain-height (x offset-data))
         (offset-water-depth (y offset-data)))

    (max 0 (+ (aref (texture water-flux-map offset-pos) flux-index)
              (* time-delta
                 *virtual-pipe-area*
                 (/ (* *gravity*
                       (height-diff pos-terrain-height
                                    pos-water-depth
                                    offset-terrain-height
                                    offset-water-depth))
                    *virtual-pipe-length*))))))

(defun-g calc-flux ((pos :vec2)
                    (time-delta :float)
                    (height-water-sediment-map :sampler-2d)
                    (water-flux-map :sampler-2d))
  (let* ((pos-data (texture height-water-sediment-map pos))
         (pos-terrain-height (x pos-data))
         (pos-water-depth (y pos-data))
         ;;
         (flux-left (flux-to-offset pos 0 (v! -1 0) time-delta
                                    pos-terrain-height
                                    pos-water-depth
                                    height-water-sediment-map
                                    water-flux-map))
         (flux-right (flux-to-offset pos 1 (v! 1 0) time-delta
                                    pos-terrain-height
                                    pos-water-depth
                                    height-water-sediment-map
                                    water-flux-map))
         (flux-top (flux-to-offset pos 2 (v! 0 1) time-delta
                                    pos-terrain-height
                                    pos-water-depth
                                    height-water-sediment-map
                                    water-flux-map))
         (flux-bottom (flux-to-offset pos 3 (v! 0 -1) time-delta
                                      pos-terrain-height
                                      pos-water-depth
                                      height-water-sediment-map
                                      water-flux-map))
         (flux (v! flux-left flux-right flux-top flux-bottom)))
    (v! (* (k-factor pos time-delta flux pos-water-depth))
        (* (k-factor pos time-delta flux pos-water-depth))
        (* (k-factor pos time-delta flux pos-water-depth))
        (* (k-factor pos time-delta flux pos-water-depth)))))


(defun-g calc-water-delta ((water-flux-map :sampler-2d)
                           (pos :vec2))
  (let ((flux-left (texture water-flux-map (+ pos (v! -1 0))))
        (flux-right (texture water-flux-map (+ pos (v! 1 0))))
        (flux-top (texture water-flux-map (+ pos (v! 0 1))))
        (flux-bottom (texture water-flux-map (+ pos (v! 0 -1))))
        (flux-pos (texture water-flux-map pos)))
    (- (+ (x flux-right)
          (y flux-left)
          (z flux-bottom)
          (w flux-top))
       (+ (x flux-pos)
          (y flux-pos)
          (z flux-pos)
          (w flux-pos)))))

(defun-g calc-new-water-depth ((height-water-sediment-map :sampler-2d)
                               (water-flux-map :sampler-2d)
                               (pos :vec2))
  (let ((current-depth (y (texture height-water-sediment-map pos)))
        (water-delta (calc-water-delta water-flux-map pos)))
    (+ current-depth
       (/ water-delta
          (* *virtual-pipe-length* *virtual-pipe-length*)))))

(defun-g k-factor ((pos :vec2)
                   (time-delta :float)
                   (flux :vec4)
                   (water-depth :float))
  (max 1 (/ (* water-depth
               *virtual-pipe-length*
               *virtual-pipe-length*)
            (* time-delta
               (+ (x flux) (y flux) (z flux) (w flux))))))

;;                            x    y    z    w
;; • water outflow flux f = ( fL , fR , fT , fB ),

;; ∆Wx = ½(fR(x−1, y) − fL(x, y) + fR(x, y) − fL(x + 1, y))

(defun-g calc-something ()
  (let ((flux-left (texture water-flux-map (+ pos (v! -1 0))))
        (flux-right (texture water-flux-map (+ pos (v! 1 0))))
        (flux-pos (texture water-flux-map pos))))
  (* 0.5
     (v! (+ (- (y flux-left) (x flux-pos))
            (- (y flux-pos) (x flux-right)))
         (+ (- (y flux-top) (x flux-pos))
            (- (y flux-pos) (x flux-bottom))))))
