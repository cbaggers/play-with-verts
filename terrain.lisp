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

(defun-g quad-vert ((vert :vec2))
  (values (v! vert 0 1) (+ (vec2 0.5) (* 0.5 vert))))

(defun-g noise-frag ((pos :vec2))
  (values (v! (perlin-noise (* 6 pos)) 0 0 0)
          (v! 0 0 0 0)
          (v! 0 0 0 0)))

(defpipeline-g blit-noise-pipeline ()
  (quad-vert :vec2)
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

(defun-g add-rain ((current-water-height :float)
                   (rain :float)
                   (time-delta :float))
  (+ current-water-height
     (* time-delta rain *rain-rate-scale-factor*)))

;; ∆hL(x, y) = bt(x, y) + d1(x, y) − bt(x−1, y) − d1 (x−1, y)

(defun-g height-diff ((pos-terrain-height :float)
                      (pos-water-height :float)
                      (offset-terrain-height :float)
                      (offset-water-height :float))
  (- (+ pos-terrain-height pos-water-height)
     (+ offset-terrain-height offset-water-height)))

(defvar *gravity* -9.81)

(defun-g flux-to-offset ((offset-data :vec4)
                         (offset-flux :float)
                         (time-delta :float)
                         (pos-terrain-height :float)
                         (pos-water-height :float)
                         (height-water-sediment-map :sampler-2d)
                         (water-flux-map :sampler-2d))
  (let* ((offset-terrain-height (x offset-data))
         (offset-water-height (y offset-data)))
    (max 0 (+ offset-flux
              (* time-delta
                 *virtual-pipe-area*
                 (/ (* *gravity*
                       (height-diff pos-terrain-height
                                    pos-water-height
                                    offset-terrain-height
                                    offset-water-height))
                    *virtual-pipe-length*))))))

(defun-g k-factor ((time-delta :float)
                   (flux :vec4)
                   (water-height :float))
  (max 1 (/ (* water-height
               *virtual-pipe-length*
               *virtual-pipe-length*)
            (* time-delta
               (+ (x flux) (y flux) (z flux) (w flux))))))

(defun-g erosion-step-0 ((uv :vec2)
                          &uniform (time-delta :float) (tex-size :float)
                          (height-water-sediment-map :sampler-2d)
                          (flux-map :sampler-2d))
  (let* ((tex-step (/ 1.0 tex-size))
         (uv-l (+ uv (v! (- tex-size) 0)))
         (uv-r (+ uv (v! tex-size 0)))
         (uv-t (+ uv (v! 0 tex-size)))
         (uv-b (+ uv (v! 0 (- tex-size))))
         ;;
         ;; data
         (data (texture height-water-sediment-map uv))
         (data-l (texture height-water-sediment-map uv-l))
         (data-r (texture height-water-sediment-map uv-r))
         (data-t (texture height-water-sediment-map uv-t))
         (data-b (texture height-water-sediment-map uv-b))
         (flux (texture flux-map uv))
         (flux-l (x flux))
         (flux-r (y flux))
         (flux-t (z flux))
         (flux-b (w flux))
         ;;
         ;; unpack
         (terrain-height (x data))
         (water-height (y data))
         (sediment-amount (z data))
         ;;
         ;; Rain
         (rain (rain uv))
         (water-plus-rain (add-rain water-height rain time-delta))
         ;;
         ;; flux
         (new-flux-l (flux-to-offset data-l flux-l time-delta
                                     terrain-height
                                     water-height
                                     height-water-sediment-map
                                     flux-map))
         (new-flux-r (flux-to-offset data-r flux-r time-delta
                                     terrain-height
                                     water-height
                                     height-water-sediment-map
                                     flux-map))
         (new-flux-t (flux-to-offset data-t flux-t time-delta
                                     terrain-height
                                     water-height
                                     height-water-sediment-map
                                     flux-map))
         (new-flux-b (flux-to-offset data-b flux-b time-delta
                                     terrain-height
                                     water-height
                                     height-water-sediment-map
                                     flux-map))
         (new-flux (v! new-flux-l new-flux-r new-flux-t new-flux-b))
         (k (k-factor time-delta new-flux water-height))
         (new-flux (v! (* k new-flux-l)
                       (* k new-flux-r)
                       (* k new-flux-t)
                       (* k new-flux-b))))
    (values (v! terrain-height water-plus-rain sediment-amount 0.0)
            new-flux)))

(defpipeline-g erosion-0 ()
  (quad-vert :vec2)
  (erosion-step-0 :vec2))

(defun blit-erosion-0 (src-state time-delta)
  (map-g #'erosion-0 (get-quad-stream-v2)
         :time-delta time-delta
         :tex-size 512.0
         :height-water-sediment-map (height-water-sediment-map src-state)
         :flux-map (water-flux-map src-state)))

(defun erode (terrain time-delta)
  (cepl-utils:with-setf (depth-test-function *cepl-context*) nil
    (with-fbo-bound ((terrain-fbo (state-1 terrain)))
      (clear)
      (blit-erosion-0 (state-0 terrain) time-delta))
    (with-fbo-bound ((terrain-fbo (state-0 terrain)))
      (clear)
      (blit-erosion-0 (state-1 terrain) time-delta))))

(defun-g calc-water-delta ((flux :vec4)
                           (flux-at-l :vec4)
                           (flux-at-r :vec4)
                           (flux-at-t :vec4)
                           (flux-at-b :vec4))
  (- (+ (y flux-at-l) (x flux-at-r) (w flux-at-t) (z flux-at-b))
     (+ (x flux) (y flux) (z flux) (w flux))))

(defun-g calc-new-water-height ((water-depth :float)
                                (water-delta :float))
  (+ water-depth
     (/ water-delta
        (* *virtual-pipe-length* *virtual-pipe-length*))))

;;                            x    y    z    w
;; • water outflow flux f = ( fL , fR , fT , fB ),

;; ∆Wx = ½(fR(x−1, y) − fL(x, y) + fR(x, y) − fL(x + 1, y))

(defun-g calc-velocity-2d ((flux :vec4)
                           (flux-at-l :vec4)
                           (flux-at-r :vec4)
                           (flux-at-t :vec4)
                           (flux-at-b :vec4))
  (* 0.5
     (v! (+ (- (y flux-at-l) (x flux))
            (- (y flux) (x flux-at-r)))
         (+ (- (y flux-at-t) (x flux))
            (- (y flux) (x flux-at-b))))))

(defun-g erosion-step-1 ((uv :vec2)
                         &uniform (time-delta :float) (tex-size :float)
                         (height-water-sediment-map :sampler-2d)
                         (flux-map :sampler-2d))
  (let* ((tex-step (/ 1.0 tex-size))
         (uv-l (+ uv (v! (- tex-size) 0)))
         (uv-r (+ uv (v! tex-size 0)))
         (uv-t (+ uv (v! 0 tex-size)))
         (uv-b (+ uv (v! 0 (- tex-size))))
         ;;
         ;; data
         (data (texture height-water-sediment-map uv))
         (data-l (texture height-water-sediment-map uv-l))
         (data-r (texture height-water-sediment-map uv-r))
         (data-t (texture height-water-sediment-map uv-t))
         (data-b (texture height-water-sediment-map uv-b))
         (flux (texture flux-map uv))
         (flux-at-l (texture flux-map uv-l))
         (flux-at-r (texture flux-map uv-r))
         (flux-at-t (texture flux-map uv-t))
         (flux-at-b (texture flux-map uv-b))
         ;;
         ;; unpack
         (terrain-height (x data))
         (water-height (y data))
         (sediment-amount (z data))
         ;;
         ;;
         (water-delta (calc-water-delta
                       flux flux-at-l flux-at-r flux-at-t flux-at-b))
         (new-water-height (calc-new-water-height water-height water-delta ))
         ;;
         ;; Velocity
         (velocity-2d (calc-velocity-2d
                       flux flux-at-l flux-at-r flux-at-t flux-at-b))
         )
    0))
