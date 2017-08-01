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
  (values (v! vert 0 1)
          (+ (vec2 0.5) (* 0.5 vert))))

(defun-g noise-frag ((uv :vec2))
  (values (v! (* 20 (perlin-noise (* 6 uv))) 10 0 0)
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
  1)

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
                         (pos-water-height :float))
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

(defun-g total-outflow ((flux :vec4) (time-delta :float))
  (* time-delta (abs (+ (x flux) (y flux) (z flux) (w flux)))))

(defun-g k-factor ((time-delta :float)
                   (flux :vec4)
                   (water-height :float))
  (max 1 (/ (* water-height
               *virtual-pipe-length*
               *virtual-pipe-length*)
            (total-outflow flux time-delta))))

(defun-g erosion-step-0 ((uv :vec2)
                          &uniform (time-delta :float) (tex-size :float)
                          (height-water-sediment-map :sampler-2d)
                          (flux-map :sampler-2d))
  (let* ((tex-step (/ 1.0 tex-size))
         (uv-l (+ uv (v! (- tex-step) 0)))
         (uv-r (+ uv (v! tex-step 0)))
         (uv-t (+ uv (v! 0 tex-step)))
         (uv-b (+ uv (v! 0 (- tex-step))))
         ;;
         ;; data
         (data (texture height-water-sediment-map uv))
         (data-at-l (texture height-water-sediment-map uv-l))
         (data-at-r (texture height-water-sediment-map uv-r))
         (data-at-t (texture height-water-sediment-map uv-t))
         (data-at-b (texture height-water-sediment-map uv-b))
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
         (new-flux-l (flux-to-offset data-at-l flux-l time-delta
                                     terrain-height water-height))
         (new-flux-r (flux-to-offset data-at-r flux-r time-delta
                                     terrain-height water-height))
         (new-flux-t (flux-to-offset data-at-t flux-t time-delta
                                     terrain-height water-height))
         (new-flux-b (flux-to-offset data-at-b flux-b time-delta
                                     terrain-height water-height))
         (new-flux (v! new-flux-l new-flux-r new-flux-t new-flux-b))

         (local-water-volume (* water-height
                                *virtual-pipe-length*
                                *virtual-pipe-length*)))

    (when (> (total-outflow new-flux time-delta) local-water-volume)
      (setf new-flux (* new-flux (k-factor time-delta new-flux water-height))))

    (values (v! terrain-height water-plus-rain sediment-amount 0.0)
            new-flux
            (v! 0 0 0 0))))

(defpipeline-g erosion-0 ()
  (quad-vert :vec2)
  (erosion-step-0 :vec2))

(defun blit-erosion-0 (src-state time-delta)
  (map-g #'erosion-0 (get-quad-stream-v2)
         :time-delta time-delta
         :tex-size 512.0
         :height-water-sediment-map (height-water-sediment-map src-state)
         :flux-map (water-flux-map src-state)))


(defun-g calc-water-delta ((time-delta :float)
                           (flux :vec4)
                           (flux-at-l :vec4)
                           (flux-at-r :vec4)
                           (flux-at-t :vec4)
                           (flux-at-b :vec4))
  (* time-delta
     (- (+ (y flux-at-l) (x flux-at-r) (w flux-at-t) (z flux-at-b))
        (+ (x flux) (y flux) (z flux) (w flux)))))

(defun-g calc-new-water-height ((water-height :float)
                                (water-delta :float))
  (+ water-height
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

;; C(x, y) = Kc · (− N(x, y) · V ) · |→v(x, y)| · lmax(d1(x, y))

(defvar *sediment-capacity* 1f0)
(defvar *maximal-erosion-depth* 10f0)

(defun-g water-depth-velocity-scale ((water-height :float))
  (- 1 (clamp (/ water-height *maximal-erosion-depth*) 0 1)))

(defun-g calc-c ((normal :vec3)
                 (water-height :float)
                 (velocity-2d :vec2)
                 (velocity-3d :vec3))
  (* *sediment-capacity*
     (dot (- normal) velocity-3d)
     (length velocity-2d)
     (water-depth-velocity-scale water-height)))

(defun-g calc-terrain-normal ((height-l :float)
                              (height-r :float)
                              (height-t :float)
                              (height-b :float))
  (let* ((va (normalize (v! 1 0 (- height-r height-l))))
         (vb (normalize (v! 0 1 (- height-t height-b)))))
    (cross va vb)))

(defvar *soil-suspension-rate* 0.5) ;; ks
(defvar *sediment-deposition-rate* 1.0) ;; kd

;; bt+∆t = bt − ∆t · Rt(x, y) · Ks(C − st ), (12b)
;; s1 = st + ∆t · Rt(x, y) · Ks(C − st ), (12a)
;; d3 = d2 + ∆t · Rt(x, y) · Ks(C − st ), (12c)

;; bt+∆t = bt + ∆t · Kd (st − C), (13b)
;; s1 = st − ∆t · Kd (st − C), (13a)
;; d3 = d2 − ∆t · Kd (st − C), (13c)

(defun-g local-hardness ()
  0.5)

(defun-g erode-sediment ((current-sediment :float)
                         (terrain-height :float)
                         (water-height :float)
                         (time-delta :float)
                         (c :float))
  ;; returns:
  ;; new-terrain-height
  ;; new-sediment
  ;; new-water-height
  (let* ((hardness (local-hardness))
         (st current-sediment)
         (sed (* time-delta
                 hardness
                 (* *soil-suspension-rate* (- c st))))
         (sed2 (* time-delta
                  *sediment-deposition-rate*
                  (- st c))))
    (if (< current-sediment c)
        (return (values (- terrain-height sed)
                        (+ current-sediment sed)
                        (+ water-height (clamp sed 0 water-height))))
        (return (values (+ terrain-height sed2)
                        (- current-sediment sed2)
                        (- water-height sed2))))))

(defun-g erosion-step-1 ((uv :vec2)
                         &uniform (time-delta :float) (tex-size :float)
                         (height-water-sediment-map :sampler-2d)
                         (flux-map :sampler-2d))
  (let* ((tex-step (/ 1.0 tex-size))
         (uv-l (+ uv (v! (- tex-step) 0)))
         (uv-r (+ uv (v! tex-step 0)))
         (uv-t (+ uv (v! 0 tex-step)))
         (uv-b (+ uv (v! 0 (- tex-step))))
         ;;
         ;; data
         (data (texture height-water-sediment-map uv))
         (data-at-l (texture height-water-sediment-map uv-l))
         (data-at-r (texture height-water-sediment-map uv-r))
         (data-at-t (texture height-water-sediment-map uv-t))
         (data-at-b (texture height-water-sediment-map uv-b))
         (flux (texture flux-map uv))
         (flux-at-l (texture flux-map uv-l))
         (flux-at-r (texture flux-map uv-r))
         (flux-at-t (texture flux-map uv-t))
         (flux-at-b (texture flux-map uv-b))
         ;;
         ;; unpack
         (terrain-height (x data))
         (water-height (y data))
         (current-sediment (z data))
         (normal (calc-terrain-normal (x data-at-l)
                                      (x data-at-r)
                                      (x data-at-t)
                                      (x data-at-b)))
         ;;
         ;;
         (water-delta (calc-water-delta
                       time-delta
                       flux flux-at-l flux-at-r flux-at-t flux-at-b))
         (new-water-height (calc-new-water-height water-height water-delta))
         ;;
         ;; Velocity
         (velocity-2d (calc-velocity-2d
                       flux flux-at-l flux-at-r flux-at-t flux-at-b))
         (velocity-3d (v! (x velocity-2d) 0 (y velocity-2d)))
         (c (calc-c normal water-height velocity-2d velocity-3d)))

    (multiple-value-bind (new-terrain-height
                          new-sediment
                          new-water-height)
        (erode-sediment current-sediment
                        terrain-height
                        new-water-height
                        0.0
                        c)
      ;; (values (v! new-terrain-height new-water-height new-sediment 0.0)
      ;;         flux
      ;;         (v! velocity-2d 0 0))
      (values (v! terrain-height
                  new-water-height
                  current-sediment
                  0.0)
              flux
              (v! 0 0 0 0)))))

(defpipeline-g erosion-1 ()
  (quad-vert :vec2)
  (erosion-step-1 :vec2))

(defun blit-erosion-1 (src-state time-delta)
  (map-g #'erosion-1 (get-quad-stream-v2)
         :time-delta time-delta
         :tex-size 512.0
         :height-water-sediment-map (height-water-sediment-map src-state)
         :flux-map (water-flux-map src-state)))

(defun swap-state (terrain)
  (rotatef (state-0 terrain) (state-1 terrain)))

(defun erode (terrain time-delta)
  (with-setf (depth-test-function *cepl-context*) nil
    (with-fbo-bound ((terrain-fbo (state-1 terrain)))
      (clear)
      (blit-erosion-0 (state-0 terrain) time-delta))
    (swap-state terrain)
    ;; (with-fbo-bound ((terrain-fbo (state-1 terrain)))
    ;;   (clear)
    ;;   (blit-erosion-1 (state-0 terrain) time-delta))
    ;; (swap-state terrain)
    ))
