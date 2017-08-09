(in-package :play-with-verts)

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
  ;; negative if neighbours height is lower
  ;; I use this as it feels right.
  (- (+ offset-terrain-height offset-water-height)
     (+ pos-terrain-height pos-water-height)))

;; positive as is the magnitude, not the velocity (no direction specified)
(defvar *gravity* 9.81)

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
                       ;; negate the height diff as we want
                       ;; positive values for outflow
                       (- (height-diff pos-terrain-height
                                       pos-water-height
                                       offset-terrain-height
                                       offset-water-height)))
                    *virtual-pipe-length*))))))

(defun-g total-outflow ((flux :vec4) (time-delta :float))
  ;; flux will always be positive
  (* time-delta (+ (x flux) (y flux) (z flux) (w flux))))

(defun-g k-factor ((time-delta :float)
                   (flux :vec4)
                   (water-height :float))
  ;; paper said max 1, but the k-factor was to scale down the
  ;; flux when it was greater than the water-height.
  ;; Given that 'the outflow flux is mutlipled by K' we need
  ;; it to be in the 0..1 range to not make the issue greater
  (min 1 (/ (* water-height
               *virtual-pipe-length*
               *virtual-pipe-length*)
            (total-outflow flux time-delta))))

;; H = max{b − bi , i = 1, ..., 8}. ;; lowest height
;; ∆St+∆t = a · ∆t · Kt · Rt(x, y) · H/2

(defvar *thermal-erosion-rate* 0.15) ;; kt

;; Let’s denote the distance between two cells by d and
;; talus angle by α = tan((b − bi )/d).
;;
;; ..hmm.. the tan is what is says in the paper, but it seems weird
;; as tan will tend to infinity for certain height differences. It
;; seems like this is a usage of tan(angle) = oposite/adjacent, but
;; then the angle is atan(oposite/adjacent)

(defun-g talus-angle ((height :float)
                      (offset-height :float))
  ;; not the same as paper, see above
  (atan (/ (- height offset-height)
           *virtual-pipe-length*)))

;; Let’s denote the set
;; of neighbors that are lying lower than the current element
;; under the talus angle by:
;; A = {bi , b − bi < 0 ∧ tan(α) > (R(x, y) ∗ Ka + Ki ), i = 1, ...8}

(defvar *talus-angle-tangent-coeff* 0.8) ;; ka
(defvar *talus-angle-tangent-bias* 0.1) ;; ki

;;
;; hmm the logic for 'under the talus angle' is odd
(defun-g in-thermal-set ((terrain-height :float)
                         (offset-height :float))
  (and (< (- terrain-height offset-height) 0)
       (< (* (local-hardness)
             (+ *talus-angle-tangent-coeff*
                *talus-angle-tangent-bias*))
          (talus-angle terrain-height offset-height))))

(defun-g thermal-height ((terrain-height :float)
                         (offset-height :float))
  (if (in-thermal-set terrain-height offset-height)
      (- terrain-height offset-height) ;; will always be negative
      0f0))

(defun-g thermal-sediment ((total-sediment :float)
                           (total-thermal-height :float)
                           (height :float))
  (* total-sediment
     ;;                          cludge  ↓↓↓
     (/ height (max total-thermal-height 0.0001))))


(defun-g thermal-step-0 ((terrain-height :float)
                         (height-l :float)
                         (height-r :float)
                         (height-t :float)
                         (height-b :float)
                         (height-tl :float)
                         (height-tr :float)
                         (height-bl :float)
                         (height-br :float)
                         (time-delta :float))
  (let* ((largest-height-diff
          ;; the height diff to lowest neighbour
          ;; cap to 0 so we can't get negative loss, it is the job
          ;; of the next pass to accumulate sediment flux from
          ;; neighbours
          (max 0
               (- terrain-height height-l)
               (- terrain-height height-r)
               (- terrain-height height-t)
               (- terrain-height height-b)
               (- terrain-height height-tl)
               (- terrain-height height-tr)
               (- terrain-height height-bl)
               (- terrain-height height-br)))

         (sediment (* (* *virtual-pipe-length* *virtual-pipe-length*) ;; area
                      time-delta
                      *thermal-erosion-rate*
                      (local-hardness)
                      (/ largest-height-diff 2)))

         (total-thermal-height
          (+ (thermal-height terrain-height height-l)
             (thermal-height terrain-height height-r)
             (thermal-height terrain-height height-t)
             (thermal-height terrain-height height-b)
             (thermal-height terrain-height height-tl)
             (thermal-height terrain-height height-tr)
             (thermal-height terrain-height height-bl)
             (thermal-height terrain-height height-br))))
    (values
     sediment
     (v! (thermal-sediment sediment
                           total-thermal-height
                           height-l)
         (thermal-sediment sediment
                           total-thermal-height
                           height-r)
         (thermal-sediment sediment
                           total-thermal-height
                           height-t)
         (thermal-sediment sediment
                           total-thermal-height
                           height-b))
     (v! (thermal-sediment sediment
                           total-thermal-height
                           height-tl)
         (thermal-sediment sediment
                           total-thermal-height
                           height-tr)
         (thermal-sediment sediment
                           total-thermal-height
                           height-bl)
         (thermal-sediment sediment
                           total-thermal-height
                           height-br)))))


(defun-g erosion-step-0 ((uv :vec2)
                          &uniform (time-delta :float) (tex-size :float)
                          (height-water-sediment-map :sampler-2d)
                          (flux-map :sampler-2d))
  (let* ((tex-step (/ 1.0 tex-size))
         (uv-l (+ uv (v! (- tex-step) 0)))
         (uv-r (+ uv (v! tex-step 0)))
         (uv-t (+ uv (v! 0 tex-step)))
         (uv-b (+ uv (v! 0 (- tex-step))))
         (uv-tl (+ uv (v! (- tex-step) tex-step)))
         (uv-tr (+ uv (v! tex-step tex-step)))
         (uv-bl (+ uv (v! (- tex-step) (- tex-step))))
         (uv-br (+ uv (v! tex-step (- tex-step))))
         ;;
         ;; data
         (data (texture height-water-sediment-map uv))
         (data-at-l (texture height-water-sediment-map uv-l))
         (data-at-r (texture height-water-sediment-map uv-r))
         (data-at-t (texture height-water-sediment-map uv-t))
         (data-at-b (texture height-water-sediment-map uv-b))
         (data-at-tl (texture height-water-sediment-map uv-tl))
         (data-at-tr (texture height-water-sediment-map uv-tr))
         (data-at-bl (texture height-water-sediment-map uv-bl))
         (data-at-br (texture height-water-sediment-map uv-br))

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
         ;; all elements will be >= 0
         (new-flux (v! new-flux-l new-flux-r new-flux-t new-flux-b))

         (local-water-volume (* water-height
                                *virtual-pipe-length*
                                *virtual-pipe-length*)))

    (multiple-value-bind (sediment-removed
                          sediment-flux-0
                          sediment-flux-1)
        (thermal-step-0
         (x data)
         (x data-at-l)
         (x data-at-r)
         (x data-at-t)
         (x data-at-b)
         (x data-at-tl)
         (x data-at-tr)
         (x data-at-bl)
         (x data-at-br)
         time-delta)

      ;; apply thermal erosion
      ;; (decf terrain-height sediment-removed)

      (when (> (total-outflow new-flux time-delta) local-water-volume)
        (setf new-flux (* new-flux (k-factor 0.01 new-flux 0))))

      (values (v! terrain-height
                  water-plus-rain
                  sediment-amount
                  0.0)
              new-flux
              (v! 0 0 0 0)
              sediment-flux-0
              sediment-flux-1))))

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
  ;; positive if more water coming into cell
  ;; negative if more water leaving cell
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
    (cross va vb) ;; this is killing it
    (v! 0 1 0)))

(defvar *soil-suspension-rate* 0.5) ;; ks
(defvar *sediment-deposition-rate* 1.0) ;; kd

;; bt+∆t = bt − ∆t · Rt(x, y) · Ks(C − st ), (12b)
;; s1 = st + ∆t · Rt(x, y) · Ks(C − st ), (12a)
;; d3 = d2 + ∆t · Rt(x, y) · Ks(C − st ), (12c)

;; bt+∆t = bt + ∆t · Kd (st − C), (13b)
;; s1 = st − ∆t · Kd (st − C), (13a)
;; d3 = d2 − ∆t · Kd (st − C), (13c)

(defun-g local-hardness ()
  0.1)

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

(defvar *water-evaporation-rate* 0.015) ;; Ke

;; dt+∆t(x, y) = d3(x, y) · (1 − Ke · ∆t)

(defun-g evaporate ((water-height :float)
                    (time-delta :float))
  (* water-height
     (- 1.0 (* *water-evaporation-rate*
               time-delta))))

(defun-g accumulate-thermal-sediment ((thermal-map-0 :sampler-2d)
                                      (thermal-map-1 :sampler-2d)
                                      (uv-l :vec2)
                                      (uv-r :vec2)
                                      (uv-t :vec2)
                                      (uv-b :vec2)
                                      (uv-tl :vec2)
                                      (uv-tr :vec2)
                                      (uv-bl :vec2)
                                      (uv-br :vec2))
  (+ (y (texture thermal-map-0 uv-l)) ;; from-l
     (x (texture thermal-map-0 uv-r)) ;; from-r
     (w (texture thermal-map-0 uv-t)) ;; from-t
     (z (texture thermal-map-0 uv-b)) ;; from-b
     (w (texture thermal-map-1 uv-tl)) ;; from-tl
     (z (texture thermal-map-1 uv-tr)) ;; from-tr
     (y (texture thermal-map-1 uv-bl)) ;; from-bl
     (x (texture thermal-map-1 uv-br)))) ;; from-br

(defun-g erosion-step-1 ((uv :vec2)
                         &uniform (time-delta :float) (tex-size :float)
                         (height-water-sediment-map :sampler-2d)
                         (flux-map :sampler-2d)
                         (thermal-map-0 :sampler-2d)
                         (thermal-map-1 :sampler-2d))
  (let* ((tex-step (/ 1.0 tex-size))
         (uv-l (+ uv (v! (- tex-step) 0)))
         (uv-r (+ uv (v! tex-step 0)))
         (uv-t (+ uv (v! 0 tex-step)))
         (uv-b (+ uv (v! 0 (- tex-step))))
         (uv-tl (+ uv (v! (- tex-step) tex-step)))
         (uv-tr (+ uv (v! tex-step tex-step)))
         (uv-bl (+ uv (v! (- tex-step) (- tex-step))))
         (uv-br (+ uv (v! tex-step (- tex-step))))
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
         ;;
         ;; hmm, velocity could be a zero vector
         (velocity-2d (calc-velocity-2d
                       flux flux-at-l flux-at-r flux-at-t flux-at-b))

         (offset-uv (* (normalize velocity-2d) tex-step))
         (offset-data (texture height-water-sediment-map offset-uv))
         (new-y (- (+ (x offset-data) (y offset-data))
                   (+ terrain-height water-height)))
         (3d-tmp (v! (x velocity-2d) new-y (y velocity-2d)))
         (velocity-3d (* (normalize 3d-tmp) (length velocity-2d)))
         ;;(velocity-3d (v! (x velocity-2d) 0 (y velocity-2d)))

         (c (calc-c normal water-height velocity-2d velocity-3d)))

    (multiple-value-bind (new-terrain-height
                          new-sediment
                          new-water-height)
        (erode-sediment current-sediment
                        terrain-height
                        new-water-height
                        time-delta
                        c)
      (let ((water-minus-evaporation
             (evaporate new-water-height time-delta)))

        ;; apply thermal erosion
        ;; (incf new-terrain-height
        ;;       (accumulate-thermal-sediment thermal-map-0 thermal-map-1
        ;;                                    uv-l uv-r uv-t uv-b
        ;;                                    uv-tl uv-tr uv-bl uv-br))

        (values (v! new-terrain-height
                    water-minus-evaporation
                    new-sediment
                    0.0)
                flux
                (v! velocity-2d 0 0)
                (v! 0 0 0 0)
                (v! 0 0 0 0))))))

(defpipeline-g erosion-1 ()
  (quad-vert :vec2)
  (erosion-step-1 :vec2))

(defun blit-erosion-1 (src-state time-delta)
  (map-g #'erosion-1 (get-quad-stream-v2)
         :time-delta time-delta
         :tex-size 512.0
         :height-water-sediment-map (height-water-sediment-map src-state)
         :flux-map (water-flux-map src-state)
         :thermal-map-0 (thermal-map-0 src-state)
         :thermal-map-1 (thermal-map-1 src-state)))

(defun swap-state (terrain)
  (rotatef (state-0 terrain) (state-1 terrain)))

(defun erode (terrain time-delta)
  (with-setf (depth-test-function *cepl-context*) nil
    (with-fbo-bound ((terrain-fbo (state-1 terrain)))
      (clear)
      (blit-erosion-0 (state-0 terrain) time-delta))
    ;;(check-state 0)
    (swap-state terrain)
    (with-fbo-bound ((terrain-fbo (state-1 terrain)))
      (clear)
      (blit-erosion-1 (state-0 terrain) time-delta))
    ;;(check-state 1)
    (swap-state terrain)))

(defun check-state (id)
  (with-c-array-freed (tmp (pull1-g
                            (sampler-texture
                             (height-water-sediment-map
                              (state-1 *terrain*)))))
    (let ((at nil)
          (val nil))
      (loop :for y :below 512 :by 1 :do
         (loop :for x :below 512 :by 1 :do
            (when (or (sb-ext:float-nan-p (x (aref-c tmp x y)))
                      (sb-ext:float-nan-p (y (aref-c tmp x y)))
                      (sb-ext:float-nan-p (z (aref-c tmp x y)))
                      (sb-ext:float-nan-p (w (aref-c tmp x y))))
              (setf at (list x y))
              (setf val (aref-c tmp x y)))))
      (when at
        (break "fack ~a ~a ~a" id at val)))))
