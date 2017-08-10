(in-package :play-with-verts)

(defun-g rain ((pos :vec2))
  0.1)

;; d1(x, y) = dt(x, y) + ∆t · rt(x, y) · Kr

(defvar *rain-rate-scale-factor* 0.012)

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
  (clamp (/ (* water-height
               *virtual-pipe-length*
               *virtual-pipe-length*)
            (min (total-outflow flux time-delta)
                 0.0001))
         0.1 1))

(defun-g calc-water-delta ((time-delta :float)
                           (fluxes water-flux))
  ;; positive if more water coming into cell
  ;; negative if more water leaving cell
  (* time-delta
     (- (+ (y (left fluxes))
           (x (right fluxes))
           (w (top fluxes))
           (z (bottom fluxes)))
        (+ (x (center fluxes))
           (y (center fluxes))
           (z (center fluxes))
           (w (center fluxes))))))

(defun-g calc-new-water-height ((water-height :float)
                                (water-delta :float))
  (+ water-height
     (/ water-delta
        (* *virtual-pipe-length* *virtual-pipe-length*))))

;;                            x    y    z    w
;; • water outflow flux f = ( fL , fR , fT , fB ),

;; ∆Wx = ½(fR(x−1, y) − fL(x, y) + fR(x, y) − fL(x + 1, y))

(defun-g calc-velocity-2d ((fluxes water-flux))
  (* 0.5
     (v! (+ (- (y (left fluxes)) (x (center fluxes)))
            (- (y (center fluxes)) (x (right fluxes))))
         (+ (- (y (top fluxes)) (x (center fluxes)))
            (- (y (center fluxes)) (x (bottom fluxes)))))))

(defun-g calc-velocity-3d ((height-water-sediment-map :sampler-2d)
                           (velocity-2d :vec2)
                           (terrain-height :float)
                           (tex-size :float))
  (let* ((tex-step (/ 1.0 tex-size))
         (offset-uv (* (normalize velocity-2d) tex-step))
         (offset-data (texture height-water-sediment-map offset-uv))
         (new-y (- (x offset-data) terrain-height))
         (3d-tmp (v! (x velocity-2d) new-y (y velocity-2d)))
         (v2-len (length velocity-2d)))
    (if (> v2-len 0)
        (* (normalize 3d-tmp) v2-len)
        (v! 0 0 0))))

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
  (let* ((va (normalize (v! 1 (- height-r height-l) 0)))
         (vb (normalize (v! 0 (- height-t height-b) 1))))
    (cross va vb)))

(defvar *soil-suspension-rate* 0.5) ;; ks
(defvar *sediment-deposition-rate* 1.0) ;; kd

;; bt+∆t = bt − ∆t · Rt(x, y) · Ks(C − st ), (12b)
;; s1 = st + ∆t · Rt(x, y) · Ks(C − st ), (12a)
;; d3 = d2 + ∆t · Rt(x, y) · Ks(C − st ), (12c)

;; bt+∆t = bt + ∆t · Kd (st − C), (13b)
;; s1 = st − ∆t · Kd (st − C), (13a)
;; d3 = d2 − ∆t · Kd (st − C), (13c)


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
        ;; revisit this ↓↓↓↓↓£
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
