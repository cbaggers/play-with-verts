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

(defun-g erosion-step-0 ((uv :vec2)
                          &uniform (time-delta :float) (tex-size :float)
                         (height-water-sediment-map :sampler-2d)
                         (flux-map :sampler-2d))
  (let* ((uvs (get-erosion-uvs uv tex-size))
         (data (get-height-water-sediment uvs height-water-sediment-map))
         (flux (texture flux-map uv))
         ;;
         ;;
         ;; unpack
         (terrain-height (x (center data)))
         (water-height (y (center data)))
         (sediment-amount (z (center data)))
         (flux-l (x flux))
         (flux-r (y flux))
         (flux-t (z flux))
         (flux-b (w flux))
         ;;
         ;; Rain
         (rain (rain uv))
         (water-plus-rain (add-rain water-height rain time-delta))
         ;;
         ;; flux
         (new-flux-l (flux-to-offset (left data) flux-l time-delta
                                     terrain-height water-height))
         (new-flux-r (flux-to-offset (right data) flux-r time-delta
                                     terrain-height water-height))
         (new-flux-t (flux-to-offset (top data) flux-t time-delta
                                     terrain-height water-height))
         (new-flux-b (flux-to-offset (bottom data) flux-b time-delta
                                     terrain-height water-height))
         ;; all elements will be >= 0
         (new-flux (v! new-flux-l new-flux-r new-flux-t new-flux-b))

         (local-water-volume (* water-height
                                *virtual-pipe-length*
                                *virtual-pipe-length*)))

    (multiple-value-bind (sediment-removed
                          sediment-flux-0
                          sediment-flux-1)
        (thermal-step-0 data time-delta)

      ;; apply thermal erosion
      ;; (decf terrain-height sediment-removed)

      (let* ((old-flux new-flux)
             (more (total-outflow old-flux time-delta))
             (k (k-factor time-delta old-flux water-plus-rain)))
        (when (> (total-outflow new-flux time-delta) local-water-volume)
          (setf new-flux (* new-flux k)))

        (let ((new-whs (v! terrain-height
                           water-plus-rain
                           sediment-amount
                           0.0)))
          (values new-whs
                  new-flux
                  (v! 0 0 0 0) ;; could be a bug
                  (v! k 0 0 0)
                  sediment-flux-1
                  (v! k water-plus-rain water-height 0)))))))


(defun-g erosion-step-1 ((uv :vec2)
                         &uniform (time-delta :float) (tex-size :float)
                         (height-water-sediment-map :sampler-2d)
                         (flux-map :sampler-2d)
                         (thermal-map-0 :sampler-2d)
                         (thermal-map-1 :sampler-2d))
  (let* ((uvs (get-erosion-uvs uv tex-size))
         (data (get-height-water-sediment uvs height-water-sediment-map))
         (fluxes (get-water-flux uvs flux-map))
         ;;
         ;; unpack
         (terrain-height (x (center data)))
         (water-height (y (center data)))
         (current-sediment (z (center data)))
         ;;
         ;;
         (normal (calc-terrain-normal (x (left data))
                                      (x (right data))
                                      (x (top data))
                                      (x (bottom data))))
         ;;
         ;;
         (water-delta (calc-water-delta time-delta fluxes))
         (water-plus-delta (calc-new-water-height water-height water-delta))
         ;;
         ;; Velocity
         (velocity-2d (calc-velocity-2d fluxes))
         (velocity-3d (calc-velocity-3d height-water-sediment-map
                                        velocity-2d
                                        terrain-height
                                        tex-size))

         ;; Sediment capacity
         (capacity
          (calc-capacity normal water-height velocity-2d velocity-3d)))

    (multiple-value-bind (new-terrain-height
                          new-sediment
                          water-plus-delta-and-sediment)
        (erode-sediment current-sediment
                        terrain-height
                        water-plus-delta
                        time-delta
                        capacity)
      (let ((water-plus-delta-and-sediment-minus-evaporation
             (evaporate water-plus-delta-and-sediment time-delta)))

        ;; apply thermal erosion
        ;; (incf new-terrain-height
        ;;       (accumulate-thermal-sediment
        ;;        thermal-map-0 thermal-map-1
        ;;        uvs))
        (let ((new-whs (v! new-terrain-height
                           water-plus-delta-and-sediment-minus-evaporation
                           new-sediment
                           0)))
          (values new-whs
                  (center fluxes)
                  (v! velocity-2d 0 0)
                  (v! 0 0 0 0)
                  (v! 0 0 0 0)
                  (v! water-delta 0 0 0)))))))

(defpipeline-g erosion-0 ()
  (quad-vert :vec2)
  (erosion-step-0 :vec2))

(defpipeline-g erosion-1 ()
  (quad-vert :vec2)
  (erosion-step-1 :vec2))

(defun blit-erosion-0 (src-state time-delta)
  (map-g #'erosion-0 (get-quad-stream-v2)
         :time-delta time-delta
         :tex-size 512.0
         :height-water-sediment-map (height-water-sediment-map src-state)
         :flux-map (water-flux-map src-state)))

(defun blit-erosion-1 (src-state time-delta)
  (map-g #'erosion-1 (get-quad-stream-v2)
         :time-delta time-delta
         :tex-size 512.0
         :height-water-sediment-map (height-water-sediment-map
                                     src-state)
         :flux-map (water-flux-map src-state)
         :thermal-map-0 (thermal-map-0 src-state)
         :thermal-map-1 (thermal-map-1 src-state)))

(defun erode (terrain time-delta)
  (with-setf (depth-test-function *cepl-context*) nil
    (with-fbo-bound ((terrain-fbo (state-dst terrain)))
      (clear)
      (blit-erosion-0 (state-src terrain) time-delta))
    ;;(check-states 0)
    (swap-state terrain)
    (with-fbo-bound ((terrain-fbo (state-dst terrain)))
      (clear)
      (blit-erosion-1 (state-src terrain) time-delta))
    ;;(check-states 1)
    (swap-state terrain)))

;;------------------------------------------------------------

(defun check-states (id)
  (let ((state (state-dst *terrain*)))
    (check-state id (debug-map state))))

(defun check-state (id sampler)
  (with-c-array-freed (tmp (pull1-g (sampler-texture sampler)))
    (let ((at nil)
          (val nil))
      (loop :for y :below 512 :by 1 :do
         (loop :for x :below 512 :by 1 :do
            (let ((d (aref-c tmp x y)))
              (when (or (sb-ext:float-nan-p (x d))
                        (sb-ext:float-nan-p (y d))
                        (sb-ext:float-nan-p (z d))
                        (sb-ext:float-nan-p (w d))
                        (sb-ext:float-infinity-p (x d))
                        (sb-ext:float-infinity-p (y d))
                        (sb-ext:float-infinity-p (z d))
                        (sb-ext:float-infinity-p (w d)))
                (setf at (list x y))
                (setf val d)))))
      (when at
        (break "in step-~a ~a ~a" id at val)))))

;;------------------------------------------------------------

(defun draw-state-dbg (terrain)
  (let ((state (state-src terrain)))
    (draw-tex-bl (height-water-sediment-map state))
    (draw-tex-br (water-flux-map state))
    (draw-tex-tr (water-velocity-map state))
    ;; (draw-tex-br (thermal-map-0 state))
    ;; (draw-tex-tr (thermal-map-1 state))
    ))

(defun draw-dbg ()
  (as-frame
    (draw-state-dbg (first *things*))))

;;------------------------------------------------------------
