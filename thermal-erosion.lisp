(in-package :play-with-verts)

(defun-g local-hardness ()
  0.1)

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


(defun-g thermal-step-0 ((hws height-water-sediment)
                         (time-delta :float))
  (let* ((terrain-height (x (center hws)))
         (largest-height-diff
          ;; the height diff to lowest neighbour
          ;; cap to 0 so we can't get negative loss, it is the job
          ;; of the next pass to accumulate sediment flux from
          ;; neighbours
          (max 0
               (- terrain-height (x (left hws)))
               (- terrain-height (x (right hws)))
               (- terrain-height (x (top hws)))
               (- terrain-height (x (bottom hws)))
               (- terrain-height (x (top-left hws)))
               (- terrain-height (x (top-right hws)))
               (- terrain-height (x (bottom-left hws)))
               (- terrain-height (x (bottom-right hws)))))

         (sediment (* (* *virtual-pipe-length* *virtual-pipe-length*) ;; area
                      time-delta
                      *thermal-erosion-rate*
                      (local-hardness)
                      (/ largest-height-diff 2)))

         (total-thermal-height
          (+ (thermal-height terrain-height (x (left hws)))
             (thermal-height terrain-height (x (right hws)))
             (thermal-height terrain-height (x (top hws)))
             (thermal-height terrain-height (x (bottom hws)))
             (thermal-height terrain-height (x (top-left hws)))
             (thermal-height terrain-height (x (top-right hws)))
             (thermal-height terrain-height (x (bottom-left hws)))
             (thermal-height terrain-height (x (bottom-right hws))))))
    (values
     sediment
     (v! (thermal-sediment sediment
                           total-thermal-height
                           (x (left hws)))
         (thermal-sediment sediment
                           total-thermal-height
                           (x (right hws)))
         (thermal-sediment sediment
                           total-thermal-height
                           (x (top hws)))
         (thermal-sediment sediment
                           total-thermal-height
                           (x (bottom hws))))
     (v! (thermal-sediment sediment
                           total-thermal-height
                           (x (top-left hws)))
         (thermal-sediment sediment
                           total-thermal-height
                           (x (top-right hws)))
         (thermal-sediment sediment
                           total-thermal-height
                           (x (bottom-left hws)))
         (thermal-sediment sediment
                           total-thermal-height
                           (x (bottom-right hws)))))))

(defun-g accumulate-thermal-sediment ((thermal-map-0 :sampler-2d)
                                      (thermal-map-1 :sampler-2d)
                                      (uvs erosion-uvs))
  (+ (y (texture thermal-map-0 (left uvs))) ;; from-l
     (x (texture thermal-map-0 (right uvs))) ;; from-r
     (w (texture thermal-map-0 (top uvs))) ;; from-t
     (z (texture thermal-map-0 (bottom uvs))) ;; from-b
     (w (texture thermal-map-1 (top-left uvs))) ;; from-tl
     (z (texture thermal-map-1 (top-right uvs))) ;; from-tr
     (y (texture thermal-map-1 (bottom-left uvs))) ;; from-bl
     (x (texture thermal-map-1 (bottom-right uvs))))) ;; from-br
