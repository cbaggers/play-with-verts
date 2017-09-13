(in-package #:play-with-verts)

;;------------------------------------------------------------

(defun-g shared-vert ((vert :vec2))
  (values (v! vert 0 1)
          (+ (vec2 0.5) (* vert 0.5))))

(defun calc-edge-len (weights)
  (multiple-value-bind (edge-len remainder) (floor (sqrt (length weights)))
    (assert (zerop remainder)) ;; is square
    (assert (oddp edge-len))
    edge-len))

(defmacro def-kernel (name (&key normalize) &body weights)
  (let* (;; helpers values
         (edge-len (calc-edge-len weights))
         (offset (/ (- edge-len 1) 2))
         (weight-sum (reduce #'+ weights))
         ;;
         ;; sampling code
         (weight-index 0)
         (samples (loop :for y :from (- offset) :to offset
                     :append
                     (loop :for x :from (- offset) :to offset
                        :collect (let ((weight (elt weights weight-index)))
                                   `(* (texture tex (+ (* (v! ,x ,y) step) uv))
                                       ,weight))
                        :do (incf weight-index))))
         (body (if normalize
                   `(/ (+ ,@samples) ,weight-sum)
                   `(+ ,@samples)))
         (frag-name (intern (format nil "~a-FRAG" name)))
         (pline-name (intern (format nil "~a-PIPELINE" name))))
    ;;
    `(progn
       (defun-g ,frag-name ((uv :vec2) &uniform (tex :sampler-2d) (step :vec2))
         ,body)
       (defpipeline-g ,pline-name ()
         :vertex (shared-vert :vec2)
         :fragment (,frag-name :vec2))
       (defun ,name (sampler)
         (map-g #',pline-name (get-quad-stream-v2)
                :tex sampler
                :step (v2:/ (v! 1 1) (viewport-resolution (current-viewport))))))))

(def-kernel k-blit ()
  1)

(def-kernel k-edge ()
  -1 -1 -1
  -1  8 -1
  -1 -1 -1)

(def-kernel k-gaussian (:normalize t)
  1  4  6  4 1
  4 16 24 16 4
  6 24 36 24 6
  4 16 24 16 4
  1  4  6  4 1)