(in-package #:play-with-verts)

(defstruct-g (ssbo-test-data :layout std-430)
  (vals (:int 100)))

(defstruct-g (some-more-data :layout std-430)
  (vals (:vec4 100)))

;;------------------------------------------------------------

;; write some nonsence here :)

;;------------------------------------------------------------


(defun-g test-compute-func (&uniform
                            (woop ssbo-test-data :ssbo))
  (declare (local-size :x 10 :y 20 :z 3))
  (with-slots (vals) woop
    (atomic-add (aref vals 0) 1))
  (values))


(defun-g test-compute-func2 (&uniform
                             (woop some-more-data :ssbo)
                             &shared
                             (foo :int))
  (declare (local-size :x 10 :y 1 :z 1))
  (with-slots (vals) woop
    (setf (aref vals 0)
          (v! gl-global-invocation-id 0)))
  (values))

(defpipeline-g test-compute-pline ()
  :compute test-compute-func)

(defpipeline-g test-compute-pline2 ()
  :compute test-compute-func2)

(defun test-compute ()
  (let* ((data (make-gpu-array
                nil :dimensions 1
                :element-type 'ssbo-test-data))
         (ssbo (make-ssbo data)))
    (unwind-protect
         (progn
           (map-g #'test-compute-pline
                  (make-compute-space 100)
                  :woop ssbo)
           (wait-on-gpu-fence (make-gpu-fence))
           (with-gpu-array-as-c-array (c-arr data)
             (aref-c
              (ssbo-test-data-vals (aref-c c-arr 0))
              0)))
      (free ssbo)
      (free data))))

(defun test-compute2 ()
  (let* ((data (make-gpu-array
                nil :dimensions 1
                :element-type 'some-more-data))
         (ssbo (make-ssbo data)))
    (unwind-protect
         (progn
           (map-g #'test-compute-pline2
                  (make-compute-space 100)
                  :woop ssbo)
           (wait-on-gpu-fence (make-gpu-fence))
           (with-gpu-array-as-c-array (c-arr data)
             (aref-c
              (some-more-data-vals (aref-c c-arr 0))
              0)))
      (free ssbo)
      (free data))))
