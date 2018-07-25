(in-package :play-with-verts)

(defstruct-g tb-data
  (tangent :vec3)
  (bitangent :vec3))

(defun test (g-verts g-indices)
  (let ((verts (pull1-g g-verts))
        (indices (pull-g g-indices)))
    (loop :for (i0 i1 i2 i3 i4 i5)
       :on indices
       :by (lambda (x) (nthcdr 6 x))
       :append
         (calc verts i0 i1 i2 i5))))

(defun test2 (g-vert g-indices)
  (make-gpu-array (test g-vert g-indices)
                  :element-type 'tb-data))

(defun calc (verts i0 i1 i2 i3)
  (declare (ignore i3))
  (let* ((pos1 (pos (aref-c verts i0)))
         (pos2 (pos (aref-c verts i1)))
         (pos3 (pos (aref-c verts i2)))
         ;; (pos4 (pos (aref-c verts i3)))
         (uv1 (tex (aref-c verts i0)))
         (uv2 (tex (aref-c verts i1)))
         (uv3 (tex (aref-c verts i2)))
         ;; (uv4 (tex (aref-c verts i3)))
         ;; (norm (norm (aref-c verts i0)))
         ;;
         (edge1 (v3:- pos2 pos1))
         (edge2 (v3:- pos3 pos1))
         (delta-uv1 (v3:- uv2 uv1))
         (delta-uv2 (v3:- uv3 uv1))
         ;;
         (f (/ 1.0 (- (* (x delta-uv1) (y delta-uv2))
                      (* (x delta-uv2) (y delta-uv1)))))
         ;;
         (tangent1
          (v3:normalize
           (v! (* f (- (* (y delta-uv2) (x edge1))
                       (* (y delta-uv1) (x edge2))))
               (* f (- (* (y delta-uv2) (y edge1))
                       (* (y delta-uv1) (y edge2))))
               (* f (- (* (y delta-uv2) (z edge1))
                       (* (y delta-uv1) (z edge2)))))))
         (bitangent1
          (v3:normalize
           (v! (* f (+ (* (- (x delta-uv2)) (x edge1))
                      (* (x delta-uv1) (x edge2))))
               (* f (+ (* (- (x delta-uv2)) (y edge1))
                       (* (x delta-uv1) (y edge2))))
               (* f (+ (* (- (x delta-uv2)) (z edge1))
                       (* (x delta-uv1) (z edge2))))))))
    (list (list tangent1 bitangent1)
          (list tangent1 bitangent1)
          (list tangent1 bitangent1)
          (list tangent1 bitangent1))))
