(in-package :play-with-verts)

(defstruct-g tb-data
  (tangent :vec3)
  (bitangent :vec3))

(defun add-tangents (g-verts g-indices)
  (let* ((verts (pull1-g g-verts))
         (indices (pull-g g-indices))
         (result (make-gpu-array
                  nil :dimensions (first (dimensions verts))
                  :element-type 'tb-data)))
    (with-gpu-array-as-c-array (data result)
      (loop :for (i0 i1 i2)
         :on indices
         :by #'cdddr
         :do (let ((pair (calc verts i0 i1 i2)))
               (setf (aref-c data i0) pair)
               (setf (aref-c data i1) pair)
               (setf (aref-c data i2) pair))))
    result))

(defun calc (verts i0 i1 i2)
  (let* ((pos1 (pos (aref-c verts i0)))
         (pos2 (pos (aref-c verts i1)))
         (pos3 (pos (aref-c verts i2)))
         (uv1 (tex (aref-c verts i0)))
         (uv2 (tex (aref-c verts i1)))
         (uv3 (tex (aref-c verts i2)))
         ;;
         (edge1 (v3:- pos2 pos1))
         (edge2 (v3:- pos3 pos1))
         (delta-uv1 (v2:- uv2 uv1))
         (delta-uv2 (v2:- uv3 uv1))
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
    (list tangent1 bitangent1)))
