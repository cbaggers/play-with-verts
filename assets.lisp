(in-package #:play-with-verts)

;;------------------------------------------------------------
;; Meshes
;;
;; We cache the data based on the the arguments so we don't
;; get lots of instances in memory

(defvar *meshes* (make-hash-table :test #'equal))

(defun sphere (&optional (radius 1f0))
  (let ((key radius))
    (or (gethash key *meshes*)
        (destructuring-bind (vert index)
            (nineveh.mesh.data.primitives:sphere-gpu-arrays :radius radius)
          (setf (gethash key *meshes*)
                (make-buffer-stream (list vert (test vert index))
                                    :index-array index))))))

(defun box (&optional (w 1f0) (h 1f0) (d 1f0))
  (let ((key (list w h d)))
    (or (gethash key *meshes*)
        (destructuring-bind (vert index)
            (nineveh.mesh.data.primitives:box-gpu-arrays :width w
                                                         :height h
                                                         :depth d)
          (setf (gethash key *meshes*)
                (make-buffer-stream (list vert (test vert index))
                                    :index-array index))))))

(defun cylinder (&optional (radius 1f0) (height 1f0))
  (let ((key (list radius height)))
    (or (gethash key *meshes*)
        (destructuring-bind (vert index)
            (nineveh.mesh.data.primitives:cylinder-gpu-arrays :radius radius
                                                              :height height)
          (setf (gethash key *meshes*)
                (make-buffer-stream (list vert (test vert index))
                                    :index-array index))))))

(defun cone (&optional (radius 1f0) (height 1f0))
  (let ((key (list radius height)))
    (or (gethash key *meshes*)
        (destructuring-bind (vert index)
            (nineveh.mesh.data.primitives:cone-gpu-arrays :radius radius
                                                          :height height)
          (setf (gethash key *meshes*)
                (make-buffer-stream (list vert (test vert index))
                                    :index-array index))))))

;;------------------------------------------------------------
;; Textures & Samplers
;;
;; We cache the data based on the the path so we don't
;; get lots of instances in memory

(defvar *samplers* (make-hash-table :test #'equal))

(defun get-tex (path &optional (force nil) (mipmap t))
  (when force
    (let ((s (gethash path *samplers*)))
      (when s
        (free (sampler-texture s)))
      (remhash path *samplers*)))
  (or (gethash path *samplers*)
      (setf (gethash path *samplers*)
            (sample
             (dirt:load-image-to-texture
              (asdf:system-relative-pathname
               :play-with-verts path)
              :rgba8
              mipmap
              t)))))

;;------------------------------------------------------------

(defstruct-g assimp-mesh
  (pos :vec3)
  (normal :vec3)
  (tangent :vec3)
  (bitangent :vec3)
  (uv :vec2))

(defun assimp-mesh-to-thing (scene-path scene mesh)
  (with-slots ((vertices ai:vertices)
               (normals ai:normals)
               (tangents ai:tangents)
               (bitangents ai:bitangents)
               (texture-coords ai:texture-coords)
               (faces ai:faces)
               (mat-index ai:material-index))
      mesh
    (let* ((texture-coords (elt texture-coords 0))
           (material (aref (slot-value scene 'ai:materials) mat-index))
           (textures (gethash "$tex.file" material))
           (sampler (get-tex "rust.jpg")
            ;; (when textures
            ;;   (get-tex
            ;;    (merge-pathnames
            ;;     (uiop:pathname-directory-pathname scene-path)
            ;;     (third (assoc :ai-texture-type-diffuse textures)))))
             ))
      (assert (= (length bitangents)
                 (length tangents)
                 (length normals)
                 (length vertices)
                 (length texture-coords)))
      (let ((v-arr (make-gpu-array nil :dimensions (length vertices)
                                   :element-type 'assimp-mesh))
            (i-arr (make-gpu-array nil :dimensions (* 3 (length faces))
                                   :element-type :ushort)))
        (with-gpu-array-as-c-array (c-arr i-arr)
          (loop
             :for indices :across faces
             :for i :from 0 :by 3
             :do (setf (aref-c c-arr i) (aref indices 0)
                       (aref-c c-arr (+ i 1)) (aref indices 1)
                       (aref-c c-arr (+ i 2)) (aref indices 2))))
        (with-gpu-array-as-c-array (c-arr v-arr)
          (loop
             :for v :across vertices
             :for n :across normals
             :for ta :across tangents
             :for bt :across bitangents
             :for tc :across texture-coords
             :for i :from 0
             :for a := (aref-c c-arr i)
             :do (setf (assimp-mesh-pos a) v
                       (assimp-mesh-normal a) n
                       (assimp-mesh-tangent a) ta
                       (assimp-mesh-bitangent a) bt
                       (assimp-mesh-uv a) (v! (x tc) (y tc)))))
        (make-instance 'assimp-thing
                       :stream (make-buffer-stream v-arr :index-array i-arr)
                       :sampler sampler)))))

(defun test2 ()
  (load-assimp-things "/home/baggers/3dModels/sponza.obj"))

(defun load-assimp-things (path)
  (let ((scene (classimp:import-into-lisp
                path
                :processing-flags
                '(:ai-process-calc-tangent-space
                  :ai-process-triangulate
                  :ai-process-gen-normals))))
    (loop
       :for mesh :across (slot-value scene 'ai:meshes)
       :for thing := (assimp-mesh-to-thing path scene mesh)
       :do (push thing *things*)))
  t)
