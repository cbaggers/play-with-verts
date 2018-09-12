(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *max-vertex-buffer-size* (* 512 1024))
(defvar *max-element-buffer-size* (* 128 1024))

(defclass ui ()
  ((nk-context :initform (nk:make-context))
   (nk-renderer :initform (nk:make-renderer))
   (pixel-ratio :initform 0f0)
   (level :initform :easy)
   (compression :initform (cffi:foreign-alloc :float))
   (background-color :initform (v! 0.10 0.18 0.24 1))))

(defvar *ui* nil)

(defun init-ui ()
  (unless *ui*
    (let ((ui (make-instance 'ui)))
      (setf *ui* ui)
      (with-slots (nk-context nk-renderer) ui
        (%nk:style-set-font nk-context (nk:renderer-font nk-renderer))))))

(defun register-input ()
  (with-slots (nk-context) *ui*
    (%nk:input-begin nk-context)

    (let* ((cursor (mouse-pos (mouse)))
           (cursor-x (floor (aref cursor 0)))
           (cursor-y (floor (aref cursor 1))))
      (%nk:input-button nk-context
                        %nk:+button-left+
                        cursor-x cursor-y
                        (if (mouse-button (mouse) 1)
                            %nk:+true+
                            %nk:+false+))
      (%nk:input-motion nk-context cursor-x cursor-y))
    (%nk:input-end nk-context)))

(defun compose-nuklear ()
  (with-slots (nk-context level compression background-color) *ui*
    (claw:c-with ((rect (:struct (%nk:rect))))
      (let ((val (%nk:begin nk-context "Demo" (%nk:rect rect 50f0 50f0 230f0 250f0)
                            (logior %nk:+window-border+ %nk:+window-movable+ %nk:+window-scalable+
                                    %nk:+window-minimizable+ %nk:+window-title+))))
        (unless (= val 0)
          (%nk:layout-row-static nk-context 30f0 80 1)
          (unless (= (%nk:button-label nk-context "button") 0)
            (format t "~&button pressed"))

          (%nk:layout-row-dynamic nk-context 30f0 2)
          (unless (= (%nk:option-label nk-context "easy" (if (eq level :easy) 1 0)) 0)
            (setf level :easy))
          (unless (= (%nk:option-label nk-context "eard" (if (eq level :hard) 1 0)) 0)
            (setf level :hard))

          (%nk:layout-row-dynamic nk-context 25f0 1)
          (%nk:property-float nk-context "Compression:" 0f0 compression 100f0 10f0 1f0)))
      (%nk:end nk-context))))

(defun step-ui ()
  (with-slots (nk-renderer nk-context) *ui*
    (register-input)
    (compose-nuklear)
    (destructuring-bind (width height)
        (surface-dimensions (current-surface))
      (render-ui nk-renderer nk-context width height))
    (%nk:clear nk-context)))

(defstruct-g ui-vert
  (pos :vec2)
  (uv :vec2)
  (color :vec4))

(defun-g ui-vert ((vert ui-vert)
                  &uniform
                  (proj-mtx :mat4))
  (with-slots (pos uv color) vert
    (values
     (* proj-mtx (v! pos 0 1))
     uv
     color)))

(defun-g ui-frag ((uv :vec2)
                  (color :vec4)
                  &uniform
                  (tex :sampler-2d))
  (* color (texture tex uv)))

(defpipeline-g ui-pipeline ()
  (ui-vert ui-vert)
  (ui-frag :vec2 :vec4))

(defclass renderer ()
  (buffer
   draw-null-texture
   font-atlas))

(defun foo ()
  (let ((cmd-buffer (cffi:foreign-alloc '(:struct %nk:buffer)) ))))

(defun render-ui (renderer nk-context width height)
  )
