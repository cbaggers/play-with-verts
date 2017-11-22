(in-package :cepl.pipelines)

(defun poodle (name &rest args)
  (let* ((split (position-if #'keywordp args))
         (important-args (subseq args 0 split))
         (vtypes
          (mapcar #'guess-a-varjo-type important-args))
         (gfunc (%gpu-function (cons name vtypes)))
         (spec (cepl.pipelines::gpu-func-spec gfunc)))
    (with-gpu-func-spec spec
      (let* ((code `(lambda-g (&uniform ,@(append in-args uniforms))
                      (labels ((,name ()
                                 ,@body))
                        (values (v! 0 0 0 0)
                                (:feedback (,name))))))
             (pline (pipeline-g (:points)
                      :vertex (compile-g nil code)))
             (key-names (mapcan
                         (lambda (x y)
                           (list (intern (symbol-name (car x))
                                         :keyword)
                                 y))
                         in-args args))
             (call-args (append key-names (subseq args split)))
             (garray (make-gpu-array nil :element-type :vec4
                                     :dimensions 1))
             (tfs (cepl:make-transform-feedback-stream
                   garray)))
        (cepl:with-transform-feedback (tfs)
          (apply pline (cepl-context)
                 (cepl:make-buffer-stream nil :primitive :points)
                 call-args))
        (first (pull-g garray))))))
