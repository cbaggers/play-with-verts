(in-package #:play-with-verts)

;;------------------------------------------------------------

(defstruct (%rtarget (:constructor %make-rtarget))
  (fbo (error "fbo is required") :type fbo)
  (col-samplers (error "samplers are required") :type (simple-array sampler (*)))
  (depth-sampler nil :type (or null sampler)))

(defun make-rtarget (&rest fuzzy-attach-args)
  (let* ((fbo (apply #'make-fbo fuzzy-attach-args))
         (col-samplers (loop :for i :below 10 :collect
                          (when (attachment fbo i)
                            (sample (attachment-tex fbo i)))))
         (depth-sampler (when (attachment fbo :d)
                             (sample (attachment-tex fbo :d)))))
    (%make-rtarget
     :fbo fbo
     :col-samplers (make-array (length col-samplers)
                               :initial-contents col-samplers )
     :depth-sampler depth-sampler)))

(defstruct (render-target (:constructor %make-render-target))
  (double-buffered-p nil :type boolean)
  (front (error "front is required") :type %rtarget)
  (back (error "front is required") :type (or null %rtarget)))

(defun make-render-target (double-buffered-p &rest fuzzy-attach-args)
  (%make-render-target
   :double-buffered-p double-buffered-p
   :front (apply #'make-rtarget fuzzy-attach-args)
   :back (when double-buffered-p
           (apply #'make-rtarget fuzzy-attach-args))))

(defun rt-sampler (rt name)
  (let ((rt (if (render-target-double-buffered-p rt)
                (render-target-back rt)
                (render-target-front rt))))
    (assert rt)
    (if (eq name :d)
        (%rtarget-depth-sampler rt)
        (aref (%rtarget-col-samplers rt) name))))

(defun rt-sampler-front (rt name)
  (let ((rt (render-target-front rt)))
    (assert rt)
    (if (eq name :d)
        (%rtarget-depth-sampler rt)
        (aref (%rtarget-col-samplers rt) name))))

(defun rt-sampler-back (rt name)
  (let ((rt (render-target-back rt)))
    (assert rt)
    (if (eq name :d)
        (%rtarget-depth-sampler rt)
        (aref (%rtarget-col-samplers rt) name))))

(defun rt-fbo (rt)
  (let ((rt (render-target-front rt)))
    (assert rt)
    (%rtarget-fbo rt)))

(defun rt-swap (rt)
  (assert (render-target-double-buffered-p rt))
  (rotatef (render-target-back rt)
           (render-target-front rt))
  rt)

;;------------------------------------------------------------
