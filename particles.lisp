(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *pbuffer-src* nil)
(defvar *pbuffer-dst* nil)
(defvar *stream-src* nil)
(defvar *stream-dst* nil)
(defvar *tfs-src* nil)
(defvar *tfs-dst* nil)
(defvar *inst-stream-src* nil)
(defvar *inst-stream-dst* nil)

(defun particle-swap ()
  (rotatef *pbuffer-src* *pbuffer-dst*)
  (rotatef *stream-src* *stream-dst*)
  (rotatef *tfs-src* *tfs-dst*)
  (rotatef *inst-stream-src* *inst-stream-dst*))

;;------------------------------------------------------------

(defun-g init-particle ((data pdata) &uniform (now :float))
  (let* ((id (* (+ gl-vertex-id now) 0.001))
         (range 100f0)
         (hrange (/ range 2))
         (pos (v! (- (* (rand (vec2 (* 3 id)))
                        range)
                     hrange)
                  (+ 2 (* (rand (vec2 (* 4 id)))
                          range))
                  (- (* (rand (vec2 (* 5 id)))
                        range)
                     hrange)))
         (target (v! 0 0 0))
         (vel (* (cross (normalize (- target pos))
                        (v! 0 1 0))
                 0.1)))
    (values
     (v! 0 0 0 0)
     (:feedback pos)
     (:feedback vel)
     (:feedback target)
     (:feedback 0f0))))

(defpipeline-g init-particle-pline (:points)
  :vertex (init-particle pdata))

(defun set-initial-particle-state (src-stream dst-tfs)
  (with-transform-feedback (dst-tfs)
    (map-g #'init-particle-pline src-stream
           :now (float (get-internal-real-time) 0f0))))

;;------------------------------------------------------------

(defun-g new-particle-vel ((pos :vec3)
                           (vel :vec3)
                           (target :vec3)
                           (id :int)
                           (now :float))
  (labels ((fudge ((id :int) (now :float) (fac :float))
             (+ 0.8
                (- (* 0.4 (* (sin id)
                             (sin (+ id now))))
                   0.2))))
    (let* ((dir (- target pos))
           (dist (max (vec3 50) (length dir)))
           (grav 12f0)
           (grav-fudge (* grav
                          (v! (fudge id now 0.0)
                              (fudge id now 0.2)
                              (fudge id now now))))
           (pull (/ (* (normalize dir)
                       grav-fudge)
                    dist)))
      (+ (* vel 0.999) (* pull 0.1)))))

(defun-g update-particle ((data pdata)
                          &uniform
                          (target0 :vec3)
                          (now :float))
  (let* ((pos (pdata-pos data))
         (vel (pdata-vel data))
         (target (pdata-target data))
         (new-vel (new-particle-vel pos vel target0 gl-instance-id now))
         (new-pos (+ pos vel)))
    (values
     (v! 0 0 0 0)
     (:feedback new-pos)
     (:feedback new-vel)
     (:feedback target)
     (:feedback (pdata-life data)))))

(defpipeline-g update-particle-pline (:points)
  :vertex (update-particle pdata))

(defun update-particle-state (src-stream dst-tfs)
  (let* ((factor (* (now) 0.4))
         (dist 80)
         (vec (v! (* dist (sin factor))
                  0
                  (* dist (cos (* 0.5 factor))))))
    (setf (pos *ball*) vec))
  (with-transform-feedback (dst-tfs)
    (map-g #'update-particle-pline src-stream
           :target0 (pos *ball*)
           :now (now))))

;;------------------------------------------------------------

(defun reset-particles ()
  (let ((count 64000))
    (unless *pbuffer-src*
      (setf *pbuffer-src* (make-gpu-array nil :dimensions count
                                          :element-type 'pdata)
            *pbuffer-dst* (make-gpu-array nil :dimensions count
                                          :element-type 'pdata)
            *stream-src* (make-buffer-stream *pbuffer-src*
                                             :primitive :points)
            *stream-dst* (make-buffer-stream *pbuffer-dst*
                                             :primitive :points)
            *tfs-src* (make-transform-feedback-stream *pbuffer-src*)
            *tfs-dst* (make-transform-feedback-stream *pbuffer-dst*))

      (destructuring-bind (vert index)
          (nineveh.mesh.data.primitives:cone-gpu-arrays
           :radius 1f0 :height 1f0)
        (setf *inst-stream-src*
              (make-buffer-stream
               (list vert (cons *pbuffer-dst* 1))
               :index-array index)))
      (destructuring-bind (vert index)
          (nineveh.mesh.data.primitives:cone-gpu-arrays
           :radius 1f0 :height 1f0)
        (setf *inst-stream-dst*
              (make-buffer-stream
               (list vert (cons *pbuffer-src* 1))
               :index-array index)))))
  (set-initial-particle-state *stream-src* *tfs-dst*)
  (set-initial-particle-state *stream-dst* *tfs-src*))
