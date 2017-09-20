(in-package #:play-with-verts)

(defparameter *vector-field* nil)
(defparameter *vector-field-sampler* nil)

(defun-g vert ((vert :vec2))
  (values (v! vert 0 1)
          (+ (vec2 0.5) (* vert 0.5))))

(defun-g frag ((uv :vec2) &uniform (now :float))
  (let* ((noise (perlin-noise (v! (* uv 8) (* now 0.001))))
         (angle (* noise pi-f))
         (vec (v! (sin angle)
                  (cos angle))))
    (v! angle 0 1)))

(defpipeline-g noise-pass ()
  (vert :vec2)
  (frag :vec2))

(defun blit-noise ()
  (with-fbo-bound (*vector-field*)
    (clear *vector-field*)
    (map-g #'noise-pass (get-quad-stream-v2)
           :now (float (get-internal-real-time) 0f0))))

;;------------------------------------------------------------

(defclass particles ()
  ((src :initarg :src :accessor front)
   (dst :initarg :dst :accessor back)
   (src-sampler :initarg :src-sampler)
   (dst-sampler :initarg :dst-sampler)))

(defun make-particles-data ()
  (let* ((src-fbo (make-fbo 0 :d))
         (dst-fbo (make-fbo 0 :d))
         (src-sampler (sample (attachment-tex src-fbo 0)))
         (dst-sampler (sample (attachment-tex dst-fbo 0))))
    (make-instance 'particles
                   :src src-fbo
                   :dst dst-fbo
                   :src-sampler src-sampler
                   :dst-sampler dst-sampler)))

(defvar *particles* nil)

(defun-g part-frag ((uv :vec2)
                    &uniform (dt :float)
                    (particles :sampler-2d)
                    (vec-field :sampler-2d))
  (let* ((particle (texture particles uv))
         (pos (s~ particle :xy))
         (vel (s~ particle :zw))
         (foo (texture vec-field uv))
         (force (s~ foo :xy))
         (new-pos (+ pos (* vel dt)))
         (new-vel (+ vel (* force dt))))
    (v! new-pos new-vel)))

(defpipeline-g particle-update-pass ()
  (vert :vec2)
  (part-frag :vec2))

(defun update-particles (delta-time)
  (with-slots (src dst src-sampler dst-sampler) *particles*
    (with-fbo-bound (dst)
      (clear dst)
      (map-g #'particle-update-pass (get-quad-stream-v2)
             :dt delta-time
             :particles src-sampler
             :vec-field *vector-field-sampler*))
    (rotatef src dst)
    (rotatef src-sampler dst-sampler)))

;;------------------------------------------------------------
