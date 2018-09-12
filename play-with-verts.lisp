(in-package #:play-with-verts)

;;------------------------------------------------------------

;; https://learnopengl.com/Advanced-Lighting/Gamma-Correction
;;
;; See fxaa.lisp for the fragment shader where we are applying
;; the gamma correction. Also see assets.lisp for where we now
;; load textures in :srgb8-alpha8

;;------------------------------------------------------------

(defvar *last-time* (get-internal-real-time))

(defvar *scene-fbo* nil)
(defvar *scene-fbo2* nil)
(defvar *scene-sampler* nil)
(defvar *bright-sampler* nil)
(defvar *final-sampler* nil)

(defvar *ping* nil)
(defvar *pong* nil)
(defvar *ping-sampler* nil)
(defvar *pong-sampler* nil)

(defvar *scene-depth-sampler* nil)
(defvar *fallback-normal-map* nil)

(defun reset (&key force)
  (when (or force (not *scene-fbo*))
    (setf (clear-color) (v! 0.2 0.2 0.2 1))
    (setf *things* nil)
    ;;(make-ground)
    (make-box (v! 0 0 0) (v! 40 1 40))
    (make-box (v! 0 15 -20) (v! 30 30 1))
    (make-ball (v! 0 10 20) 3.0)
    (setf *fallback-normal-map*
          (samples
           (make-texture (list (list (v! 0.5 0.5 1)))
                         :dimensions '(1 1)
                         :mipmap nil)))
    (test2)
    (reset-lights))
  (reset-fbos)
  (reset-camera))

;; GL_TEXTURE_MIN_FILTER, GL_LINEAR
;; GL_TEXTURE_MAG_FILTER, GL_LINEAR
;; GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE
;; GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE

(defun reset-fbos ()
  (when *scene-fbo*
    (free *scene-fbo*)
    (free *scene-fbo2*)
    (free *ping*)
    (free *pong*))
  (setf *scene-fbo*
        (make-fbo (list 0 :element-type :rgba16f)
                  (list 1 :element-type :rgba16f)
                  :d))
  (setf *scene-fbo2*
        (make-fbo (list 0 :element-type :rgba16f)))

  (setf *ping*
        (make-fbo (list 0 :element-type :rgba16f)))
  (setf *pong*
        (make-fbo (list 0 :element-type :rgba16f)))
  (setf *ping-sampler*
        (sample (attachment-tex *ping* 0)
                :minify-filter :linear
                :magnify-filter :linear
                :wrap :clamp-to-edge))
  (setf *pong-sampler*
        (sample (attachment-tex *pong* 0)
                :minify-filter :linear
                :magnify-filter :linear
                :wrap :clamp-to-edge))

  (setf *scene-sampler*
        (sample (attachment-tex *scene-fbo* 0)))
  (setf *final-sampler*
        (sample (attachment-tex *scene-fbo2* 0)))
  (setf *bright-sampler*
        (sample (attachment-tex *scene-fbo* 1)))
  (setf *scene-depth-sampler*
        (sample (attachment-tex *scene-fbo* :d))))

(defun reset-lights ()
  (when *lights*
    (free *lights*)
    (free *lights-arr*))

  (let* ((light-data (make-c-array nil :dimensions 1
                                   :element-type 'light-set))
         (set (aref-c light-data 0))
         (plights (light-set-plights set)))

    (setf (light-set-count set) 3)

    (setf (aref-c plights 0) (list (v! 0 5 -18)
                                   (v! 1 1 1)
                                   1300.0)
          (aref-c plights 1) (list (v! 0 120 24)
                                   (v! 1 1 1)
                                   3200.0)
          (aref-c plights 2) (list (v! 200 15 -10)
                                   (v! 1 1 1)
                                   3200.0))

    (setf *lights-arr* (make-gpu-array light-data)
          *lights* (make-ubo *lights-arr* 'light-set))))

(defun game-step ()
  (let* ((now (get-internal-real-time))
         (delta (* (- now *last-time*) 0.001))
         (delta (if (> delta 0.16) 0.00001 delta)))
    (setf *last-time* now)

    ;; update camera
    (update *current-camera* delta)

    ;; set the position of our viewport
    (setf (resolution (current-viewport))
          (surface-resolution (current-surface)))

    ;; draw stuff
    (with-fbo-bound (*scene-fbo*)
      (clear-fbo *scene-fbo*)
      (loop :for thing :in *things* :do
           (update thing delta)
           (draw *current-camera* thing)))

    (let ((horizontal t)
          (amount 80)
          (current-sampler *ping-sampler*)
          (next-sampler *pong-sampler*)
          (current-fbo *pong*)
          (next-fbo *ping*))
      (dotimes (i amount)
        (with-fbo-bound (current-fbo)
          (clear-fbo current-fbo)
          (map-g #'hblur-pline (nineveh:get-quad-stream-v2)
                 :image (if (= i 0)
                            *bright-sampler*
                            current-sampler)
                 :horizontal (if horizontal 1 0))
          (setf horizontal (not horizontal))
          (rotatef current-sampler next-sampler)
          (rotatef current-fbo next-fbo)))

      (with-fbo-bound (*scene-fbo2*)
        (clear-fbo *scene-fbo2*)
        (map-g #'compose-bloom-pline
               (nineveh:get-quad-stream-v2)
               :sam0 *scene-sampler*
               :sam1 next-sampler)))

    (as-frame
      (fxaa3-pass *final-sampler*)
      ;;(draw-tex *scene-sampler*)
      )

    (setf (x (pos ball)) (+ 40.0 (* (sin (now)) 60)))

    (decay-events)))

(def-simple-main-loop play (:on-start #'reset)
  (game-step))


;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
