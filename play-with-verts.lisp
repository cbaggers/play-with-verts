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
(defvar *scene-sampler* nil)
(defvar *scene-depth-sampler* nil)

(defun reset ()
  (setf (clear-color) (v! 0.002 0.002 0.002 0))
  (setf *things* nil)
  (make-ground)
  (make-box (v! 0 30 -15) (v! 30 60 2))
  (make-box (v! 15 30 0) (v! 2 60 30))
  (make-box (v! -15 30 0) (v! 2 60 30))
  (loop :for i :below 10 :do
       (make-ball (v! (- (random 20f0) 10f0)
                      (+ 3 (random 40f0))
                      (- (random 20f0) 10f0))
                  2))
  (when *scene-fbo*
    (free *scene-fbo*))
  (setf *scene-fbo*
        (make-fbo (list 0 :element-type :rgba16f) :d))
  (setf *scene-sampler*
        (sample (attachment-tex *scene-fbo* 0)))
  (setf *scene-depth-sampler*
        (sample (attachment-tex *scene-fbo* :d))))

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
           (draw #'some-pipeline *current-camera* thing)))

    (clear)
    (splat *scene-sampler*)
    (swap)

    (decay-events)))


(def-simple-main-loop play (:on-start #'reset)
  (game-step))


;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
