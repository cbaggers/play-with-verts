(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *score* 100)
(defvar *high-score* 100)

(defvar *fps* 0)
(defvar *fps-wip* 0)
(defvar *stepper* (make-stepper (seconds 1)))
(defvar *delta* 1)
(defvar *can-fire* t)
(defvar *last-time* (get-internal-real-time))

(defun reset ()
  (setf *score* 100
        *stuck-things* nil
        *falling-things*
        (append (loop :for i :below 6 :collect
                   (make-pepperoni))
                (loop :for i :below 10 :collect
                   (make-olive)))))

(defun draw ()
  (incf *fps-wip*)
  (when (funcall *stepper*)
    (setf *fps* *fps-wip*
          *fps-wip* 0))
  (setf *delta* (min 0.01 (/ 1.0 *fps*)))

  ;; update camera
  (update-camera *camera* *delta*)

  ;; Update the position of our light
  (let ((val (* 10 (now))))
    (setf *light-pos* (v! (* 20 (sin val))
                          20
                          (- (* 20 (cos val)) 14))))

  ;; set the position of our viewport
  (setf (resolution (current-viewport))
        (surface-resolution (current-surface *cepl-context*)))

  ;; clear the default fbo
  (clear)

  ;; render ALL THE *THINGS*
  (upload-uniforms-for-cam *camera*)

  (draw-pizza *pizza-base*)

  (loop :for thing :in *falling-things* :do
     (update-falling-thing thing)
     (draw-thing thing)
     (draw-shadow thing))

  (loop :for thing :in *stuck-things* :do
     (draw-thing thing))

  (loop :for bullet :in *bullets* :do
     (update-bullet bullet)
     (draw-thing bullet))

  ;; player
  (update-player *player*)
  (draw-thing *player*)

  ;; game state stuff
  (setf *high-score* (max *high-score* *score*))
  (when (<= *score* 0)
    (print "YOU SUCK!")
    (reset))

  ;; display what we have drawn
  (swap)
  (decay-events))


(defun init ()
  (unless *player*
    (make-player))

  (unless *pizza-base*
    (make-pizza-base))

  (reset))


(def-simple-main-loop play (:on-start #'init)
  (draw))


;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
