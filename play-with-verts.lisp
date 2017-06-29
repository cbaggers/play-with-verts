(in-package #:play-with-verts)

;;------------------------------------------------------------

(defvar *fps* 0)
(defvar *fps-wip* 0)
(defvar *stepper* (make-stepper (seconds 1)))
(defvar *delta* 1)
(defvar *can-fire* t)

(defun draw ()
  (incf *fps-wip*)
  (when (funcall *stepper*)
    (setf *fps* *fps-wip*
          *fps-wip* 0))
  (setf *delta* (min 0.5 (/ 1.0 *fps*)))

  ;; tell the host to pump all the events
  (step-host)

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
  (draw-thing *floor* *camera*)

  (loop :for thing :in *things* :do
     (update-falling-thing thing)
     (draw-thing thing *camera*))

  (loop :for bullet :in *bullets* :do
     (update-bullet bullet)
     (draw-thing bullet *camera*))

  ;; player
  (update-player *player*)
  (draw-thing *player* *camera*)

  ;; display what we have drawn
  (swap)
  (decay-events))


(defun init ()
  (unless *things*
    (loop :for i :below 40 :do
       (push (make-falling-thing) *things*)))

  (unless *player*
    (setf *player* (make-player)))

  (unless *floor*
    (setf *floor* (make-floor))))


(def-simple-main-loop play (:on-start #'init)
  (draw))


;; (sdl2-game-controller-db:load-db)
;; (defvar *pad* (sdl2:game-controller-open 0))
;; (skitter.sdl2:enable-background-joystick-events)
