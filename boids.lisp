(in-package #:play-with-verts)

;; Floor

(defvar *boid-length* 1f0)

(defclass boid (thing)
  ((stream :initform (cone (* *boid-length* 0.5)
                           *boid-length*))
   (sampler :initform (tex "blah.jpg"))
   (vel :initform (v! 0 0 0) :accessor vel)))

(defvar *boids* nil)

(defun make-boid ()
  (let ((boid (make-instance 'boid)))
    (push boid *boids*)
    (push boid *things*)
    (setf (pos boid)
          (v! (- (random 40f0) 20f0)
              (random 20f0)
              (- (random 40f0) 20f0)))
    boid))

(defun clamp-to-box (boid min max)
  (let ((pos (pos boid))
        (range (v3:- max min)))
    (setf (pos boid)
          (v! (+ (x min) (mod (- (x pos) (x min)) (x range)))
              (+ (y min) (mod (- (y pos) (y min)) (y range)))
              (+ (z min) (mod (- (z pos) (z min)) (z range)))))))

(defmethod update ((boid boid) dt)
  (let ((neighbors (get-neighbors boid)))
    (when neighbors
      (let* ((dir0 (seperation boid neighbors))
             (dir1 (alignment boid neighbors))
             (dir2 (cohesion boid neighbors))
             (sum (v3:*s (v3:+ (v3:*s dir0 1f0)
                               (v3:*s dir1 1f0)
                               (v3:*s dir2 1f0))
                         dt)))
        (v3:incf (vel boid) sum)))
    (v3:incf (pos boid) (v3:*s (vel boid) dt))
    (clamp-to-box boid (v! -20 0 -20) (v! 20 20 20))
    (setf (rot boid)
          (q:* (q:from-direction (v! 0 1 0) (vel boid))
               (q:from-axis-angle (v! 1 0 0) (radians -90f0))))))

(defparameter *neighbor-dist* 5f0)

(defun get-neighbors (boid)
  (loop :for b :in *boids*
     :when (< (v3:distance (pos b) (pos boid))
              *neighbor-dist*)
     :collect b))

(defparameter *seperation-dist* 2f0)

(defun seperation (boid neighbors)
  "steer to avoid crowding local flockmates"
  (let ((dir (v! 0 0 0)))
    (loop :for b :in neighbors :do
       (when (and (not (eq b boid))
                  (< (v3:distance (pos b) (pos boid))
                     *seperation-dist*))
         (v3:decf dir (v3:- (pos b) (pos boid)))))
    dir))

(defun alignment (boid neighbors)
  "steer towards the average heading of local flockmates"
  (let ((sum (v! 0 0 0)))
    (loop :for b :in neighbors :do
       (v3:incf sum (vel b)))
    (let* ((avg (v3-n:/s sum
                         (+ 1f0 (float (length neighbors) 0f0))))
           (diff (v3:- avg (vel boid))))
      (v3:/s diff 18f0))))


(defun cohesion (boid neighbors)
  "steer to move toward the average position
   (center of mass) of local flockmates"
  (let ((sum (v! 0 0 0)))
    (loop :for b :in neighbors :do
       (v3:incf sum (pos b)))
    (let* ((avg (v3-n:/s sum (float (length neighbors) 0f0)))
           (diff (v3:- avg (pos boid))))
      (v3:normalize diff))))
