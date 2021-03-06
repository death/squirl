(in-package :squirl-demo)

(defclass planet-demo (demo)
  ((planet :initarg :planet :accessor planet))
  (:default-initargs :name "Planetary Gravity OMFG."))

(defmethod update-demo ((demo planet-demo) dt)
  (incf (accumulator demo) (if (> dt *dt-threshold*) *dt-threshold* dt))
  (loop while (>= (accumulator demo) (physics-timestep demo))
     do (world-step (world demo) (physics-timestep demo))
       (body-update-position (planet demo) (physics-timestep demo))
     (decf (accumulator demo) (physics-timestep demo))))

(defbody planetary-body)

(defmethod body-update-velocity ((body planetary-body) gravity damping dt)
  (declare (ignore gravity))
  (let* ((position (body-position body))
         (gravity (vec* position (/ -50000 (vec. position position)))))
    (call-next-method body gravity damping dt)))

(defun random-position (radius)
  (loop for vec = (vec (- (random (- 640 (* 2 radius)))
                          (- 320 radius))
                       (- (random (- 480 (* 2 radius)))
                          (- 240 radius)))
     when (< 88 (vec-length vec) 200)
     return vec))

(defun add-box (world)
  (let* ((size 10)
         (mass 1)
         (verts (list (vec (- size) (- size))
                      (vec (- size) size)
                      (vec size size)
                      (vec size (- size)))))
      (world-add-body
       world
       (make-planetary-body :mass mass
                            :position (random-position (vec-length (vec size size)))
                            :velocity (vec* (angle->vec (* pi (random 2d0)))
                                            (random 200d0))
                            :shapes (list
                                     (make-poly verts :friction 0.7 :restitution 1))))))

(defmethod init-demo ((demo planet-demo))
  (let ((world (make-world :iterations 20)))
    (setf (planet demo)
          (make-body :angular-velocity 0.3 :actor :not-grabbable
                     :shapes (list (make-circle 70 :restitution 1 :friction 0.8))))
    (reset-shape-id-counter)
    (loop repeat 22 do (add-box world))
    (world-add-body world (planet demo))
    world))

(provide-demo 'planet-demo)
