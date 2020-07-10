(in-package :squirl-demo)

(defclass springies-demo (demo)
  ((static-body :initarg :box :accessor static-body))
  (:default-initargs :name "Sproing twang!" :physics-timestep (float 1/60 1d0)))

(defstruct (springy-spring (:include damped-spring)
                           (:constructor
                            make-springy-spring
                            (body-a body-b anchor1 anchor2 rest-length stiffness damping))))

(defmethod squirl::spring-force (spring distance &aux (clamp 20d0))
  (* (squirl::clamp (- (squirl::damped-spring-rest-length spring) distance) (- clamp) clamp)
     (squirl::damped-spring-stiffness spring)))

(defclass springy ()
  ((bodies :initarg :bodies :initform nil :accessor springy-bodies)))

(defmethod initialize-instance :after ((springy springy) &key)
  (dolist (body (springy-bodies springy))
    (setf (body-actor body) springy)))

(defcollision ((a springy) (b springy) contacts) (unless (eq a b) t))

(defun build-springies (world)
  (let ((springies
         (list (make-instance 'springy :bodies (list (add-bar world (vec -240 160) (vec -160 80))
                                                     (add-bar world (vec -160 80) (vec -80 160))))
               (make-instance 'springy :bodies (list (add-bar world (vec 0 160) (vec 80 0))))
               (make-instance 'springy :bodies (list (add-bar world (vec 160 160) (vec 240 160))))
               (make-instance 'springy :bodies (list (add-bar world (vec -240 0) (vec -160 -80))
                                                     (add-bar world (vec -160 -80) (vec -80 0))
                                                     (add-bar world (vec -80 0) (vec 0 0))))
               (make-instance 'springy :bodies (list (add-bar world (vec 0 -80) (vec 80 -80))))
               (make-instance 'springy :bodies (list (add-bar world (vec 240 80) (vec 160 0))
                                                     (add-bar world (vec 160 0) (vec 240 -80))))
               (make-instance 'springy :bodies (list (add-bar world (vec -240 -80) (vec -160 -160))
                                                     (add-bar world (vec -160 -160) (vec -80 -160))))
               (make-instance 'springy :bodies (list (add-bar world (vec 0 -160) (vec 80 -160))))
               (make-instance 'springy :bodies (list (add-bar world (vec 160 -160) (vec 240 -160)))))))
    (world-add-constraint world (make-pivot-joint (first (springy-bodies (first springies)))
                                                  (second (springy-bodies (first springies)))
                                                  (vec 40 -40) (vec -40 -40)))
    (world-add-constraint world (make-pivot-joint (first (springy-bodies (fourth springies)))
                                                  (second (springy-bodies (fourth springies)))
                                                  (vec 40 -40) (vec -40 -40)))
    (world-add-constraint world (make-pivot-joint (second (springy-bodies (fourth springies)))
                                                  (third (springy-bodies (fourth springies)))
                                                  (vec 40 40) (vec -40 0)))
    (world-add-constraint world (make-pivot-joint (first (springy-bodies (sixth springies)))
                                                  (second (springy-bodies (sixth springies)))
                                                  (vec -40 -40) (vec -40 40)))
    (world-add-constraint world (make-pivot-joint (first (springy-bodies (seventh springies)))
                                                  (second (springy-bodies (seventh springies)))
                                                  (vec 40 -40) (vec -40  0)))
    springies))

(defun add-bar (world point-a point-b)
  (let* ((center (vec* (vec+ point-a point-b) 0.5d0))
         (length (vec-length (vec- point-a point-b)))
         (mass (/ length 160)))
    (world-add-body world
                    (make-body :mass mass :inertia (* mass length (/ length 12)) :position center
                               :shapes (list (make-segment (vec- point-a center) (vec- point-b center)
                                                           :radius 10))))))

(defun add-springs (world static-body springies &aux (sb static-body))
  (let ((bodies (mapcan (lambda (_) (copy-list (springy-bodies _))) springies))
        (stiffness 100) (damping 0.5))
    (flet ((con (body-a-or-n body-b anchor1-x anchor1-y anchor2-x anchor2-y)
             (world-add-constraint world (make-springy-spring
                                          (if (numberp body-a-or-n)
                                              (elt bodies (1- body-a-or-n))
                                              body-a-or-n)
                                          (elt bodies (1- body-b))
                                          (vec anchor1-x anchor1-y)
                                          (vec anchor2-x anchor2-y)
                                          0 stiffness damping))))
      ;; against static body
      (con sb 1 -320 240 -40 40)
      (con sb 1 -320 80 -40 40)
      (con sb 1 -160 240 -40 40)
      (con sb 2 -160 240 40 40)
      (con sb 2 0 240 40 40)
      (con sb 3 80 240 -40 80)
      (con sb 4 80 240 -40 0)
      (con sb 4 320 240 40 0)
      (con sb 5 -320 80 -40 40)
      (con sb 9 320 80 40 40)
      (con sb 10 320 0 40 -40)
      (con sb 10 320 -160 40 -40)
      (con sb 11 -320 -160 -40 40)
      (con sb 12 -240 -240 -40 0)
      (con sb 12 0 -240 40 0)
      (con sb 13 0 -240 -40 0)
      (con sb 13 80 -240 40 0)
      (con sb 14 80 -240 -40 0)
      (con sb 14 240 -240 40 0)
      (con sb 14 320 -160 40 0)
      ;; springy/springy
      (con 1 5 40 -40 -40 40)
      (con 1 6 40 -40 40 40)
      (con 2 3 40 40 -40 80)
      (con 3 4 -40 80 -40 0)
      (con 3 4 40 -80 -40 0)
      (con 3 7 40 -80 40 0)
      (con 3 7 -40 80 -40 0)
      (con 3 8 40 -80 40 0)
      (con 3 9 40 -80 -40 -40)
      (con 4 9 40 0 40 40)
      (con 5 11 -40 40 -40 40)
      (con 5 11 40 -40 40 -40)
      (con 7 8 40 0 -40 0)
      (con 8 12 -40 0 40 0)
      (con 8 13 -40 0 -40 0)
      (con 8 13 40 0 40 0)
      (con 8 14 40 0 -40 0)
      (con 10 14 40 -40 -40 0)
      (con 10 14 40 -40 -40 0))))

(defmethod init-demo ((demo springies-demo))
  (setf (static-body demo) (make-body)
        (world demo) (make-world))
  (resize-world-active-hash (world demo) 30 999)
  (resize-world-static-hash (world demo) 200 99)
  (add-springs (world demo) (static-body demo) (build-springies (world demo)))
  (world demo))

(provide-demo 'springies-demo)
