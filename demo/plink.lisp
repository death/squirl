(in-package :squirl-demo)

(defclass plink-demo (demo)
  ((num-verts :initarg :num-verts
              :initform 5
              :accessor plink-num-verts)
   (static-body :initarg :static-body
                :initform (make-body :actor :not-grabbable)
                :accessor demo-static-body)
   (angle :accessor plink-angle))
  (:default-initargs :name "Plink!" :physics-timestep 1/60))

(defmethod initialize-instance :after ((demo plink-demo) &key)
  (setf (plink-angle demo) (/ (* -2 pi) (plink-num-verts demo))))

(defun reset-fallen-body (body)
  (let* ((position (body-position body))
         (x (vec-x position))
         (y (vec-y position)))
    (when (or (< y -260) (> (abs x) 340))
      (setf (body-position body)
            (vec (- (random 640) 320) 260)))))

(defmethod update-demo :after ((demo plink-demo) dt)
  (declare (ignore dt))
  (map-world #'reset-fallen-body (world demo)))

(defun create-static-triangles (demo)
  (let ((vertices (list (vec -15 -15) (vec 0 10) (vec 15 -15))))
    (dotimes (i 9)
      (dotimes (j 6)
        (attach-shape (make-poly vertices
                                 :offset (vec (- (* i 80) 320 (if (oddp j) -40 0))
                                              (- (* j 70) 240))
                                 :restitution 1
                                 :friction 1)
                      (demo-static-body demo))))))

(defun create-polygons (demo)
  (let* ((verts (loop for i below (plink-num-verts demo)
                      collect (vec* (angle->vec (* i (plink-angle demo))) 10d0)))
         (inertia (moment-of-inertia-for-poly 1 verts)))
    (dotimes (i 300)
      (world-add-body (world demo)
                      (make-body :mass 1
                                 :inertia inertia
                                 :position (vec (- (random 640) 320) 350)
                                 :shapes (list (make-poly verts :friction 0.2)))))))

(defmethod init-demo ((demo plink-demo))
  (setf (world demo) (make-world :iterations 5 :gravity (vec 0 -100)))
  (resize-world-active-hash (world demo) 40 999)
  (resize-world-static-hash (world demo) 30 2999)
  (world-add-body (world demo) (demo-static-body demo))
  (create-static-triangles demo)
  (create-polygons demo)
  (world demo))

(provide-demo 'plink-demo)
