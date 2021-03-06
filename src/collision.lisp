;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(define-constant +collision-slop+ 0.1
  "Amount of allowed penetration.  Used to reduce vibrating contacts.")

;;;
;;; Collision resolution functions
;;;
(defun circle-to-circle-query (p1 p2 r1 r2)
  (declare (optimize speed) (double-float r1 r2) (vec p1 p2))
  (let* ((delta (vec- p2 p1))
         (mindist (+ r1 r2))
         (distsq (vec-length-sq delta)))
   (when (< distsq (* mindist mindist))
     (let* ((dist (sqrt distsq)))
       (make-contact (vec+ p1 (vec* delta
                                    (+ 0.5d0 (maybe/ (- r1 (/ mindist 2))
                                                     dist))))
                     (vec* delta (maybe/ 1d0 dist)) ; Same as (vec-normalize delta)
                     (- dist mindist))))))

(defun circle-to-segment (circle segment)
  (let* ((radius-sum (+ (circle-radius circle) (segment-radius segment)))
         (normal-distance (- (vec. (segment-trans-normal segment)
                                   (circle-transformed-center circle))
                             (vec. (segment-trans-a segment)
                                   (segment-trans-normal segment))))
         (distance (- (abs normal-distance) radius-sum)))
    (unless (plusp distance)
      (let ((tangent-distance (- (vec-cross (segment-trans-normal segment)
                                            (circle-transformed-center circle))))
            (tangent-distance-min (- (vec-cross (segment-trans-normal segment)
                                                (segment-trans-a segment))))
            (tangent-distance-max (- (vec-cross (segment-trans-normal segment)
                                                (segment-trans-b segment)))))
        (cond
          ((< tangent-distance tangent-distance-min)
           (when (>= tangent-distance (- tangent-distance-min radius-sum))
             (circle-to-circle-query (circle-transformed-center circle)
                                     (segment-trans-a segment)
                                     (circle-radius circle)
                                     (segment-radius segment))))
          ((< tangent-distance tangent-distance-max)
           (let ((normal (if (minusp normal-distance)
                             (segment-trans-normal segment)
                             (vec- (segment-trans-normal segment)))))
             (make-contact (vec+ (circle-transformed-center circle)
                                 (vec* normal (+ (circle-radius circle) (/ distance 2))))
                           normal distance)))
          ((< tangent-distance (+ tangent-distance-max radius-sum))
           (circle-to-circle-query (circle-transformed-center circle)
                                   (segment-trans-b segment)
                                   (circle-radius circle)
                                   (segment-radius segment)))
          (t nil))))))

;;; This function has VERY HAIRY control flow. Frob with EXTREME caution.
(defun find-min-separating-axis (poly1 poly2)
  (loop with msa
       with min-distance
     for axis across (poly-transformed-axes poly2)
     for distance = (poly-value-on-axis poly1 (poly-axis-normal axis) (poly-axis-distance axis))
     never (plusp distance)
     when (or (null min-distance) (> distance min-distance))
     do (setf msa axis
              min-distance distance)
     finally (return (values msa min-distance))))

(defun find-vertices (poly1 poly2 normal distance &aux contacts)
  "Add contacts for penetrating vertices"
  (declare (optimize speed) (vec normal))
  (let ((-normal (vec- normal)))
    (do-vector ((i vertex) (poly-transformed-vertices poly1))
      (when (partial-poly-contains-vertex-p poly2 vertex -normal)
        (push (make-contact vertex normal distance (hash-pair (shape-id poly1) i)) contacts))))
  (do-vector ((i vertex) (poly-transformed-vertices poly2) contacts)
    (when (partial-poly-contains-vertex-p poly1 vertex normal)
      (push (make-contact vertex normal distance (hash-pair (shape-id poly2) i)) contacts)))
  contacts)

(defun segment-value-on-axis (segment normal distance)
  (- (min (- (vec. normal (segment-trans-a segment)) (segment-radius segment))
          (- (vec. normal (segment-trans-b segment)) (segment-radius segment)))
     distance))

(defun find-points-behind-segment (segment poly p-dist coefficient &aux contacts)
  "Identify vertices that have penetrated the segment."
  (let* ((segment-normal (segment-trans-normal segment))
         (dta (vec-cross segment-normal (segment-trans-a segment)))
         (dtb (vec-cross segment-normal (segment-trans-b segment)))
         (normal (vec* segment-normal coefficient))
         (threshhold (+ (* (vec. segment-normal (segment-trans-a segment))
                           coefficient)
                        (segment-radius segment))))
    (do-vector ((i vertex) (poly-transformed-vertices poly) contacts)
      (when (< (vec. vertex normal) threshhold)
        (let ((dt (vec-cross segment-normal vertex)))
          (when (>= dta dt dtb)
            (push (make-contact vertex normal p-dist (hash-pair (shape-id poly) i))
                  contacts)))))
    contacts))

;;; This is complicated. Not gross, but just complicated. It needs to be simpler
;;; and/or commented, preferably both.
(defun segment-to-poly (segment poly &aux contacts)
  (let* ((axes (poly-transformed-axes poly))
         (segD (vec. (segment-trans-normal segment)
                     (segment-trans-a segment)))
         (min-norm (- (poly-value-on-axis poly (segment-trans-normal segment) segD)
                      (segment-radius segment)))
         (min-neg (- (poly-value-on-axis poly (vec- (segment-trans-normal segment)) (- segD))
                     (segment-radius segment))))
    (unless (or (> min-neg 0) (> min-norm 0))
      (let ((min-i 0)
            (poly-min (segment-value-on-axis segment
                                             (poly-axis-normal (aref axes 0))
                                             (poly-axis-distance (aref axes 0)))))
        (unless (or (plusp poly-min)
                    (do-vector ((i axis) axes)
                      (let ((distance (segment-value-on-axis segment
                                                             (poly-axis-normal axis)
                                                             (poly-axis-distance axis))))
                        (when (> distance 0) (return t))
                        (when (> distance poly-min)
                          (setf poly-min distance
                                min-i i)))))
          (let* ((poly-normal (vec- (poly-axis-normal (aref axes min-i))))
                 (vertex-a (vec+ (segment-trans-a segment)
                                 (vec* poly-normal (segment-radius segment))))
                 (vertex-b (vec+ (segment-trans-b segment)
                                 (vec* poly-normal (segment-radius segment)))))
            (flet ((try-vertex (vertex i)
                     (when (poly-contains-vertex-p poly vertex)
                       (push (make-contact vertex poly-normal poly-min
                                           (hash-pair (shape-id segment) i))
                             contacts))))
              (try-vertex vertex-a 0)
              (try-vertex vertex-b 1))
            ;; "Floating point precision problems here.
            ;;  This will have to do for now."
            (decf poly-min +collision-slop+)
            (when (or (>= min-norm poly-min)
                      (>= min-neg poly-min))
              (setf contacts
                    (nconc contacts
                           (if (> min-norm min-neg)
                               (find-points-behind-segment segment poly min-norm 1d0)
                               (find-points-behind-segment segment poly min-neg -1d0)))))
            ;; If no other collision points were found, try colliding endpoints.
            (if contacts contacts
                (flet ((try-endpoint (point vertex)
                         (let ((collision (circle-to-circle-query
                                           point vertex (segment-radius segment) 0d0)))
                           (when collision (return-from segment-to-poly (list collision))))))
                  (let ((vert-a (aref (poly-transformed-vertices poly) min-i))
                        (vert-b (aref (poly-transformed-vertices poly)
                                      (rem (1+ min-i) (length (poly-transformed-vertices poly))))))
                    (try-endpoint (segment-trans-a segment) vert-a)
                    (try-endpoint (segment-trans-b segment) vert-a)
                    (try-endpoint (segment-trans-a segment) vert-b)
                    (try-endpoint (segment-trans-b segment) vert-b))))))))))

(defun circle-to-poly (circle poly)
  (let* ((axes (poly-transformed-axes poly))
         (min-i 0)
         (min (- (vec. (poly-axis-normal (svref axes 0))
                       (circle-transformed-center circle))
                 (poly-axis-distance (svref axes 0))
                 (circle-radius circle))))
    (when (loop
             for i from 0
             for axis across axes
             for distance = (- (vec. (poly-axis-normal axis)
                                     (circle-transformed-center circle))
                               (poly-axis-distance axis)
                               (circle-radius circle))
             when (> distance 0) return nil
             when (> distance min)
             do (setf min distance
                      min-i i)
             finally (return t))
      (let* ((normal (poly-axis-normal (svref axes min-i)))
             (a (aref (poly-transformed-vertices poly) min-i))
             (b (aref (poly-transformed-vertices poly)
                      (rem (1+ min-i) (length (poly-transformed-vertices poly)))))
             (dta (vec-cross normal a))
             (dtb (vec-cross normal b))
             (dt (vec-cross normal (circle-transformed-center circle))))
        (cond
          ((< dt dtb)
           (circle-to-circle-query (circle-transformed-center circle)
                                   b (circle-radius circle) 0d0))
          ((< dt dta)
           (make-contact (vec- (circle-transformed-center circle)
                               (vec* normal
                                     (+ (circle-radius circle)
                                        (/ min 2))))
                         (vec- normal) min))
          (t (circle-to-circle-query (circle-transformed-center circle)
                                     a (circle-radius circle) 0d0)))))))

(defun poly-to-poly (poly1 poly2)
  ;; This is definitely returning contacts, and they look correct.
  ;; The problem is elsewhere.
  (multiple-value-bind (msa1 min1) (find-min-separating-axis poly2 poly1)
    (multiple-value-bind (msa2 min2) (find-min-separating-axis poly1 poly2)
      (when (and msa1 msa2)
        (if (> min1 min2)
            (find-vertices poly1 poly2 (poly-axis-normal msa1) min1)
            (find-vertices poly1 poly2 (vec- (poly-axis-normal msa2)) min2))))))

(defun closest-point-on-segment (segment point &aux
                                 (a (segment-trans-a segment))
                                 (line-vec (vec- (segment-trans-b segment) a)))
  (vec+ a (vec* line-vec (clamp (/ (vec. (vec- point a) line-vec)
                                   (vec. line-vec line-vec))
                                0d0 1d0))))

(defun segment-intersection (a b &aux (a-a (segment-trans-a a))
                                      (a-b (segment-trans-b a))
                                      (b-a (segment-trans-a b))
                                      (b-b (segment-trans-b b)))
  ;; Based on http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/
  (let ((a-a.x (vec-x a-a))
        (a-a.y (vec-y a-a))
        (a-b.x (vec-x a-b))
        (a-b.y (vec-y a-b))
        (b-a.x (vec-x b-a))
        (b-a.y (vec-y b-a))
        (b-b.x (vec-x b-b))
        (b-b.y (vec-y b-b)))
    (let ((x-factor-num (- (* (- b-b.x b-a.x) (- a-a.y b-a.y))
                           (* (- a-a.x b-a.x) (- b-b.y b-a.y))))
          (y-factor-num (- (* (- a-b.x b-a.x) (- a-a.y b-a.y))
                           (* (- a-a.x b-a.x) (- a-b.y a-a.y))))
          (denom (- (* (- a-b.x a-a.x) (- b-b.y b-a.y))
                    (* (- b-b.x b-a.x) (- a-b.y a-a.y)))))
      (cond
        ((= 0 denom x-factor-num y-factor-num) ; Coincident
         (vec* (vec- (segment-center a) (segment-center b)) 0.5d0))
        ((= 0 denom) nil)               ; Parallel
        (t (let* ((intersection (vec+ a-a
                                      (vec (* (/ x-factor-num denom)
                                              (- a-b.x a-a.x))
                                           (* (/ y-factor-num denom)
                                              (- a-b.y a-a.y)))))
                  (delta-a (vec- intersection a-a))
                  (delta-b (vec- intersection b-a))
                  (vec-a (vec- a-b a-a))
                  (vec-b (vec- b-b b-a)))
             (when (and (< (vec-length-sq delta-a)
                           (vec-length-sq vec-a))
                        (< (vec-length-sq delta-b)
                           (vec-length-sq vec-b))
                        ;; Make sure we're going along, not away from, the segment.
                        (< (abs (- (vec->angle vec-a)
                                   (vec->angle delta-a)))
                           pi)
                        (< (abs (- (vec->angle vec-b)
                                   (vec->angle delta-b)))
                           pi))
               intersection)))))))

(defun segment-center (segment)
  (vec* (vec+ (segment-trans-a segment) (segment-trans-b segment))
        0.5d0))

(defun segment-to-segment (a b &aux (end-a-a (segment-trans-a a))
                                    (end-a-b (segment-trans-b a))
                                    (end-b-a (segment-trans-a b))
                                    (end-b-b (segment-trans-b b))
                                    (radius-a (segment-radius a))
                                    (radius-b (segment-radius b)))
  (if (= 0 radius-a radius-b)
      (let ((point (segment-intersection a b)))
        (when point
          (let ((delta (vec- (segment-center b) (segment-center a))))
            (list (make-contact point (vec-normalize delta) (vec-length delta))))))
      (let (contacts)
        (let ((contact (circle-to-circle-query (closest-point-on-segment a end-b-a)
                                               end-b-a
                                               radius-a
                                               radius-b)))
          (when contact
            (push contact contacts)))
        (let ((contact (circle-to-circle-query (closest-point-on-segment a end-b-b)
                                               end-b-b
                                               radius-a
                                               radius-b)))
          (when contact
            (push contact contacts)))
        (when (< (length contacts) 2)
          (let ((contact (circle-to-circle-query (closest-point-on-segment b end-a-a)
                                                 end-a-a
                                                 radius-b
                                                 radius-a)))
            (when contact
              (setf (contact-normal contact) (vec- (contact-normal contact)))
              (push contact contacts)))
          (when (< (length contacts) 2)
            (let ((contact (circle-to-circle-query (closest-point-on-segment b end-a-b)
                                                   end-a-b
                                                   radius-b
                                                   radius-a)))
              (when contact
                (setf (contact-normal contact) (vec- (contact-normal contact)))
                (push contact contacts)))))
        contacts)))

;;;
;;; Generic function
;;;
(defgeneric collide-shapes (a b)
  (:documentation "Collide shapes A and B together!")
  (:method ((shape-1 circle) (shape-2 circle))
    (ensure-list (circle-to-circle-query (circle-transformed-center shape-1)
                                         (circle-transformed-center shape-2)
                                         (circle-radius shape-1)
                                         (circle-radius shape-2))))
  (:method ((segment segment) (circle circle))
    (ensure-list (circle-to-segment circle segment)))
  (:method ((circle circle) (segment segment))
    (ensure-list (circle-to-segment circle segment)))
  (:method ((segment segment) (poly poly))
    (segment-to-poly segment poly))
  (:method ((poly poly) (segment segment))
    (segment-to-poly segment poly))
  (:method ((circle circle) (poly poly))
    (ensure-list (circle-to-poly circle poly)))
  (:method ((poly poly) (circle circle))
    (ensure-list (circle-to-poly circle poly)))
  (:method ((poly1 poly) (poly2 poly))
    (poly-to-poly poly1 poly2))
  (:method ((seg1 segment) (seg2 segment))
    (segment-to-segment seg1 seg2)))
