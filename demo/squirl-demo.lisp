(defpackage #:squirl-demo
  (:use :cl :squirl :squirl.utils)
  (:export
   :run-all-demos
   :run-demo))

(in-package :squirl-demo)

(defparameter *sleep-ticks* 16)

(defvar *demos* nil)
(defvar *current-demo* nil)

(defvar *key-up* nil)
(defvar *key-down* nil)
(defvar *key-left* nil)
(defvar *key-right* nil)

(defvar *arrow-direction* +zero-vector+)

(defvar *aa-enabled-p* nil)

(defparameter *dt-threshold* 0.01)

;;;; Utils

(defun now ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun time-difference (time-before)
  "Checks the difference between the internal-time provided and the current time.
Returns both the difference in time and the current-time used in the computation"
  (let* ((time-now (now))
         (difference (- time-now time-before)))
    (if (minusp difference)
        0                               ; just in case
        (values (- time-now time-before)
                time-now))))

(defparameter *fps-sample-size* 10)

(defstruct (frame-stats (:conc-name fs-))
  (last-frame nil)
  (stack '())
  (frames 0)
  (cumulative-mean 0))

(defvar *frame-stats*
  (make-frame-stats))

(defun notify-frame ()
  (let ((fs *frame-stats*))
    (symbol-macrolet ((fps-stack (fs-stack fs))
                      (last-frame (fs-last-frame fs))
                      (cumulative-mean (fs-cumulative-mean fs))
                      (frames (fs-frames fs)))
      (when (> (length fps-stack) *fps-sample-size*)
        (setf (cdr (last fps-stack 2)) nil))
      (let ((now (now)))
        (when last-frame
          (let ((time-delta (- now last-frame)))
            (unless (zerop time-delta)
              (push (/ time-delta) fps-stack)
              (setf cumulative-mean (/ (+ (last-fps)
                                          (* frames cumulative-mean))
                                       (1+ frames)))
              (incf frames))))
        (setf last-frame now)))))

(defun notify-unpause ()
  (setf (fs-last-frame *frame-stats*) (now)))

(defun last-fps ()
  (first (fs-stack *frame-stats*)))

(defun mean-fps ()
  (let ((stack (fs-stack *frame-stats*)))
    (when stack
      (/ (reduce #'+ stack) (length stack)))))

(defun cumulative-mean-fps ()
  (fs-cumulative-mean *frame-stats*))

(defun reset-cumulative-mean-fps ()
  "Restart calculating the cumulative mean relative to the current frame."
  (let ((fs *frame-stats*))
    (setf (fs-frames fs) 0)
    (setf (fs-cumulative-mean fs) 0)))

;;;; Demo class

(defclass demo ()
  ((name :initarg :name :accessor demo-name)
   (pausedp :initform nil :accessor pausedp)
   (accumulator :initform 0 :accessor accumulator)
   (world :initarg :world :accessor world)
   (delta-time :initform 1 :accessor delta-time)
   (last-frame-time :initform (now) :accessor last-frame-time)
   (physics-timestep :initarg :physics-timestep
                     :initform (float 1/120 1d0) :accessor physics-timestep)
   (mouse-joint :initform nil :accessor mouse-joint)
   (mouse-body :initarg :mouse-body :initform (make-body) :accessor mouse-body)
   (last-mouse-position :initform +zero-vector+ :accessor last-mouse-position)
   ;; drawing
   (line-thickness :initarg :line-thickness :initform 1 :accessor line-thickness)
   (draw-shapes-p :initarg :draw-shapes-p :initform t :accessor draw-shapes-p)
   (draw-bb-p :initarg :draw-bb-p :initform nil :accessor draw-bb-p)
   (draw-force-p :initarg :draw-force-p :initform nil :accessor draw-force-p)
   (draw-velocity-p :initarg :draw-velocity-p :initform nil :accessor draw-velocity-p)
   (body-point-size :initarg :body-point-size :initform 0 :accessor body-point-size)
   (collision-point-size :initarg :collision-point-size :initform 2 :accessor collision-point-size)
   (draw-collision-normal-p :initarg :draw-collision-normal-p :initform nil :accessor draw-collision-normal-p)))

(defgeneric mouse-position (demo))

(defgeneric (setf mouse-position) (new-pos demo))

(defgeneric draw-demo (demo))

(defgeneric update-demo (demo dt))

(defgeneric init-demo (demo))

(defgeneric grabbablep (actor))

(defmethod mouse-position ((demo demo))
  (body-position (mouse-body demo)))

(defmethod (setf mouse-position) (new-pos (demo demo))
  (setf (body-position (mouse-body demo)) new-pos))

(defun update-time (demo)
  (with-slots (delta-time last-frame-time) demo
    (multiple-value-bind (new-dt now)
        (time-difference last-frame-time)
      (setf last-frame-time now
            delta-time new-dt))))

(defmethod draw-demo ((demo demo))
  (with-slots (line-thickness draw-shapes-p draw-bb-p body-point-size
               collision-point-size draw-force-p draw-velocity-p
               draw-collision-normal-p)
      demo
    (draw-world (world demo)
                :draw-shapes-p draw-shapes-p
                :draw-bb-p draw-bb-p :line-thickness line-thickness
                :body-point-size body-point-size :collision-point-size collision-point-size
                :draw-force draw-force-p :draw-velocity draw-velocity-p
                :draw-collision-normal draw-collision-normal-p)))

(defmethod grabbablep ((actor t))
  t)

(defmethod grabbablep ((actor (eql :not-grabbable)))
  nil)

(defun toggle-pause (demo)
  (cond ((pausedp demo)
         (setf (pausedp demo) nil)
         (notify-unpause))
        (t
         (setf (pausedp demo) t))))

(defmethod update-demo :around ((demo demo) dt)
  (declare (ignore dt))
  (unless (pausedp demo)
    (call-next-method)))

(defmethod update-demo ((demo demo) dt)
  "The default method locks the update loop to 'realtime'. That is, it
makes sure that the current world is updated by 1 time unit per second."
  (incf (accumulator demo) (min dt *dt-threshold*))
  (loop while (>= (accumulator demo) (physics-timestep demo))
        do (world-step (world demo) (physics-timestep demo))
           (decf (accumulator demo) (physics-timestep demo))))

;;;; Drawing the demos

(defclass squirl-window (glut:window)
  ()
  (:default-initargs :width 640 :height 480 :mode '(:double :rgba :multisample)
                     :title "Squirl Demo App"))

(defun draw-string (x y string)
  (gl:color 0 0 0)
  (gl:raster-pos x y)
  (glut:bitmap-string glut:+bitmap-helvetica-10+ string))

(defun draw-instructions ()
  (let ((x -300)
        (y 220))
    (draw-string x y (format nil
                             "Controls:~@
                              #\\N chooses the next demo~@
                              #\\P chooses the previous demo~@
                              #\\Space toggles pause~@
                              #\\Return restarts the current demo~@
                              Use the mouse to grab objects~@
                              Arrow keys control some demos~@
                              #\\A toggles anti-aliasing~@
                              #\\[ and #\\] control the size of body points~@
                              #\\{ and #\\} control the size of collision points.~@
                              #\\V toggles velocity vectors~@
                              #\\F toggles force vectors~@
                              #\\C toggles collision normals"))))

(defun draw-fps ()
  (let ((x -300)
        (y 0)
        (string (format nil "Last FPS: ~7,2f~%Mean FPS: ~7,2f~%Cumulative Mean FPS:~7,2f"
                        (last-fps)
                        (mean-fps)
                        (cumulative-mean-fps))))
    (draw-string x y string)))

(defun draw-pause-state ()
  (when (pausedp *current-demo*)
    (draw-string -300 10 "SIMULATION PAUSED")))

(defmethod glut:idle ((w squirl-window))
  (unless (pausedp *current-demo*)
    (notify-frame))
  (update-time *current-demo*)
  (glut:post-redisplay))

(defmethod glut:display ((w squirl-window))
  (gl:clear :color-buffer-bit)
  (draw-demo *current-demo*)
  (draw-fps)
  (draw-pause-state)
  (draw-instructions)
  (glut:swap-buffers)
  (let ((new-point (vec-lerp (last-mouse-position *current-demo*)
                             (mouse-position *current-demo*)
                             1/4)))
    (setf (mouse-position *current-demo*) new-point)
    (setf (body-velocity (mouse-body *current-demo*))
          (vec* (vec- new-point (last-mouse-position *current-demo*)) 60d0))
    (setf (last-mouse-position *current-demo*) new-point)
    (update-demo *current-demo* (delta-time *current-demo*))))

(defun demo-title (demo)
  (concatenate 'string "Demo: " (demo-name demo)))

(defun ensure-demo (demo-designator)
  (if (typep demo-designator 'demo)
      demo-designator
      (make-instance demo-designator)))

(defun set-current-demo (demo-designator)
  (let ((old-demo *current-demo*))
    (reset-cumulative-mean-fps)
    (clear-color-hash)
    (setf *arrow-direction* +zero-vector+)
    (setf *current-demo* (ensure-demo demo-designator))
    (setf (world *current-demo*) (init-demo *current-demo*))
    (when old-demo
      (setf (mouse-position *current-demo*) (mouse-position old-demo))
      (setf (last-mouse-position *current-demo*) (last-mouse-position old-demo)))))

(defmethod glut:keyboard ((w squirl-window) key x y)
  (declare (ignore x y))
  (when (upper-case-p key)
    (setf key (char-downcase key)))
  (case key
    ((#\Esc #\q)
     (glut:destroy-current-window))
    (#\Return
     (set-current-demo (class-of *current-demo*)))
    (#\Space
     (toggle-pause *current-demo*))
    (#\n
     (set-current-demo (elt *demos*
                            (mod (1+ (position (class-name (class-of *current-demo*)) *demos*))
                                 (length *demos*)))))
    (#\p
     (set-current-demo (elt *demos*
                            (mod (1- (position (class-name (class-of *current-demo*)) *demos*))
                                 (length *demos*)))))
    (#\a
     (toggle-anti-aliasing))
    (#\]
     (incf (body-point-size *current-demo*)))
    (#\[
     (unless (<= (body-point-size *current-demo*) 0)
       (decf (body-point-size *current-demo*))))
    (#\}
     (incf (collision-point-size *current-demo*)))
    (#\{
     (unless (<= (collision-point-size *current-demo*) 0)
       (decf (collision-point-size *current-demo*))))
    (#\v
     (setf (draw-velocity-p *current-demo*)
           (not (draw-velocity-p *current-demo*))))
    (#\f
     (setf (draw-force-p *current-demo*)
           (not (draw-force-p *current-demo*))))
    (#\c
     (setf (draw-collision-normal-p *current-demo*)
           (not (draw-collision-normal-p *current-demo*))))))

(defun toggle-anti-aliasing ()
  (if *aa-enabled-p*
      (disable-anti-aliasing)
      (enable-anti-aliasing)))

(defun disable-anti-aliasing ()
  (gl:disable :line-smooth :point-smooth :blend :multisample)
  (setf *aa-enabled-p* nil))

(defun enable-anti-aliasing ()
  (gl:enable :line-smooth :point-smooth :blend :multisample)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:hint :polygon-smooth-hint :nicest)
  (gl:hint :line-smooth-hint :nicest)
  (setf *aa-enabled-p* t))

(defun mouse-to-space (x y)
  (let ((model (gl:get-double :modelview-matrix))
        (proj (gl:get-double :projection-matrix))
        (view (gl:get-double :viewport)))
    (multiple-value-bind (mx my)
        (glu:un-project x
                        (- (glut:get :window-height) y)
                        0
                        :modelview model
                        :projection proj
                        :viewport view)
      (vec mx my))))

(defmethod glut:motion ((w squirl-window) x y)
  (setf (mouse-position *current-demo*) (mouse-to-space x y)))

(defmethod glut:passive-motion ((w squirl-window) x y)
  (setf (mouse-position *current-demo*) (mouse-to-space x y)))

(defmethod glut:mouse ((w squirl-window) button state x y)
  (case button
    (:left-button
     (case state
       (:down
        (let* ((point (mouse-to-space x y))
               (shape (world-point-query-first (world *current-demo*) point)))
          (when (and shape (grabbablep (body-actor (shape-body shape))))
            (let ((body (shape-body shape)))
              (setf (mouse-joint *current-demo*)
                    (make-pivot-joint (mouse-body *current-demo*)
                                      body
                                      +zero-vector+
                                      (world->body-local body point)))
              (setf (squirl::constraint-max-force (mouse-joint *current-demo*)) 50000)
              (world-add-constraint (world *current-demo*) (mouse-joint *current-demo*))))))
       (t
        (world-remove-constraint (world *current-demo*) (mouse-joint *current-demo*))
        (setf (mouse-joint *current-demo*) nil))))))

(cffi:defcallback timercall :void ((value :int))
  (declare (ignore value))
  (glut:timer-func 16 (cffi:callback timercall) 0)
  (glut:post-redisplay))

(defun set-arrow-direction ()
  (let ((x 0)
        (y 0))
    (when *key-up* (incf y))
    (when *key-down* (decf y))
    (when *key-right* (incf x))
    (when *key-left* (decf x))
    (setf *arrow-direction* (vec x y))))

(defmethod glut:special ((w squirl-window) key x y)
  (declare (ignore x y))
  (case key
    (:key-up (setf *key-up* t))
    (:key-down (setf *key-down* t))
    (:key-left (setf *key-left* t))
    (:key-right (setf *key-right* t)))
  (set-arrow-direction))

(defmethod glut:special-up ((w squirl-window) key x y)
  (declare (ignore x y))
  (case key
    (:key-up (setf *key-up* nil))
    (:key-down (setf *key-down* nil))
    (:key-left (setf *key-left* nil))
    (:key-right (setf *key-right* nil)))
  (set-arrow-direction))

(defmethod glut:display-window :before ((w squirl-window))
  (gl:clear-color 1 1 1 0)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -320 320 -240 240 -1 1)
  (gl:translate 1/2 1/2 0)
  (gl:enable-client-state :vertex-array)
  (enable-anti-aliasing))

(defun run-demo (demo-designator)
  (set-current-demo demo-designator)
  (glut:display-window (make-instance 'squirl-window)))

(defun run-all-demos ()
  (when *demos*
    (set-current-demo (nth (random (length *demos*)) *demos*))
    (glut:display-window (make-instance 'squirl-window))
    ;; this is a kludge around an apparent cl-glut bug.
    (setf glut::*glut-initialized-p* nil)))

(defun provide-demo (demo-class)
  (pushnew demo-class *demos*))
