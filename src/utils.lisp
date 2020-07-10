;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl.utils)

(declaim (inline make-adjustable-vector))
(declaim (ftype (function (fixnum) vector) make-adjustable-vector))

(defun make-adjustable-vector (length)
  (declare (optimize speed))
  (make-array length :adjustable t :fill-pointer 0))

(declaim (inline ensure-list))
(declaim (ftype (function (t) list) ensure-list))

(defun ensure-list (x)
  (declare (optimize speed))
  (if (listp x) x (list x)))

(declaim (inline clamp))
(declaim (ftype (function (double-float double-float double-float) double-float) clamp))

(defun clamp (n min max)
  (declare (double-float n min max))
  (declare (optimize speed))
  (min (max n min) max))

(declaim (inline maybe/))
(declaim (ftype (function (double-float &optional double-float) double-float) maybe/))

(defun maybe/ (a &optional b)
  ;; Don't declare me (optimize speed), because that chokes SBCL
  (if (zerop b) 0d0 (/ a b)))

(defun maybe-inverse (x)
  (if (zerop x) 0d0 (/ x)))

;; from alexandria:
(declaim (inline delete/swapped-arguments))

(defun delete/swapped-arguments (sequence item &rest keyword-arguments)
  (apply #'delete item sequence keyword-arguments))

(declaim (inline delete-if/swapped-arguments))

(defun delete-if/swapped-arguments (sequence predicate &rest keyword-arguments)
  (apply #'delete-if predicate sequence keyword-arguments))

(define-modify-macro deletef (item &rest remove-keywords)
  delete/swapped-arguments
  "Modify-macro for DELETE. Sets place designated by the first argument to
the result of calling DELETE with ITEM, place, and the REMOVE-KEYWORDS.")

(define-modify-macro delete-iff (predicate &rest remove-keywords)
  delete-if/swapped-arguments
  "Modify-macro for DELETE-IF. Sets place designated by the first argument to
the result of calling DELETE with PREDICATE, place, and the REMOVE-KEYWORDS.")

(defmacro with-gensyms ((&rest vars) &body body)
  `(let ,(loop for var in vars collect `(,var (gensym ,(symbol-name var))))
     ,@body))

(defmacro without-floating-point-underflow (form)
  #+clisp `(ext:without-floating-point-underflow ,form)
  form)

(defun symbolicate (&rest things)
  "Concatenate together the names of some strings and symbols,
producing a symbol in the current package."
  (let ((name (make-string (reduce #'+ things :key (lambda (thing) (length (string thing)))))))
    (let ((index 0))
      (dolist (thing things (values (intern name)))
        (let ((x (string thing)))
          (replace name x :start1 index)
          (incf index (length x)))))))

(defun ensure-car (object &optional (default object))
  (if (atom object)
      default
      (car object)))

(defun ensure-cadr (object &optional (default object))
  (if (atom object)
      default
      (cadr object)))

(defmacro push-cons (cons place)
  "Like `cl:push', but reuses CONS"
  (with-gensyms (cons-sym)
    `(let ((,cons-sym ,cons))
       (setf (cdr ,cons-sym) ,place
             ,place ,cons-sym))))

(defmacro define-constant (name value &optional doc)
  "ANSI-compliant replacement for `defconstant'. cf SBCL Manual 2.3.4."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defmacro define-print-object ((class &key (identity t) (type t)) &body body)
  "Defines a `print-object' method on class CLASS, using the standard macro
`print-unreadable-object'. The IDENTITY and TYPE keyword arguments are passed
through to `print-unreadable-object', although they default to T if not supplied.

CLASS can be a list of the form (VARIABLE CLASS-NAME), in which case
the `print-object' method will be specialized on class CLASS-NAME and VARIABLE
will be used as the parameter name. Alternatively, as shorthand, CLASS can be a
single symbol, which will be used for both the variable and the class name."
  (let ((object (ensure-car class))
        (class-name (ensure-cadr class)))
    (with-gensyms (stream)
      `(defmethod print-object ((,object ,class-name) ,stream)
         (print-unreadable-object (,object ,stream :type ,type :identity ,identity)
           (let ((*standard-output* ,stream)) ,@body))))))

(defmacro do-vector ((var vector-form &optional result) &body body)
  "See `dolist'. If VAR is a list of the form (INDEX VAR), then INDEX is used
as the index vector. Note that this macro doesn't handle declarations properly."
  (let ((var-name (ensure-cadr var))
        (idx-name (ensure-car var (gensym "INDEX"))))
    (with-gensyms (vector)
      `(let ((,vector ,vector-form) ,var-name)
         (declare (ignorable ,var-name) (vector ,vector))
         (dotimes (,idx-name (length ,vector) ,result)
           (let ((,var-name (aref ,vector ,idx-name))) ,@body))))))

(defun parse-defmethod (args)
  (let (qualifiers lambda-list body (parse-state :qualifiers))
    (dolist (arg args)
      (ecase parse-state
        (:qualifiers (if (and (atom arg)
                              (not (null arg)))
                         (push arg qualifiers)
                         (setf lambda-list arg
                               parse-state :body)))
        (:body (push arg body))))
    (values qualifiers lambda-list (nreverse body))))

(defmacro pop-declarations (place)
  "Returns and removes all leading declarations from PLACE, which should be
a setf-able form. NOTE: This is a kludge hack shit substitute for parse-declarations"
  (with-gensyms (form)
    `(loop for ,form in ,place
        while (handler-case (string-equal (car ,form) 'declare) (type-error ()))
        collect (pop ,place))))
