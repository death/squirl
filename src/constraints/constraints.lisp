;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defgeneric pre-step (constraint dt dt-inverse))
(defgeneric apply-impulse (constraint))
(defgeneric get-impulse (constraint))

(defstruct constraint
  body-a body-b
  (max-force most-positive-double-float)
  (error-bias (expt (- 1.0d0 0.1d0) 60.0d0))
  (max-bias most-positive-double-float)
  data)

(defun constraint-bias-coefficient (constraint dt)
  (- 1.0d0 (expt (constraint-error-bias constraint) dt)))
