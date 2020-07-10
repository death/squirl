;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(asdf:defsystem squirl-demo-2
  :version "0.1"
  :maintainer "Michael Compton <michael.compton@littleedge.co.uk>"
  :licence "MIT"
  :depends-on (:squirl :lispbuilder-sdl)
  :components
  ((:module "demo"
            :components
            ((:file "demo")))))
