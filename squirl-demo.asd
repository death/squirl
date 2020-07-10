;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(asdf:defsystem squirl-demo
  :version "0.1"
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :licence "MIT"
  :depends-on (:squirl :cl-opengl :cl-glu :cl-glut)
  :components
  ((:module "demo"
            :components
            ((:file "squirl-demo")
             (:file "draw-world" :depends-on ("squirl-demo"))
             (:file "logo-smash" :depends-on ("squirl-demo"))
             ;;(:file "planet" :depends-on ("squirl-demo"))
             (:file "tumble" :depends-on ("squirl-demo"))
             (:file "plink" :depends-on ("squirl-demo"))
             (:file "pump" :depends-on ("squirl-demo"))
             (:file "pyramid" :depends-on ("squirl-demo"))
             (:file "pyramid-stack" :depends-on ("squirl-demo"))
             (:file "theo-jansen" :depends-on ("squirl-demo"))
             (:file "springies" :depends-on ("squirl-demo"))))))
