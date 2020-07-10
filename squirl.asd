;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(asdf:defsystem squirl
  :version "0.1"
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :licence "MIT"
  :description "A pure lisp implementation of the Chipmunk 2D physics engine."
  :components
  ((:module "src"
            :components
            ((:file "arbiter"        :depends-on ("vec" "shape" "collision" "contact"))
             (:file "body"           :depends-on ("vec"))
             (:file "bounding-box"   :depends-on ("vec"))
             (:file "collision"      :depends-on ("shape" "poly-shape" "contact"))
             (:file "contact"        :depends-on ("vec"))
             (:file "convenience"    :depends-on ("body" "shape" "poly-shape" "vec"))
             (:file "hash-set"       :depends-on ("utils"))
             (:file "package")
             (:file "shape"          :depends-on ("vec" "bounding-box" "body"))
             (:file "poly-shape"     :depends-on ("shape"))
             (:file "squirl"         :depends-on ("vec"))
             (:file "utils"          :depends-on ("package"))
             (:file "vec"            :depends-on ("utils"))
             (:file "world"          :depends-on ("vec" "arbiter" "body"))
             (:file "world-hash"     :depends-on ("hash-set" "vec"))
             (:module "constraints"  :depends-on ("shape" "poly-shape" "arbiter")
                      :components
                      ((:file "breakable-joint"      :depends-on ("constraints"))
                       (:file "constraints"          :depends-on ("util"))
                       (:file "damped-rotary-spring" :depends-on ("spring"))
                       (:file "damped-spring"        :depends-on ("spring"))
                       (:file "gear-joint"           :depends-on ("constraints"))
                       (:file "groove-joint"         :depends-on ("constraints"))
                       (:file "pin-joint"            :depends-on ("constraints"))
                       (:file "pivot-joint"          :depends-on ("constraints"))
                       (:file "ratchet-joint"        :depends-on ("constraints"))
                       (:file "rotary-limit-joint"   :depends-on ("constraints"))
                       (:file "simple-motor"         :depends-on ("constraints"))
                       (:file "slide-joint"          :depends-on ("constraints"))
                       (:file "spring"               :depends-on ("constraints"))
                       (:file "util")))))))
