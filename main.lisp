(in-package :cl-user)

(format t "~%Loading 3rd party dependencies with quicklisp...~%")
(ql:quickload "mcclim") ;; https://github.com/McCLIM/McCLIM
(ql:quickload "osc") ;; https://github.com/zzkt/osc

(format t "~%Loading music engine...")
(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,*default-pathname-defaults*)
   :inherit-configuration))
(asdf:load-system "music-engine")

(format t "~%Starting up...")
(print "hi!")
