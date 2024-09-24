(in-package :cl-user)

(format t "~%Loading 3rd party dependencies with quicklisp...~%")
(ql:quickload "cffi") ;; https://github.com/zzkt/osc

(format t "~%Loading my code...")
(load "src/lisp/button.lisp")

(use-package :button)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library button
    (t "build/libbuttonloopback.so")))
(init-buttons 'button)
