(in-package :cl-user)

(format t "~%Loading 3rd party dependencies with quicklisp...~%")
(ql:quickload "cffi") ;; https://github.com/zzkt/osc

(format t "~%Loading my code...")
(load "src/lisp/button.lisp")
(load "src/lisp/musicengine.lisp")

(use-package :button)
(use-package :musicengine)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library button
    (t "build/libbuttonloopback.so"))

  (cffi:define-foreign-library musicengine
    (t "build/libmusicengine.so")))

(init-buttons 'button)
(musicengine-init 'musicengine)

;; Run this, then run `aconnect -lio`
(defvar *seq* (musicengine-open-seq "MusicEngine" :output))
(defvar *port* (musicengine-open-midi-port *seq* "control:out" :output))

(musicengine-register-node-interest "MusicEngine" "Focusrite Scarlett 2i2 Analog Stereo")
(musicengine-get-node-state) ;; Nice, this works a bit.. but need to actually do some C code and register the interests now.


;; (musicengine-close-midi-port *seq* *port*)
;; (musicengine-close-seq *seq*)
;; (musicengine-teardown)

