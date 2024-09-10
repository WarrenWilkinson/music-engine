(defpackage #:cl-pipewire
  (:use #:common-lisp #:cl-pipewire.ffi)
  (:export #:pw-get-headers-version
	   #:pw-get-library-version))

(in-package #:cl-pipewire)

(defun pw-get-headers-version ()
  ;; Copied out of the headers included with this project.
  "0.3.65")

(defun pw-get-library-version ()
  (cl-pipewire.ffi::|pw_get_library_version|))
