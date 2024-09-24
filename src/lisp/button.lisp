(defpackage :button
  (:use :common-lisp :cffi)
  (:export #:button-state
	   #:init-buttons))

(in-package :button)

(cffi:defcfun (c/button-state "button_state") :int
  (button-number :int))
(cffi:defcfun (c/init-buttons "init_buttons") :int)
(cffi:defcfun (c/set-button-state "set_button_state") :int
  (button-number :int)
  (new-value :int))

(defun button-state (button-number)
  "Get the button state of the specific button-number."
  (let ((ret-value (c/button-state button-number)))
    (when (< ret-value 0)
      (error "Error getting button state for button-number ~d" button-number))
    (if (zerop ret-value) nil t)))

(defun (setf button-state) (nv button-number)
  "Button-state is SETF-able in the simulator only."
  (let* ((value (if nv 1 0))
	 (ret-value (c/set-button-state button-number value)))
    (when (< ret-value 0)
      (error "Error setting button state for button-number ~d to ~a" button-number value))
    (if (zerop ret-value) nil t)))

(defun init-buttons (foreign-library-name)
  "Initialize the button subsystem."
  (cffi:load-foreign-library foreign-library-name)
  (unless (zerop (c/init-buttons))
    (error "Error initing buttons!")))
