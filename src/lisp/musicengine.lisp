(defpackage :musicengine
  (:documentation "Interface to C code for controlling alsa/pipewire.")
  (:use :common-lisp :cffi)
  (:export #:musicengine-init
	   #:musicengine-teardown
	   #:musicengine-open-seq
	   #:musicengine-close-seq
	   #:musicengine-open-midi-port
	   #:musicengine-close-midi-port))

(in-package :musicengine)

(deftype musicengine-seq () '(integer 0 1))
(deftype musicengine-midi-port () '(integer 0 15))
(deftype musicengine-seq-mode () '(member :input :output :duplex))

(defmacro musicengine-call (name &rest args)
  (let ((out-error-message-gs (gensym))
	(return-value-gs (gensym)))
  `(cffi:with-foreign-object (,out-error-message-gs '(:pointer :string))
     (let ((,return-value-gs (,name ,@args ,out-error-message-gs)))
       (if (< ,return-value-gs 0)
	   (error "Error calling ~a, return value ~a, message was: ~a"
		  ',name ,return-value-gs (cffi:foreign-string-to-lisp ,out-error-message-gs :count 16))
	   ,return-value-gs)))))

(cffi:defcfun (c/musicengine-init "musicengine_init") :int
  (error-message (:pointer :string)))

(defun musicengine-init (foreign-library-name)
  "Initialize the musicengine."
  (cffi:load-foreign-library foreign-library-name)
  (musicengine-call c/musicengine-init))

(cffi:defcfun (c/musicengine-teardown "musicengine_teardown") :int
  (error-message (:pointer :string)))

(defun musicengine-teardown ()
  "Shutdown the musicengine."
  (musicengine-call c/musicengine-teardown))

(cffi:defcfun (c/musicengine-open-seq "musicengine_open_seq") :int
  (name :string)
  (output :int)
  (input :int)
  (error-message (:pointer :string)))

(defun musicengine-open-seq (name &optional (mode :duplex))
  (check-type name string)
  (check-type mode musicengine-seq-mode)
  (Let ((input (if (member mode '(:duplex :input)) 1 0))
	(output (if (member mode '(:duplex :output)) 1 0)))
    (let ((result (musicengine-call c/musicengine-open-seq name output input)))
      (check-type result musicengine-seq)
      result)))

(cffi:defcfun (c/musicengine-close-seq "musicengine_close_seq") :int
  (seq_number :int)
  (error-message (:pointer :string)))

(defun musicengine-close-seq (seq)
  (check-type seq musicengine-seq)
  (musicengine-call c/musicengine-close-seq seq)
  t)

(cffi:defcfun (c/musicengine-open-midi-port "musicengine_open_midi_port") :int
  (seq_number :int)
  (name :string)
  (output :int)
  (input :int)
  (error-message (:pointer :string)))

(defun musicengine-open-midi-port (seq name &optional (mode :duplex))
  (check-type seq musicengine-seq)
  (check-type name string)
  (check-type mode musicengine-seq-mode)
  (Let ((input (if (member mode '(:duplex :input)) 1 0))
	(output (if (member mode '(:duplex :output)) 1 0)))
    (let ((result (musicengine-call c/musicengine-open-midi-port seq name output input)))
      (check-type result musicengine-midi-port)
      result)))

(cffi:defcfun (c/musicengine-close-midi-port "musicengine_close_midi_port") :int
  (seq_number :int)
  (midi_port_number :int)
  (error-message (:pointer :string)))

(defun musicengine-close-midi-port (seq midi)
  (check-type seq musicengine-seq)
  (check-type midi musicengine-midi-port)
  (musicengine-call c/musicengine-close-midi-port seq midi)
  t)
