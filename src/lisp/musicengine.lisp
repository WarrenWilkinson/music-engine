(defpackage :musicengine
  (:documentation "Interface to C code for controlling alsa/pipewire.")
  (:use :common-lisp :cffi)
  (:export #:musicengine-init
	   #:musicengine-teardown
	   #:musicengine-open-seq
	   #:musicengine-close-seq
	   #:musicengine-open-midi-port
	   #:musicengine-close-midi-port
	   #:musicengine-register-node-interest
	   #:musicengine-get-node-state))

(in-package :musicengine)

(deftype musicengine-seq () '(integer 0 1))
(deftype musicengine-midi-port () '(integer 0 15))
(deftype musicengine-seq-mode () '(member :input :output :duplex))

(defmacro musicengine-call (name &rest args)
  (let ((out-error-message-gs (gensym))
	(return-value-gs (gensym)))
    `(cffi:with-foreign-object (,out-error-message-gs ':pointer)
       (let* ((,return-value-gs (,name ,@args (cffi:make-pointer (cffi:pointer-address ,out-error-message-gs)))))
	 (if (< ,return-value-gs 0)
	     (error "~a returned ~a (~a)"
		    ',name ,return-value-gs (cffi:foreign-string-to-lisp (cffi:mem-ref ,out-error-message-gs :pointer) :max-chars 128))
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

(cffi:defcfun (c/musicengine-register-node-interest "musicengine_register_node_interest") :int
  (names :string)
  (error-message (:pointer :string)))

(defun musicengine-register-node-interest (&rest names)
  "Pass the C library the list of node names that we are interested in."
  (let ((names-string (format nil "~{~a~^,~}" names)))
    (print names-string)
    (musicengine-call
     c/musicengine-register-node-interest
     names-string)))

(cffi:defcfun (c/musicengine-get-node-state "musicengine_get_node_state") :int
  (buffer (:pointer :char))
  (buffer-size :int)
  (error-message (:pointer :string)))

(defstruct port
  (node nil :type string :read-only t)
  (name nil :type string :read-only t)
  (direction nil :type (member :input :output) :read-only t)
  (midi-p nil :type boolean :read-only t))

(flet ((split-by-commas (str start end)
	 (loop :with position = start
	       :for comma = (position #\, str :start position :end end)
	       :collect (subseq str position (or comma end))
	       :do (setf position (+ 1 (or comma end) ))
	       :until (null comma))))
  (defun musicengine-get-node-state ()
    "Returns data about all the stuff. Returns three values
   1. a list of node names
   2. a list of ports
   3. a list of edges (a cons of two ports)"
    (let* ((buffer-size 2048)
	   (results (cffi:with-foreign-object (buffer :char buffer-size)
		      (musicengine-call
		       c/musicengine-get-node-state
		       buffer
		       buffer-size)
		      (cffi:foreign-string-to-lisp buffer :count buffer-size)))
	   (first-newline (position #\Newline results))
	   (second-newline (position #\Newline results :start (1+ first-newline)))
	   (third-newline (position #\Newline results :start (1+ second-newline)))
	   (names (split-by-commas results 0 first-newline))
	   (ports (split-by-commas results (1+ first-newline) second-newline))
	   (edges (split-by-commas results (1+ second-newline) third-newline)))
      (values names ports edges))))

