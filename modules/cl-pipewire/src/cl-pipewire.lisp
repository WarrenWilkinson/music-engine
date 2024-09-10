(defpackage #:cl-pipewire
  (:use #:common-lisp #:cffi #:cl-pipewire.ffi)
  (:export #:pw-get-headers-version
	   #:pw-get-library-version
	   #:pw-init
	   #:pw-deinit
	   #:pw-main-loop-new
	   #:pw-main-loop-get-loop
	   #:pw-context-new
	   #:pw-context-connect))

(in-package #:cl-pipewire)

(define-foreign-library pipewire
  (t (:default "libpipewire-0.3")))

(use-foreign-library pipewire)

(defcfun ("pw_init" pw-init/c) :void
  (argc (:pointer :int))
  (argv (:pointer :string)))

(defun pw-init (args)
  "Initializes the underlying C library.  Args can be passed in, but
   we just ignore them.  The specification says Pipewire could use
   and or modify them, but it doesn't as of version 0.3.  So it's
   simpler to just not pass them in."
  (declare (ignore args))
  (let ((null (null-pointer)))
    (pw-init/c null null)))

(defcfun ("pw_get_library_version" pw-get-library-version) :string)



;;  return-type &body [docstring] arguments [&rest] ⇒ lisp-name

#|

(defun pw-get-headers-version ()
  "Returns the version of the header files used to produce this FFI."
  ;; Copied out of the headers included with this project.
  "0.3.65")

(defun pw-get-library-version ()
  
  (cl-pipewire.ffi::|pw_get_library_version|))


(defun pw-deinit ()
  "De-initializes the underlying C library."
  (cl-pipewire.ffi::|pw_deinit|))

(defun string-assoc-list-p (list)
  (and (listp list)
       (every #'(lambda (thing)
		  (and (consp thing)
		       (stringp (car thing))
		       (stringp (cdr thing))))
	      list)))

(deftype string-assoc-list () '(satisfies string-assoc-list-p))

(defun call-with-spa-dict (properties thunk)
  "Converts assoc-list to a stack-bound spa-dict, then calls thunk."
  (check-type properties string-assoc-list)
  (let ((len (length properties)))
    (if (zerop len)
	(funcall thunk (cffi:null-pointer))
	(cffi:with-foreign-object (dictionary '(:STRUCT cl-pipewire.ffi::|spa_dict|))
	  (cffi:with-foreign-slots (((flags cl-pipewire.ffi::|flags|)
				     (n-items cl-pipewire.ffi::|n_items|)
				     (items cl-pipewire.ffi::|items|))
				    dictionary (:struct cl-pipewire.ffi::|spa_dict|))
	    (cffi:with-foreign-object (allocated-items '(:STRUCT cl-pipewire.ffi::|spa_dict_item|) len)
	      (setf flags 0)
	      (setf n-items len)
	      (setf items allocated-items)
	      (loop :for i :below len
		    :for (k . v) :in properties
		    :nconc (cffi:with-foreign-slots (((key cl-pipewire.ffi::|key|)
						      (value cl-pipewire.ffi::|value|))
						     (cffi:mem-aref allocated-items '(:STRUCT cl-pipewire.ffi::|spa_dict_item|) i)
						(:struct cl-pipewire.ffi::|spa_dict_item|))
			     (list (setf key (cffi:foreign-string-alloc k))
				   (setf value (cffi:foreign-string-alloc v))))
		      :into foreign-strings
		    :finally (unwind-protect
				  (funcall thunk dictionary)
			       ;; Free all the foreign strings
			       (map nil #'cffi:foreign-string-free foreign-strings)))))))))

(defmacro with-spa-dict ((value) properties &body body)
  `(call-with-spa-dict ,properties #'(lambda (,value) ,@body)))

(defstruct pw-main-loop
  (pointer nil :type t :read-only t))

(defstruct pw-loop
  (pointer nil :type t :read-only t))

(defstruct pw-context
  (pointer nil :type t :read-only t))

(defmacro wrap-pointer (thing value)
  (let ((value-gs (gensym)))
    `(let ((,value-gs ,value))
       (if (zerop (sb-sys:sap-int ,value-gs))
	   (error 'cl-pipewire-null-pointer-returned :format-control "The expression ~a returned a null pointer." :format-value ',value)
	   (,(intern (concatenate 'string "MAKE-" (symbol-name (second thing)))) :pointer ,value-gs)))))

(defun pw-main-loop-new(properties)
  "Creates a main loop.  Properties should be an association list where both the keys and values are strings."
  (check-type properties string-assoc-list)
  ;; It appears that things are copied out of the dictionary, so it only needs to live as long as the call.
  (with-spa-dict (spa-dict) properties
    (wrap-pointer 'pw-main-loop (cl-pipewire.ffi::|pw_main_loop_new| spa-dict))))

(defun pw-main-loop-get-loop (main-loop)
  "Returns the loop of a main-loop."
  (check-type main-loop pw-main-loop)
  (wrap-pointer 'pw-loop (cl-pipewire.ffi::|pw_main_loop_get_loop| (pw-main-loop-pointer main-loop))))

(defun pw-context-new(pw-loop properties user-data-size)
  (check-type pw-loop pw-loop)
  (check-type properties string-assoc-list)
  (check-type user-data-size (integer 0 65536)) ;; Size can probably be bigger than this, I didn't check.

  ;; Is this dictionary okay? 
  (with-spa-dict (spa-dict) properties
    (wrap-pointer
     'pw-context
     (cl-pipewire.ffi::|pw_context_new|
				(pw-loop-pointer pw-loop)
				spa-dict
				user-data-size))))

(defstruct pw-core
  (pointer nil :type t :read-only t))

(defun pw-context-connect (pw-context properties user-data-size)
  (check-type pw-context pw-context)
  (check-type properties string-assoc-list)
  (check-type user-data-size (integer 0 65536)) ;; Size can probably be bigger than this, I didn't check.
  (with-spa-dict (spa-dict) properties
    (wrap-pointer
     'pw-core
     (cl-pipewire.ffi::|pw_context_connect|
		       (pw-context-pointer pw-context)
		       spa-dict
		       user-data-size))))
  
;; (defun pw-core-get-registry (pw-core ??? user-data-size)
;;   (check-type pw-context pw-context)
;;   (check-type properties string-assoc-list)
;;   (check-type user-data-size (integer 0 65536)) ;; Size can probably be bigger than this, I didn't check.

;; OKAY, no good, too much C macro magic. I need to precompile a special library for myself.
;; to give me access to the real C environment and all that so I can use these defuns and
;; turn them into functions.

;; https://cffi.common-lisp.dev/manual/html_node/The-Groveller.html

  ;; My thing has callabacks...
  ;; funcs
  ;; 

;;   (wrap-pointer
;;    'pw-registry
;;    (cl-pipewire.ffi::|spa_callbacks_call_res|
;; 		       (pw-context-pointer pw-context)
;; 		       spa-dict
;; 		       user-data-size))
  
;; registry = pw_core_get_registry(core, PW_VERSION_REGISTRY,
;;                 0 /* user_data size */);


;; /**
;;  * Invoke method named \a method in the \a callbacks.
;;  * The \a method_type defines the type of the method struct.
;;  *
;;  * The return value is stored in \a res.
;;  */
;; #define spa_callbacks_call_res(callbacks,type,res,method,vers,...)		\
;; ({										\
;; 	const type *_f = (const type *) (callbacks)->funcs;			\
;; 	if (SPA_LIKELY(SPA_CALLBACK_CHECK(_f,method,vers)))			\
;; 		res = _f->method((callbacks)->data, ## __VA_ARGS__);		\
;; 	res;									\
;; })
|#
