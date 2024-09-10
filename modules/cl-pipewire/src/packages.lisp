(defpackage #:cl-pipewire.ffi
  (:use)
  (:export #:pw-get-headers-version))

cffi-grovel::(define-wrapper-syntax pkg-config-cflags (pkg &key optional)
  (let ((output-stream (make-string-output-stream))
        (program+args (list "pkg-config" pkg "--cflags")))
    (format *debug-io* "~&;~{ ~a~}~%" program+args)
    (handler-case
        (progn
          (run-program program+args
                       :output (make-broadcast-stream output-stream *debug-io*)
                       :error-output output-stream)
          (appendf *cc-flags*
                   (parse-command-flags (get-output-stream-string output-stream))))
      (error (e)
        (let ((message (format nil "~a~&~%~a~&"
                               e (get-output-stream-string output-stream))))
          (cond (optional
                 (format *debug-io* "~&; ERROR: ~a" message)
                 (format *debug-io* "~&~%; Attempting to continue anyway.~%"))
                (t
                 (grovel-error "~a" message))))))))
