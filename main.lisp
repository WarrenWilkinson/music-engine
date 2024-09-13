(in-package :cl-user)

(format t "~%Loading 3rd party dependencies with quicklisp...~%")
(ql:quickload "osc") ;; https://github.com/zzkt/osc
(ql:quickload "cffi") ;; https://github.com/zzkt/osc
(ql:quickload "cl-gobject-introspection") ;; https://github.com/andy128k/cl-gobject-introspection
(ql:quickload "cl-cairo2") ;; https://github.com/andy128k/cl-gobject-introspection

(format t "~%Loading music engine...")
(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,*default-pathname-defaults*)
   :inherit-configuration))
;; (asdf:load-system "music-engine")

(format t "~%Starting up...")

;; This is a good start:
;; I've got GTK going for drawing and a connection to wireplumber.

;; sudo apt-get install gir1.2-wp-0.4
(defvar *wp* (gir:require-namespace "Wp" "0.4")) ;; Wire Plumber
(defvar *gio* (gir:require-namespace "Gio")) 
(defvar *gtk* (gir:require-namespace "Gtk" "4.0")) ;; GTK 4.0

;; Okay, how to handle drawing this thing?  lets just focus on the buttons
;; I guess...   Where do those get positioned?  It's a tree I guess...

;; First matrix positions the drawing within the drawing area (full size I suppose
;; and maps it from 0 to 1 each way...   Maybe.. I want to preserve a good aspect
;; ratio I think...

(defstruct music-engine
  "My thing"
  (initialized-p nil :type boolean :read-only nil))

(defun music-engine-startup (me)
  (check-type me music-engine)
  (setf (music-engine-initialized-p me) t))

(defun music-engine-shutdown (me)
  (check-type me music-engine)
  (setf (music-engine-initialized-p me) nil))

(defvar *music-engine* (make-music-engine))

(labels ((draw-instrument ()
	   (cl-cairo2:rectangle 0.0 0.0 1.0 1.0)
	   (cl-cairo2:set-source-rgb 1.0 0.2 0.5)
	   (cl-cairo2:fill-path)))
  (defun draw (cairo width height)
    (let ((cl-cairo2:*context* cairo))
      ;; Background
      (cl-cairo2:rectangle 0 0 width height)
      (cl-cairo2:set-source-rgb 0.2 0.2 0.5)
      (cl-cairo2:fill-path)

      ;; Position drawing space with fixed aspect ratio in the middle.
      (let ((smaller-dim (min width height)))
	;; Translate to middle
	(cond ((= width smaller-dim)
	       (cl-cairo2:translate 0 (truncate (- height smaller-dim) 2)))
	      (t
	       (cl-cairo2:translate (truncate (- width smaller-dim) 2) 0)))

	;; Scale so drawing is 0 to 1 in both axis
	(cl-cairo2:scale smaller-dim smaller-dim)

	;; Now fill in the drawing region
	(draw-instrument)))))

(defun test-draw ()
  (let* ((width 200)
	 (height 100)
	 (surface (cl-cairo2:create-pdf-surface "example.pdf" width height)))
    (setf cl-cairo2:*context* (cl-cairo2:create-context surface))
    (cl-cairo2:destroy surface)
    (draw cl-cairo2:*context* width height)
    (cl-cairo2:destroy cl-cairo2:*context*)))

(cffi:defcallback draw-thing :void ((gtk-drawing-area :pointer) (cairo :pointer)
                                    (width :int) (height :int)
				    (user-data :pointer))
  (declare (ignore user-data))
  (let ((cairo (make-instance 'cl-cairo2:context
			      :pixel-based-p t
			      :height height
			      :width width
			      :pointer cairo))
	;; (gtk-drawing-area (gir:build-object-ptr (gir:nget-desc *gtk* "DrawingArea") gtk-drawing-area))
	)
    (ignore-errors (draw cairo width height))))

(cffi:defcallback cleanup-draw-thing :void ((user-data :pointer))
  (declare (ignore user-data))
  (format t "~%Inside my cleanup-draw-thing callback!"))

(defun create-gui ()
  "Starts the GUI, which starts up a music engine and displays it."
  (let ((app (gir:invoke (*gtk* "Application" 'new)
			 "org.gtk.example"
			 (gir:nget *gio* "ApplicationFlags" :default-flags))))
    (format t "~%APP is ~a" app)
    (gir:connect app "startup"
		 (lambda (app)
		   (declare (ignore app))
		   (format t "~%Application Start-up!")
		   (music-engine-startup *music-engine*)))
    (gir:connect app "shutdown"
		 (lambda (app)
		   (declare (ignore app))
		   (format t "~%Application Shut-down!")
		   (music-engine-shutdown *music-engine*)))
    (gir:connect app "activate"
		 (lambda (app)
		   (format t "~%Application Activate... create a window!")
		   (let ((window (gir:invoke (*gtk* "ApplicationWindow" 'new) app))
			 (drawing-area (gir:invoke (*gtk* "DrawingArea" 'new))))

		     ;; Setup the drawing area...
		     (gir:invoke (drawing-area 'set-draw-func)
				 (cffi:callback draw-thing)
				 (cffi:null-pointer) ;; user data
				 (cffi:callback cleanup-draw-thing))

		     (setf (gir:property window "default-height") 600)
		     (setf (gir:property window "title") "Virtual Warren Controllerist Instrument")
		     (setf (gir:property window "default-width") 800)

		     (gir:invoke (window 'set-child) drawing-area)
    
		     (gir:invoke (window 'show)))))
    (gir:invoke (app 'run) nil)))

(defun start-gui ()
  (sb-thread:make-thread #'create-gui :name "GUI"))
