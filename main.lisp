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


;; Okay, divide the problem... EVERY THING draws in a 1x1 box. Not sure about
;; stroke widths... but there you go... wait even this doesn't work exactly...
;; Subdividing is correct --- but things change position, clicks happen, etc...
;; There is definitely "STATE" being passed in... GUI state stuff...
;; For example, an LED has state "ON" or "OFF".

(defstruct gui-basic-led-state
  "This stuff changes at run-time."
  (illuminated nil :type boolean :read-only nil))

(defstruct cairo-color
  (r 0.0d0 :type double-float :read-only t)
  (g 0.0d0 :type double-float :read-only t)
  (b 0.0d0 :type double-float :read-only t)
  (a 1.0d0 :type double-float :read-only t))

(defparameter *black* (make-cairo-color :r 0.0d0 :g 0.0d0 :b 0.0d0))
(defparameter *basic-led-red-on*  (make-cairo-color :r 0.6d0 :g 0.0d0 :b 0.0d0))
(defparameter *basic-led-red-off* (make-cairo-color :r 0.2d0 :g 0.0d0 :b 0.0d0))
(defparameter *basic-led-red-glow* (make-cairo-color :r 1.0d0 :g 0.1d0 :b 0.3d0 :a 0.3d0))
(defparameter *basic-led-green-on*  (make-cairo-color :r 0.0d0 :g 0.6d0 :b 0.0d0))
(defparameter *basic-led-green-off* (make-cairo-color :r 0.0d0 :g 0.2d0 :b 0.0d0))
(defparameter *basic-led-green-glow* (make-cairo-color :r 0.3d0 :g 1.0d0 :b 0.3d0 :a 0.3d0))

(defstruct gui-basic-led-parameters
  "This stuff changes at design time."
  (on-color nil :type cairo-color :read-only t)
  (off-color nil :type cairo-color :read-only t)
  (glow-color nil :type cairo-color :read-only t)
  (stroke-width nil :type double-float :read-only t)
  (stroke-color *black* :type cairo-color :read-only t))

(defstruct gui-label-state)

(defstruct gui-label-parameters
  "This stuff changes at run-time."
  (text nil :type string :read-only nil)
  (font nil :type string :read-only t)
  (font-size 1.0d0 :type double-float :read-only t))

;; The recursion metadata is what's tricky... because it might includes things like
;; what covers what, where the button is...  Do things move?  For example, a wammy
;; bar might MOVE when clicked... how does that get communicated?   I think
;; the best approach is it DOESN'T.

(defun cairo-draw-basic-led (parameters state)
  (with-slots (on-color off-color glow-color stroke-width stroke-color) parameters
    (with-slots (illuminated) state
      (cl-cairo2:arc 0.5d0 0.5d0 0.4d0 0.0 (* 2 pi))
      (with-slots (r g b a) (if illuminated on-color off-color)
	(cl-cairo2:set-source-rgba r g b a))
      (cl-cairo2:fill-path)

      (cl-cairo2:set-line-width stroke-width)
      (cl-cairo2:arc 0.5d0 0.5d0 0.4d0 0.0 (* 2 pi))
      (with-slots (r g b a) stroke-color
	(cl-cairo2:set-source-rgba r g b a))
      (cl-cairo2:stroke)

      (when illuminated
	(with-slots (r g b a) glow-color
	  (dotimes (rev-x 3)
	    (let* ((x (- 3 rev-x))
		   (current-a (* (/ x 3) a))
		   (current-r (+ .4d0 (* (/ x 3) .1d0))))
	      (cl-cairo2:arc 0.5d0 0.5d0 current-r 0.0 (* 2 pi))
	      (cl-cairo2:set-source-rgba r g b current-a)
	      (cl-cairo2:fill-path))))))))

;; Okay, how to handle drawing this thing?  lets just focus on the buttons
;; I guess...   Where do those get positioned?  It's a tree I guess...

;; First matrix positions the drawing within the drawing area (full size I suppose
;; and maps it from 0 to 1 each way...   Maybe.. I want to preserve a good aspect
;; ratio I think...

;; Okay, so then I need to draw a neck, a body, a keytar ... where do these go?

;; ACTUALLY, before doing this, lets get mouse clicking first.  See if I can click
;; this box.  I have a feeling that I'll want to describe my drawing with code so
;; I can figure out where I've clicked and also apply colors and junk... there
;; is some state to this GUI representation.

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
	   (cl-cairo2:set-source-rgb 0.7 0.7 0.8)
	   (cl-cairo2:fill-path)

	   (let ((m (cl-cairo2:get-trans-matrix)))
	     (cl-cairo2:set-trans-matrix m)
	     (cl-cairo2:translate 0d0 0d0)
	     (cl-cairo2:scale 0.5 0.5)
	     (cairo-draw-basic-led
	      (make-gui-basic-led-parameters
	       :on-color *basic-led-red-on*
	       :off-color *basic-led-red-off*
	       :glow-color *basic-led-red-glow*
	       :stroke-width .02d0
	       :stroke-color *black*)
	      (make-gui-basic-led-state
	       :illuminated t))

	     (cl-cairo2:set-trans-matrix m)
	     (cl-cairo2:translate 0d0 0.5d0)
	     (cl-cairo2:scale 0.5 0.5)
	     (cairo-draw-basic-led
	      (make-gui-basic-led-parameters
	       :on-color *basic-led-red-on*
	       :off-color *basic-led-red-off*
	       :glow-color *basic-led-red-glow*
	       :stroke-width .02d0
	       :stroke-color *black*)
	      (make-gui-basic-led-state
	       :illuminated nil))

	     (cl-cairo2:set-trans-matrix m)
	     (cl-cairo2:translate 0.5d0 0.5d0)
	     (cl-cairo2:scale 0.5 0.5)
	     (cairo-draw-basic-led
	      (make-gui-basic-led-parameters
	       :on-color *basic-led-green-on*
	       :off-color *basic-led-green-off*
	       :glow-color *basic-led-green-glow*
	       :stroke-width .02d0
	       :stroke-color *black*)
	      (make-gui-basic-led-state
	       :illuminated nil))

	     (cl-cairo2:set-trans-matrix m)
	     (cl-cairo2:translate 0.5d0 0.0d0)
	     (cl-cairo2:scale 0.5 0.5)
	     (cairo-draw-basic-led
	      (make-gui-basic-led-parameters
	       :on-color *basic-led-green-on*
	       :off-color *basic-led-green-off*
	       :glow-color *basic-led-green-glow*
	       :stroke-width .02d0
	       :stroke-color *black*)
	      (make-gui-basic-led-state
	       :illuminated t)))))
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
