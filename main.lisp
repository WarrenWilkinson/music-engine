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

(defstruct grid
  (elements nil :type list :read-only t)
  (columns 1 :type (and fixnum (integer 1)) :read-only t))

(defstruct basic-led
  (on-color *basic-led-red-on* :type cairo-color :read-only t)
  (off-color *basic-led-red-off* :type cairo-color :read-only t)
  (glow-color *basic-led-red-glow* :type cairo-color :read-only t)
  (stroke-width 0.01d0 :type double-float :read-only t)
  (stroke-color *black* :type cairo-color :read-only t)
  (on-click nil :type (or null function symbol) :read-only t)
  (illuminated nil :type boolean :read-only nil))

;; (defstruct label
;;   "A text label... Maybe needs a background?"
;;   (text nil :type string :read-only nil)
;;   (font nil :type string :read-only t)
;;   (font-size 1.0d0 :type double-float :read-only t))

(defun red-led (&key on-click)
  (make-basic-led :on-click on-click))

(defun green-led (&key on-click)
  (make-basic-led
   :on-color *basic-led-green-on*
   :off-color *basic-led-green-off*
   :glow-color *basic-led-green-glow*
   :on-click on-click))

(defparameter *gui*
  (let ((light #'(lambda (self)
		 (setf (basic-led-illuminated self) (not (basic-led-illuminated self)))
		   t)))
    (make-grid
     :columns 2
     :elements (list
		(red-led :on-click light)
		(green-led :on-click light)))))

(defgeneric draw (element)
  (:documentation "Draw the given element.")
  (:method ((grid grid))
    (assert (= (grid-columns grid) 2))
    (assert (= 2 (length (grid-elements grid))))
    (let ((m (cl-cairo2:get-trans-matrix)))
      (cl-cairo2:set-trans-matrix m)
      (cl-cairo2:translate 0d0 0d0)
      (cl-cairo2:scale 0.5 0.5)
      (draw (first (grid-elements grid)))

      (cl-cairo2:set-trans-matrix m)
      (cl-cairo2:translate 0.5d0 0.0d0)
      (cl-cairo2:scale 0.5 0.5)
      (draw (second (grid-elements grid)))))
  (:method ((element basic-led))
    (with-slots (on-color off-color glow-color stroke-width stroke-color illuminated) element
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

(defgeneric click (element x y)
  (:documentation "Click the given element. x and y should be relative to top corner of
   the elements range and from 0.0 to 1.0.  It should return T if the
   click resulted in the GUI needing to be redrawn.")
  (:method ((grid grid) x y)
    (assert (= (grid-columns grid) 2))
    (assert (= 2 (length (grid-elements grid))))
    (if (< x 0.5d0)
	(click (first (grid-elements grid)) (/ x 0.5d0) (/ y 0.5d0))
	(click (second (grid-elements grid)) (/ (- x 0.5d0) 0.5d0) (/ y 0.5d0))))
  (:method ((element basic-led) x y)
    (unless (<= .1 x .9)
      (return-from click nil))
    (unless (<= .1 y .9)
      (return-from click nil))
    (let* ((x (- x 0.5))
	   (y (- y 0.5))
	   (d (+ (* x x) (* y y))))
      (when (< d (* 0.4d0 0.4d0 ))
	(funcall (basic-led-on-click element) element)))))

(defun draw-gui (cairo width height)
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
      (draw *gui*))))

(defun click-gui (x y width height)
  "x and y should be in range of (0,0) to (width,hight)"
  (check-type x double-float)
  (check-type y double-float)
  (check-type width fixnum)
  (check-type height fixnum)
  ;; (format t "~%click-gui ~a ~a ~a ~a" x y width height)
  (unless (> width 0)
    (return-from click-gui nil))

  (unless (> height 0)
    (return-from click-gui nil))

  (unless (<= 0 x width)
    (return-from click-gui nil))

  (unless (<= 0 y height)
    (return-from click-gui nil))

  (let ((smaller-dim (min width height)))
    ;; Translate the coordinates...
    (cond ((= width smaller-dim)
	   ;; Then Y is too big, subtract out the margin.
	   (decf y (truncate (- height smaller-dim) 2)))
	  (t
	   ;; Then X is too big, subtract out the margin.
	   (decf x (truncate (- width smaller-dim) 2))))

    (setf x (coerce (/ x smaller-dim) 'double-float))
    (setf y (coerce (/ y smaller-dim) 'double-float))
    ;; (format t "~% -> click ~a ~a" x y)
    (click *gui* x y)))

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

(defun test-draw-gui ()
  (let* ((width 200)
	 (height 100)
	 (surface (cl-cairo2:create-pdf-surface "example.pdf" width height)))
    (setf cl-cairo2:*context* (cl-cairo2:create-context surface))
    (cl-cairo2:destroy surface)
    (draw-gui cl-cairo2:*context* width height)
    (cl-cairo2:destroy cl-cairo2:*context*)))

(cffi:defcallback draw-thing :void ((gtk-drawing-area :pointer) (cairo :pointer)
                                    (width :int) (height :int)
				    (user-data :pointer))
  (declare (ignore user-data gtk-drawing-area))
  (let ((cairo (make-instance 'cl-cairo2:context
			      :pixel-based-p t
			      :height height
			      :width width
			      :pointer cairo))
	;; (gtk-drawing-area (gir:build-object-ptr (gir:nget-desc *gtk* "DrawingArea") gtk-drawing-area))
	)
    (ignore-errors (draw-gui cairo width height))))

(cffi:defcallback cleanup-draw-thing :void ((user-data :pointer))
  (declare (ignore user-data))
  (format t "~%Inside my cleanup-draw-thing callback!"))

(declaim (type fixnum *gui-height* *gui-width*))
(defparameter *gui-height* 600)
(defparameter *gui-width* 800)

(defun create-gui ()
  "Starts the GUI, which starts up a music engine and displays it."
  (let ((app (gir:invoke (*gtk* "Application" 'new)
			 "org.gtk.example"
			 (gir:nget *gio* "ApplicationFlags" :default-flags)))
	(default-width 800)
	(default-height 600))
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
			 (gesture-click (gir:invoke (*gtk* "GestureClick" 'new)))
			 (drawing-area (gir:invoke (*gtk* "DrawingArea" 'new))))

		     ;; Add a click handler to the drawing area.
		     (gir:connect gesture-click "pressed"
				  (lambda (self button-number x y)
				    (declare (ignore self button-number))
				    (when (click-gui x y *gui-width* *gui-height*)
				      ;; Trigger redraw when click-gui returns T
				      (gir:invoke (drawing-area 'queue-draw)))))
		     (setf (gir:property gesture-click "button") 1)
		     (gir:invoke (drawing-area 'add-controller) gesture-click)

		     ;; Setup the drawing area's callback
		     (gir:invoke (drawing-area 'set-draw-func)
				 (cffi:callback draw-thing)
				 (cffi:null-pointer) ;; user data
				 (cffi:callback cleanup-draw-thing))

		     ;; Setup a resize handler. We need to know the size
		     ;; so we can give it to the click handler
		     (gir:connect drawing-area "resize"
				  (lambda (self width height)
				      (declare (ignore self))
				      (setf *gui-width* width)
				      (setf *gui-height* height))
				  :after t)

		     ;; Set some initial properties and values...
		     (setf (gir:property window "default-width") default-width)
		     (setf (gir:property window "default-height") default-height)
		     (setf *gui-width* default-width)
		     (setf *gui-height* default-height)
		     (setf (gir:property window "title") "Virtual Warren Controllerist Instrument")

		     (gir:invoke (window 'set-child) drawing-area)
		     (gir:invoke (window 'show)))))
    (gir:invoke (app 'run) nil)))

(defun start-gui ()
  (sb-thread:make-thread #'create-gui :name "GUI"))
