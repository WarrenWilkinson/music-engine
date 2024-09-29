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

(asdf:load-system :cl-alsa-midi)

;; TODO, it's time to start working on pipewire's session manager.
;; I want some my MIDI to automatically connect to fluidsynth...
;; BUT IF either isn't started, then some LED should be RED.

;; Also, I don't like the "on-click" method of the piano sending the
;; events... that SHOULD be coming from the instrument itself.
;; The gui is flaky... and I'd like to be able to trigger things
;; even if it's not running.

(format t "~%Starting up MIDI interface...")
;; Start up the MIDI interface; run `aconnect -lio` to see it.
(cl-alsa-midi/midihelper:midihelper-start)

;; This might work, but I found timidy to be less reliable: pw-jack timidity -Oj
;; pw-jack fluidsynth -a jack -g 5 -v
;; qpwgraph

;; sudo apt-get install gir1.2-wp-0.4
(defvar *gobject* (gir:require-namespace "GObject"))

(defvar *glib* (gir:require-namespace "GLib"))

;; Why this not work? It feels like it should, maybe needs wp-0.5.
(gir:invoke (*gobject* "type_from_name") "WpDevice") 
;; (gir:invoke (*gobject* "g_type_from_name" "WpNode"))

(defvar *wp* (gir:require-namespace "Wp" "0.4")) ;; Wire Plumber


;; Find the structure...
;; (find "WpObjectInterest" (gir:repository-get-infos nil "Wp") :key #'gir:registered-type-info-get-type-name :test #'string-equal)
;;

;; (defvar *wp-node* (gir:nget *wp* "Node"))

(gir:nget *wp*  "InitFlags")

(gir:invoke (*wp* "init") (gir:invoke (*wp*  "InitFlags") :all))

(defvar *wp-node-g-type* (gir:invoke (*gobject* "type_from_name") "WpNode") )
(defvar *context* (gir:invoke (*glib* "main_context_default")))
(defvar *core* (gir:invoke (*wp* "Core" 'new) *context* nil))
;; (defvar *wp-proxy-g-type* (wp-proxy-get-type))

(defvar *output* *standard-output*)

;; CALLBACKS always called by the thread's context that was in place when the callback was created... interesting.

(gir:connect *core* "connected" (lambda (&rest args ) (format *output* "~%~s is now connected." args)))
(gir:connect *core* "disconnected" (lambda (&rest args) (format *output* "~%~s is now disconnected." args)))


#|
;; In order to get the config, I'm finding it's not exposed via the GIR
;; in 0.4:

;; However, it doesn't seem like it should be needed. In 0.4 you don't
;; pass in a configuration file, but a GMainContext -- and it should just grab
;; the default one if it needs it... 
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library wp
    ;;(:darwin "libwp-0.4.dylib")
    (:unix "libwireplumber-0.4.so.0")))
(cffi:use-foreign-library wp)
(cffi:defcfun (c/wp-conf-new-open "wp_conf_new_open") :pointer
  (path :string)
  (out :pointer))
(defun wp-conf-new-open (path)
  (cffi:with-foreign-object (error-message ':pointer)
    (let ((conf-object (c/wp-conf-new-open path (cffi:make-pointer (cffi:pointer-address error-message)))))
      (unless (cffi:null-pointer-p error-message)
	(error (cffi:foreign-string-to-lisp (cffi:mem-ref error-message :pointer 8)
					    :max-chars 128)))
      conf-object)))
(wp-conf-new-open "")
|#

					;(cffi:defcfun (wp-proxy-get-type "wp_proxy_get_type") :uint)
(print "Is my core connected?")
(print (gir:invoke (*core* "is_connected")))

;; (print (gir:invoke (*context* "iteration") t))

(gir:invoke (*core* "connect"))

(print "Is my core connected?")
(print (gir:invoke (*core* "is_connected")))


(defvar *interest* (gir:invoke (*wp* "ObjectInterest" 'new_type) *wp-node-g-type*))
(defvar *object-manager* (gir:invoke (*wp* "ObjectManager" 'new)))
(gir:invoke (*object-manager* "add_interest_full") *interest*) ;; this fails if wp_init hasn't been called.

(print "My g_main_context is...")
(print (gir:invoke (*core* "get_g_main_context")))

(print "Is my object manager installed?")
(print (gir:invoke (*object-manager* "is_installed")))

;; Maybe I need some properties?

;; AHA ! The problem was my event loop wasn't running, so nothing happens. If I manually run it several times things work well.
;; My next task is to understand this gmaincontext loop stuff and figure out what threads I'll have and how to ensure my
;; gui works without fucking me over.
(gir:invoke (*core* "install_object_manager") *object-manager*)
(print "Is my object manager installed?")
(print (gir:invoke (*object-manager* "is_installed")))

(break "stop here!")



;; Okay, so wireplumber integration is working better... but still not getting output out yet. Keep trying.
;; If I totally fail I can use pw-dump and pw-link command line tools to do the same effect.
;; Still isn't installed... why?


;; Okay, so I've got wireplumber 0.4.13 (ls /usr/lib/x86_64-linux-gnu/libwir*)
;; The current version is 0.5.56. Need Ubuntu "The Oracular Oriole" to get that!

;; (gir:struct-info-get-methods
;; ;;    (slot-value (gir:nget *gtk* "ApplicationWindow")   'gir::info))

;; (gir:list-constructors-desc (gir:nget *wp*  "ObjectManager"))

;; (defvar *wp-object-interest-struct* (find "WpObjectInterest" (gir:repository-get-infos nil "Wp") :key #'gir:registered-type-info-get-type-name :test #'string-equal))
;; (gir:struct-info-get-methods *wp-object-interest-struct*)
;; (gir:list-constructors-desc (gir:nget *wp*  "ObjectInterest"));*wp-object-interest-struct*)

;; (gir:list-constructors-desc (gir:nget *wp*  "ObjectInterest"))

;; (mapcar #'gir:function-info-get-symbol (gir:struct-info-get-methods *wp-object-interest-struct*))

;; ;; (gir:nget *wp*  "ObjectManager")
;; (gir:struct-info-get-methods
;;    (slot-value (gir:nget *gtk* "ApplicationWindow")   'gir::info))

;; (gir:struct-info-get-methods *wp-object-interest-struct*)

;; The above is probably fine... we're interesting in ANY node...
;; Lets get em!

;; (defvar *wp-node* (gir:nget *wp* "Node"))



;; I think fluidsynth can have the midi port name set... because I think
;; I might want multiple?  midi.alsa_seq.id  -- Yeah, you can do it.
;; Also it's a library... I can make a program that does all of them..
;; including a router and all kinds of effects... interesting.
;;
;; Okay, so I think I want an object manager.
;; I want to be told if fluidsynth comes on or off, and if my keytar is present or
;; not.
;;
;; So yeah, that's the one... bind that, and see if I can be notified when stuff
;; starts and stops and set an LED based on that.
;;
;; So yeah, as soon as Keytar shows up AND so does fluidsynth, the rule should be
;; to link them. That's the first step.  Eventually it'll be a whole stateful beast.

;; WHOAA a soundfont collection is like a gigabyte!

(defvar *gui-redraw-callback* nil
  "Set to true when the GUI needs to be redrawn." )

(defun queue-redraw ()
  (when *gui-redraw-callback*
    (funcall *gui-redraw-callback*)))

;; Okay, divide the problem... EVERY THING draws in a 1x1 box. Not sure about
;; stroke widths... but there you go... wait even this doesn't work exactly...
;; Subdividing is correct --- but things change position, clicks happen, etc...
;; There is definitely "STATE" being passed in... GUI state stuff...
;; For example, an LED has state "ON" or "OFF".
;;
;; It would be bad practice to tie the GUI strongly to the MIDI library...
;; I think I should have a callback handler...

(defstruct cairo-color
  (r 0.0d0 :type double-float :read-only t)
  (g 0.0d0 :type double-float :read-only t)
  (b 0.0d0 :type double-float :read-only t)
  (a 1.0d0 :type double-float :read-only t))

(defparameter *black* (make-cairo-color :r 0.0d0 :g 0.0d0 :b 0.0d0))
(defparameter *white* (make-cairo-color :r 1.0d0 :g 1.0d0 :b 1.0d0))
(defparameter *basic-led-red-on*  (make-cairo-color :r 0.6d0 :g 0.0d0 :b 0.0d0))
(defparameter *basic-led-red-off* (make-cairo-color :r 0.2d0 :g 0.0d0 :b 0.0d0))
(defparameter *basic-led-red-glow* (make-cairo-color :r 1.0d0 :g 0.1d0 :b 0.3d0 :a 0.3d0))
(defparameter *basic-led-green-on*  (make-cairo-color :r 0.0d0 :g 0.6d0 :b 0.0d0))
(defparameter *basic-led-green-off* (make-cairo-color :r 0.0d0 :g 0.2d0 :b 0.0d0))
(defparameter *basic-led-green-glow* (make-cairo-color :r 0.3d0 :g 1.0d0 :b 0.3d0 :a 0.3d0))

(defstruct widget
  "Position of the GUI element. Note for development: they can be changed
   with slot-value and you can setf *gui-redraw-callback*.
   Rotation is in degrees, and a widget can assume that it's initial
   scale has x going from 0 to 1, and y from 0 to y/x (and is in terms
   of an offset in it's parents space). Ergo a translation of 0.1 is
   pretty big."
  (translate-x 0d0 :type (double-float 0d0 1d0) :read-only t)
  (translate-y 0d0 :type (double-float 0d0 1d0) :read-only t)
  (rotation 0d0 :type (double-float 0d0 360d0) :read-only t)
  (rotation-origin :top-left :type (member :top-left) :read-only t)
  (scale 1d0 :type double-float :read-only t))

(defstruct (grid (:include widget))
  (elements nil :type list :read-only t)
  (columns 1 :type (and fixnum (integer 1)) :read-only t))

(defstruct (basic-led (:include widget))
  (on-color *basic-led-red-on* :type cairo-color :read-only t)
  (off-color *basic-led-red-off* :type cairo-color :read-only t)
  (glow-color *basic-led-red-glow* :type cairo-color :read-only t)
  (stroke-width 0.01d0 :type double-float :read-only t)
  (stroke-color *black* :type cairo-color :read-only t)
  (on-click nil :type (or null function symbol) :read-only t)
  (%illuminated nil :type boolean :read-only nil))

(declaim (inline basic-led-illuminated))
(defun basic-led-illuminated (basic-led)
  (basic-led-%illuminated basic-led))

(defun (setf basic-led-illuminated) (new-color basic-led)
  (prog1 (setf (basic-led-%illuminated basic-led) new-color)
    (queue-redraw)))

(defstruct keytar-key-event
  "Stores the MIDI key."
  ;; middle C is note 60 in the C3 convention
  (note 0 :type integer :read-only t)
  (event :press :type (member :press :release) :read-only t))

(deftype keytar-event () '(or keytar-key-event))

(defstruct (keytar (:include widget))
  (fill-color (make-cairo-color :r 0.3d0 :g 0.3d0 :b 0.4d0) :type cairo-color :read-only t)
  (stroke-width 0.1d0 :type double-float :read-only t)
  (stroke-color *black* :type cairo-color :read-only t)
  (white-key-color *white* :type cairo-color :read-only t)
  (white-key-stroke-color (make-cairo-color :r .6d0 :g .6d0 :b .6d0) :type cairo-color :read-only t)
  (white-key-pressed-color (make-cairo-color :r .9d0 :g .8d0 :b .8d0) :type cairo-color :read-only t)
  (black-key-color (make-cairo-color :r .05d0 :g .05d0 :b .05d0) :type cairo-color :read-only t)
  (black-key-pressed-color (make-cairo-color :r .3d0 :g .2d0 :b .2d0) :type cairo-color :read-only t)
  (black-key-stroke-color *black* :type cairo-color :read-only t)
  (orange-group-color (make-cairo-color :r 1.0d0 :g 0.5d0 :b 0.0d0) :type cairo-color :read-only t)
  (green-group-color (make-cairo-color :r 0.0d0 :g 0.9d0 :b 0.0d0) :type cairo-color :read-only t)
  (blue-group-color (make-cairo-color :r 0.35d0 :g 0.7d0 :b 0.9d0) :type cairo-color :read-only t)
  (yellow-group-color (make-cairo-color :r 0.9d0 :g 0.9d0 :b 0.5d0) :type cairo-color :read-only t)
  (red-group-color (make-cairo-color :r 6.0d0 :g 0.2d0 :b 0.2d0) :type cairo-color :read-only t)
  (modulation-control-color (make-cairo-color :r 0.2d0 :g 0.1d0 :b 0.1d0) :type cairo-color :read-only t)
  (modulation-control-stroke-color *black* :type cairo-color :read-only t)
  (overdrive-button-color (make-cairo-color :r 0.22d0 :g 0.1d0 :b 0.1d0) :type cairo-color :read-only t)
  (overdrive-button-pressed-color (make-cairo-color :r 0.1d0 :g 0.1d0 :b 0.1d0) :type cairo-color :read-only t)
  (led-on-color (make-cairo-color :r 1.0d0 :g 0.2d0 :b 0.2d0) :type cairo-color :read-only t)
  (led-off-color (make-cairo-color :r 0.5d0 :g 0.5d0 :b 0.5d0) :type cairo-color :read-only t)
  (overdrive-button-stroke-color *black* :type cairo-color :read-only t)
  (dpad-stroke-color *black* :type cairo-color :read-only t)
  (dpad-base-color (make-cairo-color :r 0.19d0 :g 0.19d0 :b 0.19d0) :type cairo-color :read-only t)
  (dpad-highlight-color (make-cairo-color :r 0.4d0 :g 0.4d0 :b 0.4d0) :type cairo-color :read-only t)
  (button-base-color (make-cairo-color :r 0.22d0 :g 0.22d0 :b 0.24d0) :type cairo-color :read-only t)
  (button-pressed-color (make-cairo-color :r 0.15d0 :g 0.0d0 :b 0.0d0) :type cairo-color :read-only t)
  (button-stroke-color *black* :type cairo-color :read-only t)
  (green-triangle-color (make-cairo-color :r 0.2d0 :g 0.5d0 :b 0.2d0) :type cairo-color :read-only t)
  (green-triangle-pressed-color (make-cairo-color :r 0.1d0 :g 0.3d0 :b 0.0d0) :type cairo-color :read-only t)
  (red-circle-color (make-cairo-color :r 0.6d0 :g 0.3d0 :b 0.3d0) :type cairo-color :read-only t)
  (red-circle-pressed-color (make-cairo-color :r 0.3d0 :g 0.0d0 :b 0.0d0) :type cairo-color :read-only t)
  (pink-square-color (make-cairo-color :r 1.0d0 :g 0.8d0 :b 0.8d0) :type cairo-color :read-only t)
  (pink-square-pressed-color (make-cairo-color :r 0.4d0 :g 0.3d0 :b 0.3d0) :type cairo-color :read-only t)
  (white-cross-color (make-cairo-color :r 0.8d0 :g 0.8d0 :b 0.8d0) :type cairo-color :read-only t)
  (white-cross-pressed-color (make-cairo-color :r 0.35d0 :g 0.3d0 :b 0.3d0) :type cairo-color :read-only t)
  (callback #'(lambda (keytar-event) (declare (ignore keytar-event))) :type (or null symbol function) :read-only t)
  (%dpad-position nil :type (or null (member :up :down :left :right)) :read-only nil)
  (midi-notes (make-array 25 :element-type t :initial-contents
		     '(60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84))
   :type vector :read-only t)
  (%depressed (make-array 25 :element-type 'bit :initial-element 0)
   :type bit-vector :read-only t)
  (%buttons (make-array 7 :element-type 'bit :initial-element 0) :type bit-vector :read-only t)
  (%led (make-array 4 :element-type 'bit :initial-element 0)))

(defun keytar-dpad-position (keytar)
  (keytar-%dpad-position keytar))

(defun (setf keytar-dpad-position) (np keytar)
  (check-type np (or null (member :up :down :left :right)))
  (unless (eq np (keytar-%dpad-position keytar))
    (setf (keytar-%dpad-position keytar) np)
    (queue-redraw))
  np)

(defconstant +button-square+ 0)
(defconstant +button-triangle+ 1)
(defconstant +button-circle+ 2)
(defconstant +button-cross+ 3)
(defconstant +button-start+ 4)
(defconstant +button-select+ 5)
(defconstant +button-playstation+ 6)

(defun keytar-button-pressed (keytar index)
  (check-type index (integer 0 6))
  (= 1 (aref (keytar-%buttons keytar) index)))

(defun (setf keytar-button-pressed) (value keytar index)
  (check-type index (integer 0 6))
  (check-type value boolean)
  (let ((value (if value 1 0)))
    (unless (= value (aref (keytar-%buttons keytar) index))
      (setf (aref (keytar-%buttons keytar) index) value)
      (queue-redraw)))
  value)

(defun keytar-key (keytar key-number)
  (check-type key-number (integer 0 24))
  (= 1 (aref (keytar-%depressed keytar) key-number)))

(defun keytar-midi-note (keytar key-number)
  (aref (keytar-midi-notes keytar) key-number))

(defun (setf keytar-key) (value keytar key-number)
  (check-type value boolean)
  (check-type key-number (integer 0 24))
  (let ((value (if value 1 0)))
    (unless (= value (aref (keytar-%depressed keytar) key-number))
      (setf (aref (keytar-%depressed keytar) key-number) value)
      (queue-redraw)))
  value)

(defun keytar-led (keytar led-number)
  (check-type led-number (integer 0 3))
  (= 1 (aref (keytar-%led keytar) led-number)))

(defun (setf keytar-led) (value keytar led-number)
  (check-type value boolean)
  (check-type led-number (integer 0 3))
  (let ((value (if value 1 0)))
    (unless (= value (aref (keytar-%led keytar) led-number))
      (setf (aref (keytar-%led keytar) led-number) value)
      (queue-redraw)))
  value)



(defun red-led (&key on-click)
  (make-basic-led :on-click on-click))

(defun green-led (&key on-click)
  (make-basic-led
   :on-color *basic-led-green-on*
   :off-color *basic-led-green-off*
   :glow-color *basic-led-green-glow*
   :on-click on-click))

(defun basic-led-toggle (basic-led)
  (setf (basic-led-illuminated basic-led) (not (basic-led-illuminated basic-led))))

(defvar *out* *standard-output*)
(defun handle-keytar-event (keytar-event)
  (format *out* "~%Got event ~a" keytar-event)
  (check-type keytar-event keytar-key-event)
  (cl-alsa-midi/midihelper::fifo-push
   cl-alsa-midi/midihelper:*writer-fifo*
   (ecase (keytar-key-event-event keytar-event)
     (:press
      (cl-alsa-midi/midihelper:ev-noteon
       1 (keytar-key-event-note keytar-event)
       60))
     (:release
      (cl-alsa-midi/midihelper:ev-noteoff
       1 (keytar-key-event-note keytar-event)
       0)))))

(defparameter *keytar* (make-keytar :callback 'handle-keytar-event))

(defparameter *gui* *keytar*)

;; 					;(dotimes (x 100)
;; (progn
;;   (setf (slot-value *keytar* 'scale) 1d0)
;;   (format t "~%KEYTAR SCALE IS NOW ~a" (slot-value *keytar* 'scale))
;;   (funcall *gui-state-change-callback*))
  ;; (make-grid
  ;;  :columns 2
  ;;  :elements (list
  ;; 	      *power-led*
  ;; 	      (green-led :on-click 'basic-led-toggle))))

;; Okay, so I'm thinking for elements
;; I should have ROT, ORIGIN, OFFSET, SCALE
;; right inside the element.
;;
;; Then draw should be given width and height of the drawing area (in what units?)
;; Part of my thinking is that for some elements 0 to 1 is not best... and some
;; have aspect ratios.. something like (25 wide,10 high) is better...
;;
;; SO, controlling the scaling is something the element should be doing ITSELF.
;; And being able to specify the rotation, translation and scaling on the GUI element
;; itself will make things simpler for me... well maybe... I can't change things
;; dynamically (e.g. with a C-c) without restarting... can I fix that?
;; Having the tree be "lazy" would fix it... but would be irritating because all
;; state would get dropped...  unless I split out state again.
;;
;; Okay, so one approach would be to name these elements and have a separate
;; list of coordinates... Another is just to C-c the SETFs... that works.
;;
;; Okay, so even if I have this... what exactly does scale mean?  That one works...
;; because it works regardless of current units... and rotation also works...
;; the only one I'm less sure of is translate.   If that one was ALWAYS in terms
;; of 0 to 1, then it works....

;; Okay, so for me

(defmethod draw :before (widget)
  "Apply the widget properties of scale, rotation, etc.
   At the start of a widget call one can expect that X goes from 0 to 1 and Y from 0 to y/x. The offset is
   applied to this."

  (with-slots (translate-x translate-y rotation scale) widget
    (assert (zerop rotation))
    (cl-cairo2:translate translate-x translate-y)

    ;; If scale is .5 we want it to be smaller... 
    (cl-cairo2:scale (/ scale) (/ scale))))

(defgeneric draw (widget)
  (:documentation "Draw the given widget")
  (:method ((k keytar))
    ;; 24 units wide
    ;; 8 units high.
    ;; 0,0 is the top left of the first piano key.
    (with-slots (fill-color stroke-width stroke-color
		 white-key-color white-key-stroke-color white-key-pressed-color
		 black-key-color black-key-stroke-color black-key-pressed-color
		 orange-group-color green-group-color blue-group-color
		 yellow-group-color red-group-color
		 modulation-control-color modulation-control-stroke-color
		 overdrive-button-color overdrive-button-pressed-color overdrive-button-stroke-color
		 led-on-color led-off-color
		 dpad-stroke-color dpad-base-color dpad-highlight-color
		 button-base-color button-stroke-color button-pressed-color
		 green-triangle-color green-triangle-pressed-color
		 red-circle-color red-circle-pressed-color
		 pink-square-color pink-square-pressed-color
		 white-cross-color white-cross-pressed-color
		 ) k
      (cl-cairo2:scale (/ 24d0) (/ 24d0))
      (cl-cairo2:translate 1.1d0 .3d0)

      ;; Draw and outline the body
      (cl-cairo2:move-to -1d0 0d0)
      (cl-cairo2:line-to 16d0 0d0)
      (cl-cairo2:line-to 22.5d0 2d0)
      (cl-cairo2:line-to 22.0d0 3.5d0)
      (cl-cairo2:line-to 18.5d0 2.8d0)
      (cl-cairo2:line-to 15.5d0 6.5d0)
      (cl-cairo2:line-to  0.0d0 7.9d0)
      (cl-cairo2:line-to -1.0d0 7.0d0)
      (cl-cairo2:line-to -1.0d0 0.0d0)
      (cl-cairo2:set-line-width stroke-width)

      (with-slots (r g b a) fill-color
	(cl-cairo2:set-source-rgba r g b a))
      (cl-cairo2:fill-preserve)

      (with-slots (r g b a) stroke-color
	(cl-cairo2:set-source-rgba r g b a))
      (cl-cairo2:stroke)

      ;; Draw the white keys
      (loop :for x :from 0d0 :by 1d0 :below 15d0
	    :for key-number :in '(24 23 21 19 17 16 14 12 11 9 7 5 4 2 0)
	    :for depressed = (keytar-key k key-number)
	    :for fill-color = (if depressed
				  white-key-pressed-color
				  white-key-color)
	    :do (progn
		  (cl-cairo2:move-to x 0d0)
		  (cl-cairo2:rectangle x 0d0 1d0 5.5d0)
		  (cl-cairo2:set-line-width stroke-width)
		  (with-slots (r g b a) fill-color
		    (cl-cairo2:set-source-rgba r g b a))
		  (cl-cairo2:fill-preserve)
		  (with-slots (r g b a) white-key-stroke-color
		    (cl-cairo2:set-source-rgba r g b a))
		  (cl-cairo2:stroke)))

      ;; Draw the black keys
      (loop :for x :from .8d0 :by 1d0 :below 15d0
	    :for key-number :in '(nil 22 20 18 nil 15 13 nil 10 8 6 nil 3 1)
	    :for depressed = (and key-number (keytar-key k key-number))
	    :for fill-color = (if depressed
				  black-key-pressed-color
				  black-key-color)
	    :when key-number
	      :do (progn
		    (cl-cairo2:move-to x 2d0)
		    (cl-cairo2:rectangle x 2d0 .4d0 3.5d0)
		    (cl-cairo2:set-line-width stroke-width)
		    (with-slots (r g b a) fill-color
		      (cl-cairo2:set-source-rgba r g b a))
		    (cl-cairo2:fill-preserve)
		    (with-slots (r g b a) black-key-stroke-color
		      (cl-cairo2:set-source-rgba r g b a))
		    (cl-cairo2:stroke)))

      ;; Draw the colored bands...
      (cl-cairo2:move-to 0d0 5.5d0)
      (cl-cairo2:rectangle 0d0 5.5d0 1d0 .1d0)
      (with-slots (r g b a) orange-group-color
	(cl-cairo2:set-source-rgba r g b a))
      (cl-cairo2:fill-path)
      (cl-cairo2:move-to 1d0 5.5d0)
      (cl-cairo2:rectangle 1d0 5.5d0 4d0 .1d0)
      (with-slots (r g b a) green-group-color
	(cl-cairo2:set-source-rgba r g b a))
      (cl-cairo2:fill-path)
      (cl-cairo2:move-to 5d0 5.5d0)
      (cl-cairo2:rectangle 5d0 5.5d0 3d0 .1d0)
      (with-slots (r g b a) blue-group-color
	(cl-cairo2:set-source-rgba r g b a))
      (cl-cairo2:fill-path)
      (cl-cairo2:move-to 8d0 5.5d0)
      (cl-cairo2:rectangle 8d0 5.5d0 4d0 .1d0)
      (with-slots (r g b a) yellow-group-color
	(cl-cairo2:set-source-rgba r g b a))
      (cl-cairo2:fill-path)
      (cl-cairo2:move-to 12d0 5.5d0)
      (cl-cairo2:rectangle 12d0 5.5d0 3d0 .1d0)
      (with-slots (r g b a) red-group-color
	(cl-cairo2:set-source-rgba r g b a))
      (cl-cairo2:fill-path)

      ;; Modulation control
      ;; Draw and outline the controller
      (cl-cairo2:move-to 20.75d0 2d0)
      (cl-cairo2:line-to 22.1d0 2.4d0)
      (cl-cairo2:line-to 21.87d0 3.1d0)
      (cl-cairo2:line-to 20.45d0 2.7d0)
      (cl-cairo2:close-path)

      (cl-cairo2:set-line-width stroke-width)
      (with-slots (r g b a) modulation-control-color
	(cl-cairo2:set-source-rgba r g b a))
      (cl-cairo2:fill-preserve)
      (with-slots (r g b a) modulation-control-stroke-color
	(cl-cairo2:set-source-rgba r g b a))
      (cl-cairo2:stroke)

      ;; Overdrive Button
      ;; Draw and outline the button
      (cl-cairo2:move-to 20.4d0 1.9d0)
      (cl-cairo2:line-to 20.1d0 2.6d0)
      (cl-cairo2:line-to 19.2d0 2.3d0)
      (cl-cairo2:line-to 19.3d0 2.0d0)
      (cl-cairo2:close-path)

      (cl-cairo2:set-line-width stroke-width)
      (with-slots (r g b a) overdrive-button-color
	(cl-cairo2:set-source-rgba r g b a))
      (cl-cairo2:fill-preserve)
      (with-slots (r g b a) overdrive-button-stroke-color
	(cl-cairo2:set-source-rgba r g b a))
      (cl-cairo2:stroke)

      ;; Draw the 4 leds...
      (loop :with y = 6.1d0
	    :for i :from 0 :below 4
	    :for x :from 2.8d0 :by .15d0
	    :for color = (if (keytar-led k i)
			     led-on-color
			     led-off-color)
	    :do (progn
	          ;;(cl-cairo2:move-to 3d0 1.9d0)
		  (cl-cairo2:arc x y 0.05d0 0.0 (* 2 pi))
		  (with-slots (r g b a) color
		    (cl-cairo2:set-source-rgba r g b a))
		  (cl-cairo2:fill-path)))

      ;; Draw the dpad
      (let ((x 4.95d0)
	    (y 6.5d0))
	(cl-cairo2:arc x y 0.6d0 0.0 (* 2 pi))
	(with-slots (r g b a) dpad-base-color
	  (cl-cairo2:set-source-rgba r g b a))
	(cl-cairo2:fill-preserve)
	(with-slots (r g b a) dpad-stroke-color
	  (cl-cairo2:set-source-rgba r g b a))
	(cl-cairo2:stroke)

	;; Draw the arc lines... We're going to do dark ones on left, lighter ones
	;; on right.  But they're positions shift depending on dpad position.
	;; Cairo can't do a perspective warp, so I gotta do it by hand probably by
	;; just shifting.
	(case (keytar-dpad-position k)
	  (:up (decf y .1d0))
	  (:down (incf y .1d0))
	  (:left (decf x .1d0))
	  (:right (incf x .1d0)))

	(let ((r .5d0)
	      (shift-x .1d0)
	      (shift-y .1d0))

	  ;; top left
	  (cl-cairo2:arc (- (- x shift-x) r)
			 (- (- y shift-y) r)
			 r (* 1/10 pi) (* 4/10 pi))
	  (with-slots (r g b a) dpad-stroke-color
	    (cl-cairo2:set-source-rgba r g b a))
	  (cl-cairo2:stroke)

	  ;; top right
	  (cl-cairo2:arc (+ (+ x shift-x) r)
			 (- (- y shift-y) r)
			 r (* 6/10 pi) (* 9/10 pi))
	  (with-slots (r g b a) dpad-highlight-color
	    (cl-cairo2:set-source-rgba r g b a))
	  (cl-cairo2:stroke)

	  ;; bottom right
	  (cl-cairo2:arc (+ (+ x shift-x) r)
			 (+ (+ y shift-y) r)
			 r (* 11/10 pi) (* 14/10 pi))
	  (with-slots (r g b a) dpad-highlight-color
	    (cl-cairo2:set-source-rgba r g b a))
	  (cl-cairo2:stroke)

          ;; bottom left
	  (cl-cairo2:arc (- (- x shift-x) r)
			 (+ (+ y shift-y) r)
			 r (* 16/10 pi) (* 19/10 pi))
	  (with-slots (r g b a) dpad-stroke-color
	    (cl-cairo2:set-source-rgba r g b a))
	  (cl-cairo2:stroke)))

      ;; Draw the triangle, circle, cross and square buttons
      (flet ((button (x y i)
	       (cl-cairo2:arc x y 0.2d0 0.0 (* 2 pi))
	       (with-slots (r g b a) (if (keytar-button-pressed k i)
					 button-pressed-color
					 button-base-color)
		 (cl-cairo2:set-source-rgba r g b a))
	       (cl-cairo2:fill-preserve)
	       (with-slots (r g b a) button-stroke-color
		 (cl-cairo2:set-source-rgba r g b a))
	       (cl-cairo2:stroke)))
	(button 0.98d0 6.1d0 +button-cross+) ;; 4. White Cross
	(button 0.4d0 6.6d0 +button-circle+)  ;; 3. Red Circle
	(button 1.5d0 6.6d0 +button-square+)  ;; 1. Pink Square
	(button 0.98d0 7.1d0 +button-triangle+)) ;; 2. Green Triangle

      (cl-cairo2:set-line-width (/ stroke-width 2))

      ;; 1. Pink Square
      (with-slots (r g b a) (if (keytar-button-pressed k 0)
				pink-square-pressed-color
				pink-square-color)
	(cl-cairo2:set-source-rgba r g b a))
      (cl-cairo2:rectangle 1.4d0 6.5d0 .2d0 .2d0)
      (cl-cairo2:stroke)

      ;; 2. Green Triagle
      (with-slots (r g b a) (if (keytar-button-pressed k 1)
				green-triangle-pressed-color
				green-triangle-color)
	(cl-cairo2:set-source-rgba r g b a))
      (cl-cairo2:move-to 0.98d0 7.18d0)
      (cl-cairo2:line-to 1.08d0 7.00d0)
      (cl-cairo2:line-to 0.88d0 7.00d0)
      (cl-cairo2:close-path)
      (cl-cairo2:stroke)
      
      ;; 3. Red Circle
      (with-slots (r g b a) (if (keytar-button-pressed k 2)
				red-circle-pressed-color
				red-circle-color)
	(cl-cairo2:set-source-rgba r g b a))
      (cl-cairo2:arc 0.4d0 6.6d0 0.12d0 0.0d0 (* 2 pi))
      (cl-cairo2:stroke)

      ;; 4. White Cross
      (with-slots (r g b a) (if (keytar-button-pressed k 3)
				white-cross-pressed-color
				white-cross-color)
	(cl-cairo2:set-source-rgba r g b a))
      (cl-cairo2:move-to 1.08d0 6.0d0)
      (cl-cairo2:line-to 0.88d0 6.2d0)
      (cl-cairo2:stroke)
      (cl-cairo2:move-to 0.88d0 6.0d0)
      (cl-cairo2:line-to 1.08d0 6.2d0)
      (cl-cairo2:stroke)

      ;; Start and select buttons.
      (flet ((button (x y i)
	       (cl-cairo2:arc x y 0.16d0 0.0 (* 2 pi))
	       (with-slots (r g b a) (if (keytar-button-pressed k i)
					 button-pressed-color
					 button-base-color)
		 (cl-cairo2:set-source-rgba r g b a))
	       (cl-cairo2:fill-preserve)
	       (with-slots (r g b a) button-stroke-color
		 (cl-cairo2:set-source-rgba r g b a))
	       (cl-cairo2:stroke)))
	(button 2.3d0 6.6d0 +button-start+)
	(button 3.7d0 6.6d0 +button-select+)
      )))
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
    (with-slots (on-color off-color glow-color stroke-width stroke-color (illuminated %illuminated)) element
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
   the elements range and from 0.0 to 1.0.")
  (:method ((k keytar) x y)
    ;; What's clickable?  The white and black keys, the  sensor, overdrive button, dpad, etc.

    ;; At the start, x goes from 0 to 1 and y from 0 to y/x.
    ;; Scale them up, and translate them.
    (let ((mx (- (* x 24d0) 1.1d0))
	  (my (- (* y 24d0) 0.3d0)))

      ;; Black key presses
      (loop :for x :from .8d0 :by 1d0 :below 15d0
	    :for key-number :in '(nil 22 20 18 nil 15 13 nil 10 8 6 nil 3 1)
	    :when (and key-number
		       (<= x mx (+ x .4d0))
		       (<= 2d0 my (+ 2d0 3.5d0)))
	      :do (setf (keytar-key k key-number) (not (keytar-key k key-number)))
	      :and :do (when (keytar-callback k)
			 (funcall (keytar-callback k)
				  (make-keytar-key-event
				   :note (keytar-midi-note k key-number)
				   :event (if (keytar-key k key-number)
					      :press
					      :release))))
	      :and :do (return-from click))

      ;; White key presses
      (loop :for x :from 0d0 :by 1d0 :below 15d0
	    :for key-number :in '(24 23 21 19 17 16 14 12 11 9 7 5 4 2 0)
	    :when (and (<= x mx (+ x 1d0))
		       (<= 0d0 my 5.5d0))
	      :do (setf (keytar-key k key-number) (not (keytar-key k key-number)))
	      :and :do (when (keytar-callback k)
			 (funcall (keytar-callback k)
				  (make-keytar-key-event
				   :note (keytar-midi-note k key-number)
				   :event (if (keytar-key k key-number)
					      :press
					      :release))))
	      :and :do (return-from click))

      ;; Dpad directions
      (let ((x 4.95d0)
	    (y 6.5d0)
	    (r .6d0))
	(let ((clicked-position
		(cond ((and (<= (- x r) mx (- x (/ r 6d0)))
			    (<= (- y (/ r 2d0)) my (+ y (/ r 2d0))))
		       :left)
		      ((and (<= (+ x (/ r 6d0)) mx (+ x r))
			    (<= (- y (/ r 2d0)) my (+ y (/ r 2d0))))
		       :right)
		      ((and (<= (- y r) my (- y (/ r 6d0)))
			    (<= (- x (/ r 2d0)) mx (+ x (/ r 2d0))))
		       :up)
		      ((and (<= (+ y (/ r 6d0)) my (+ y r))
			    (<= (- x (/ r 2d0)) mx (+ x (/ r 2d0))))
		       :down))))
	  (when clicked-position
	    (setf (keytar-dpad-position k) (if (eq (keytar-dpad-position k) clicked-position) nil clicked-position))
	    (setf (keytar-led k (random 4)) (zerop (random 2)))
	    (return-from click))))

      (flet ((button (x y i)
	       (when (< (sqrt (+ (expt (- x mx) 2)
				 (expt (- y my) 2))) 0.2d0)
		 (setf (keytar-button-pressed k i)
		       (not (keytar-button-pressed k i)))
		 (return-from click))))
	(button 0.98d0 6.1d0 +button-cross+) ;; 4. White Cross
	(button 0.4d0 6.6d0 +button-circle+)  ;; 3. Red Circle
	(button 1.5d0 6.6d0 +button-square+)  ;; 1. Pink Square
	(button 0.98d0 7.1d0 +button-triangle+))

      (flet ((button (x y i)
	       (when (< (sqrt (+ (expt (- x mx) 2)
				 (expt (- y my) 2))) 0.16d0)
		 (setf (keytar-button-pressed k i)
		       (not (keytar-button-pressed k i)))
		 (return-from click))))
	(button 2.3d0 6.6d0 +button-start+)
	(button 3.7d0 6.6d0 +button-select+))
      ))
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
  "Clears the background color and sets up a basic coordinate of (0,0)
   to (1,y/x) for the full drawable area."
  (let ((cl-cairo2:*context* cairo))
    ;; Background
    (cl-cairo2:rectangle 0 0 width height)
    (cl-cairo2:set-source-rgb 0.2 0.2 0.5)
    (cl-cairo2:fill-path)

    ;; Scale so drawing is 0 to 1 in the X axis and 0 to y/x in the Y axis.
    (cl-cairo2:scale width width)
    (draw *gui*)))

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

  (setf x (coerce (/ x width) 'double-float))
  (setf y (coerce (/ y width) 'double-float))
  ;; (format t "~% -> click ~a ~a" x y)
  (click *gui* x y))

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
  (let (;; (gtk-drawing-area (gir:build-object-ptr (gir:nget-desc *gtk* "DrawingArea") gtk-drawing-area))
	(report-error t)
	(cairo (make-instance 'cl-cairo2:context
			      :pixel-based-p t
			      :height height
			      :width width
			      :pointer cairo)))
    (handler-case (prog1 (draw-gui cairo width height)
		    (setf report-error t))
      (error (e)
	(when report-error
	  (warn (format nil "~In draw-thing callback, got error: ~a" e)))
	(setf report-error nil))))
  0)

(cffi:defcallback cleanup-draw-thing :void ((user-data :pointer))
  (declare (ignore user-data))
  (format t "~%Inside my cleanup-draw-thing callback!")
  0)

(declaim (type fixnum *gui-height* *gui-width*))
(defparameter *gui-height* 600)
(defparameter *gui-width* 800)

;; Why is this so unstable?  Even recompiling in Lisp causes it to crash.
;; that has NOTHING to do with the drawing code.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *gio* (gir:require-namespace "Gio"))
(defvar *gtk* (gir:require-namespace "Gtk" "4.0")) ;; GTK 4.0
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
		   (setf *gui-redraw-callback* nil)
		   (music-engine-shutdown *music-engine*)
		   (format t "~%Shutdown complete!")))
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
				    (let ((report-error t))
				      (handler-case (prog1 (click-gui x y *gui-width* *gui-height*)
						      (setf report-error t))
					(error (e)
					  (when report-error
					    (warn (format nil "In click handler got error ~a" e)))
					  (setf report-error nil))))))
		     (setf (gir:property gesture-click "button") 1)
		     (gir:invoke (drawing-area 'add-controller) gesture-click)

		     (setf *gui-redraw-callback* #'(lambda ()
						     (gir:invoke (drawing-area 'queue-draw))))

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
  (format t "~%Starting the GUI. The way GTK works though, once~%this window is quit you can't restart it without quitting lisp.~%By this I mean, calling run on a previously quit window is undefined behavior.")
  (sb-thread:make-thread #'create-gui :name "gui thread"))

(sb-posix:setenv "GDK_SYNCHRONIZE" "1" 1)
