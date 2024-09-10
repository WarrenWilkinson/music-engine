(defsystem "music-engine"
  :description "music-engine: prototype instrument controller"
  :version "0.0.1"
  :author "Warren Wilkinson <warrenwilkinson@gmail.com>"
  :licence "GPLv3"
  :depends-on ("mcclim" "osc"))
  ;; :version (:read-file-form "variables" :at (3 2))
  ;; :components ((:file "package")
  ;; 	       (:file "variables" :depends-on ("package"))
  ;; 	       (:module "mod"
  ;; 		:depends-on ("package")
  ;; 		:serial t
  ;; 		:components ((:file "utils")
  ;; 			     (:file "reader")
  ;; 			     (:file "cooker")
  ;; 			     (:static-file "data.raw"))
  ;; 		:output-files (compile-op (o c) (list "data.cooked"))
  ;; 		:perform (compile-op :after (o c)
  ;; 				     (cook-data
  ;; 				      :in (component-pathname (find-component c "data.raw"))
  ;; 				      :out (first (output-files o c)))))
  ;; 	       (:file "foo" :depends-on ("mod"))))
