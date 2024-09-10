(defsystem "cl-pipewire"
  :description "Interface to pipewire audio system"
  :version "0.0.1"
  :author "Warren Wilkinson <warrenwilkinson@gmail.com>"
  :licence "GPLv3"
  :defsystem-depends-on (:cffi/c2ffi)
  :depends-on (:alexandria
               :cffi
               :cffi/c2ffi
               :cffi-libffi)
  :in-order-to ((test-op (test-op "cl-pipewire/test")))
  :components ((:module "src"
                :depends-on ("c2ffi-spec")
                :serial t
                :components ((:file "cl-pipewire")))
               (:module "c2ffi-spec"
                :components ((:cffi/c2ffi-file "pipewire/pipewire.h"
                              :package "CL-PIPEWIRE.FFI"
			      ;; You'll need to change this directory if you rebuild the spec files.
			      :sys-include-paths ("/home/warren/shared/wfiles/organization/projects/music/software/modules/cl-pipewire/c2ffi-spec/")
                              :foreign-library-name "cl-pipewire.ffi::pipewire"
                              :foreign-library-spec ((t (:default "libpipewire-0.3")))
                              :include-sources ("pipewire/pipewire\\.h$"
						"pipewire/array\\.h$"
						"pipewire/buffers\\.h$"
						"pipewire/client\\.h$"
						"pipewire/conf\\.h$"
						"pipewire/context\\.h$"
						"pipewire/control\\.h$"
						"pipewire/core\\.h$"
						"pipewire/data-loop\\.h$"
						"pipewire/device\\.h$"
						"pipewire/factory\\.h$"
						"pipewire/filter\\.h$"
						"pipewire/global\\.h$"
						"pipewire/i18n\\.h$"
						"pipewire/impl-client\\.h$"
						"pipewire/impl-core\\.h$"
						"pipewire/impl-device\\.h$"
						"pipewire/impl-factory\\.h$"
						"pipewire/impl\\.h$"
						"pipewire/impl-link\\.h$"
						"pipewire/impl-metadata\\.h$"
						"pipewire/impl-module\\.h$"
						"pipewire/impl-node\\.h$"
						"pipewire/impl-port\\.h$"
						"pipewire/keys\\.h$"
						"pipewire/link\\.h$"
						"pipewire/log\\.h$"
						"pipewire/loop\\.h$"
						"pipewire/main-loop\\.h$"
						"pipewire/map\\.h$"
						"pipewire/mem\\.h$"
						"pipewire/module\\.h$"
						"pipewire/node\\.h$"
						"pipewire/permission\\.h$"
						"pipewire/pipewire\\.h$"
						"pipewire/pipewire\\.h.~1~$"
						"pipewire/port\\.h$"
						"pipewire/private\\.h$"
						"pipewire/properties\\.h$"
						"pipewire/protocol\\.h$"
						"pipewire/proxy\\.h$"
						"pipewire/resource\\.h$"
						"pipewire/stream\\.h$"
						"pipewire/thread\\.h$"
						"pipewire/thread-loop\\.h$"
						"pipewire/type\\.h$"
						"pipewire/utils\\.h$"
						"pipewire/work-queue\\.h$"
						"pipewire/version\\.h$")
                              :exclude-sources :all
                              ;; :include-definitions ()
                              ;; :exclude-definitions ()
			      )))))

(defsystem "cl-pipewire/test"
  ;; :defsystem-depends-on (:hu.dwim.asdf)
  ;; :class "hu.dwim.asdf:hu.dwim.test-system"
  :depends-on ("cl-pipewire"
               ;;:hu.dwim.zlib
               ;; you probably also want to load :hu.dwim.stefil+swank one way or another
               )
  ;; Unfortunately ASDF swallows the return value (i.e. it cannot be
  ;; inspected in Slime), so we at least print it.
  ;; :perform (test-op (o c) (print (funcall (intern (string '#:test)
  ;;                                                 (find-package :hu.dwim.zlib/test)))))
  :components ((:module "test"
                :components ((:file "package")
                             (:file "suite" :depends-on ("package"))
                             (:file "zlib" :depends-on ("suite"))
                             (:file "random" :depends-on ("suite"))))))
