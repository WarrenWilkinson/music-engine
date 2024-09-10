(in-package #:cl-pipewire.ffi)
(pkg-config-cflags "libpipewire-0.3")
(include "pipewire/pipewire.h")

(defwrapper ("pw_get_headers_version" pw-get-headers-version) :string); :void)
