(defpackage #:cl-lz4-rs/build
  (:use #:cl)
  (:export #:build-native-library
           #:ensure-native-library
           #:get-library-path
           #:clean-build-artifacts
           #:library-exists-p
           #:library-needs-rebuild-p
           #:build-status
           #:rust-installed-p))

(defpackage #:cl-lz4-rs
  (:use #:cl)
  (:local-nicknames (#:build #:cl-lz4-rs/build))
  (:export 
   #:compress
   #:decompress
   
   #:test-compression
   #:library-info
   
   #:lz4-compress-bound
   #:lz4-compress-into
   #:lz4-decompress-into
   
   #:ub8-vector
   
   #:build-native-library
   #:clean-build-artifacts
   #:build-status))