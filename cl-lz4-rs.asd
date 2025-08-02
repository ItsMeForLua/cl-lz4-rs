(defsystem #:cl-lz4-rs
  :description "Fast LZ4 compression/decompression for Common Lisp using Rust FFI"
  :author "Andrew D. France andrewforlua@gmail.com"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/ItsMeForLua/cl-lz4-rs"
  :bug-tracker "https://github.com/ItsMeForLua/cl-lz4-rs/issues"
  :source-control (:git "https://github.com/ItsMeForLua/cl-lz4-rs.git")
  :depends-on (#:cffi)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "build" :depends-on ("package"))
                             (:file "ffi" :depends-on ("package" "build")))))
  :encoding :utf-8)
