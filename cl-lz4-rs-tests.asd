(defsystem #:cl-lz4-rs-tests
  :description "Tests for cl-lz4-rs"
  :author "Andrew D. France andrewforlua@gmail.com"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:cl-lz4-rs #:fiveam)
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op c)
             (symbol-call '#:cl-lz4-rs-tests '#:run-tests)))
