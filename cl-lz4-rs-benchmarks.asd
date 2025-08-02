(defsystem #:cl-lz4-rs-benchmarks
  :description "Benchmarks for cl-lz4-rs"
  :author "Andrew D. France andrewforlua@gmail.com"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:cl-lz4-rs #:trivial-benchmark #:salza2 #:chipz)
  :components ((:module "benchmarks"
                :components ((:file "main")))))
