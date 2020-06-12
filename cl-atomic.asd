(defsystem "cl-atomic"
  :name "cl-atomic"
  :description "Atomic operations for Common Lisp"
  :version "0.0.1"
  :author "Muyinliu Xing <muyinliu@gmail.com>"
  :license "MIT"
  :in-order-to ((test-op (test-op "cl-atomic-test")))
  :serial t
  :components ((:module
                :src
                :serial t
                :components ((:file "packages")
                             (:file "utils")
                             (:file "atomic-fixnum")
                             (:file "atomic-boolean")))))
