(defsystem "cl-atomic-test"
  :name "cl-atomic"
  :description "Test cases for cl-atomic"
  :version "0.0.1"
  :author "Muyinliu Xing <muyinliu@gmail.com>"
  :license "MIT"
  :depends-on ("cl-atomic" "prove" "bordeaux-threads")
  :defsystem-depends-on ("prove-asdf")
  :components ((:module
                :test
                :serial t
                :components ((:file "packages")
                             (:file "atomic-fixnum-test")
                             (:file "atomic-boolean-test"))))
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
