(defsystem "cl-atomic-benchmark"
  :name "cl-atomic"
  :description "Benchmark for cl-atomic"
  :version "0.0.1"
  :author "Muyinliu Xing <muyinliu@gmail.com>"
  :license "MIT"
  :depends-on ("cl-atomic" "prove")
  :defsystem-depends-on ("prove-asdf")
  :components ((:module
                :benchmark
                :serial t
                :components ((:file "packages")
                             (:file "atomic-fixnum-benchmark"))))
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
