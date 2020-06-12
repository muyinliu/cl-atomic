(in-package :atomic-test)

(plan nil)

(subtest "Testing atomic-fixnum"
  (subtest "Testing default value of atomic-fixnum"
    (let ((atomic-fixnum (atomic:make-atomic-fixnum)))
      (is (atomic:atomic-fixnum-value atomic-fixnum) 0))
    (let ((atomic-fixnum (atomic:make-atomic-fixnum :value 3)))
      (is (atomic:atomic-fixnum-value atomic-fixnum) 3)))
  (subtest "Testing (atomic:atomic-fixnum-incf atomic-fixnum)"
    (subtest "Testing (atomic:atomic-fixnum-incf atomic-fixnum) return old value"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum)))
        (is (atomic:atomic-fixnum-incf atomic-fixnum) 0)
        (is (atomic:atomic-fixnum-value atomic-fixnum) 1)))
    (subtest "Testing (atomic:atomic-fixnum-incf atomic-fixnum) range"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum :value most-positive-fixnum)))
        (is (atomic:atomic-fixnum-incf atomic-fixnum) most-positive-fixnum)
        (is (atomic:atomic-fixnum-value atomic-fixnum) most-negative-fixnum)))
    (subtest "Testing (atomic:atomic-fixnum-incf atomic-fixnum) with multiple threads"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum))
            (thread-count 4)
            (iterations 1000000))
        (loop for thread in (loop for i from 0 below thread-count
                               collect
                                 (let ((thread-index i))
                                   (bt:make-thread
                                    (lambda ()
                                      (dotimes (i iterations)
                                        (atomic:atomic-fixnum-incf atomic-fixnum)))
                                    :name (format nil "atomic-fixnum-incf test thread ~A"
                                                  thread-index))))
           do (bt:join-thread thread))
        (is (atomic:atomic-fixnum-value atomic-fixnum)
            (* thread-count iterations)))))
  (subtest "Testing (atomic:atomic-fixnum-incf atomic-fixnum diff)"
    (subtest "Testing (atomic:atomic-fixnum-incf atomic-fixnum diff) return old value"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum))
            (diff 2))
        (is (atomic:atomic-fixnum-incf atomic-fixnum diff) 0)
        (is (atomic:atomic-fixnum-value atomic-fixnum) 2)))
    (subtest "Testing (atomic:atomic-fixnum-incf atomic-fixnum diff) range"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum :value (1- most-positive-fixnum)))
            (diff 2))
        (is (atomic:atomic-fixnum-incf atomic-fixnum diff) (1- most-positive-fixnum))
        (is (atomic:atomic-fixnum-value atomic-fixnum) most-negative-fixnum)))
    (subtest "Testing (atomic:atomic-fixnum-incf atomic-fixnum diff) with multiple threads"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum))
            (diff 2)
            (thread-count 4)
            (iterations 1000000))
        (loop for thread in (loop for i from 0 below thread-count
                               collect
                                 (let ((thread-index i))
                                   (bt:make-thread
                                    (lambda ()
                                      (dotimes (i iterations)
                                        (atomic:atomic-fixnum-incf atomic-fixnum diff)))
                                    :name (format nil "atomic-fixnum-incf test thread ~A"
                                                  thread-index))))
           do (bt:join-thread thread))
        (is (atomic:atomic-fixnum-value atomic-fixnum)
            (* thread-count iterations 2)))))
  (subtest "Testing (atomic:atomic-fixnum-incf-and-get atomic-fixnum)"
    (subtest "Testing (atomic:atomic-fixnum-incf-and-get atomic-fixnum) return new value"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum)))
        (is (atomic:atomic-fixnum-incf-and-get atomic-fixnum) 1)
        (is (atomic:atomic-fixnum-value atomic-fixnum) 1)))
    (subtest "Testing (atomic:atomic-fixnum-incf-and-get atomic-fixnum) range"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum :value most-positive-fixnum)))
        (is (atomic:atomic-fixnum-incf-and-get atomic-fixnum) most-negative-fixnum)
        (is (atomic:atomic-fixnum-value atomic-fixnum) most-negative-fixnum)))
    (subtest "Testing (atomic:atomic-fixnum-incf-and-get atomic-fixnum) with multiple threads"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum))
            (thread-count 4)
            (iterations 1000000))
        (loop for thread in (loop for i from 0 below thread-count
                               collect
                                 (let ((thread-index i))
                                   (bt:make-thread
                                    (lambda ()
                                      (dotimes (i iterations)
                                        (atomic:atomic-fixnum-incf-and-get atomic-fixnum)))
                                    :name (format nil "atomic-fixnum-incf-and-get test thread ~A"
                                                  thread-index))))
           do (bt:join-thread thread))
        (is (atomic:atomic-fixnum-value atomic-fixnum)
            (* thread-count iterations)))))
  (subtest "Testing (atomic:atomic-fixnum-incf-and-get atomic-fixnum diff)"
    (subtest "Testing (atomic:atomic-fixnum-incf-and-get atomic-fixnum diff) return new value"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum))
            (diff 2))
        (is (atomic:atomic-fixnum-incf-and-get atomic-fixnum diff) 2)
        (is (atomic:atomic-fixnum-value atomic-fixnum) 2)))
    (subtest "Testing (atomic:atomic-fixnum-incf-and-get atomic-fixnum diff) range"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum :value (1- most-positive-fixnum)))
            (diff 2))
        (is (atomic:atomic-fixnum-incf-and-get atomic-fixnum diff) most-negative-fixnum)
        (is (atomic:atomic-fixnum-value atomic-fixnum) most-negative-fixnum)))
    (subtest "Testing (atomic:atomic-fixnum-incf-and-get atomic-fixnum diff) with multiple threads"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum))
            (diff 2)
            (thread-count 4)
            (iterations 1000000))
        (loop for thread in (loop for i from 0 below thread-count
                               collect
                                 (let ((thread-index i))
                                   (bt:make-thread
                                    (lambda ()
                                      (dotimes (i iterations)
                                        (atomic:atomic-fixnum-incf-and-get atomic-fixnum diff)))
                                    :name (format nil "atomic-fixnum-incf-and-get test thread ~A"
                                                  thread-index))))
           do (bt:join-thread thread))
        (is (atomic:atomic-fixnum-value atomic-fixnum)
            (* thread-count iterations 2)))))
  (subtest "Testing (atomic:atomic-fixnum-decf atomic-fixnum)"
    (subtest "Testing (atomic:atomic-fixnum-decf atomic-fixnum) return old value"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum)))
        (is (atomic:atomic-fixnum-decf atomic-fixnum) 0)
        (is (atomic:atomic-fixnum-value atomic-fixnum) -1)))
    (subtest "Testing (atomic:atomic-fixnum-decf atomic-fixnum) range"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum :value most-negative-fixnum)))
        (is (atomic:atomic-fixnum-decf atomic-fixnum) most-negative-fixnum)
        (is (atomic:atomic-fixnum-value atomic-fixnum) most-positive-fixnum)))
    (subtest "Testing (atomic:atomic-fixnum-decf atomic-fixnum) with multiple threads"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum))
            (thread-count 4)
            (iterations 1000000))
        (loop for thread in (loop for i from 0 below thread-count
                               collect
                                 (let ((thread-index i))
                                   (bt:make-thread
                                    (lambda ()
                                      (dotimes (i iterations)
                                        (atomic:atomic-fixnum-decf atomic-fixnum)))
                                    :name (format nil "atomic-fixnum-decf test thread ~A"
                                                  thread-index))))
           do (bt:join-thread thread))
        (is (atomic:atomic-fixnum-value atomic-fixnum)
            (- (* thread-count iterations))))))
  (subtest "Testing (atomic:atomic-fixnum-decf atomic-fixnum diff)"
    (subtest "Testing (atomic:atomic-fixnum-decf atomic-fixnum diff) return old value"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum))
            (diff 2))
        (is (atomic:atomic-fixnum-decf atomic-fixnum diff) 0)
        (is (atomic:atomic-fixnum-value atomic-fixnum) -2)))
    (subtest "Testing (atomic:atomic-fixnum-decf atomic-fixnum diff) range"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum :value (1+ most-negative-fixnum)))
            (diff 2))
        (is (atomic:atomic-fixnum-decf atomic-fixnum diff) (1+ most-negative-fixnum))
        (is (atomic:atomic-fixnum-value atomic-fixnum) most-positive-fixnum)))
    (subtest "Testing (atomic:atomic-fixnum-decf atomic-fixnum diff) with multiple threads"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum))
            (diff 2)
            (thread-count 4)
            (iterations 1000000))
        (loop for thread in (loop for i from 0 below thread-count
                               collect
                                 (let ((thread-index i))
                                   (bt:make-thread
                                    (lambda ()
                                      (dotimes (i iterations)
                                        (atomic:atomic-fixnum-decf atomic-fixnum diff)))
                                    :name (format nil "atomic-fixnum-decf test thread ~A"
                                                  thread-index))))
           do (bt:join-thread thread))
        (is (atomic:atomic-fixnum-value atomic-fixnum)
            (- (* thread-count iterations 2))))))
  (subtest "Testing (atomic:atomic-fixnum-decf-and-get atomic-fixnum)"
    (subtest "Testing (atomic:atomic-fixnum-decf-and-get atomic-fixnum) return new value"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum)))
        (is (atomic:atomic-fixnum-decf-and-get atomic-fixnum) -1)
        (is (atomic:atomic-fixnum-value atomic-fixnum) -1)))
    (subtest "Testing (atomic:atomic-fixnum-decf-and-get atomic-fixnum) range"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum :value most-negative-fixnum)))
        (is (atomic:atomic-fixnum-decf-and-get atomic-fixnum) most-positive-fixnum)
        (is (atomic:atomic-fixnum-value atomic-fixnum) most-positive-fixnum)))
    (subtest "Testing (atomic:atomic-fixnum-decf-and-get atomic-fixnum) with multiple threads"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum))
            (thread-count 4)
            (iterations 1000000))
        (loop for thread in (loop for i from 0 below thread-count
                               collect
                                 (let ((thread-index i))
                                   (bt:make-thread
                                    (lambda ()
                                      (dotimes (i iterations)
                                        (atomic:atomic-fixnum-decf-and-get atomic-fixnum)))
                                    :name (format nil "atomic-fixnum-decf-and-get test thread ~A"
                                                  thread-index))))
           do (bt:join-thread thread))
        (is (atomic:atomic-fixnum-value atomic-fixnum)
            (- (* thread-count iterations))))))
  (subtest "Testing (atomic:atomic-fixnum-decf-and-get atomic-fixnum diff)"
    (subtest "Testing (atomic:atomic-fixnum-decf-and-get atomic-fixnum diff) return new value"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum))
            (diff 2))
        (is (atomic:atomic-fixnum-decf-and-get atomic-fixnum diff) -2)
        (is (atomic:atomic-fixnum-value atomic-fixnum) -2)))
    (subtest "Testing (atomic:atomic-fixnum-decf-and-get atomic-fixnum diff) range"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum :value (1+ most-negative-fixnum)))
            (diff 2))
        (is (atomic:atomic-fixnum-decf-and-get atomic-fixnum diff) most-positive-fixnum)
        (is (atomic:atomic-fixnum-value atomic-fixnum) most-positive-fixnum)))
    (subtest "Testing (atomic:atomic-fixnum-decf-and-get atomic-fixnum diff) with multiple threads"
      (let ((atomic-fixnum (atomic:make-atomic-fixnum))
            (diff 2)
            (thread-count 4)
            (iterations 1000000))
        (loop for thread in (loop for i from 0 below thread-count
                               collect
                                 (let ((thread-index i))
                                   (bt:make-thread
                                    (lambda ()
                                      (dotimes (i iterations)
                                        (atomic:atomic-fixnum-decf-and-get atomic-fixnum diff)))
                                    :name (format nil "atomic-fixnum-decf-and-get test thread ~A"
                                                  thread-index))))
           do (bt:join-thread thread))
        (is (atomic:atomic-fixnum-value atomic-fixnum)
            (- (* thread-count iterations 2))))))
  (subtest "Testing (atomic:atomic-fixnum-compare-and-swap atomic-fixnum old new)"
    (let ((atomic-fixnum (atomic:make-atomic-fixnum)))
      (is (atomic:atomic-fixnum-compare-and-swap atomic-fixnum 0 1) t)
      (is (atomic:atomic-fixnum-value atomic-fixnum) 1)
      (is (atomic:atomic-fixnum-compare-and-swap atomic-fixnum 0 2) nil)
      (is (atomic:atomic-fixnum-value atomic-fixnum) 1)
      (is (atomic:atomic-fixnum-compare-and-swap atomic-fixnum 1 2) t)
      (is (atomic:atomic-fixnum-value atomic-fixnum) 2)))
  (subtest "Testing (atomic:atomic-fixnum-compare-and-swap atomic-fixnum old new) with multiple threads"
    (flet ((atomic-fixnum-spin-lock-incf (atomic-fixnum)
             (loop
                for old = (atomic:atomic-fixnum-value atomic-fixnum)
                for new = (1+ old)
                until (atomic:atomic-fixnum-compare-and-swap atomic-fixnum old new)
                finally (return new))))
      (let ((atomic-fixnum (atomic:make-atomic-fixnum))
            (thread-count 4)
            (iterations 10000000))
        (loop for thread in (loop for i from 0 below thread-count
                               collect
                                 (let ((thread-index i))
                                   (bt:make-thread
                                    (lambda ()
                                      (dotimes (i iterations)
                                        (atomic-fixnum-spin-lock-incf atomic-fixnum)))
                                    :name (format nil "atomic-fixnum-compare-and-swap test thread ~A"
                                                  thread-index))))
           do (bt:join-thread thread))
        (is (atomic:atomic-fixnum-value atomic-fixnum)
            (* thread-count iterations)))))
  (subtest "Testing (atomic:atomic-fixnum-swap atomic-fixnum function) with multiple threads"
    (let ((atomic-fixnum (atomic:make-atomic-fixnum))
          (thread-count 4)
          (iterations 1000000))
      (loop for thread in (loop for i from 0 below thread-count
                             collect
                               (let ((thread-index i))
                                 (bt:make-thread
                                  (lambda ()
                                    (dotimes (i iterations)
                                      (atomic:atomic-fixnum-swap atomic-fixnum #'1+)))
                                  :name (format nil "atomic-fixnum-swap test thread ~A"
                                                thread-index))))
         do (bt:join-thread thread))
      (is (atomic:atomic-fixnum-value atomic-fixnum)
          (* thread-count iterations))))
  (subtest "Testing (atomic:atomic-fixnum-swap atomic-fixnum function &rest args) with multiple threads"
    (let ((atomic-fixnum (atomic:make-atomic-fixnum))
          (thread-count 4)
          (iterations 1000000))
      (loop for thread in (loop for i from 0 below thread-count
                             collect
                               (let ((thread-index i))
                                 (bt:make-thread
                                  (lambda ()
                                    (dotimes (i iterations)
                                      (atomic:atomic-fixnum-swap atomic-fixnum #'+ 1)))
                                  :name (format nil "atomic-fixnum-swap test thread ~A"
                                                thread-index))))
         do (bt:join-thread thread))
      (is (atomic:atomic-fixnum-value atomic-fixnum)
          (* thread-count iterations))))
  (subtest "Testing (atomic:atomic-fixnum-get-and-set atomic-fixnum new)"
    (let ((atomic-fixnum (atomic:make-atomic-fixnum)))
      (is (atomic:atomic-fixnum-get-and-set atomic-fixnum 5) 0)
      (is (atomic:atomic-fixnum-get-and-set atomic-fixnum 10) 5))))

(finalize)
