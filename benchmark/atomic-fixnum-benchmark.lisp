(in-package :atomic-benchmark)

(defmacro takes-time-ms (&body body)
  "Return takes time(ms) of evaluate a form.
  Return type: (values real-time-ms run-time form-result-1 form-result-2 ...)"
  (let ((begin-internal-real-time-symbol (gensym "begin-internal-real-time"))
        (begin-internal-run-time-symbol (gensym "begin-internal-run-time"))
        (takes-real-time-symbol (gensym "takes-real-time"))
        (takes-run-time-symbol (gensym "takes-run-time"))
        (result-symbol (gensym "result")))
    `(let ((,begin-internal-real-time-symbol (get-internal-real-time))
           (,begin-internal-run-time-symbol (get-internal-run-time))
           (,result-symbol nil))
       (unwind-protect
            (setf ,result-symbol (multiple-value-list (progn ,@body)))
         (let ((,takes-real-time-symbol (- (get-internal-real-time)
                                           ,begin-internal-real-time-symbol))
               (,takes-run-time-symbol (- (get-internal-run-time)
                                          ,begin-internal-run-time-symbol)))
           (setf ,result-symbol
                 (append (list (/ (* 1000 ,takes-real-time-symbol)
                                  internal-time-units-per-second)
                               (/ (* 1000 ,takes-run-time-symbol)
                                  internal-time-units-per-second))
                         ,result-symbol))))
       (values-list ,result-symbol))))

(defmacro takes-real-time-ms (&body body)
  "Return takes real time(ms) of evaluate a form.
  Return type: (values real-time-ms form-result-1 form-result-2 ...)"
  (let ((begin-internal-real-time-symbol (gensym "begin-internal-real-time"))
        (takes-real-time-symbol (gensym "takes-real-time"))
        (result-symbol (gensym "result")))
    `(let ((,begin-internal-real-time-symbol (get-internal-real-time))
           (,result-symbol nil))
       (unwind-protect
            (setf ,result-symbol (multiple-value-list (progn ,@body)))
         (let ((,takes-real-time-symbol (- (get-internal-real-time)
                                           ,begin-internal-real-time-symbol)))
           (setf ,result-symbol
                 (append (list (/ (* 1000 ,takes-real-time-symbol)
                                  internal-time-units-per-second))
                         ,result-symbol))))
       (values-list ,result-symbol))))

(subtest "Benchmark atomic-fixnum"
  (subtest "Benchmark (atomic:atomic-fixnum-incf atomic-fixnum) with multiple threads"
    (let* ((run-count 5)
           (thread-count 2)
           (iterations 10000000)
           (takes-ms-list
            (loop repeat run-count
               collect (let* ((atomic-fixnum (atomic:make-atomic-fixnum))
                              (run-once-takes-ms 
                               (loop
                                  for thread
                                  in (loop for i from 0 below thread-count
                                        collect
                                          (let ((thread-index i))
                                            (bt:make-thread
                                             #'(lambda ()
                                                 (takes-real-time-ms
                                                   (dotimes (i iterations)
                                                     (atomic:atomic-fixnum-incf atomic-fixnum))))
                                             :name (format nil
                                                           "atomic-fixnum-incf test thread ~A"
                                                           thread-index))))
                                  maximize (bt:join-thread thread))))
                         (is (atomic:atomic-fixnum-value atomic-fixnum)
                             (* thread-count iterations))
                         run-once-takes-ms))))
      (diag
       (format nil "~S"
               (list 
                (cons :avg-ops (truncate (* iterations thread-count 1000)
                                         (truncate (apply #'+ takes-ms-list)
                                                   run-count)))
                (cons :max-ops (truncate (* iterations thread-count 1000)
                                         (apply #'min takes-ms-list)))
                (cons :mix-ops (truncate (* iterations thread-count 1000)
                                         (apply #'max takes-ms-list)))))))))
