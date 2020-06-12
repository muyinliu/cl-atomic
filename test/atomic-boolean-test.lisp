(in-package :atomic-test)

(plan nil)

(subtest "Testing atomic-boolean"
  (subtest "Testing default value of atomic-boolean"
    (let ((atomic-boolean (atomic:make-atomic-boolean)))
      (is (atomic:atomic-boolean-value atomic-boolean) nil))
    (let ((atomic-boolean (atomic:make-atomic-boolean :value t)))
      (is (atomic:atomic-boolean-value atomic-boolean) t)))
  (subtest "Testing (atomic:atomic-boolean-compare-and-swap atomic-boolean old new)"
    (let ((atomic-boolean (atomic:make-atomic-boolean :value t)))
      (is (atomic:atomic-boolean-compare-and-swap atomic-boolean t nil) t)
      (is (atomic:atomic-boolean-value atomic-boolean) nil)
      (is (atomic:atomic-boolean-compare-and-swap atomic-boolean nil t) t)
      (is (atomic:atomic-boolean-value atomic-boolean) t)))
  (subtest "Testing (atomic:atomic-boolean-swap atomic-boolean function)"
    (let ((atomic-boolean (atomic:make-atomic-boolean :value t)))
      (is (atomic:atomic-boolean-swap atomic-boolean #'not) nil)
      (is (atomic:atomic-boolean-value atomic-boolean) nil)
      (is (atomic:atomic-boolean-swap atomic-boolean #'not) t)
      (is (atomic:atomic-boolean-value atomic-boolean) t)))
  (subtest "Testing (atomic:atomic-boolean-get-and-set atomic-boolean new)"
    (let ((atomic-boolean (atomic:make-atomic-boolean :value t)))
      (is (atomic:atomic-boolean-get-and-set atomic-boolean nil) t)
      (is (atomic:atomic-boolean-get-and-set atomic-boolean t) nil))))

(finalize)
