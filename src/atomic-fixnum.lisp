(in-package :atomic)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(declaim (inline make-atomic-fixnum))
(defun make-atomic-fixnum (&key (value 0))
  "Return atomic-fixnum"
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum value))
  #+ccl
  (make-array 1 :initial-element value)
  #-ccl
  (cons value nil))

(defmacro atomic-fixnum-value-macro (atomic-fixnum)
  "Return value of atomic-fixnum in macro."
  #+ccl
  `(svref ,atomic-fixnum 0)
  #-ccl
  `(car ,atomic-fixnum))

(declaim (inline atomic-fixnum-value))
(defun atomic-fixnum-value (atomic-fixnum)
  "Return value of atomic-fixnum."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           #+ccl
           (type simple-vector atomic-fixnum)
           #-ccl
           (type cons atomic-fixnum))
  (atomic-fixnum-value-macro atomic-fixnum))

(declaim (inline atomic-fixnum-incf))
(defun atomic-fixnum-incf (atomic-fixnum &optional (diff 1))
  "Atomic incf fixnum with diff and return old value."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           #+ccl
           (type simple-vector atomic-fixnum)
           #-ccl
           (type cons atomic-fixnum)
           (type fixnum diff))
  #+sbcl
  (sb-ext:atomic-incf (atomic-fixnum-value-macro atomic-fixnum) diff)
  #+ccl
  (let ((old (svref atomic-fixnum 0)))
    (ccl::atomic-incf-decf (svref atomic-fixnum 0) diff)
    old)
  #+(and cmu mp)
  (mp:without-scheduling
      (let ((old (atomic-fixnum-value-macro atomic-fixnum)))
        (setf (atomic-fixnum-value-macro atomic-fixnum)
              (let ((new (+ (atomic-fixnum-value-macro atomic-fixnum) diff)))
                (if (typep new 'fixnum)
                    new
                    (the fixnum (+ most-negative-fixnum
                                   (- old most-positive-fixnum)
                                   (1- diff))))))
        old))
  #+ecl
  (mp:atomic-incf (atomic-fixnum-value-macro atomic-fixnum) diff)
  #+(and lispworks (not (or lispworks3 lispworks4 lispworks5)))
  (let ((old (atomic-fixnum-value-macro atomic-fixnum)))
    (system:atomic-fixnum-incf (atomic-fixnum-value-macro atomic-fixnum) diff)
    old)
  #+allegro
  (sys:without-scheduling
      ;; Note: excl:incf-atomic will NOT overflow as expected
      ;;       even if value is most-positive-fixnum, so use sys:without-scheduling
      (let ((old (atomic-fixnum-value-macro atomic-fixnum)))
        (setf (atomic-fixnum-value-macro atomic-fixnum)
              (let ((new (+ (atomic-fixnum-value-macro atomic-fixnum) diff)))
                (if (typep new 'fixnum)
                    new
                    (the fixnum (+ most-negative-fixnum
                                   (- old most-positive-fixnum)
                                   (1- diff))))))
        old))
  #-(or sbcl
        ccl
        (and cmu mp)
        ecl
        (and lispworks (not (or lispworks3 lispworks4 lispworks5)))
        allegro)
  (error "NOT supported yet.")) ;; TODO should use lock

(declaim (inline atomic-fixnum-incf-and-get))
(defun atomic-fixnum-incf-and-get (atomic-fixnum &optional (diff 1))
  "Atomic incf fixnum with diff and return new value."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           #+ccl
           (type simple-vector atomic-fixnum)
           #-ccl
           (type cons atomic-fixnum)
           (type fixnum diff))
  #+sbcl
  (progn
    (sb-ext:atomic-incf (atomic-fixnum-value-macro atomic-fixnum) diff)
    (atomic-fixnum-value-macro atomic-fixnum))
  #+ccl
  (ccl::atomic-incf-decf (svref atomic-fixnum 0) diff)
  #+(and cmu mp)
  (mp:without-scheduling
      (setf (atomic-fixnum-value-macro atomic-fixnum)
            (let ((new (+ (atomic-fixnum-value-macro atomic-fixnum) diff)))
              (if (typep new 'fixnum)
                  new
                  (the fixnum (+ most-negative-fixnum
                                 (the fixnum (- (atomic-fixnum-value-macro atomic-fixnum) most-positive-fixnum))
                                 (the fixnum (1- diff))))))))
  #+ecl
  (progn
    (mp:atomic-incf (atomic-fixnum-value-macro atomic-fixnum) diff)
    (atomic-fixnum-value-macro atomic-fixnum))
  #+(and lispworks (not (or lispworks3 lispworks4 lispworks5)))
  (system:atomic-fixnum-incf (atomic-fixnum-value-macro atomic-fixnum) diff)
  #+allegro
  (sys:without-scheduling
      ;; Note: excl:incf-atomic will NOT overflow as expected
      ;;       even if value is most-positive-fixnum, so use sys:without-scheduling
      (setf (atomic-fixnum-value-macro atomic-fixnum)
            (let ((new (+ (atomic-fixnum-value-macro atomic-fixnum) diff)))
              (if (typep new 'fixnum)
                  new
                  (the fixnum (+ most-negative-fixnum
                                 (the fixnum (- (atomic-fixnum-value-macro atomic-fixnum)
                                                most-positive-fixnum))
                                 (the fixnum (1- diff))))))))
  #-(or sbcl
        ccl
        (and cmu mp)
        ecl
        (and lispworks (not (or lispworks3 lispworks4 lispworks5)))
        allegro)
  (error "NOT supported yet.")) ;; TODO should use lock

(declaim (inline atomic-fixnum-decf))
(defun atomic-fixnum-decf (atomic-fixnum &optional (diff 1))
  "Atomic incf fixnum with diff and return old value."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           #+ccl
           (type simple-vector atomic-fixnum)
           #-ccl
           (type cons atomic-fixnum)
           (type fixnum diff))
  #+sbcl
  (sb-ext:atomic-decf (atomic-fixnum-value-macro atomic-fixnum) diff)
  #+ccl
  (let ((old (svref atomic-fixnum 0)))
    (ccl::atomic-incf-decf (svref atomic-fixnum 0) (- diff))
    old)
  #+(and cmu mp)
  (let ((old (atomic-fixnum-value-macro atomic-fixnum)))
    (mp:without-scheduling
        (setf (atomic-fixnum-value-macro atomic-fixnum)
              (let ((new (- (atomic-fixnum-value-macro atomic-fixnum) diff)))
                (if (typep new 'fixnum)
                    new
                    (the fixnum (- most-positive-fixnum
                                   (- most-negative-fixnum old)
                                   (1- diff)))))))
    old)
  #+ecl
  (mp:atomic-decf (atomic-fixnum-value-macro atomic-fixnum) diff)
  #+(and lispworks (not (or lispworks3 lispworks4 lispworks5)))
  (let ((old (atomic-fixnum-value-macro atomic-fixnum)))
    (system:atomic-fixnum-decf (atomic-fixnum-value-macro atomic-fixnum) diff)
    old)
  #+allegro
  (sys:without-scheduling
      ;; Note: excl:decf-atomic will NOT overflow as expected
      ;;       even if value is most-negative-fixnum, so use sys:without-scheduling
      (let ((old (atomic-fixnum-value-macro atomic-fixnum)))
        (setf (atomic-fixnum-value-macro atomic-fixnum)
              (let ((new (- (atomic-fixnum-value-macro atomic-fixnum) diff)))
                (if (typep new 'fixnum)
                    new
                    (the fixnum (- most-positive-fixnum
                                   (- most-negative-fixnum old)
                                   (1- diff))))))
        old))
  #-(or sbcl
        ccl
        (and cmu mp)
        ecl
        (and lispworks (not (or lispworks3 lispworks4 lispworks5)))
        allegro)
  (error "NOT supported yet.")) ;; TODO should use lock

(declaim (inline atomic-fixnum-decf-and-get))
(defun atomic-fixnum-decf-and-get (atomic-fixnum &optional (diff 1))
  "Atomic incf fixnum with diff and return new value."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           #+ccl
           (type simple-vector atomic-fixnum)
           #-ccl
           (type cons atomic-fixnum)
           (type fixnum diff))
  #+sbcl
  (progn
    (sb-ext:atomic-decf (atomic-fixnum-value-macro atomic-fixnum) diff)
    (atomic-fixnum-value-macro atomic-fixnum))
  #+ccl
  (ccl::atomic-incf-decf (svref atomic-fixnum 0) (- diff))
  #+(and cmu mp)
  (mp:without-scheduling
      (setf (atomic-fixnum-value-macro atomic-fixnum)
            (let ((new (- (atomic-fixnum-value-macro atomic-fixnum) diff)))
              (if (typep new 'fixnum)
                  new
                  (the fixnum (- most-positive-fixnum
                                 (- most-negative-fixnum (atomic-fixnum-value-macro atomic-fixnum))
                                 (1- diff)))))))
  #+ecl
  (progn
    (mp:atomic-decf (atomic-fixnum-value-macro atomic-fixnum) diff)
    (atomic-fixnum-value-macro atomic-fixnum))
  #+(and lispworks (not (or lispworks3 lispworks4 lispworks5)))
  (system:atomic-fixnum-decf (atomic-fixnum-value-macro atomic-fixnum) diff)
  #+allegro
  (sys:without-scheduling
      ;; Note: excl:decf-atomic will NOT overflow as expected
      ;;       even if value is most-negative-fixnum, so use sys:without-scheduling
      (setf (atomic-fixnum-value-macro atomic-fixnum)
            (let ((new (- (atomic-fixnum-value-macro atomic-fixnum) diff)))
              (if (typep new 'fixnum)
                  new
                  (the fixnum (- most-positive-fixnum
                                 (- most-negative-fixnum (atomic-fixnum-value-macro atomic-fixnum))
                                 (1- diff)))))))
  #-(or sbcl
        ccl
        (and cmu mp)
        ecl
        (and lispworks (not (or lispworks3 lispworks4 lispworks5)))
        allegro)
  (error "NOT supported yet.")) ;; TODO should use lock

(declaim (inline atomic-fixnum-compare-and-swap))
(defun atomic-fixnum-compare-and-swap (atomic-fixnum old new)
  "Atomic compare and set value of fixnum
  only when current value of fixnum equal to old value,
  return T if swap success otherwise return NIL."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           #+ccl
           (type simple-vector atomic-fixnum)
           #-ccl
           (type cons atomic-fixnum)
           (type fixnum old new))
  #+sbcl
  (= (the fixnum (sb-ext:compare-and-swap (atomic-fixnum-value-macro atomic-fixnum) old new))
     old)
  #+ccl
  (ccl::conditional-store (atomic-fixnum-value-macro atomic-fixnum) old new)
  #+(and cmu mp)
  (mp:without-scheduling
      (when (= (the fixnum (atomic-fixnum-value-macro atomic-fixnum)) old)
        (setf (atomic-fixnum-value-macro atomic-fixnum) new)
        t))
  #+ecl
  (= (the fixnum (mp:compare-and-swap (atomic-fixnum-value-macro atomic-fixnum) old new))
     old)
  #+(and lispworks (not (or lispworks3 lispworks4 lispworks5)))
  (system:compare-and-swap (atomic-fixnum-value-macro atomic-fixnum) old new)
  #+allegro
  (excl:atomic-conditional-setf (atomic-fixnum-value-macro atomic-fixnum) new old)
  #-(or sbcl
        ccl
        (and cmu mp)
        ecl
        (and lispworks (not (or lispworks3 lispworks4 lispworks5)))
        allegro)
  (error "NOT supported yet."))

(defmacro atomic-fixnum-swap (atomic-fixnum function &rest args)
  "Atomic swap value of atomic-fixnum with function and return new value.
  For example: (atomic-fixnum-swap *atomic-fixnum* #'1+)"
  #+sbcl
  `(sb-ext:atomic-update (atomic-fixnum-value-macro ,atomic-fixnum) ,function ,@args)
  #+(and cmu mp)
  `(mp:without-scheduling
       (setf (atomic-fixnum-value-macro ,atomic-fixnum)
             (funcall ,function
                      (atomic-fixnum-value-macro ,atomic-fixnum)
                      ,@args)))
  #+ecl
  `(mp:atomic-update (atomic-fixnum-value-macro ,atomic-fixnum) ,function ,@args)
  #+allegro
  (let ((var-symbol (gensym "var")))
    `(excl:update-atomic (,var-symbol (atomic-fixnum-value-macro ,atomic-fixnum))
                         (funcall ,function ,var-symbol ,@args)))
  #+(or ccl
        (and lispworks (not (or lispworks3 lispworks4 lispworks5))))
  (let ((old-symbol (gensym "old"))
        (new-symbol (gensym "new")))
    `(loop
        for ,old-symbol fixnum = (atomic-fixnum-value-macro ,atomic-fixnum)
        for ,new-symbol fixnum = (funcall ,function
                                          (atomic-fixnum-value-macro ,atomic-fixnum)
                                          ,@args)
        until (atomic-fixnum-compare-and-swap ,atomic-fixnum
                                              ,old-symbol
                                              ,new-symbol)
        finally (return ,new-symbol)))
  #-(or sbcl
        ccl
        (and cmu mp)
        ecl
        (and lispworks (not (or lispworks3 lispworks4 lispworks5)))
        allegro)
  `(error "NOT supported yet."))

(defmacro atomic-fixnum-get-and-set (atomic-fixnum new)
  "Atomic set value of atomic-fixnum and return old value."
  #+(and lispworks (not (or lispworks3 lispworks4 lispworks5)))
  `(system:atomic-exchange (atomic-fixnum-value-macro ,atomic-fixnum) ,new)
  #+(or sbcl
        ccl
        (and cmu mp)
        ecl
        allegro)
  (let ((old-symbol (gensym "old")))
    `(loop
        for ,old-symbol fixnum = (atomic-fixnum-value-macro ,atomic-fixnum)
        while (not (atomic-fixnum-compare-and-swap ,atomic-fixnum ,old-symbol ,new))
        finally (return ,old-symbol)))
  #-(or sbcl
        ccl
        (and cmu mp)
        ecl
        (and lispworks (not (or lispworks3 lispworks4 lispworks5)))
        allegro)
  `(error "NOT supported yet."))

(defconstant +atomic-fixnum-max+ most-positive-fixnum)
(defconstant +atomic-fixnum-min+ most-negative-fixnum)
