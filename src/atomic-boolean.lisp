(in-package :atomic)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(declaim (inline make-atomic-boolean))
(defun make-atomic-boolean (&key value)
  "Return atomic-boolean,
  if NOT passing value with keyword :value then value is NIL as default"
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type boolean value))
  (let ((internal-value (if value 1 0)))
    (make-atomic-fixnum :value internal-value)))

(declaim (inline atomic-boolean-value))
(defun atomic-boolean-value (atomic-boolean)
  "Return value of atomic-boolean."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           #+ccl
           (type simple-vector atomic-boolean)
           #-ccl
           (type cons atomic-boolean))
  (= 1 (the fixnum (atomic-fixnum-value-macro atomic-boolean))))

(declaim (inline atomic-boolean-compare-and-swap))
(defun atomic-boolean-compare-and-swap (atomic-boolean old new)
  "Atomic compare and set value of fixnum
  only when current value of fixnum equal to old value,
  return T if swap success otherwise return NIL."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           #+ccl
           (type simple-vector atomic-boolean)
           #-ccl
           (type cons atomic-boolean)
           (type boolean old new))
  #+(or sbcl
        ccl
        (and cmu mp)
        ecl
        (and lispworks (not (or lispworks3 lispworks4 lispworks5)))
        allegro)
  (let ((old (if old 1 0))
        (new (if new 1 0)))
    #+sbcl
    (= (the fixnum
            (sb-ext:compare-and-swap (atomic-fixnum-value-macro atomic-boolean) old new))
       old)
    #+ccl
    (ccl::conditional-store (atomic-fixnum-value-macro atomic-boolean) old new)
    #+(and cmu mp)
    (mp:without-scheduling
        (when (= (the fixnum (atomic-fixnum-value-macro atomic-boolean)) old)
          (setf (atomic-fixnum-value-macro atomic-boolean) new)
          t))
    #+ecl
    (= (the fixnum (mp:compare-and-swap (atomic-fixnum-value-macro atomic-boolean) old new))
       old)
    #+(and lispworks (not (or lispworks3 lispworks4 lispworks5)))
    (system:compare-and-swap (atomic-fixnum-value-macro atomic-boolean) old new)
    #+allegro
    (excl:atomic-conditional-setf (atomic-fixnum-value-macro atomic-boolean) new old))
  #-(or sbcl
        ccl
        (and cmu mp)
        ecl
        (and lispworks (not (or lispworks3 lispworks4 lispworks5)))
        allegro)
  `(error "NOT supported yet."))

(defmacro atomic-boolean-swap (atomic-boolean function &rest args)
  "Atomic swap value of atomic-boolean with function and return new value.
  For example: (atomic-boolean-swap *atomic-boolean* #'1+)"
  #+(or sbcl
        ccl
        (and cmu mp)
        ecl
        (and lispworks (not (or lispworks3 lispworks4 lispworks5)))
        allegro)
  (let ((old-symbol (gensym "old"))
        (new-symbol (gensym "new")))
    `(loop
        for ,old-symbol of-type boolean = (atomic-boolean-value ,atomic-boolean)
        for ,new-symbol of-type boolean = (funcall ,function
                                                   (atomic-boolean-value ,atomic-boolean)
                                                   ,@args)
        until (atomic-boolean-compare-and-swap ,atomic-boolean
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

(defmacro atomic-boolean-get-and-set (atomic-boolean new)
  "Atomic set value of atomic-boolean and return new value."
  #+(or sbcl
        ccl
        (and cmu mp)
        ecl
        (and lispworks (not (or lispworks3 lispworks4 lispworks5)))
        allegro)
  (let ((old-symbol (gensym "old")))
    `(loop
        for ,old-symbol of-type boolean = (atomic-boolean-value ,atomic-boolean)
        while (not (atomic-boolean-compare-and-swap ,atomic-boolean ,old-symbol ,new))
        finally (return ,old-symbol)))
  #-(or sbcl
        ccl
        (and cmu mp)
        ecl
        (and lispworks (not (or lispworks3 lispworks4 lispworks5)))
        allegro)
  `(error "NOT supported yet."))
