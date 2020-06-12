(in-package :atomic)

(defmacro compare-and-swap (place old new)
  "Atomic compare and set value of integer
  only when current value of integer equal to old value,
  return T if swap success, otherwise return NIL."
  #+sbcl
  `(= (sb-ext:compare-and-swap ,place ,old ,new)
      ,old)
  #+ccl
  `(ccl::conditional-store ,place ,old ,new)
  #+(and cmu mp)
  `(mp:without-scheduling
       (when (= ,place ,old)
         (setf ,place ,new)
         t))
  #+ecl
  `(= (mp:compare-and-swap ,place ,old ,new)
      ,old)
  #+(and lispworks (not (or lispworks3 lispworks4 lispworks5)))
  `(system:compare-and-swap ,place ,old ,new)
  #+allegro
  `(excl:atomic-conditional-setf ,place ,new ,old)
  #-(or sbcl
        ccl
        (and cmu mp)
        ecl
        (and lispworks (not (or lispworks3 lispworks4 lispworks5)))
        allegro)
  `(error "NOT support yet."))

;; TODO useless??
(defmacro atomic-swap (place function &rest args)
  "Atomic swap value of place with function and return new value."
  (let ((old (gensym "old"))
        (new (gensym "new")))
    `(loop
        for ,old = ,place
        for ,new = (funcall ,function
                            ,place
                            ,@args)
        until (compare-and-swap ,place ,old ,new)
        finally (return ,new))))
