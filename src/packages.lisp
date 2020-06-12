(in-package :cl-user)

(defpackage cl-atomic
  (:nicknames #:atomic)
  (:use :cl)
  (:shadow #:defconstant)
  (:export
   ;;; atomic-fixnum
   #:make-atomic-fixnum
   #:atomic-fixnum-value
   #:atomic-fixnum-incf
   #:atomic-fixnum-incf-and-get
   #:atomic-fixnum-decf
   #:atomic-fixnum-decf-and-get
   #:atomic-fixnum-compare-and-swap
   #:atomic-fixnum-swap
   #:atomic-fixnum-get-and-set
   #:+atomic-fixnum-max+
   #:+atomic-fixnum-min+

   ;;; atomic-boolean
   #:make-atomic-boolean
   #:atomic-boolean-value
   #:atomic-boolean-compare-and-swap
   #:atomic-boolean-swap
   #:atomic-boolean-get-and-set))

(in-package :atomic)

(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))
