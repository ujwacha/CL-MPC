(require 'asdf)

(asdf:load-system :cl-autowrap)
(asdf:load-system :cffi)

(defpackage :mylib-ffi
  (:use :common-lisp :autowrap))

(in-package :mylib-ffi)


(cffi:load-foreign-library "/home/light/stuff/MPC/mylib.so")


(autowrap:c-include "/home/light/stuff/MPC/C/mylib.h"
                      :spec-path "/home/light/stuff/MPC/specs/" 
                      :sysincludes (uiop:split-string (uiop:getenv "C_INCLUDE_PATH") :separator ":"))



(defparameter *my-float-ptr* (cffi:null-pointer))



(defparameter *struct-pointer* (my-struct-generator 10 20 30 65 3.1f0))


;; Method 1: WITH-FOREIGN-OBJECT (stack allocated, no manual free)


(cffi:with-foreign-object (float-ptr :float)
  (setf (cffi:mem-ref float-ptr :float) 3.1f0)
  (defparameter *struct-ptr* 
    (my-struct-generator 10 20 30 65 float-ptr)))

(cffi:with-foreign-object (float-ptr :float)
  (setf (cffi:mem-ref float-ptr :float) 3.1415f0)
  (defparameter *test-struct*
    (my-struct-generator 10 20 30 56 float-ptr))
 )

(pint-struct *test-struct*)



(my-struct.a *test-struct*)

;; (defparameter *struct-ptr* 
;;   (my-struct-generator 10 20 30 40 *my-float-ptr*))



;; (setf (autowrap:c-aptr *struct-ptr* mylib-ffi:my-struct :a) 123)


