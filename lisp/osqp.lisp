(require 'asdf)
(asdf:load-system :cl-autowrap)
(asdf:load-system :cffi)

(defpackage :osqp-ffi
  (:use :common-lisp :autowrap))

(in-package :osqp-ffi)

(cffi:load-foreign-library
 (concatenate 'string 
	      (car (uiop:split-string (uiop:getenv "LIBRARY_PATH") :separator ":"))
	      "/libosqp.so"))


(autowrap:c-include (concatenate 'string
				 (car  (uiop:split-string (uiop:getenv "C_INCLUDE_PATH") :separator ":"))
				 "/osqp/osqp.h")
                      :spec-path "/home/light/stuff/MPC/specs/" 
                      :sysincludes (uiop:split-string (uiop:getenv "C_INCLUDE_PATH") :separator ":"))


