;; ;; this is for interfacing C with Common lisp
;; (require 'asdf)
;; (asdf:load-system :cl-autowrap)
;; (asdf:load-system :cffi)

;; (defpackage :osqp-ffi
;;   (:use :common-lisp :autowrap))

;; (in-package :osqp-ffi)

(require 'cffi)
(require 'cl-autowrap)

(cffi:load-foreign-library
 (concatenate 'string 
	      (car (uiop:split-string (uiop:getenv "LIBRARY_PATH") :separator ":"))
	      "/libosqp.so"))


(autowrap:c-include (concatenate 'string
				 (car  (uiop:split-string (uiop:getenv "C_INCLUDE_PATH") :separator ":"))
				 "/osqp/osqp.h")
                      :spec-path "/home/light/stuff/MPC/specs/" 
                      :sysincludes (uiop:split-string (uiop:getenv "C_INCLUDE_PATH") :separator ":"))







(cffi:with-foreign-objects ((Array :double 5))
  (loop for i from 0 below 5
	do
	   (setf (cffi:mem-aref Array :double i) (* i 5.0d0)))

  (loop for i from 0 below 5
	do
	   (print (cffi:mem-aref Array :double i)))
  )


(defun cl-list->double-c-array (list)
  (let* ((len (length list))
	 (Array (cffi:foreign-alloc :double :count len)))
    
    (loop for value in list
	  for i from 0
	  do
	     (setf (cffi:mem-aref Array :double i) value))
    Array))


(defun cl-list->int-c-array (list)
  (let* ((len (length list))
	 (Array (cffi:foreign-alloc :int :count len)))
    
    (loop for value in list
	  for i from 0
	  do
	     (setf (cffi:mem-aref Array :long-long i) value))
    Array))


(let* ((P_x '(4.0d0 1.0d0 2.0d0))
       (P_x-c (cl-list->double-c-array P_x))
       
       (P_i '(0 0 1))
       (P_i-c (cl-list->int-c-array P_i))
       
       (P_p '(0 1 3))
       (P_p-c (cl-list->int-c-array P_p))
       
       (q '(1.0d0 1.0d0))
       (q-c (cl-list->double-c-array q))
       
       (A_x '(1.0d0 1.0d0 1.0d0 1.0d0))
       (A_x-c (cl-list->double-c-array A_x))
       
       (A_i '(0 1 0 2))
       (A_i-c (cl-list->int-c-array A_i))
       
       (A_p '(0 2 4))
       (A_p-c (cl-list->int-c-array A_p))
       
       (l '(1.0d0 0.0d0 0.0d0))
       (l-c (cl-list->double-c-array l))
       
       (u '(1.0d0 0.7d0 0.7d0))
       (u-c (cl-list->double-c-array u))

       (P_nnz 3)
       (A_nnz 4)
       (n 2)
       (m 3)
       (exitflag 0)
       (p (osqp-csc-matrix-new n n p_nnz p_x-c p_i-c p_p-c))
       (a (osqp-csc-matrix-new m n a_nnz a_x-c a_i-c a_p-c))
       (settings (osqp-settings-new))
       (solver (cffi:foreign-alloc :pointer))
       )

  (osqp-set-default-settings settings)
  ;; (setf (osqp-settings.alpha settings) 1.0d0)
  (setq this-stupid-test  (osqp-setup solver p q-c a l-c u-c m n settings))

  )


