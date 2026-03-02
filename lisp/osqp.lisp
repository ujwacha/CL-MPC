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

;; Design Goal, make something cool
;; when designing the MPC, we would want the optimizer to be global....
;; sorry if you want another controller, make another repl
;; the main function will be the repl, from where users will strt MPC
;; firstly, create-mpc-instance



;; define a bunch of global
(progn
(defvar *p-x* 'nil)
  (defvar *p-i* 'nil)
  (defvar *p-p* 'nil)
  (defvar *p-nnz* 'nil)
  
  (defvar *q-vec* 'nil)

  (defvar *n* 'nil)
  (defvar *m* 'nil)

  (defvar *a-x* 'nil)
  (defvar *a-i* 'nil)
  (defvar *a-p* 'nil)
  (defvar *a-nnz* 'nil)

  (defvar *l-vec* 'nil)
  (defvar *u-vec* 'nil)

  (defvar *p* 'nil)
  (defvar *q* 'nil)
  (defvar *a* 'nil)
  (defvar *l* 'nil)
  (defvar *u* 'nil)


  (defvar *mpc-solver-settings* 'nil)
  (defvar *mpc-solver* 'nil)
  )


(defun list->float-list (list-of-numbers)
  (mapcar (lambda (x) (coerce x 'double-float)) list-of-numbers))


(defun all-c-vars-free ()
  (if (not (eq *p-x* 'nil)) (progn (cffi:foreign-free *p-x*)
				   (setf *p-x* 'nil)))
  (if (not (eq *p-i* 'nil)) (progn (cffi:foreign-free *p-i*)
				   (setf *p-i* 'nil)))
  (if (not (eq *p-p* 'nil)) (progn (cffi:foreign-free *p-p*)
				   (setf *p-p* 'nil)))
  (if (not (eq *p-nnz* 'nil)) (setf *p-nnz* 'nil))
  
  (if (not (eq *q-vec* 'nil)) (progn (cffi:foreign-free *q-vec*)
				     (setf *q-vec* 'nil)))

  (if (not (eq *n* 'nil)) (setf *n* 'nil))
  (if (not (eq *m* 'nil)) (setf *m* 'nil))

  (if (not (eq *a-x* 'nil)) (progn (cffi:foreign-free *a-x*)
				   (setf *a-x* 'nil)))
  (if (not (eq *a-i* 'nil)) (progn (cffi:foreign-free *a-i*)
				   (setf *a-i* 'nil)))
  (if (not (eq *a-p* 'nil)) (progn (cffi:foreign-free *a-p*)
				   (setf *a-p* 'nil)))
  (if (not (eq *a-nnz* 'nil)) (setf *a-nnz* 'nil))

  (if (not (eq *l-vec* 'nil)) (progn (cffi:foreign-free *l-vec*)
				     (setf *l-vec* 'nil)))
  (if (not (eq *u-vec* 'nil)) (progn (cffi:foreign-free *u-vec*)
				     (setf *u-vec* 'nil)))

  (if (not (eq *p* 'nil)) (progn (osqp-csc-matrix-free *p*)
				 (setf *p* 'nil)))
  (if (not (eq *q* 'nil)) (progn (cffi:foreign-free *q*)
				 (setf *q* 'nil)))
  (if (not (eq *a* 'nil)) (progn (osqp-csc-matrix-free *a*)
				 (setf *a* 'nil)))
  (if (not (eq *l* 'nil)) (progn (cffi:foreign-free *l*)
				 (setf *l* 'nil)))
  (if (not (eq *u* 'nil)) (progn (cffi:foreign-free *u*)
				 (setf *u*  'nil)))

  (if (not (eq *mpc-solver-settings* 'nil)) (progn (osqp-settings-free *mpc-solver-settings*)
						   (setf *mpc-solver-settings* 'nil)))

  (if (not (eq *mpc-solver* 'nil)) (progn (osqp-cleanup *mpc-solver*)
					  (setf *mpc-solver* 'nil)))
  )

(defun set-mpc-problem (dim p q a l u)
  (all-c-vars-free)
  (print "all vars cleared")
  (destructuring-bind (row col) dim
    (setf *m* row)
    (setf *n* col)
    (print *m*)
    (print *n*)
    
    ;; quardratic cost
    (destructuring-bind (p-x p-i p-p) p
      (setf *p-x* (cffi:foreign-alloc :double :initial-contents (list->float-list p-x)))
      (setf *p-i* (cffi:foreign-alloc :int64 :initial-contents  p-i))
      (setf *p-p* (cffi:foreign-alloc :int64 :initial-contents  p-p))
      (setf *p-nnz* (length p-x))
      (setf *p* (osqp-csc-matrix-new *n* *n* *p-nnz* *p-x* *p-i* *p-p*)))


    ;; linear cost
    (setf *q* (cffi:foreign-alloc :double :initial-contents (list->float-list q)))

    ;; model
    (destructuring-bind (a-x a-i a-p) a
      (setf *a-x* (cffi:foreign-alloc :double :initial-contents (list->float-list a-x)))
      (setf *a-i* (cffi:foreign-alloc :int64 :initial-contents  a-i))
      (setf *a-p* (cffi:foreign-alloc :int64 :initial-contents  a-p))
      (setf *a-nnz* (length a-x))
      (setf *a* (osqp-csc-matrix-new *m* *n* *a-nnz* *a-x* *a-i* *a-p*)))

    ;; bounds
    (setf *l* (cffi:foreign-alloc :double :initial-contents (list->float-list l)))
    (setf *u* (cffi:foreign-alloc :double :initial-contents (list->float-list u)))

    ;; Now actually make the solver and settings
    ;; solver
    (setf *mpc-solver-settings* (osqp-settings-new))
    (osqp-set-default-settings *mpc-solver-settings*)


    (autowrap:with-alloc (solver-ptr-ptr :pointer)
      (let ((exitflag (osqp-setup solver-ptr-ptr
				  *p*
				  *q*
				  *a*
				  *l*
				  *u*
				  *m*
				  *n*
				  *mpc-solver-settings*)))

	(print exitflag)
	(if (= 0 exitflag)
	    (let* ((raw-solver-ptr (cffi:mem-ref solver-ptr-ptr :pointer))
		   (solver (make-osqp-solver :ptr raw-solver-ptr)))
	      (setf *mpc-solver* solver)
	      (print "completed the solver")
	      ))))))



(defun float-list->vector-list (input-list)
  (map '(vector double-float) 
       (lambda (x) (coerce x 'double-float)) 
       input-list))


(defun mpc-matrix-update-alu (a-mat l u)
  (let ((a-mat-floats (float-list->vector-list
		       (list->float-list a-mat)))
	(l-floats (float-list->vector-list
		   (list->float-list l)))
	(u-floats (float-list->vector-list
		   (list->float-list u))))
    (cffi:with-foreign-array (a-mat-c a-mat-floats `(:array
						     :double
						     ,(length a-mat-floats)))

      (cffi:with-foreign-array (l-vec-c l-floats `(:array
						   :double
						   ,(length l-floats)))

	(cffi:with-foreign-array (u-vec-c u-floats `(:array
						     :double
						     ,(length u-floats)))
	  (let ((c-null (cffi:null-pointer)))
	    (osqp-update-data-mat *mpc-solver*
				  c-null c-null 0
				  a-mat-c c-null (length a-mat))
	    
	    (osqp-update-data-vec *mpc-solver*
				  c-null
				  l-vec-c
				  u-vec-c)))))))

(defun mpc-matrix-update-p (p-mat)
  (let ((p-mat-floats (float-list->vector-list
		       (list->float-list p-mat))))
    (cffi:with-foreign-array (p-mat-c p-mat-floats `(:array
						     :double
						     ,(length p-mat-floats)))
      (let ((c-null (cffi:null-pointer)))
	(osqp-update-data-mat *mpc-solver*
			      p-mat-c c-null (length p-mat)
			      c-null c-null 0)))))


(defun mpc-matrix-update-q (q-vec)
  (let ((q-floats (float-list->vector-list
		   (list->float-list q-vec))))
    (cffi:with-foreign-array (q-vec-c q-floats `(:array
						 :double
						 ,(length q-floats)))
      (let ((c-null (cffi:null-pointer)))
	(osqp-update-data-vec *mpc-solver* q-vec-c c-null c-null)))))

(defun solve-mpc ()
  (if (not *mpc-solver*)
      (error "Run (set-mpc-problem ...) first.")
      (progn
        (osqp-solve *mpc-solver*)
        (let* ((sol-raw (osqp-solver.solution *mpc-solver*))
               (sol-wrap (make-osqp-solution :ptr sol-raw))
               (x-ptr (osqp-solution.x sol-wrap)))
          (loop for i from 0 below *n*
                collect (cffi:mem-aref x-ptr :double i))))))


(set-mpc-problem '(3 2) 
                 '((4.0 1.0 2.0) (0 0 1) (0 1 3))
                 '(1.0 1.0)
                 '((1.0 1.0 1.0 1.0) (0 1 0 2) (0 2 4))
                 '(1.0 0.0 0.0)
                 '(1.0 0.7 0.7))


(time 
 (solve-mpc))

