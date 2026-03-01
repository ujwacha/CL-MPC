;; ;;; symbolic.lisp - Symbolic differentiation and Jacobian utilities

;; (defpackage :mpc-control.symbolic
;;   (:use :cl)
;;   (:export
;;    ;; Core differentiation
;;    :diff
;;    :simplify
;;    :jacobian
;;    :variadic->binary
;;    ;; Lambda generation
;;    :get-lambda
;;    ;; State management (if needed)
;;    ))

;; (in-package :mpc-control.symbolic)



(require :ALEXANDRIA)

(defun variadic->binary (expression)
  (let ((f (car expression))
	(a (cdr expression)))
    (if (eq (length a) 2) expression ;; base case
	(let ((first-two (subseq a 0 2))
	      (rest-args (subseq a 2 (length a))))
	  (variadic->binary `(,f (,f ,@first-two) ,@rest-args))))))


(defun diff (expression var)
  (cond
    ;; base cases
    ((symbolp expression) (if (eq expression var) 1 0))
    ((numberp expression) 0)
    ((and (listp expression)
	  (eq (length expression) 1))
     (diff (car expression) var))
    ((listp expression)
     (let ((f (car expression))
	   (a (cdr expression)))
       (cond
	 ((eq f '+) `(+ ,@(map 'list (lambda (expr) (diff expr var)) a)))

	 ((eq f '-) `(- ,@(map 'list (lambda (expr) (diff expr var)) a)))

	 ((eq f '*) (if (eq (length a) 2)
			(let ((u (car a))
			      (v (car (cdr a))))
			  `(+ (* ,u ,(diff v var)) (* ,v ,(diff u var))))
			(diff (variadic->binary expression) var)))

	 ((eq f '/) (if (eq (length a) 2)
			(let ((u (car a))
			      (v (car (cdr a))))
			  `(/ (+ (* ,v ,(diff u var)) (- (* ,u ,(diff v var))))
			      (* ,v ,v)))
			(error "Division should be Binary Expression")))
	 
	 ((eq f 'cos) (let ((argument (car a)))
			`(* (- (sin ,argument))  ,(diff argument var))))
	 ((eq f 'sin) (let ((argument (car a)))
			`(* (cos ,argument)  ,(diff argument var))))
	 
	 ((eq f 'log) (let ((argument (car a)))
			`(* (/ 1 ,argument) ,(diff argument var))))

	 (t (error "Couldn't differenciate, No rule for function inside a list")))))
    (t (error "Couldn't Differenciate The Expression"))))


(defun simplify (expression)
  (if (listp expression)
      (if (eq (length expression) 1) (car expression) ;; base case
	  (let ((symb (car expression))
		(args (mapcar #'simplify (cdr expression))))
	    (cond
	      
	      ((eq symb '+)
	       (let ((filtered (remove-if (lambda (x) (eq x 0)) args)))
		 (cond 
		   ((eq filtered nil) 0)
		   ((eq (length filtered) 1) (car filtered))
		   (t `(+ ,@filtered)))))

	      ((eq symb '*)
	       (let ((filtered (remove-if (lambda (x) (eq x 1)) args)))
		 (cond 
		   ((eq filtered nil) 1)
		   ((eq (length filtered) 1) (car filtered))
		   ((member 0 args) 0)
		   (t `(* ,@filtered)))))
	      
	      (t `(,symb ,@args)))))
      expression))



(defun jacobian (state transition)
  (mapcar (lambda (trans)
            (mapcar (lambda (var)
		      (simplify  (diff trans var)))
                    state))
          transition))


(defun get-lambda (vars matrix)
  `(lambda (,@vars)
     (list ,@(mapcar (lambda (col) (cons 'list col)) matrix))))




(defun state-limit-symbolic->list (state-vector u inequality l)
  (list u
	(alexandria:flatten  (jacobian state-vector inequality))
	l))
