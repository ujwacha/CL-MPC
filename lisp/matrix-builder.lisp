;; (defpackage :mpc-control.matrix-builder
;;   (:use :cl :alexandria :mpc-control.symbolic)
;;   (:export
;;    ;; Core matrix builders
;;    :get-p
;;    :get-q
;;    :get-a-l-u-matrix
;;    ;; CSC conversion
;;    :list-matrix->csc-list
;;    :number-grouper
;;    ;; Matrix utilities
;;    :insert-matrix
;;    :identity-matrix-nil
;;    :mat-constant-mul
;;    :range-get
;;    :make-list-filled))


(require :alexandria)

(defun range-get (l g) 
  (loop for i from l below g collect i))

(defun get-trajectory(N)
  (mapcar (lambda (x) `(,x 0)) (range-get 1 (+ N 1))))

(defun identity-matrix (n)
  (mapcar (lambda (i)
	    (let ((retval (make-list  n :initial-element 0)))
	      (setf (nth i retval) 1)
	      retval))
	  (range-get 0 n)))

(defun identity-matrix-nil (n)
  (mapcar (lambda (i)
	    (let ((retval (make-list  n :initial-element nil)))
	      (setf (nth i retval) 1)
	      retval))
	  (range-get 0 n)))

(defun mat-constant-mul (constant matrix)
  (mapcar (lambda (col)
	    (mapcar (lambda (elem) (* constant elem)) col))
	  matrix))


(defun get-q (time-horizon trajectory initial-state state-symbols control-symbols)
  (if (not (and (<= time-horizon (length trajectory))
		(eq (length state-symbols) (length  (car trajectory)))))
      (error "Trajectory length must be greater than Time Horizon must be equal")
      (let ((traj (subseq trajectory 0 time-horizon)))
	(mapcar (lambda (x) (* 2 (- x)))
		(alexandria:flatten
		 (concatenate 'list initial-state traj
			      (mapcar (lambda (x)
					(make-list  (length control-symbols) :initial-element 0))
				      (range-get 0 time-horizon))))))))



(defun get-p (horizon-length state-weights control-weights &optional prop-lambda-state prop-lambda-conrtol)

  (declare (optimize (speed 3) (safety 1) (debug 0))
           (type fixnum horizon-length)
           (type list state-weights control-weights))
  
  (let* ((N (+ (* (+ horizon-length 1) (length state-weights))
	       (* horizon-length (length control-weights))))
	 
	 (fn (cond
	       ((eq prop-lambda-state nil) (lambda (i y) y))
	       (t prop-lambda-state)))

	 (cfn (cond
		((eq prop-lambda-conrtol nil) (lambda (i y) y))
		(t prop-lambda-conrtol)))


	 (state-len (length state-weights))

	 (control-len (length control-weights))

	 (state-weights-list (loop for i from 0 below (+ horizon-length 1)
				   collect
				   `((,(* i state-len) ,(* i state-len)) ;; position part
				     ,(mapcar (lambda (col)
						;; lambdaed part
						(loop for elem in col
						      collect
						      (funcall fn i elem)))
					      state-weights))))

	 (control-weights-list (loop for i from 0 below horizon-length
				     collect
				     `((,(+
					  (* (+ horizon-length 1) (length state-weights))
					  (* i control-len))
					,(+
					  (* (+ horizon-length 1) (length state-weights))
					  (* i control-len)))
				       ;; position part
				       ,(mapcar (lambda (col)
						  ;; lambdaed part
						  (loop for elem in col
							collect
							(funcall cfn i elem)))
						control-weights))))
	 
	 (empty (loop
		  for unused_var from 1 to n
		  collect
		  (make-list  n :initial-element nil))))

    
    (if (not (or (eq (length state-weights) (length (car state-weights)))
		 (eq (length control-weights) (length (car control-weights)))))
	(error "State weights and Control weights should both be square matrices")
	(reduce
	 (lambda (empty pos-smatrix)
	   (destructuring-bind (pos smatrix) pos-smatrix
	     (insert-matrix empty smatrix pos)))
	 (concatenate 'list state-weights-list control-weights-list)
	 :initial-value empty))))



(defun insert-matrix (big-matrix small-matrix pos)
  (declare (optimize (speed 3) (safety 1) (debug 0))
           (type list big-matrix small-matrix pos))
  
  (destructuring-bind (start-row start-col) pos
    (declare (type fixnum start-row start-col))
    (let* ((n-rows (the fixnum (length (the list (first big-matrix)))))
           (n-cols (the fixnum (length big-matrix)))
           (small-cols (the fixnum (length small-matrix)))
           (small-rows (the fixnum (length (the list (first small-matrix))))))
      (declare (type fixnum n-rows n-cols small-cols small-rows))
      
      ;; Pre-compute end bounds
      (let ((max-row (min (+ start-row small-rows) n-rows))
            (max-col (min (+ start-col small-cols) n-cols)))
        (declare (type fixnum max-row max-col))
        
        ;; Loop over destination columns
        (loop for big-col fixnum from start-col below max-col
              for small-col fixnum from 0
              do (let ((big-column (nth big-col big-matrix))
                       (small-column (nth small-col small-matrix)))
                   (declare (type list big-column small-column))
                   
                   ;; Loop over rows in this column
                   (loop for big-row fixnum from start-row below max-row
                         for small-row fixnum from 0
                         do (setf (nth big-row big-column)
                                  (nth small-row small-column)))))))
    big-matrix))




(defun get-a-l-u-matrix (time-horizon
			 state-jacobian
			 control-jacobian
			 state-symbols
			 current-state
			 control-symbols
			 &optional
			   control-limits
			   state-limits)
  
  "supposed to return a list of (A L U) Matrices"

  ;; Put verify code for size of state jacobian and control jacobian here
  (let* (
	 (state-len (length state-symbols))
	 (control-len (length control-symbols))

	 (m (+ (* (+ time-horizon 1) state-len)
	       (* time-horizon control-len)
	       (if (eq 'nil state-limits)
		   0
		   (* time-horizon 1))))
	 
	 (n (+ (* (+ time-horizon 1) state-len)
	       (* time-horizon control-len))) ;; no of cols


	 (empty (loop
		  for unused_var from 1 to n
		  collect
		  (make-list  m :initial-element nil)))

	 (initial-matrix (insert-matrix empty
					(identity-matrix-nil n)
					'(0 0)))
	 
	 (state-jac-offset-row (length state-jacobian))

	 (control-jac-offset-col (* (+ time-horizon 1) state-len))
	 
	 (state-jacobian-list (mapcar (lambda (i)
					(let ((pos
						(list (+ state-jac-offset-row
							 (* i state-len))
						      (* i state-len))))
					  (list pos (mat-constant-mul -1  state-jacobian))))
				      (range-get 0 time-horizon)))

	 (control-jacobian-list (mapcar (lambda (i)
					  (let ((pos
						  (list (+ state-jac-offset-row
							   (* i state-len))
							(+ control-jac-offset-col
							   (* i control-len)))))
					    (list pos (mat-constant-mul -1  control-jacobian))))
					(range-get 0 time-horizon)))

	 (state-limit-list (if (eq state-limits 'nil) ()
			       (loop for i from 0 below time-horizon
				     collect
				     (let ((pos (list (+ n i)
						      (* (+ i 1) state-len))))
				       (list pos (nth 1 state-limits))))))
	 ) 
    ;;    state-limit-list
    (let ((A (reduce
	      (lambda (initial-matrix pos-smatrix)
		(destructuring-bind (pos smatrix) pos-smatrix
		  (insert-matrix initial-matrix smatrix pos)))
	      (concatenate 'list
			   state-jacobian-list
			   control-jacobian-list
			   state-limit-list)
	      :initial-value initial-matrix))
	  (L (concatenate 'list current-state
			  (alexandria:flatten 
			   (mapcar (lambda (unused)
				     (make-list  state-len :initial-element 0))
				   (range-get 0 time-horizon)))
			  (alexandria:flatten 
			   (mapcar (lambda (unused)
				     (if (and  (listp control-limits)
					       (= control-len
						  (length (car control-limits)))
					       (= control-len
						  (length (cadr control-limits))))
					 (car control-limits)
					 (make-list  control-len :initial-element -1000.0)))
				   (range-get 0 time-horizon)))
			  (make-list  time-horizon :initial-element (nth 0 state-limits))
			  ))
	  (U (concatenate 'list current-state
			  (alexandria:flatten 
			   (mapcar (lambda (unused)
				     (make-list  state-len :initial-element 0))
				   (range-get 0 time-horizon)))
			  (alexandria:flatten 
			   (mapcar (lambda (unused)
				     (if (and  (listp control-limits)
					       (= control-len
						  (length (car control-limits)))
					       (= control-len
						  (length (cadr control-limits))))
					 (cadr control-limits)
					 (make-list  control-len :initial-element +1000.0)))
				   (range-get 0 time-horizon)))
			  (make-list  time-horizon :initial-element (nth 2 state-limits))
			  )))
      (list A L U))
    ))


(defun number-grouper (numbers cols)
  (let ((array-counter (make-array cols :initial-element 0))
	(array-answer (make-array (1+ cols) :initial-element 0))
	(acc 0))
    (loop for num in numbers
	  do
	     (setf (aref array-counter num)
		   (1+ (aref array-counter num))))
    (loop for num across array-counter
	  for i from 1
	  do
	  (setf acc (+ acc num))
	  (setf (aref array-answer i) acc))
    (coerce array-answer 'list)))




(defun list-matrix->csc-list (col-matrix)
  (let ((floats '())
	(rows '())
	(cols '()))
    (loop for col in col-matrix
	  for col-no from 0
	  do
	     (loop for element in col
		   for row-no from 0
		   do
		      (if (numberp element)
			  (progn 
			    (setf floats (cons element floats))
			    (setf rows (cons row-no rows))
			    (setf cols (cons col-no cols))))))
    (list (length (nth (1- (length col-matrix))  col-matrix)) ;; no of rows
	  (length col-matrix) ;; no of columns
	  (length floats)     ;; max number of non zeros
	  (reverse floats)
	  (reverse rows)
	  (number-grouper (reverse cols) (length col-matrix)))))


(let ((thing '(1 2 3 4 5 7 9)))
  (nth (- (length thing) 1) thing))

(defun list-matrix->list-upper-triangular-matrix (list-matrix)
  (loop for column in list-matrix
	for i from 1
	collect
	(subseq column 0 i)))


(defun list-matrix->upper-tirangular-csc-list (col-matrix)
  (list-matrix->csc-list (list-matrix->list-upper-triangular-matrix col-matrix)))

(defun insert-matrix-sparse (sparse-matrix smatrix pos)
  (destructuring-bind (row col) pos
    (loop for column in smatrix
	  for i from 0
	  do
	     (loop for element in column
		   do
		      (if (not (eq element 'nil))
			  (setf (nth (+ col i) sparse-matrix)
				(append (nth (+ col i) sparse-matrix)
					(list  element))))))
    sparse-matrix))





(defun try-nth (n op-list)
  (if (>= n (length op-list))
      (car op-list)
      (nth n op-list)))

(defun get-sparse-a-l-u (time-horizon
			 state-jacobians
			 control-jacobians
			 state-symbols
			 current-state
			 control-symbols
			 &optional
			   control-limits
			   state-limits)
  
  ;; Put verify code for size of state jacobian and control jacobian here
  (let* ((state-len (length state-symbols))
	 (control-len (length control-symbols))

	 (m (+ (* (+ time-horizon 1) state-len)
	       (* time-horizon control-len)
	       (if (eq 'nil state-limits)
		   0
		   (* time-horizon 1))))
	 
	 (n (+ (* (+ time-horizon 1) state-len)
	       (* time-horizon control-len))) ;; no of cols


	 (empty (loop
		  for unused_var from 1 to n
		  collect
		  ()))

	 (state-jac-offset-row (length (car state-jacobians)))

	 (control-jac-offset-col (* (+ time-horizon 1) state-len))

	 (control-identity-row-offset  (* (1+ time-horizon) state-len))

	 (sparse-builder-list (append
			       ;; first set the first identity, to set the initial condition
			       ;; at t = 0
			       (list (list (list '(0 0) (identity-matrix-nil state-len))))

			       ;; they all set the model rules, for every single row
			       (loop for i from 0 below time-horizon
				     collect
				     (append
				      (let ((pos
					      (list (+ state-jac-offset-row
						       (* i state-len))
						    (* i state-len))))
					(list (list pos (mat-constant-mul -1 (try-nth i state-jacobians)))))
				      
				      (let ((pos
					      (list (+ state-jac-offset-row
						       (* i state-len))
						    (* (1+ i) state-len))))
					(list (list pos (identity-matrix-nil state-len))))

				      (let ((pos
					      (list (+ state-jac-offset-row
						       (* i state-len))
						    (+ control-jac-offset-col
						       (* i control-len)))))
					(list (list pos (mat-constant-mul -1 (try-nth i control-jacobians)))))
				      ))
			       ;; the identity for the limitations for Control
			       (loop for i from 0 below time-horizon
				     collect
				     (let ((pos
					     (list (+ control-identity-row-offset
						      (* i control-len))
						   (+ control-identity-row-offset
						      (* i control-len)))))
				       (list (list pos (identity-matrix-nil control-len)))))
			       ))
	 
	 
	 (state-limit-list (if (eq state-limits 'nil) ()
			       (loop for i from 0 below time-horizon
				     collect
				     (let ((pos (list (+ n i)
						      (* (+ i 1) state-len))))
				       (list (list pos (nth 1 state-limits))))))))
    
    (let ((A (alexandria:flatten
	      (reduce (lambda (initial-matrix row-operation)

			(reduce (lambda (initial pos-smatrix)
				  (destructuring-bind (pos smatrix) pos-smatrix
				    (insert-matrix-sparse initial smatrix pos)
				    ))
				row-operation
				:initial-value initial-matrix))
		      (append 
		       sparse-builder-list
		       state-limit-list)
		      :initial-value empty)))
	  (L (concatenate 'list current-state
			  (alexandria:flatten 
			   (mapcar (lambda (unused)
				     (make-list  state-len :initial-element 0))
				   (range-get 0 time-horizon)))
			  (alexandria:flatten 
			   (mapcar (lambda (unused)
				     (if (and  (listp control-limits)
					       (= control-len
						  (length (car control-limits)))
					       (= control-len
						  (length (cadr control-limits))))
					 (car control-limits)
					 (make-list  control-len :initial-element -1000.0)))
				   (range-get 0 time-horizon)))
			  (make-list  time-horizon :initial-element (nth 0 state-limits))
			  ))
	  (U (concatenate 'list current-state
			  (alexandria:flatten 
			   (mapcar (lambda (unused)
				     (make-list  state-len :initial-element 0))
				   (range-get 0 time-horizon)))
			  (alexandria:flatten 
			   (mapcar (lambda (unused)
				     (if (and  (listp control-limits)
					       (= control-len
						  (length (car control-limits)))
					       (= control-len
						  (length (cadr control-limits))))
					 (cadr control-limits)
					 (make-list  control-len :initial-element +1000.0)))
				   (range-get 0 time-horizon)))
			  (make-list  time-horizon :initial-element (nth 2 state-limits))
			  )))
      (list A L U))
    ))

