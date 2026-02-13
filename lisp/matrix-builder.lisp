(defvar A '((1 3) (2 4)))

(defvar B '((5 7) (6 8)))

;; the state and column don't mean anything, they are just symbols
(defvar state '(x v))
(defvar control '(f a))



;; firstly let's define horizon
(defvar N 2)

;; let's make the cost function
;; let's not worry about V here,
(defun get-trajectory(N)
  (mapcar (lambda (x) `(,x 0)) (range-get 1 (+ N 1))))


(defun get-Q (trajectory initial-state control-size)
  (mapcar (lambda (x) `(,x)) ;; this mapcar has to be done because column thing
	  (alexandria:flatten
	   (concatenate trajectory)))) 

(get-q (get-trajectory 2)) 


(defun get-p (horizon-length state-weights control-weights &optional prop-lambda-state prop-lambda-conrtol)
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
		  (get-number-list 0 n))))


    ;; verify the good stuff
    ;; the state and control weights should be square matrices

    ;;    state-weights-list

    (if (not (or (eq (length state-weights) (length (car state-weights)))
		 (eq (length control-weights) (length (car control-weights)))))
	(error "State weights and Control weights should both be square matrices")
	(reduce
	 (lambda (empty pos-smatrix)
	   (destructuring-bind (pos smatrix) pos-smatrix
	     (insert-matrix empty smatrix pos)))
	 (concatenate 'list state-weights-list control-weights-list) :initial-value empty))))

(defun insert-matrix (big-matrix small-matrix pos)
  "Insert small-matrix into big-matrix at position (row, col).
   Both matrices are in column-major format: ((col0) (col1) ...)
   where each inner list is a COLUMN (top to bottom)."
  (destructuring-bind (start-row start-col) pos
    (loop with n-rows = (length (first big-matrix))    ; number of rows
          with n-cols = (length big-matrix)            ; number of cols
          for small-col from 0 below (length small-matrix)
          for big-col = (+ start-col small-col)
          when (< big-col n-cols)
          do (let ((small-column (nth small-col small-matrix))
                   (big-column (nth big-col big-matrix)))
               (loop for small-row from 0 below (length small-column)
                     for big-row = (+ start-row small-row)
                     when (< big-row n-rows)
                     do (setf (nth big-row big-column)
                              (nth small-row small-column)))))
    big-matrix))


(get-p 2 '((1 0) (0 3)) '((2 0) (0 4)) (lambda (i y) (* (exp (* 0.1  (- i)))  y))
       )




(get-trajectory 2)

(defun range-get (l g) 
  (loop for i from l below g collect i))

(defun get-number-list (number size)
  (loop for i from 0 below size collect number))





()


