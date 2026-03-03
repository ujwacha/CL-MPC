(defparameter *g*     9.8d0)   ; gravity positive
(defparameter *r*     0.1d0)    ; pendulum length
(defparameter *invr*   (/ 1.0d0 *r*))   ; 1/r
(defparameter *delt*   0.02d0)

(defparameter *states*   '(theta omega x v))
(defparameter *controls* '(a))

(defparameter *horizon* 50)

(defparameter *state-weights*
  '((10   0   0   0)
    (0    0.5   0   0)
    (0    0   3   0)
    (0    0   0   0.1)))

;; Control weights  R  (1x1)
(defparameter *control-weights*
  '((0.1)))

;; Control limits  [a_min, a_max]
(defparameter *control-limits*
  '((-30.0) (30.0)))

(defparameter *state-limits*
  (state-limit-symbolic->list
   *states*
   -0.5
   '((+ theta))
   +0.5)
  )

;; State transition (symbolic, delt/g/invr2 spliced in at load time)
(defparameter *state-transition*
  `((+ theta (* omega ,*delt*))
    (+ omega (* ,*delt* (* ,*invr* (- (* ,*g* (sin theta)) (* a (cos theta))))))
    (+ x (* v ,*delt*))
    (+ v (* a ,*delt*))))


(defun get-transition-lambda (vars function-list)
  `(lambda (,@vars) (list ,@function-list)))


(defparameter *f-fn*
  (compile nil
	   (get-transition-lambda (append *states* *controls*)
				  *state-transition*)))

;; Compile numerical Jacobian functions
(defparameter *a-fn*
    (compile nil
	     (get-lambda (append *states* *controls*)
			 (jacobian *states* *state-transition*))))

(defparameter *b-fn*
  (compile nil
    (get-lambda (append *states* *controls*)
                (jacobian *controls* *state-transition*))))


(defparameter *current-state* '(-0.3 0 5 0))
(defparameter *current-control* '(0))


(jacobian *states* *state-transition*)
(jacobian *controls* *state-transition*)


(apply *a-fn* (append *current-state* *current-control*))

;; now make initial matrices for the MPC
;; make Q vector


(defun trajectory-to-state-q (current-state final-state)
  (let ((traj (loop for i from 0 to *horizon*
			collect final-state)))
	(get-q *horizon* traj current-state '(theta omega x v) '(a))))


(defvar *initial-q* nil)

(setf *initial-q* 
      (trajectory-to-state-q *current-state* '(0 0 2.5 0)))



(defvar *initial-p* nil)

(defun crc-to-function-helper (crc-list)
  (let ((output crc-list))
    (loop for i from 3 below 6
	  collect
	  (nth i output))))



(defun group (list n)
  (if (= 0 (mod (length list) n))
      (loop while list
	    collect (loop repeat n
			  while list
			  collect (pop list)))
      (error "Size Not valid for grouping")))



(defun solved-to-proper-list (output states controls horizon)
  (let* ((state-len (length states))
	 (control-len (length controls))
	 (upto-state (* state-len (1+ horizon)))
	 (state-seq (subseq output 0 upto-state))
	 (control-seq (subseq output upto-state (length output)))
	 (states-values (group state-seq state-len))
	 (controls-values (group control-seq control-len)))
    (list states-values controls-values)))

(setf *initial-p*
      (crc-to-function-helper (list-matrix->upper-tirangular-csc-list
			       (get-p *horizon* *state-weights* *control-weights*))))





(apply *b-fn* (append *current-state* *current-control*))

(let ((current-state-jacobian
	(apply *a-fn* (append *current-state* *current-control*)))
      (current-control-jacobian
	(apply *b-fn* (append *current-state* *current-control*))))
  (list-matrix->csc-list
   (car 
    (get-a-l-u-matrix *horizon*
		      current-state-jacobian
		      current-control-jacobian
		      *states*
		      *current-state*
		      *controls*))))


(defvar *current-alu* 'nil)

(setf *current-alu*
  (let* ((current-state-jacobian
	   (apply *a-fn* (append *current-state* *current-control*)))
	 (current-control-jacobian
	   (apply *b-fn* (append *current-state* *current-control*)))
	 (alu-matrix 
	   (get-a-l-u-matrix *horizon*
			     current-state-jacobian
			     current-control-jacobian
			     *states*
			     *current-state*
			     *controls*
			     *control-limits*
			     *state-limits*
			     )))
    alu-matrix))



(defvar *current-a* nil)
(defvar *current-l* nil)
(defvar *current-u* nil)



(destructuring-bind (a l u) *current-alu*
  (setf *current-a* a)
  (setf *current-l* l)
  (setf *current-u* u))


(let* ((n (length *current-a*))
       (m (length (car *current-a*)))
       (dim (list m n))
       (p *initial-p*)
       (q *initial-q*)
       (a  (crc-to-function-helper
	    (list-matrix->csc-list *current-a*)))
       (l *current-l*)
       (u *current-u*))
  (print "setting up mpc problem")
  (print m)
  (print n)
  (set-mpc-problem dim
		   p
		   q
		   a
		   l
		   u))





(defun run-mpc-once () 
  (destructuring-bind (states controls)
      (solved-to-proper-list (solve-mpc)
			     *states*
			     *controls*
			     *horizon*)
    (let ((new-state (car states))
	  (arguments (mapcar (lambda (x y) (append x y))
			     states
			     controls)))
      
      (list
       new-state
       (mapcar (lambda (x) (apply *a-fn* x)) arguments) 
       (mapcar (lambda (x) (apply *b-fn* x)) arguments)))))



(defun try-converge-non-linear (n) 
  (loop for i from 0 below n
	do
	   (destructuring-bind (new-state state-jacs control-jacs) (run-mpc-once)
	     (destructuring-bind (a-upd l-upd u-upd)  (get-sparse-a-l-u
						       *horizon*
						       state-jacs
						       control-jacs
						       *states*
						       new-state
						       *controls*
						       *control-limits*
						       *state-limits*)
	       (mpc-matrix-update-alu a-upd l-upd u-upd)
	       ))))


(try-converge-non-linear *horizon*)

;; (run-mpc-once)

(loop for times from 0 to 600
      do
	 (destructuring-bind (states controls) (solved-to-proper-list
						(solve-mpc)
						*states*
						*controls*
						*horizon*)
	   
	   (let* ((next-state ;; (car (cdr states))
			      (apply *f-fn* (append (car states) (car controls))))
		  (next-control (car (cdr controls)))
		  (planned-traj (trajectory-to-state-q next-state
						       '(0 0 5.5 0)))
		  (state-control-list (mapcar (lambda (x y) (append x y))
					      (cdr states) controls))
		  
		  (new-jac-states (mapcar (lambda (x) (apply *a-fn* x))
					  state-control-list))
		  
		  (new-jac-controls (mapcar (lambda (x) (apply *b-fn* x))
					    state-control-list))

		  (new-alu (get-sparse-a-l-u *horizon*
					     new-jac-states
					     new-jac-controls
					     *states*
					     next-state
					     *controls*
					     *control-limits*
					     *state-limits*))
		  )
	     (destructuring-bind (a-mat l-vec u-vec ) new-alu
	       (mpc-matrix-update-alu a-mat l-vec u-vec))
	     (try-converge-non-linear 1)
	     (mpc-matrix-update-q planned-traj))))


(solved-to-proper-list (solve-mpc)
		       *states*
		       *controls*
		       *horizon*)

