(load "symbolic.lisp")
(load "matrix-builder.lisp")
(load "osqp.lisp")
(load "udp.lisp")


(defparameter *g*     9.8d0)   ; gravity positive
(defparameter *r*     1.0d0)    ; pendulum length
(defparameter *invr*   (/ 1.0d0 *r*))   ; 1/r
(defparameter *delt*   0.02d0)

(defparameter *states*   '(theta omega x v))
(defparameter *controls* '(a))

(defparameter *horizon* 50)

(defparameter *state-weights*
  '((10   0   0   0)
    (0    0.5   0   0)
    (0    0   10   0)
    (0    0   0   0.1)))



;; Control weights  R  (1x1)
(defparameter *control-weights*
  '((0.1)))

;; Control limits  [a_min, a_max]
(defparameter *control-limits*
  '((-50.0) (50.0)))

(defparameter *state-limits*
  (state-limit-symbolic->list
   *states*
   -70000.0
   '((+ 0))
   +70000.0)
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


(defparameter *initial-state* 'nil)
(setf *initial-state* '(0.0 0 0 0))
(defparameter *initial-control* 'nil)
(setf *initial-control* '(0))


(defvar *initial-q* nil)
(defvar *initial-p* nil)


(defun trajectory-to-state-q (current-state final-state)
  (let ((traj (loop for i from 0 to *horizon*
			collect final-state)))
	(get-q *horizon*
	       traj
	       current-state
	       '(theta omega x v)
	       '(a)
	       (get-diagonal-elements *state-weights*))))



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



(defvar *current-alu* 'nil)
(defvar *current-a* nil)
(defvar *current-l* nil)
(defvar *current-u* nil)




(defvar *target-state* nil)
(setf *target-state* '(0 0 5 0))




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




(defun initialize-mpc-system ()
  
  (setf *initial-q* 
	(trajectory-to-state-q *initial-state* *target-state*))

  (setf *initial-p*
	(crc-to-function-helper (list-matrix->upper-tirangular-csc-list
				 (get-p *horizon* *state-weights* *control-weights*))))

  
  (setf *current-alu*
	(let ((current-state-jacobian
		(apply *a-fn* (append *initial-state* *initial-control*)))
	      (current-control-jacobian
		(apply *b-fn* (append *initial-state* *initial-control*))))
	  (get-a-l-u-matrix *horizon*
			    current-state-jacobian
			    current-control-jacobian
			    *states*
			    *initial-state*
			    *controls*
			    *control-limits*
			    *state-limits*)))

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
    (set-mpc-problem dim p q a l u))

  (run-mpc-once)
  (try-converge-non-linear *horizon*))


;; (mpc-matrix-update-p (nth 3 (list-matrix->csc-list (get-p *horizon* *state-weights* *control-weights*))))


(setf *state-weights*
      '((30   0   0   0)
       (0    0.5   0   0)
       (0    0   5   0)
       (0    0   0   0.1)))


(initialize-mpc-system)


(defun mpc-next-goto (objective)
  (if (not (= (length *states*)
	      (length objective)))
      (error "Dimention of or objective wrong"))
  
  (destructuring-bind (states controls) (solved-to-proper-list
					 (solve-mpc)
					 *states*
					 *controls*
					 *horizon*)
    
    (let* ((next-state ;; (car (cdr states))
	     (apply *f-fn* (append (car states) (car controls))))
	   (next-control (car (cdr controls)))
	   (planned-traj (trajectory-to-state-q next-state
						objective))
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
      (try-converge-non-linear *horizon*)
      (mpc-matrix-update-q planned-traj)
      (list states controls))))



(defun mpc-next-goto-from (current-state objective)
  (if (not (= (length *states*)
	      (length current-state)
	      (length objective)))
      (progn
	(print (length *states*))
	(print (length current-state))
	(print (length objective))
	(error "Dimention of current state or objective wrong")))

  ;; if (equal objective *target-state*)
  (destructuring-bind (states controls) (solved-to-proper-list
					 (solve-mpc)
					 *states*
					 *controls*
					 *horizon*)
    (let* ((next-state current-state)
	   (next-control (car (cdr controls)))
	   (planned-traj (trajectory-to-state-q next-state
						objective))
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
				      *state-limits*)))
      
      (destructuring-bind (a-mat l-vec u-vec ) new-alu
	(mpc-matrix-update-alu a-mat l-vec u-vec))
      (mpc-matrix-update-q planned-traj)
      (try-converge-non-linear 6)
      (list states controls))))


;; (defun mpc-change-objective-to (current-state objective)
;;   (if (not (= (length *states*)
;; 	       (length current-state)
;; 	       (length objective)))
;;       (error "Dimention of current state or objective wrong"))
;;   (destructuring-bind (states controls) (solved-to-proper-list
;; 					 (solve-mpc)
;; 					 *states*
;; 					 *controls*
;; 					 *horizon*)
;;     (setf *target-state* objective)
    
;;     (let* ((next-state current-state)
;; 	   (planned-traj (trajectory-to-state-q next-state
;; 						objective))
;; 	   (state-control-list (mapcar (lambda (x y) (append x y))
;; 				       (cdr states) controls))
	   
;; 	   (new-jac-states (mapcar (lambda (x) (apply *a-fn* x))
;; 				   state-control-list))
	   
;; 	   (new-jac-controls (mapcar (lambda (x) (apply *b-fn* x))
;; 				     state-control-list))

;; 	   (new-alu (get-sparse-a-l-u *horizon*
;; 				      new-jac-states
;; 				      new-jac-controls
;; 				      *states*
;; 				      next-state
;; 				      *controls*
;; 				      *control-limits*
;; 				      *state-limits*)))
      
;;       (destructuring-bind (a-mat l-vec u-vec ) new-alu
;; 	(mpc-matrix-update-alu a-mat l-vec u-vec))
;;       (try-converge-non-linear *horizon*)
;;       (mpc-matrix-update-q planned-traj)
;;       (list states controls))))



(defvar *my-obj* nil)
(setf *my-obj* *target-state*)

(socket-clean)
(socket-open-udp "localhost" 9999)

(send-socket-data (floats->bytes '(-0.00)))

(kill-recieve-thread)
(make-recieve-theread)
(read-udp-message)

(loop while (not *initial-state*)
      do
	 (setf *initial-state* (read-udp-message)))
(setf *initial-state* (read-udp-message))

(setf *initial-control* '(0))

(initialize-mpc-system)
(try-converge-non-linear *horizon*)

(defvar *run-hz* 50)

(mpc-next-goto-from (read-udp-message)
	       *my-obj*)

;; (read-udp-message)

(defvar *run-stuff* 'nil)


(defun mpc-loop ()
  (print "runnnn")
  (loop while *run-stuff*
	do
	   (destructuring-bind (predicted-trajectory predicted-control)
	       (let ((this-state (read-udp-message)))
		 (print "state: ")
		 (print this-state)
		 (mpc-next-goto-from this-state *my-obj*)
		 )
	     (print "control: ")
	     (print (car predicted-control))
	     (print (car predicted-trajectory))
	     (if (> (car (car  predicted-control)) 100000)
		 (error "we got bad value"))
	     (send-socket-data (floats->bytes
				(car predicted-control)))
	     )
	   (sleep 0.02)

	))

(setf *run-stuff* 1)

(mpc-loop)

(all-c-vars-free)

(initialize-mpc-system)

(defvar *mpc-loop-thread* nil)


(socket-clean)
(kill-recieve-thread)
(setf *message-recieve-thread* nil)

;; *state-weights*
;; (setf *my-obj* '(0 0 2 0))


;; (mpc-matrix-update-q 

;;  (trajectory-to-state-q '(0 0 25 0)
;; 			'(0 0 5 


;; (setf *state-limits*
;;       (state-limit-symbolic->list
;;        *states*
;;        -1.1
;;        '((+ theta))
;;        +1.1))


;; (setf *control-limits*
;;       '((-200.0) (200.0)))
