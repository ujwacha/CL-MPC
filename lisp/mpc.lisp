;;;; mpc.lisp  --  MPC control loop for inverted pendulum
;;;;
;;;; Ties together: symbolic.lisp, matrix-builder.lisp, osqp.lisp, udp.lisp
;;;;
;;;; State vector : [theta  omega  x  v]
;;;; Control vector: [a]
;;;; Goal          : stabilize theta -> 0 (upright)

;; ── Parameters ───────────────────────────────────────────────────────────────

(defparameter *g*     -9.8d0)
(defparameter *r*      0.1d0)
(defparameter *invr2*  (/ 1.0d0 (* *r* *r*)))
(defparameter *delt*   0.02d0)   ;; matches 50Hz from python sim

(defparameter *states*   '(theta omega x v))
(defparameter *controls* '(a))

(defparameter *horizon* 15)   ;; MPC horizon N

;; State weights  Q  (4x4 diagonal)  -- penalise theta heavily
(defparameter *state-weights*
  '((10   0   0   0)
    (0    1   0   0)
    (0    0   1   0)
    (0    0   0   0.1)))

;; Control weights  R  (1x1)
(defparameter *control-weights*
  '((0.1)))

;; Control limits  [a_min, a_max]
(defparameter *control-limits*
  '((-30.0) (30.0)))

;; State transition (symbolic, delt/g/invr2 spliced in at load time)
(defparameter *state-transition*
  `((+ theta (* omega ,*delt*))
    (+ omega (* ,*delt* (* ,*invr2* (+ (* ,*g* (sin theta)) (* a (cos theta))))))
    (+ x (* v ,*delt*))
    (+ v (* a ,*delt*))))

;; Compile numerical Jacobian functions
(defparameter *a-fn*
  (compile nil
    (get-lambda (append *states* *controls*)
                (jacobian *states* *state-transition*))))

(defparameter *b-fn*
  (compile nil
    (get-lambda (append *states* *controls*)
                (jacobian *controls* *state-transition*))))

;; ── MPC setup ─────────────────────────────────────────────────────────────────

(defun eval-jacobians (state control)
  "Evaluate A and B matrices at current state+control."
  (destructuring-bind (theta omega x v) state
    (let ((a-val (or (car control) 0.0)))
      (values
       (funcall *a-fn* theta omega x v a-val)
       (funcall *b-fn* theta omega x v a-val)))))

(defun make-trajectory (horizon target-state)
  "Constant reference trajectory - all steps aim at target."
  (loop repeat horizon collect target-state))

(defun setup-mpc (initial-state)
  "Build and hand the initial QP to OSQP. Call once before the loop."
  (multiple-value-bind (A-jac B-jac)
      (eval-jacobians initial-state '(0.0))

    (let* ((trajectory (make-trajectory *horizon* '(0 0 0 0)))
           (P (get-p *horizon* *state-weights* *control-weights*))
           (Q (get-q *horizon* trajectory initial-state *states* *controls*))
           (alu (get-a-l-u-matrix *horizon*
                                  A-jac
                                  B-jac
                                  *states*
                                  initial-state
                                  *controls*
                                  *control-limits*))

           (A-mat (nth 0 alu))
           (L-vec (nth 1 alu))
           (U-vec (nth 2 alu))

           (p-csc (cdr (list-matrix->upper-tirangular-csc-list P)))  ;; drop row/col/nnz header, keep x i p
           (a-csc (cdr (list-matrix->csc-list A-mat)))

           ;; dimensions
           (n-vars (+ (* (1+ *horizon*) (length *states*))
                      (* *horizon* (length *controls*))))
           (m-cons (length L-vec)))

      (set-mpc-problem
       (list m-cons n-vars)
       ;; P: upper triangular CSC  (x  i  p)
       (list (nth 0 p-csc) (nth 1 p-csc) (nth 2 p-csc))
       ;; q
       Q
       ;; A  (x  i  p)
       (list (nth 0 a-csc) (nth 1 a-csc) (nth 2 a-csc))
       ;; l  u
       L-vec
       U-vec))))


(defun update-and-solve (current-state last-control)
  "Update A/L/U with current linearisation and solve. Returns first control."
  (multiple-value-bind (A-jac B-jac)
      (eval-jacobians current-state (list last-control))

    (let* ((alu (get-a-l-u-matrix *horizon*
                                  A-jac
                                  B-jac
                                  *states*
                                  current-state
                                  *controls*
                                  *control-limits*))
           (A-mat (nth 0 alu))
           (L-vec (nth 1 alu))
           (U-vec (nth 2 alu))

           (a-csc      (list-matrix->csc-list A-mat))
           (a-x-values (nth 3 a-csc)))   ;; non-zero floats only

      (mpc-matrix-update-alu a-x-values L-vec U-vec)

      (let* ((solution (solve-mpc))
             ;; first control input is at index (N+1)*n_states
             (ctrl-offset (* (1+ *horizon*) (length *states*)))
             (a-optimal  (nth ctrl-offset solution)))
        (or a-optimal 0.0)))))


;; ── Control loop ──────────────────────────────────────────────────────────────

(defvar *mpc-running* nil)
(defvar *mpc-thread*  nil)
(defvar *last-control* 0.0)

(defun mpc-loop ()
  "Main control loop. Blocks until *mpc-running* is nil."
  (format t "[MPC] Waiting for first state message...~%")
  ;; wait until we get a real state
  (loop until (and (read-udp-message)
                   (= 4 (length (read-udp-message))))
        do (sleep 0.01))

  (let ((state (read-udp-message)))
    (format t "[MPC] Got initial state ~a, setting up QP...~%" state)
    (setup-mpc state))

  (format t "[MPC] QP ready, entering control loop at ~aHz~%" (round (/ 1.0 *delt*)))

  (loop while *mpc-running* do
    (let ((state (read-udp-message)))
      (if (and (listp state) (= (length state) 4))
          (handler-case
              (let ((a (update-and-solve state *last-control*)))
                (setf *last-control* a)
                (send-socket-data (floats->bytes (list (coerce a 'single-float))))
                ;; Optional: print every ~50 steps
                )
            (error (e)
              (format t "[MPC] Solver error: ~a~%" e)))
          (format t "[MPC] Bad state: ~a~%" state)))
    (sleep *delt*)))


(defun start-mpc ()
  "Open UDP socket and start the MPC thread."
  (when *mpc-thread*
    (stop-mpc))
  ;; fix: open socket properly (original code had if/setf swapped)
  (when (eq *connection-socket* 'nil)
    (setf *connection-socket*
          (usocket:socket-connect "localhost" 9999
                                  :protocol :datagram)))
  (make-recieve-theread)
  (setf *mpc-running* t)
  (setf *mpc-thread*
        (bt:make-thread #'mpc-loop :name "mpc-control-loop"))
  (format t "[MPC] Started.~%"))


(defun stop-mpc ()
  "Stop the MPC loop and clean up."
  (setf *mpc-running* nil)
  (when *mpc-thread*
    (bt:destroy-thread *mpc-thread*)
    (setf *mpc-thread* nil))
  (kill-recieve-thread)
  (socket-clean)
  (all-c-vars-free)
  (format t "[MPC] Stopped.~%"))
