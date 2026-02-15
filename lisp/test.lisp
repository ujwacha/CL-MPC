;;; MPC Benchmark – after fixing jacobian to column‑major and get‑lambda

;; Compile symbolic primitives for speed
(compile 'variadic->binary)
(compile 'diff)
(compile 'simplify)
(compile 'jacobian)
(compile 'get-lambda)

;; System parameters (already defined, but ensure they exist)
(defparameter g -9.8d0)
(defparameter invr2 (/ 1.0d0 (* 0.1d0 0.1d0)))
(defparameter delt 0.01d0)
(defparameter states '(theta omega x v))
(defparameter controls '(a))

(defparameter *initial-state* '(0 0 0 0))

(defparameter state-transition
  `((+ theta (* omega ,delt))
    (+ omega (* ,delt (* ,invr2 (+ (* ,g (sin theta)) (* a (cos theta))))))
    (+ x (* v ,delt))
    (+ v (* a ,delt))))

;; Compile numerical Jacobian functions (column‑major output)
(defparameter *a-fn*
  (compile nil
    (get-lambda (append states controls)
                (jacobian states state-transition))))


(defparameter *b-fn*
  (compile nil
    (get-lambda (append states controls)
                (jacobian controls state-transition))))
(time 
 (progn 

   (defvar A-this (funcall *a-fn* 0 0 0 0 0))

   (defvar B-this (funcall *b-fn* 0 0 0 0 0))

   (list-matrix->csc-list (car (get-a-l-u-matrix 25
						 A-this
						 B-this
						 states
						 '(0 0 0 0)
						 controls)))))

