
(defpackage :mpc-control.main
  (:use :cl 
        :mpc-control.matrix-builder
        :osqp-ffi)
  (:export :main
           :start-controller
           :stop-controller
           :*solver*))

(in-package :mpc-control.main)

(defun main ()
  ;; Your MPC control loop here
  (format t "MPC Controller Started~%")
  ;; ... actual control code ...
  (sb-ext:quit))
