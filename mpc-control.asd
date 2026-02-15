(asdf:defsystem "mpc-control"
  :description "MPC Controller with OSQP and symbolic differentiation"
  :version "0.1"
  :author "Ujwol Acharya"
  :license "GPL"
  :depends-on ("alexandria" "cffi" "cl-autowrap")
  :components ((:module "lisp"
                :pathname "lisp/"
                :components
                ((:file "osqp")
                 (:file "symbolic")
                 (:file "matrix-builder" :depends-on ("symbolic"))
                 (:file "main" :depends-on ("matrix-builder" "osqp"))))))
