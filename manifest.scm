(use-modules (gnu)
	     (guix profiles)
	     (guix packages))

(use-package-modules base
		     commencement
		     pkg-config
		     build-tools
		     tls
		     maths
		     lisp
		     lisp-xyz
		     cpp
		     compression
		     gdb)

(define mpc-packages
  (list
   ;; C/C++ toolchain
   gcc-toolchain
   (list gcc-toolchain "debug")
   (list gcc-toolchain "static")
   
   ;; Lisp
   sbcl
   (list sbcl "doc")
   cl-alexandria
   cl-cffi
   cl-autowrap
   cl-asdf
   
   ;; Build tools
   coreutils
   glibc
   pkg-config
   gnu-make
   c2ffi
   bear
   
   ;; Libraries
   openssl
   zlib
   osqp
   
   ;; Debugging
   gdb
   
   ;; Graphics
   gnuplot))

(packages->manifest mpc-packages)
