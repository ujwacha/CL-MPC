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

(define mpc-packages  (list
		       gcc-toolchain
		       (list gcc-toolchain "debug")
		       (list gcc-toolchain "static")
		       sbcl
		       (list sbcl "doc")
		       coreutils
		       glibc
		       openssl
		       bear
		       pkg-config
		       gnu-make
		       sbcl-cl-autowrap
		       cl-autowrap
		       c2ffi
		       zlib
		       osqp
		       gdb
		       cl-asdf
		       ;; Graphics
		       gnuplot
		       ))

(packages->manifest mpc-packages)
