(use-modules (gnu)
	     (guix profiles)
	     (guix packages))

(use-package-modules base
		     commencement
		     pkg-config
		     tls
		     maths
		     gdb)

(define mpc-packages  (list
		       gcc-toolchain
		       (list gcc-toolchain "debug")
		       (list gcc-toolchain "static")
		       coreutils
		       glibc
		       openssl
		       pkg-config
		       gnu-make
		       osqp
		       gdb
		       ;; Graphics
		       gnuplot
		       ))

(packages->manifest mpc-packages)
