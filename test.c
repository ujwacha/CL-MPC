#include <stdlib.h>
#include "osqp.h"
#include <stdio.h>

int main(int argc, char **argv) {
    /* Load problem data */
    OSQPFloat P_x[3] = {4.0, 1.0, 2.0, };
    OSQPInt P_nnz = 3;
    OSQPInt P_i[3] = {0, 0, 1, };
    OSQPInt P_p[3] = {0, 1, 3, };
    OSQPFloat q[2] = {1.0, 1.0, };
    OSQPFloat A_x[4] = {1.0, 1.0, 1.0, 1.0, };
    OSQPInt A_nnz = 4;
    OSQPInt A_i[4] = {0, 1, 0, 2, };
    OSQPInt A_p[3] = {0, 2, 4, };
    OSQPFloat l[3] = {1.0, 0.0, 0.0, };
    OSQPFloat u[3] = {1.0, 0.7, 0.7, };
    OSQPInt n = 2;
    OSQPInt m = 3;

    /* Exitflag */
    OSQPInt exitflag = 0;

    /* Solver */
    OSQPSolver *solver;

    /* Create CSC matrices that are backed by the above data arrays. */
    OSQPCscMatrix* P = OSQPCscMatrix_new(n, n, P_nnz, P_x, P_i, P_p);
    OSQPCscMatrix* A = OSQPCscMatrix_new(m, n, A_nnz, A_x, A_i, A_p);

    /* Setup settings */
    OSQPSettings *settings = OSQPSettings_new();
    settings->alpha = 1.0; /* Change alpha parameter */

    // Setup solver
    exitflag = osqp_setup(&solver, P, q, A, l, u, m, n, settings);

    printf("exitflag: %i\n\n\n\n", exitflag);

    /* Solve problem */
    if (!exitflag) exitflag = osqp_solve(solver);

    printf("exitflag: %i\n\n\n\n", exitflag);

    printf("Solution: x1 = %f, x2 = %f\n", solver->solution->x[0], solver->solution->x[1]);
    
    /* Cleanup */
    osqp_cleanup(solver);
    OSQPCscMatrix_free(A);
    OSQPCscMatrix_free(P);
    OSQPSettings_free(settings);

    return (int)exitflag;
};
