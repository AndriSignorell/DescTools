#include <R.h>
#include <Rmath.h>
/* Fortran function for Wilcoxon-Mann-Whitney PDF */
double F77_SUB(fdwilcox)(double *i, double *m, double *n) { return dwilcox(*i, *m, *n, 0); }
