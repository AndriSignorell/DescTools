/* External and interal  API  of  C and Fortran routines in robustbase */

#include <R.h>


/* --------- ./qn_sn.c : -------- */
#define Sint int


/* call via .C() from R : */
void wgt_himed_i(double *x, Sint *n,  Sint *iw, double *res);
void wgt_himed  (double *x, Sint *n, double *w, double *res);

