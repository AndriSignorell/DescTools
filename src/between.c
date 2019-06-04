#include <R.h>
#include <Rdefines.h>



SEXP between_num_lr( SEXP x, SEXP from, SEXP to) {

  int len = Rf_length(x);
  double lower = REAL(from)[0], upper = REAL(to)[0], *xp = REAL(x);

  SEXP res = PROTECT( NEW_LOGICAL( len ) );
  int *resp = LOGICAL(res);

  for( int i=0; i < len; ++i ) {
    resp[i] = xp[i] >= lower && xp[i] <= upper;
  }

  UNPROTECT(1);
  return res;

}

SEXP between_num_lrm( SEXP x, SEXP from, SEXP to) {

  int len = Rf_length(x);
  double *lower = REAL(from), *upper = REAL(to), *xp = REAL(x);

  SEXP res = PROTECT( NEW_LOGICAL( len ) );
  int *resp = LOGICAL(res);

  for( int i=0; i < len; ++i ) {
    resp[i] = xp[i] >= lower[i] && xp[i] <= upper[i];
  }

  UNPROTECT(1);
  return res;

}



SEXP between_num_l( SEXP x, SEXP from, SEXP to) {

  int len = Rf_length(x);
  double lower = REAL(from)[0], upper = REAL(to)[0], *xp = REAL(x);

  SEXP res = PROTECT( NEW_LOGICAL( len ) );
  int *resp = LOGICAL(res);

  for( int i=0; i < len; ++i ) {
    resp[i] = xp[i] >= lower && xp[i] < upper;
  }

  UNPROTECT(1);
  return res;

}

SEXP between_num_lm( SEXP x, SEXP from, SEXP to) {

  int len = Rf_length(x);
  double *lower = REAL(from), *upper = REAL(to), *xp = REAL(x);

  SEXP res = PROTECT( NEW_LOGICAL( len ) );
  int *resp = LOGICAL(res);

  for( int i=0; i < len; ++i ) {
    resp[i] = xp[i] >= lower[i] && xp[i] < upper[i];
  }

  UNPROTECT(1);
  return res;

}



SEXP between_num_r( SEXP x, SEXP from, SEXP to) {

  int len = Rf_length(x);
  double lower = REAL(from)[0], upper = REAL(to)[0], *xp = REAL(x);

  SEXP res = PROTECT( NEW_LOGICAL( len ) );
  int *resp = LOGICAL(res);

  for( int i=0; i < len; ++i ) {
    resp[i] = xp[i] > lower && xp[i] <= upper;
  }

  UNPROTECT(1);
  return res;

}

SEXP between_num_rm( SEXP x, SEXP from, SEXP to) {

  int len = Rf_length(x);
  double *lower = REAL(from), *upper = REAL(to), *xp = REAL(x);

  SEXP res = PROTECT( NEW_LOGICAL( len ) );
  int *resp = LOGICAL(res);

  for( int i=0; i < len; ++i ) {
    resp[i] = xp[i] > lower[i] && xp[i] <= upper[i];
  }

  UNPROTECT(1);
  return res;

}




SEXP between_num_( SEXP x, SEXP from, SEXP to) {

  int len = Rf_length(x);
  double lower = REAL(from)[0], upper = REAL(to)[0], *xp = REAL(x);

  SEXP res = PROTECT( NEW_LOGICAL( len ) );
  int *resp = LOGICAL(res);

  for( int i=0; i < len; ++i ) {
    resp[i] = xp[i] > lower && xp[i] < upper;
  }

  UNPROTECT(1);
  return res;

}

SEXP between_num_m( SEXP x, SEXP from, SEXP to) {

  int len = Rf_length(x);
  double *lower = REAL(from), *upper = REAL(to), *xp = REAL(x);

  SEXP res = PROTECT( NEW_LOGICAL( len ) );
  int *resp = LOGICAL(res);

  for( int i=0; i < len; ++i ) {
    resp[i] = xp[i] > lower[i] && xp[i] < upper[i];
  }

  UNPROTECT(1);
  return res;

}















