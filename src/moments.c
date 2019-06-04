
/*
  this is code for calculating skewness and kurtosis fast for larger vectors
  it will be about 15 times faster than R-code
  
  written by Andri Signorell / 1.7.2014
  
*/


#include <R.h> 
#include <Rdefines.h>


SEXP rskew(SEXP x, SEXP mean)
{

  /*
    n <- length(x)
    z3 <- 1/n * sum( (x - mean(x))^3 )
    std <- (sum( (x - mean(x))^2 )/n)^0.5
    
    # method 1: older textbooks
    r.skew <- z3/std^3 
  */


  //define some other variables
	int ii;
	double sum2 = 0, sum3 = 0, d = 0;

  //define the pointers to the variables
  double n = length(x);   // don't set that to int, we will divide afterwards
  double *xp = REAL(x);
  
  SEXP res;
  PROTECT(res = NEW_NUMERIC(1));

	REAL(res)[0] = 0.4521;
  
	//cycle through the points
	for (ii=0; ii < n; ii++) {
    d = xp[ii] - REAL(mean)[0];
    sum2 += d*d;
    sum3 += d*d*d;
	}  

  REAL(res)[0] = ((1/n * sum3) /  pow((sum2 / n), 1.5)) ;

	//return the output data
	UNPROTECT(1);
  return( res ); 

}


SEXP rkurt(SEXP x, SEXP mean)
{

  /*
  std <- (sum((x-mean(x))^2)/n)^0.5
  z4 <- 1/n*sum((x-mean(x))^4)
  r.kurt <- z4/std^4 - 3
  */

  //define some other variables
  int ii;
	double sum2 = 0, sum4 = 0, d = 0;

  //define the pointers to the variables
  double n = length(x);
  double *xp = REAL(x);
  
  SEXP res;
  PROTECT(res = NEW_NUMERIC(1));

	REAL(res)[0] = 0.4521;
  
	//cycle through the points
	for (ii=0; ii < n; ii++) {
    d = xp[ii] - REAL(mean)[0];
    sum2 += d*d;
    sum4 += d*d*d*d;
	}  

  REAL(res)[0] = ((1/n * sum4) /  pow((sum2 / n), 2)) - 3 ;

	//return the output data
	UNPROTECT(1);
  return( res ); 

}



