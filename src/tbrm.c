#include <limits.h>
#include <stddef.h>
/* #include "dplR.h" */

#include "exactsum.h"

/* Tukey's Biweight Robust Mean (tbrm).
   There must be no NAs in 'x'.
   
   Input:
   - x   Array of numbers to be summarized by tbrm (double)
   - C   Parameter C which adjusts the scaling of the data (double, length 1)
   Output: numeric vector of length 1

   Written by Mikko Korpela.
*/

size_t dplRlength(SEXP x) {
    size_t xlength;
    SEXP sn, tmp, ncall;
    PROTECT_INDEX ipx;
    PROTECT(tmp = ncall = allocList(2));
    SET_TYPEOF(ncall, LANGSXP);
    SETCAR(tmp, install("length")); tmp = CDR(tmp);
    SETCAR(tmp, x);
    PROTECT_WITH_INDEX(sn = eval(ncall, R_BaseEnv), &ipx);
    REPROTECT(sn = coerceVector(sn, REALSXP), ipx);
    xlength = (size_t) *REAL(sn);
    UNPROTECT(2);
    return xlength;
}


SEXP tbrm(SEXP x, SEXP C){
    SEXP ans, C2;
    Rboolean n_odd;
    int i, half, my_count, n;
    size_t nlong;
    double C_val, this_val, min_val, div_const, x_med, this_wt;
    double *x2, *abs_x_dev, *wt, *wtx, *x_p;
    listnode tmp;
    nlong = dplRlength(x);

    /* Long vectors not supported (limitation of rPsort) */
    if (nlong > INT_MAX) {
	error(_("long vectors not supported"));
    }
    C2 = PROTECT(coerceVector(C, REALSXP));
    if (length(C2) != 1) {
	UNPROTECT(1);
	error(_("length of 'C' must be 1"));
    }
    C_val = REAL(C2)[0];
    UNPROTECT(1);
    n = (int) nlong;
    ans = PROTECT(allocVector(REALSXP, 1));
    /* Avoid complexity and possible crash in case of empty input
     * vector */
    if(n == 0){
	REAL(ans)[0] = R_NaN;
	UNPROTECT(1);
	return ans;
    }
    /* Note: x must be a numeric vector */
    x_p = REAL(x);

    /* x2 is a copy of the data part of argument x */
    x2 = (double *) R_alloc(n, sizeof(double));
    for(i = 0; i < n; i++)
	x2[i] = x_p[i];

    /* Median of x */
    if((n & 0x1) == 1){ /* n is odd */
	half = ((unsigned int)n) >> 1;
	rPsort(x2, n, half); /* Partial sort: */
	x_med = x2[half];    /* element at position half is correct.*/
	n_odd = TRUE;
    } else { /* n is even */
	half = ((unsigned int)n) >> 1;
	rPsort(x2, n, half-1);       /* Elements at positions half-1 */
	min_val = x2[half];
	for(i = half+1; i < n; i++){/* and half */ 
	    this_val = x2[i];        /* (minimum in the */
	    if(this_val < min_val)  /* "larger than" side) */
		min_val = this_val;
	}
	x_med = (x2[half-1]+min_val)/2.0f; 
	n_odd = FALSE;
    }

    /* abs(x - median(x)) */
    abs_x_dev = (double *) R_alloc(n, sizeof(double));
    for(i = 0; i < n; i++){
	this_val = x2[i]-x_med;
	abs_x_dev[i] = this_val<0 ? -this_val : this_val;
    }

    /* Median of abs_x_dev, stored in div_const */
    if(n_odd == TRUE){
	rPsort(abs_x_dev, n, half); /* Element at position half */
	div_const = abs_x_dev[half];
    } else {
	rPsort(abs_x_dev, n, half-1); /* Elements at positions half-1 */
	min_val = abs_x_dev[half];
	for(i=half+1; i<n; i++){  /* and half */
	    this_val = abs_x_dev[i];
	    if(this_val < min_val)
		min_val = this_val;
	}
	div_const = (abs_x_dev[half-1]+min_val)/2.0f;
    }
    /* This is a normalization constant (well, constant over x2[i]) */
    div_const = div_const * C_val + 1e-6;

    /* Number of values x2[i] with non-zero weights */
    my_count = 0;

    /* Recycling memory, i.e. renaming the same space */
    wt = abs_x_dev;
    wtx = x2; /* Have to be careful not to overwrite too soon */

    /* Weights (wt) and weighted data (wtx) */
    for(i = 0; i < n; i++){
	this_wt = (x2[i]-x_med) / div_const;
	if(this_wt >= -1.0f && this_wt <= 1.0f){ /* absolute value <= 1 */
	    this_wt = 1.0f - this_wt * this_wt;
	    this_wt *= this_wt;
	    wt[my_count] = this_wt;
	    wtx[my_count++] = this_wt * x2[i];
	}
    }

    /* Important!
       Sum of my_count values. No more, no less.
       The tails of the arrays are now garbage, not harmlessly zero. */
    if(my_count == 1){ /* Avoid call to sum function in border case */
	REAL(ans)[0] = wtx[0] / wt[0];
    } else if(my_count > 0){
	/* Setup for msum. */
	tmp.next = NULL;
	/* Not the usual 'sum of data divided by sum of ones' */
	REAL(ans)[0] = msum(wtx, my_count, &tmp) / msum(wt, my_count, &tmp);
    } else{ /* Nothing to sum */
	REAL(ans)[0] = R_NaN;
    }
    UNPROTECT(1);
    return ans;
}
