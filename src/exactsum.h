/* Written by Mikko Korpela. */
#ifndef EXACTSUM_H
#define EXACTSUM_H

#include <R.h>  /* to include Rconfig.h */
#include <Rversion.h>
#include <Rinternals.h>
size_t dplRlength(SEXP x);
          
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("dplR", String)
#else
#define _(String) (String)
#define dngettext(pkg, String, StringP, N) (N > 1 ? StringP: String)
#endif

#if defined(R_VERSION) && R_VERSION >= R_Version(3, 0, 0)
#define DPLR_RGEQ3
#endif


/* Conditional typedef of dplr_double */
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
#include <math.h>
typedef double_t dplr_double;
/*
  "C9X also requires that the <math.h> header file define the types
  float_t and double_t, which are at least as wide as float and
  double, respectively, and are intended to match the types used to
  evaluate float and double expressions.  For example, if
  FLT_EVAL_METHOD is 2, both float_t and double_t are long double."

  From "Differences Among IEEE 754 Implementations" by Doug Priest.
  Available at http://www.validlab.com/goldberg/addendum.html,
  referenced on 2010-10-18.

  The use of double_t here is intended to be a portable way to avoid
  the loss of exactness in the exact summation algorithm due to
  double-rounding errors caused by the mix of 80-bit internal storage
  and 64-bit memory storage of doubles on the 32-bit x86 platform (*).
  This requires a C99 compiler (also required by R >= 2.12.0).  On
  architectures that evaluate doubles with double precision, double_t
  should be equivalent to double.  On 32-bit x86 without SSE2 (and
  math coprocessor in 80-bit mode), double_t should be equivalent to
  long double.

  (*) Loss of precision because of double-rounding may still occur
  when the result of the msum function is returned to R.
*/
#else
typedef long double dplr_double;
/*
  In case of an old / pre-C99 compiler that does not define double_t,
  dplr_double is defined as long double.  This is designed for exact
  computations on 32-bit x86, and also works fine on x86-64 (see
  Footnote).  On some architectures / compilers, long double may be
  the same as double.  On architectures where long double does not
  conform to IEEE floating-point standards, non-exactness of the msum
  function may result.

  Footnote on the performance of 64-bit vs 80-bit FP math on x86-64
  =================================================================

  On x86-64, one might guess SSE2 would give a speed gain in cases
  where 80-bit precision is not really needed, like here.  However,
  (non-rigorous) tests showed double_t (64-bit) to have a running time
  penalty compared to long double (80-bit).  The test was to compute
  exactmean for a uniformly random vector of length 1e8 (once or
  repeatedly).  In the table, penalty (% of user time) has one
  significant digit.  R version probably does not matter, because we
  are dealing with compiled code.

  OS        Processor      R version  Penalty (%)  Notes
  ---------------------------------------------------------------------
  Linux     AMD Athlon II  2.12.0               5  -O2 -march=barcelona
  Linux     Intel Core 2   2.9.2               30
  Mac OS X  Intel Core 2   2.11.1              20

  Maybe future compilers or processors will fare better with SSE2. In
  the meantime, the performance penalty of SSE2 is small enough.  The
  C99 compiler case above uses double_t, which I believe to be the
  most elegant and portable solution.
*/
#endif

/* A linked list for storing dplr_doubles */
struct liststruct{
    Rboolean valid;
    dplr_double data;
    struct liststruct *next;
};
typedef struct liststruct listnode;

/*
  Compute the sum of an array of numbers.  This function uses an exact
  procedure adapted and from
  http://code.activestate.com/recipes/393090/ which itself is based on
  the 1996 article "Adaptive Precision Floating-Point Arithmetic and
  Fast Robust Geometric Predicates" by J. R. Shewchuk, available at
  http://www-2.cs.cmu.edu/afs/cs/project/quake/public/papers/robust-arithmetic.ps
  A journal version of the paper is in Discrete Comput Geom 18:305-363
  (1997).

  N.B.: The exactness of the result will depend on the compiler not
  optimizing away some operations because of symbolic identities.
  This may depend on compiler options.  Exactness will also require
  that floating point operations employ exact rounding.  See the
  comments related to dplr_double above.

  Input:
  - array     Array of numbers to be summed
  - n         Length of array
  - expansion Temporary work space. Must be a pointer to a valid location.
  Using a list is more economical than using a full, length n
  array, because the list usually only has a handful of elements.
  Output: the sum of the numbers
*/
dplr_double msum(double *array, size_t n, listnode *expansion);

/* Cumulative sum, overwrites array */
dplr_double cumsum(double *array, size_t n, listnode *expansion);

/* Add number a to the sum represented by expansion */
void grow_exp(listnode *expansion, dplr_double a);

#endif


