/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2006--2007	the R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Used to be part of ./qn_sn.c

 Note by MM: We have explicit permission from P.Rousseeuw to
 licence it under the GNU Public Licence.

 See also ../inst/Copyrights
*/
#include <inttypes.h>
/*        ^^^^^^^^^^ is supposedly more common and standard than
 * #include <stdint.h>
 * or #include <sys/types.h> */
/* --> int64_t ; if people don't have the above, they can forget about it.. */
/* #include "int64.h" */

#include <Rmath.h> /* -> <math.h> and much more */

#include "robustbase.h"

// whimed() and whimed_i()  function called from C : in ./mc.c , ./qn_sn.c :
#define _i_whimed_
#include "wgt_himed_templ.h"

#define _d_whimed_
#include "wgt_himed_templ.h"

/* Interface routines to be called via .C() : */

void wgt_himed_i(double *x, Sint *n, Sint *iw, double *res)
{
    double *a_srt, *acand;
    int *iw_cand, nn = (int)*n;
    char *vmax;

    vmax = vmaxget();
    acand  = (double *)R_alloc(nn, sizeof(double));
    a_srt  = (double *)R_alloc(nn, sizeof(double));
    iw_cand= (int *)   R_alloc(nn, sizeof(int));

    *res = whimed_i(x, (int *)iw, nn, acand, a_srt, iw_cand);
    vmaxset(vmax);
}

void wgt_himed(double *x, Sint *n, double *w, double *res)
{
    double *a_srt, *a_cand, *w_cand;
    int nn = (int)*n;
    char *vmax;

    vmax = vmaxget();
    a_cand = (double *) R_alloc(nn, sizeof(double));
    a_srt  = (double *) R_alloc(nn, sizeof(double));
    w_cand = (double *) R_alloc(nn, sizeof(double));

    *res = whimed(x, w, nn, a_cand, a_srt, w_cand);
    vmaxset(vmax);
}
