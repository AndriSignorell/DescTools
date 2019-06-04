
#include <R.h> 
#include <Rdefines.h>

#include <limits.h>
#include <stddef.h>


SEXP conc(SEXP y, SEXP wt2,  SEXP indx2, SEXP ntree2) {
    int i, j, k, index;
    int child, parent;
    int n, ntree;
    double *time, *status;
    double *twt, *nwt, *count;
    double vss, myrank, wsum1, wsum2, wsum3; /*sum of wts below, tied, above*/
    double lmean, umean, oldmean, newmean;
        
    double ndeath;   /* weighted number of deaths at this point */
    
    SEXP count2;
    double *wt;
    int    *indx;
    
    n = nrows(y);
    ntree = asInteger(ntree2);
    wt = REAL(wt2);
    indx = INTEGER(indx2);
    
    time = REAL(y);
    status = time + n;
    PROTECT(count2 = allocVector(REALSXP, 5));
    count = REAL(count2);  /* count5 contains the information matrix */
    twt = (double *) R_alloc(2*ntree, sizeof(double));
    nwt = twt + ntree;
    for (i=0; i< 2*ntree; i++) twt[i] =0.0;
    for (i=0; i<5; i++) count[i]=0.0;
    vss=0;

    for (i=n-1; i>=0; ) {
        ndeath =0;
        if (status[i]==1) { /* process all tied deaths at this point */
            for (j=i; j>=0 && status[j]==1 && time[j]==time[i]; j--) {
                ndeath += wt[j];
                index = indx[j];
                for (k=i; k>j; k--) count[3] += wt[j]*wt[k]; /* tied on time */
                count[2] += wt[j] * nwt[index];              /* tied on x */
                child = (2*index) +1;  /* left child */
                if (child < ntree)
                    count[0] += wt[j] * twt[child];  /*left children */
                child++;
                if (child < ntree)
                    count[1] += wt[j] * twt[child]; /*right children */
                
                while (index >0) {  /* walk up the tree  */
                    parent = (index-1)/2;
                    if (index & 1)   /* I am the left child */
                        count[1] += wt[j] * (twt[parent] - twt[index]);
                    else count[0] += wt[j] * (twt[parent] - twt[index]);
                    index = parent;
                    }
                }
            }                    
        else j = i-1;
        
        /* Add the weights for these obs into the tree and update variance*/
        for (; i>j; i--) {
            wsum1=0; 
            oldmean = twt[0]/2;
            index = indx[i];
            nwt[index] += wt[i];
            twt[index] += wt[i];
            wsum2 = nwt[index];
            child = 2*index +1;  /* left child */
            if (child < ntree) wsum1 += twt[child];

            while (index >0) {
                parent = (index-1)/2;
                twt[parent] += wt[i];
                if (!(index&1)) /* I am a right child */
                    wsum1 += (twt[parent] - twt[index]);
                index=parent;
                }
            wsum3 = twt[0] - (wsum1 + wsum2); /* sum of weights above */
            lmean = wsum1/2;
            umean = wsum1 + wsum2 + wsum3/2;  /* new upper mean */
            newmean = twt[0]/2;
            myrank = wsum1 + wsum2/2;
            vss += wsum1*(newmean+ oldmean - 2*lmean) * (newmean - oldmean);
            vss += wsum3*(newmean+ oldmean+ wt[i]- 2*umean) *(oldmean-newmean);
            vss += wt[i]* (myrank -newmean)*(myrank -newmean);
            }
        count[4] += ndeath * vss/twt[0];
        }
        
    UNPROTECT(1);
    return(count2);
}
