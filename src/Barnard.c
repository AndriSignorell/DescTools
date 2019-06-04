#include "R.h"
#include "Rmath.h"

void ScoreS(int *a, int *b, int *c, int *d, double *dp, int *mat_size, double *statistic_table, double *statistic) {
  int i,j;
  int c1  = (*a)+(*c);
  int c2  = (*b)+(*d);
  int n   = c1+c2;
  double irat = (1.0 / (double)(c1) + 1.0 / (double)(c2));
  double pxo = (double)((*a)+(*b)) / (double)(n);
  (*statistic) = (pxo<=0 || pxo>=1) ? 0 : (((double)((*b))/(double)(c2))-((double)((*a))/(double)(c1))) / sqrt(pxo*(1.0-pxo)*(irat));

  double *IJ = statistic_table;

  double tx;
  double px;
  int ccc = 0;

  for (i=0; i<=c1; i++) for (j=0; j<=c2; j++) {
      px = (double)(i+j)/(double)(n);
      tx = (px<=0 || px>=1) ? 0 : (((double)j/(double)c2)-((double)i/(double)c1)) / sqrt(px*(1.0-px)*(irat));
      IJ[ccc++] = i; IJ[ccc++] = j; IJ[ccc++] = tx; IJ[ccc] = 0;
      ccc++;
    }

  (*mat_size) = ccc;
}

void WaldS(int *a, int *b, int *c, int *d, double *dp, int *mat_size, double *statistic_table, double *statistic) {
  int i,j;
  int c1  = (*a)+(*c);
  int c2  = (*b)+(*d);
  double px1o = (double)(*a) / (double)(c1);
  double px2o = (double)(*b) / (double)(c2);
  (*statistic) = ((px1o<=0 || px1o>=1) && (px2o<=0 || px2o>=1)) ? 0 : (((double)((*b))/(double)(c2))-((double)((*a))/(double)(c1))) / sqrt( px1o*(1.0-px1o)/(double)(c1) + px2o*(1.0-px2o)/(double)(c2) );

  double *IJ = statistic_table;

  double tx;
  double px1;
  double px2;
  int ccc = 0;

  for (i=0; i<=c1; i++) for (j=0; j<=c2; j++) {
      px1 = (double)(i)/(double)(c1);
      px2 = (double)(j)/(double)(c2);
      tx = ((px1<=0 || px1>=1) && (px2<=0 || px2>=1)) ? 0 : (((double)j/(double)c2)-((double)i/(double)c1)) / sqrt( px1*(1.0-px1)/(double)(c1) + px2*(1.0-px2)/(double)(c2) );
      IJ[ccc++] = i; IJ[ccc++] = j; IJ[ccc++] = tx; IJ[ccc] = 0;
      ccc++;
    }

  (*mat_size) = ccc;
}

void Barnard(int *a, int *b, int *c, int *d, double *dp, int *mat_size, double *nuisance_vector_x, double *nuisance_vector_y0, double *nuisance_vector_y1, double *statistic_table) {
  int i,j;
  int c1  = (*a)+(*c);
  int c2  = (*b)+(*d);
  int n   = c1+c2;

  double *IJ = statistic_table;

  double n1  = lgamma(c1+1);
  double n2  = lgamma(c2+1);
  double p, ad=0;
  int k, ii;
  double ps  = 1.0+1.0/(*dp);

  for (k=0; k<ps; k++) {
    p = (double)(k)*(*dp);
    nuisance_vector_x[k] = p;
    nuisance_vector_y0[k] = 0;
    nuisance_vector_y1[k] = 0;

    for (ii=0; ii<(*mat_size); ii+=4) {
      if (!IJ[ii+3]) continue;
      i = IJ[ii];
      j = IJ[ii+1];
      ad = exp(n1+n2+(double)(i+j)*log(p)+(double)(n-i-j)*log(1.0-p)-(lgamma(i+1)+lgamma(j+1)+lgamma(c1-i+1)+lgamma(c2-j+1)));
       if (IJ[ii+3]==1) nuisance_vector_y0[k] += ad;
      nuisance_vector_y1[k] += ad;
    }
  }
}

