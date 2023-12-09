
// Port of Monahans algorithm for Hodges-Lehman estimator
// https://dl.acm.org/doi/10.1145/1271.319414
// 
  // by Cyril Flurin Moser 

// 2023-11-29



#include <Rcpp.h>

#include <iostream>
#include <vector>
#include<numeric>

#include <algorithm>
#include <cstdio>
#include <cstdlib>

using namespace std;
using namespace Rcpp;

 
void *safe_malloc2(size_t n) {
  void *p = malloc(n);
  if (p == NULL) {
    stop("Fatal: failed to allocate enough memory.\n");
  }
  return p;
};



// [[Rcpp::export]]
double hl2qest(NumericVector x, NumericVector y) {
  
  int m = x.size();
  int n = y.size();
  
  std::sort(x.begin(), x.end(), [](double &a, double &b){ return a<b; });
  std::sort(y.begin(), y.end(), [](double &a, double &b){ return a<b; });
  
  int nn = m * n;
  int k1 = (nn + 1) / 2;
  int k2 = (nn + 2) / 2;
  
  if (n == 1) {
    return (x[k1 - 1] + x[k2 - 1]) / 2. - y[0];
  }
  if (m == 1) {
    return x[0] - (y[k1 - 1] + y[k2 - 1]) / 2.;
  }
  int *lb = (int *)safe_malloc2(sizeof(int) * n);
  int *rb = (int *)safe_malloc2(sizeof(int) * n);
  int *q = (int *)safe_malloc2(sizeof(int) * n);
  
  double retval = 0.0;
  double am, amx, amn;
  
  for (int i = 0; i < n; i++) {
    lb[i] = std::max(1, (m + 1) - (nn + 1 - k1) / (i + 1));
    rb[i] = std::min(m, k2 / (n - i));
  }
  
  int sm = nn;  // number in set s at step m
  int l = 0;    // number of pairs less than those in set s at step m
  int j = 0;
  int sq = 0;
  
  int method = 1;
  
  for (;;) {
    switch (method) {
      case 1:
        am = x[((m + 1) / 2) - 1] - y[((n + 1) / 2) - 1];
        break;
        case 2:
          amx = x[0] - y[n - 1];
          amn = x[m - 1] - y[0];
          for (int i = 0; i < n; i++) {
            if (lb[i] <= rb[i]) {  // skip this row if no element in it is in set s on this step
              int lbi = lb[i] - 1;
              amn = std::min(amn, x[lbi] - y[i]);  // get the smallest in this row
              int rbi = rb[i] - 1;
              amx = std::max(amx, x[rbi] - y[i]);  // get the largest in this row
            }
          }
          am = (amx + amn) / 2.;
          if ((am <= amn) || (am > amx)) {  // roundoff
            am = amx;
          }
          if ((amn == amx) || (sm == 2)) {
            retval = am;
            goto cleanup;
          }
          break;
          default:
            int k = std::rand() % sm;
          for (int i = 0; i < n; i++) {
            j = i;
            if (k <= (rb[i] - lb[i]))
              break;
            k = k - rb[i] + lb[i] - 1;
          }
          int mdlrow = (lb[j] + rb[j]) / 2 - 1;
          am = x[mdlrow] - y[j];
          break;
    }
    // Partition Step
    j = 0;
    sq = 0;
    for (int i = 1; i <= n; i++) {
      q[i - 1] = j;
      while (j < m) {
        if ((x[j] - y[i - 1]) >= am)
          break;
        j++;
      }
      q[i - 1] = j;
      sq += j;
    }
    // start branching
    if (sq == l) {
      method = 2;
    } else {
      if ((sq == (k2 - 1)) || (sq == k1))
        break;
      if (sq > k1) {
        for (int i = 0; i < n; i++) {
          rb[i] = q[i];
        }
      } else {
        for (int i = 0; i < n; i++) {
          lb[i] = q[i] + 1;
        }
      }
      l = 0;
      sm = 0;
      for (int i = 0; i < n; i++) {
        l += lb[i] - 1;
        sm += rb[i] - lb[i] + 1;
      }
      method = 3;
    }
    if (sm == 2)
      method = 2;
  }
  
  amn = x[m - 1] - y[0];
  amx = x[0] - y[n - 1];
  for (int i = 0; i < n; i++) {
    int iq = q[i];
    if (iq > 0)
      amx = std::max(amx, x[iq - 1] - y[i]);
    if (iq < m)
      amn = std::min(amn, x[iq] - y[i]);
  }
  retval = (amn + amx) / 2.;
  if (k1 < k2) {
    goto cleanup;
  } else if (sq == k1) {
    retval = amx;
  } else if (sq == k1 - 1) {
    retval = amn;
  }
  
  cleanup:
    free(lb);
    free(rb);
    free(q);
  
  return retval;
}
;
