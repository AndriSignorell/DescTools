

// Port of Monahans algorithm for Hodges-Lehman estimator
// https://dl.acm.org/doi/10.1145/1271.319414
// 
// by Cyril Flurin Moser 

// 2023-11-29


#include <Rcpp.h>

#include <iostream>
#include <vector>
#include<numeric>

using namespace std;
using namespace Rcpp;


#include <algorithm>
#include <cstdio>


void *safe_malloc(size_t n) {
  void *p = malloc(n);
  if (p == NULL) {
    stop("Fatal: failed to allocate enough memory.\n");
  }
  return p;
};

// double rng(int ixx) {
//   int a = 16807;
//   int b15 = 32768;
//   int b16 = 65536;
//   int p = 2147483647;
//   static int ix = 0;
//   if (ix == 0) {
//     ix = ixx;
//   }
//   int xhi = ix / b16;
//   int xalo = (ix - xhi * b16) * a;
//   int leftlo = xalo / b16;
//   int fhi = xhi * a + leftlo;
//   int k = fhi / b15;
//   ix = (((xalo - leftlo * b16) - p) + (fhi - k * b15) * b16) + k;
//   if (ix < 0) {
//     ix = ix + p;
//   }
//   return ((float)ix) * 4.656612875E-10;
// }
// ;


int rng(int max) //range : [0, max]
{
  // return std::rand() % ( max + 1 );
  return std::floor(R::runif(0, 1) * (max+1));
}  


// NumericVector stl_sort(NumericVector x) {
//   NumericVector y = clone(x);
//   std::sort(y.begin(), y.end());
//   return y;
// }



// [[Rcpp::export]]
double hlqest(NumericVector x) {
  
  int n = x.size();
  
  // x = stl_sort(x);
  std::sort(x.begin(), x.end(), [](double &a, double &b){ return a<b; });
  
  if (n <= 2) {  // special cases for n=1 and n=2
  if (n == 1) {
    return x[0];
  } else {
    return (x[0] + x[1]) / 2.;
  }
  }
  double retval = 0.0;
  int *lb = (int *)safe_malloc(sizeof(int) * n);
  int *rb = (int *)safe_malloc(sizeof(int) * n);
  int *q = (int *)safe_malloc(sizeof(int) * n);
  
// not needed (?):  rng(434977);  // set up random number generator
  
  int nn = n * (n + 1) / 2;  // total number of pairs
  int k1 = (nn + 1) / 2;     // median(s)
  int k2 = (nn + 2) / 2;
  
  for (int i = 0; i < n; i++) {  // probably used for indexing, therefore adjust to n-1
    lb[i] = i + 1;
    rb[i] = n;
  }
  
  int sm = nn;  // number in set s at step m
  int l = 0;    // number of pairs less than those in set s at step m
  
  int mdll = (n + 1) / 2;
  int mdlu = (n + 2) / 2;
  
  double am = x[mdll - 1] + x[mdlu - 1];
  double amx, amn;
  // Partition step: partition s0 into 2 groups: x[i] < am , x[i] >= am
  // q[i] = how many pairs x[i]+x[j] < am in row i
  
  for (;;) {  // 80
    int j = n;
    int sq = 0;
    for (int i = 1; i <= n; i++)  // go until diagonal hit, or the pairs are no longer < am
    {
      q[i - 1] = 0;
      for (; j >= i; j--) {
        if (x[i - 1] + x[j - 1] < am) {
          q[i - 1] = j - i + 1;
          sq += q[i - 1];
          break;
        }
      }
    }
    if (sq == l) {  // if consecutive partitions are the same we probably have ties
      // 30
      amx = x[0] + x[0];
      amn = x[n - 1] + x[n - 1];
      for (int i = 0; i < n; i++) {
        if (lb[i] > rb[i]) {  // skip this row if no element in it is in set s on this step
          continue;
        }
        int lbi = lb[i];
        int rbi = rb[i];
        
        double curi_x = x[i];
        amn = std::min(amn, x[lbi - 1] + curi_x);  // get the smallest in this row
        amx = std::max(amx, x[rbi - 1] + curi_x);  // get the largest in this row
      }
      // 40
      am = (amx + amn) / 2.;
      
      if ((am <= amn) || (am > amx)) {  // be careful to cut off something, roundoff can do weird things
        am = amx;
      }
      if (amn != amx && sm != 2) {  // unless finished jump to partition step
        continue;                 // goto 80
      }
      retval = am / 2.;  // all done if all of s is the same or if only 2 elements are left
      goto cleanup;
    }
    // are we nearly done, with the values we want on the border?
      if (sq == k2 - 1 || sq == k1) {  // if(we need MAX of those < am or MIN of those >= am)
        // 180
        // find: max of those < am, min of those >= am
        amn = x[n - 1] + x[n - 1];
        amx = x[0] + x[0];
        for (int i = 1; i <= n; i++) {
          int iq = q[i - 1];
          int ipiq = i + iq;
          double curi_x = x[i - 1];
          if (iq > 0) {
            amx = std::max(amx, curi_x + x[ipiq - 2]);
          }
          // Was included, but I don t see the point: ipiq = i + iq;
          if (iq < n - i + 1) {
            amn = std::min(amn, curi_x + x[ipiq - 1]);
          }
        }
        if (k1 < k2) {
          retval = (amn + amx) / 4.;
          goto cleanup;
        }
        if (sq == k1) {
          retval = amx / 2.;
          goto cleanup;
        }
        if (sq == k1 - 1) {
          retval = amn / 2.;
          goto cleanup;
        }
        retval = (amn + amx) / 4.;
        goto cleanup;
      }
    if (sq < k1) {
      // 140
      for (int j = 0; j < n; j++) {  // reset left bounds for each row
        lb[j] = j + q[j] + 1;      // no subtraction in original 1-indexed fortran code.
      }
    } else {  // sq > k1 as sq != k1
      // 120
      for (int j = 0; j < n; j++) {  // reset right bounds for each row
        rb[j] = j + q[j];
      }
    }
    // 160
    l = 0;   // number of pairs less than those in new set s
    sm = 0;  // number of pairs still in new set s
    for (int j = 1; j <= n; j++) {
      l += lb[j - 1] - j;
      sm += rb[j - 1] - lb[j - 1] + 1;
    }
    // 170
    if (sm > 2) {
      // 50
      int k = rng(sm);  // k is a random int from 0 to sm
      for (int i = 0; i < n; i++) {
        j = i + 1;
        if (k <= rb[i] - lb[i]) {
          break;
        }
        k = k - rb[i] + lb[i] - 1;
      }
      // 70
      // j is a random row, get its median
      int mdlrow = (lb[j - 1] + rb[j - 1]) / 2;
      am = x[j - 1] + x[mdlrow - 1];
      continue;  // go to 80
    } else {       // can only get to 2 left if k1 != k2, get their average
      // 30
      // use the midrange of set s as partition element when ties are likely
      // or get the average of the last 2 elements
      
      amx = x[0] + x[0];
      amn = x[n - 1] + x[n - 1];
      for (int i = 0; i < n; i++) {
        if (lb[i] > rb[i]) {  // skip this row if no element in it is in set s on this step
          continue;
        }
        int lbi = lb[i];
        int rbi = rb[i];
        
        double curi_x = x[i];
        amn = std::min(amn, x[lbi - 1] + curi_x);  // get the smallest in this row
        amx = std::max(amx, x[rbi - 1] + curi_x);  // get the largest in this row
      }
      // 40
      am = (amx + amn) / 2.;
      if ((am <= amn) || (am > amx)) {  // be careful to cut off something, roundoff can do weird things
        am = amx;
      }
      if ((amn != amx) && (sm != 2)) {  // unless finished jump to partition step
        continue;                     // goto 80
      }
      retval = am / 2.;  // all done if all of s is the same or if only 2 elements are left
      goto cleanup;
    }
  }
  
  cleanup:
    free(lb);
    free(rb);
    free(q);
  
  return retval;
}
;

