
//
// Port of Monahans algorithm for Hodges-Lehman estimator
//
// https://dl.acm.org/doi/10.1145/1271.319414
// https://www4.stat.ncsu.edu/~monahan/jul10/
//
// by Cyril Flurin Moser 
//
// 2023-11-29


#define STRICT_R_HEADERS
#include <Rcpp.h>
#include <R.h>

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


int rng(int max) //range : [0, max]
{
  // return std::rand() % ( max + 1 );
  return std::floor(R::runif(0, 1) * (max+1));
}  


// [[Rcpp::export]]
double hlqest(NumericVector x) {
  
  long long n = x.size();
  
  std::sort(x.begin(), x.end(), [](double &a, double &b){ return a<b; });
  
  if (n <= 2) {  // special cases for n=1 and n=2
  if (n == 1) {
    return x[0];
  } else {
    return (x[0] + x[1]) / 2.;
  }
  }
  double retval = 0.0;
  long long *lb = (long long *)safe_malloc(sizeof(long long) * n);
  long long *rb = (long long *)safe_malloc(sizeof(long long) * n);
  long long *q = (long long *)safe_malloc(sizeof(long long) * n);
  
// not needed (?):  rng(434977);  // set up random number generator
  
  long long nn = n * (n + 1) / 2;  // total number of pairs
  long long k1 = (nn + 1) / 2;     // median(s)
  long long k2 = (nn + 2) / 2;
  
  for (long long i = 0; i < n; i++) {  // probably used for indexing, therefore adjust to n-1
    lb[i] = i + 1;
    rb[i] = n;
  }
  
  long long sm = nn;  // number in set s at step m
  long long l = 0;    // number of pairs less than those in set s at step m
  
  long long mdll = (n + 1) / 2;
  long long mdlu = (n + 2) / 2;
  
  double am = x[mdll - 1] + x[mdlu - 1];
  double amx, amn;
  // Partition step: partition s0 into 2 groups: x[i] < am , x[i] >= am
  // q[i] = how many pairs x[i]+x[j] < am in row i
  
  for (;;) {  // 80
    long long j = n;
    long long sq = 0;
    for (long long i = 1; i <= n; i++)  // go until diagonal hit, or the pairs are no long longer < am
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
      for (long long i = 0; i < n; i++) {
        if (lb[i] > rb[i]) {  // skip this row if no element in it is in set s on this step
          continue;
        }
        long long lbi = lb[i];
        long long rbi = rb[i];
        
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
        for (long long i = 1; i <= n; i++) {
          long long iq = q[i - 1];
          long long ipiq = i + iq;
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
      for (long long j = 0; j < n; j++) {  // reset left bounds for each row
        lb[j] = j + q[j] + 1;      // no subtraction in original 1-indexed fortran code.
      }
    } else {  // sq > k1 as sq != k1
      // 120
      for (long long j = 0; j < n; j++) {  // reset right bounds for each row
        rb[j] = j + q[j];
      }
    }
    // 160
    l = 0;   // number of pairs less than those in new set s
    sm = 0;  // number of pairs still in new set s
    for (long long j = 1; j <= n; j++) {
      l += lb[j - 1] - j;
      sm += rb[j - 1] - lb[j - 1] + 1;
    }
    // 170
    if (sm > 2) {
      // 50
      long long k = rng(sm);  // k is a random int from 0 to sm
      for (long long i = 0; i < n; i++) {
        j = i + 1;
        if (k <= rb[i] - lb[i]) {
          break;
        }
        k = k - rb[i] + lb[i] - 1;
      }
      // 70
      // j is a random row, get its median
      long long mdlrow = (lb[j - 1] + rb[j - 1]) / 2;
      am = x[j - 1] + x[mdlrow - 1];
      continue;  // go to 80
    } else {       // can only get to 2 left if k1 != k2, get their average
      // 30
      // use the midrange of set s as partition element when ties are likely
      // or get the average of the last 2 elements
      
      amx = x[0] + x[0];
      amn = x[n - 1] + x[n - 1];
      for (long long i = 0; i < n; i++) {
        if (lb[i] > rb[i]) {  // skip this row if no element in it is in set s on this step
          continue;
        }
        long long lbi = lb[i];
        long long rbi = rb[i];
        
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
    R_Free(lb);
    R_Free(rb);
    R_Free(q);
  
  return retval;
}
;



// [[Rcpp::export]]
double hl2qest(NumericVector x, NumericVector y) {
  
  long long m = x.size();
  long long n = y.size();
  
  std::sort(x.begin(), x.end(), [](double &a, double &b){ return a<b; });
  std::sort(y.begin(), y.end(), [](double &a, double &b){ return a<b; });
  
  long long nn = m * n;
  long long k1 = (nn + 1) / 2;
  long long k2 = (nn + 2) / 2;
  
  if (n == 1) {
    return (x[k1 - 1] + x[k2 - 1]) / 2. - y[0];
  }
  if (m == 1) {
    return x[0] - (y[k1 - 1] + y[k2 - 1]) / 2.;
  }
  long long *lb = (long long *)safe_malloc(sizeof(long long) * n);
  long long *rb = (long long *)safe_malloc(sizeof(long long) * n);
  long long *q = (long long *)safe_malloc(sizeof(long long) * n);
  
  double retval = 0.0;
  double am, amx, amn;
  
  for (long long i = 0; i < n; i++) {
    lb[i] = std::max(1LL, (m + 1) - (nn + 1 - k1) / (i + 1));
    rb[i] = std::min(m, k2 / (n - i));
  }
  
  long long sm = nn;  // number in set s at step m
  long long l = 0;    // number of pairs less than those in set s at step m
  long long j = 0;
  long long sq = 0;
  
  long long method = 1;
  
  for (;;) {
    switch (method) {
    case 1:
      am = x[((m + 1) / 2) - 1] - y[((n + 1) / 2) - 1];
      break;
    case 2:
      amx = x[0] - y[n - 1];
      amn = x[m - 1] - y[0];
      for (long long i = 0; i < n; i++) {
        if (lb[i] <= rb[i]) {  // skip this row if no element in it is in set s on this step
          long long lbi = lb[i] - 1;
          amn = std::min(amn, x[lbi] - y[i]);  // get the smallest in this row
          long long rbi = rb[i] - 1;
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
//      long long k = std::rand() % sm;
      long long k = rng(sm);  // k is a random int from 0 to sm

    for (long long i = 0; i < n; i++) {
      j = i;
      if (k <= (rb[i] - lb[i]))
        break;
      k = k - rb[i] + lb[i] - 1;
    }
    long long mdlrow = (lb[j] + rb[j]) / 2 - 1;
    am = x[mdlrow] - y[j];
    break;
    }
    // Partition Step
    j = 0;
    sq = 0;
    for (long long i = 1; i <= n; i++) {
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
        for (long long i = 0; i < n; i++) {
          rb[i] = q[i];
        }
      } else {
        for (long long i = 0; i < n; i++) {
          lb[i] = q[i] + 1;
        }
      }
      l = 0;
      sm = 0;
      for (long long i = 0; i < n; i++) {
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
  for (long long i = 0; i < n; i++) {
    long long iq = q[i];
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
    R_Free(lb);
    R_Free(rb);
    R_Free(q);
  
  return retval;
}
;


