#ifndef DISTRIBUTION_H
#define DISTRIBUTION_H

#include <Rcpp.h>

namespace {

  inline double below_distribution(bool lower_tail, bool give_log) {
    if (lower_tail) {
      if (give_log) {return R_NegInf;} else {return 0;}
    } else {
      if (give_log) {return 0;} else {return R_NegInf;}
    } 
  }

  template
  <typename T1>
  inline
  Rcpp::NumericVector perhaps_exp(const T1& y, bool log) {
    if (log) {
      return y;
    } else {
      return Rcpp::exp(y);
    }
  }
  
}

#endif
