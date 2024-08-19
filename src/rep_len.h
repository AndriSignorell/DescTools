#ifndef REPLEN_H
#define REPLEN_H

#define STRICT_R_HEADERS
#include <Rcpp.h>

namespace {
  namespace flexsurv {
    
    template <int RTYPE, bool NA, typename T>
    inline Rcpp::sugar::Rep_len<RTYPE,NA,T>
    rep_len(const Rcpp::VectorBase<RTYPE,NA,T>& t, R_xlen_t len ){
      
      if (t.size() == 0) {
	Rcpp::stop("zero length vector provided");
      } else {
	return Rcpp::rep_len(t, len);
      }

    } 

  }
}

#endif
