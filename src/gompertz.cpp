#include <cmath>

#define STRICT_R_HEADERS
#include <Rcpp.h>

#include "distribution.h"
#include "rep_len.h"

namespace {

  namespace gompertz {
    inline double exprel(const double x) {
      if (x!=0) {
	return expm1(x) / x;
      } else {
	return 1;
      }
    }

    inline double safe_coeff(const double q,
			     const double shape,
			     const double rate) {
      if (! std::isinf(q)) {
	const double scale_q = shape * q;
	return - rate * q * exprel(scale_q);
      } else {
	// q is infinite (and positive)
	return R_NegInf;
      }
    }
    

    
    inline bool bad(const double shape, const double rate) {
      if (rate < 0) {
	Rcpp::warning("Negative rate parameter");
	return true;
      } else {
	return false;
      }
    }
    
    class density {
    public:
      typedef double result_type;
      
      inline double operator()(const double x,
			       const double shape,
			       const double rate) const {
	if (bad(shape, rate)) {
	  return NA_REAL;
	}

	if (x < 0) {
	  return R_NegInf;
	}
	
	const double scale_x = shape * x;
	const double shift   = x * exprel(scale_x);
	return std::log(rate) + scale_x - rate * shift;
      }
    };

    class cdf {
    public:
      typedef double result_type;
      cdf(bool lower_tail_, bool give_log_) :
	lower_tail(lower_tail_),
	give_log(give_log_) {}

      inline double operator()(const double q,
			       const double shape,
			       const double rate) const {
	if (bad(shape, rate)) {
	  return NA_REAL;
	}

	if (q < 0) {
	  return below_distribution(lower_tail, give_log);
	}

	if (shape != 0) {
	  const double coeff = safe_coeff(q, shape, rate);

	  /* my, there's a lot of cases here */
	  
	  if ((!give_log) & (lower_tail)) {
	    return -expm1(coeff);
	  }
	  if ( (!give_log) & (!lower_tail)) {
	    return std::exp(coeff);
	  }
	  if (give_log & lower_tail) {
	    return log1p(-std::exp(coeff));
	  }
	  // fall off the end here
	  return coeff;
	  
	} else {
	  return R::pexp(q * rate, 1, lower_tail, give_log);
	}
      }
    private:
      bool lower_tail;
      bool give_log;
    };

  }

}

// [[Rcpp::export(rng=false)]]
Rcpp::NumericVector
dgompertz_work(const Rcpp::NumericVector& x,
	       const Rcpp::NumericVector& shape,
	       const Rcpp::NumericVector& rate,
	       const bool log) {
  if (x.size() == 0) { return x; }
  
  const R_xlen_t size = std::max(x.size(),
				 std::max(shape.size(),
					  rate.size()));
  
  return perhaps_exp(Rcpp::mapply(flexsurv::rep_len(x, size),
				  flexsurv::rep_len(shape, size),
				  flexsurv::rep_len(rate, size),
				  gompertz::density()),
		     log);
}

// [[Rcpp::export(rng=false)]]
Rcpp::NumericVector
pgompertz_work(const Rcpp::NumericVector& q,
	       const Rcpp::NumericVector& shape,
	       const Rcpp::NumericVector& rate,
	       const bool lower_tail,
	       const bool give_log) {
  if (q.size() == 0) { return q; }
  
  const R_xlen_t size = std::max(q.size(),
				 std::max(shape.size(),
					  rate.size()));
  return Rcpp::mapply(flexsurv::rep_len(q, size),
		      flexsurv::rep_len(shape, size),
		      flexsurv::rep_len(rate, size),
		      gompertz::cdf(lower_tail, give_log));
}

// [[Rcpp::export(name="check.gompertz", rng=false)]]
Rcpp::LogicalVector check_gompertz(const Rcpp::NumericVector& shape,
				   const Rcpp::NumericVector& rate) {
  if ( (0==shape.size()) && (0==rate.size()) ) {
    Rcpp::LogicalVector null_result(0);
    return null_result;
  }

  const R_xlen_t size = shape.size();
  return !Rcpp::mapply(shape,
		       flexsurv::rep_len(rate, size),
		       gompertz::bad);
}
