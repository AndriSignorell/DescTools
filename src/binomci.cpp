

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double acceptBin(int x, int n, double p) {

  // used in the Blaker method of BinomCI()
    
  // p1 = P(X >= x)
  double p1 = 1.0 - R::pbinom(x - 1, n, p, 1, 0);
  
  // p2 = P(X <= x)
  double p2 = R::pbinom(x, n, p, 1, 0);
  
  // a1
  double q1 = R::qbinom(p1, n, p, 1, 0);
  double a1 = p1 + R::pbinom(q1 - 1, n, p, 1, 0);
  
  // a2
  double q2 = R::qbinom(1.0 - p2, n, p, 1, 0);
  double a2 = p2 + 1.0 - R::pbinom(q2, n, p, 1, 0);
  
  return std::min(a1, a2);
}


