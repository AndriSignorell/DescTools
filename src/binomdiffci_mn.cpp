


#include <Rcpp.h>
using namespace Rcpp;

// ------------------------------------------------------------
// Score function (Miettinen–Nurminen)
// ------------------------------------------------------------


double binomdiffci_mn_score(double p1, int n1,
                            double p2, int n2,
                            double dif) {
  
  double diff = p1 - p2 - dif;
  if (diff == 0.0) return 0.0;
  
  double t  = (double)n2 / (double)n1;
  double a  = 1.0 + t;
  double b  = -(1.0 + t + p1 + t * p2 + dif * (t + 2.0));
  double c  = dif * dif + dif * (2.0 * p1 + t + 1.0) + p1 + t * p2;
  double d  = -p1 * dif * (1.0 + dif);
  
  double ba3 = b / a / 3.0;
  double s2  = ba3 * ba3 - c / a / 3.0;
  if (s2 <= 0.0) return R_PosInf;
  
  double s = std::sqrt(s2);
  double v = ba3 * ba3 * ba3 - b * c / (6.0 * a * a) + d / (2.0 * a);
  
  double u = (v > 0.0) ? s : -s;
  double w = (M_PI + std::acos(v / (u * u * u))) / 3.0;
  
  double p1d = 2.0 * u * std::cos(w) - ba3;
  double p2d = p1d - dif;
  
  double n = (double)(n1 + n2);
  double var =
    (p1d * (1.0 - p1d) / n1 +
    p2d * (1.0 - p2d) / n2) * n / (n - 1.0);
  
  return diff * diff / var;
}

// ------------------------------------------------------------
// CI boundary (lower or upper) (Miettinen–Nurminen)
// ------------------------------------------------------------

// [[Rcpp::export]]
double binomdiffciMN(int x1, int n1,
                      int x2, int n2,
                      double z,
                      bool lower) {
  
  double p1 = (double)x1 / n1;
  double p2 = (double)x2 / n2;
  
  double phat = p1 - p2;                 // <-- mutable!
  double sign = lower ? -1.0 : 1.0;
  double dp   = 1.0 + (lower ? 1.0 : -1.0) * phat;
  
  double y = phat;
  
  for (int i = 0; i < 50; i++) {
    
    dp *= 0.5;
    y = phat + sign * dp;
    
    double score = binomdiffci_mn_score(p1, n1, p2, n2, y);
    
    if (score < z) {
      phat = y;
    }
    
    if (dp < 1e-7 || std::fabs(z - score) < 1e-6)
      break;
  }
  
  return y;
}


