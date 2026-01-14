
#include <Rcpp.h>
using namespace Rcpp;

// ============================================================
  // Mee / Farrington–Manning: SD under H0
// ============================================================
  

double binomdiffci_mee_sd(double p1, int n1,
                          double p2, int n2,
                          double dif) {
  
  if (dif >  1.0) dif =  1.0;
  if (dif < -1.0) dif = -1.0;
  
  double diff = p1 - p2 - dif;
  if (diff == 0.0) return 0.0;
  
  double t = (double)n2 / n1;
  double a = 1.0 + t;
  double b = -(1.0 + t + p1 + t * p2 + dif * (t + 2.0));
  double c = dif * dif + dif * (2.0 * p1 + t + 1.0) + p1 + t * p2;
  double d = -p1 * dif * (1.0 + dif);
  
  double ba3 = b / a / 3.0;
  double s2  = ba3 * ba3 - c / a / 3.0;
  if (s2 <= 0.0) return R_PosInf;
  
  double s = std::sqrt(s2);
  double v = ba3 * ba3 * ba3
  - b * c / (6.0 * a * a)
  + d / (2.0 * a);
  
  if (std::fabs(v) < DBL_EPSILON) v = 0.0;
  
  double u = (v > 0.0) ? s : -s;
  double w = (M_PI + std::acos(v / (u * u * u))) / 3.0;
  
  double p1d = 2.0 * u * std::cos(w) - ba3;
  double p2d = p1d - dif;
  
  double var =
    p1d * (1.0 - p1d) / n1 +
    p2d * (1.0 - p2d) / n2;
  
  return std::sqrt(std::max(0.0, var));
}


// ============================================================
  // Mee / Farrington–Manning p-value
// ============================================================
  

double binomdiffci_mee_pval(int x1, int n1,
                            int x2, int n2,
                            double delta) {
  
  double p1  = (double)x1 / n1;
  double p2  = (double)x2 / n2;
  double est = p1 - p2;
  
  double sd = binomdiffci_mee_sd(p1, n1, p2, n2, delta);
  if (!R_finite(sd) || sd == 0.0) return 0.0;
  
  double z = (est - delta) / sd;
  double p = R::pnorm(z, 0.0, 1.0, true, false);
  
  return 2.0 * std::min(p, 1.0 - p);
}


// ============================================================
// Mee / Farrington–Manning CI boundary (bisection)
// ============================================================
  
  // [[Rcpp::export]]
  double binomdiffciMee(int x1, int n1,
                        int x2, int n2,
                        double alpha,
                        bool lower) {
    
    double p1  = (double)x1 / n1;
    double p2  = (double)x2 / n2;
    double est = p1 - p2;
    
    double lo = lower ? -1.0 + 1e-6 : est + 1e-6;
    double hi = lower ? est - 1e-6 :  1.0 - 1e-6;
    
    for (int i = 0; i < 80; i++) {
      
      double mid = 0.5 * (lo + hi);
      double p   = binomdiffci_mee_pval(x1, n1, x2, n2, mid);
      
      if (lower) {
        // p(delta) increasing in delta
        if (p > alpha)
          hi = mid;
        else
          lo = mid;
      } else {
        // p(delta) decreasing in delta
        if (p > alpha)
          lo = mid;
        else
          hi = mid;
      }
      
      if (std::fabs(hi - lo) < 1e-7)
        break;
    }
    
    return 0.5 * (lo + hi);
  }
