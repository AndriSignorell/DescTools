
#include <Rcpp.h>

using namespace std;
using namespace Rcpp;


// Tukey's Biweight Robust Mean (tbrm).'
//   There must be no NAs in 'x'.
//           
//   Input:
//     - x   Vector of numbers to be summarized by tbrm (std::vector<double>)
//     - C   Parameter C which adjusts the scaling of the data (double)
//   Output: numeric value
//            
//   Rewritten by Andri Signorell in C++ based on C-Code by Mikko Korpela.
//   


// [[Rcpp::export]]
double tbrm(const std::vector<double>& x, double C) {
// double tbrm(Rcpp::NumericVector x, double C) {

    int n = static_cast<int>(x.size());
    if (n == 0) {
        return NAN;
    }

    std::vector<double> x2 = x;  // Copy of the data part of argument x

    // Median of x
    double x_med;
    if (n % 2 == 1) {  // n is odd
        int half = n / 2;
        std::nth_element(x2.begin(), x2.begin() + half, x2.end());
        x_med = x2[half];
    } else {  // n is even
        int half = n / 2;
        std::nth_element(x2.begin(), x2.begin() + half - 1, x2.end());
        double min_val = *std::min_element(x2.begin() + half, x2.end());
        x_med = (x2[half - 1] + min_val) / 2.0;
    }

    // abs(x - median(x))
    std::vector<double> abs_x_dev(n);
    for (int i = 0; i < n; ++i) {
        abs_x_dev[i] = std::abs(x2[i] - x_med);
    }

    // Median of abs_x_dev, stored in div_const
    double div_const;
    if (n % 2 == 1) {
        int half = n / 2;
        std::nth_element(abs_x_dev.begin(), abs_x_dev.begin() + half, abs_x_dev.end());
        div_const = abs_x_dev[half];
    } else {
        int half = n / 2;
        std::nth_element(abs_x_dev.begin(), abs_x_dev.begin() + half - 1, abs_x_dev.end());
        double min_val = *std::min_element(abs_x_dev.begin() + half, abs_x_dev.end());
        div_const = (abs_x_dev[half - 1] + min_val) / 2.0;
    }

    // This is a normalization constant (well, constant over x2[i])
    div_const = div_const * C + 1e-6;

    // Number of values x2[i] with non-zero weights
    int my_count = 0;

    // Recycling memory, i.e. renaming the same space
    std::vector<double>& wt = abs_x_dev;
    std::vector<double>& wtx = x2;  // Have to be careful not to overwrite too soon

    // Weights (wt) and weighted data (wtx)
    for (int i = 0; i < n; ++i) {
        double this_wt = (x2[i] - x_med) / div_const;
        if (this_wt >= -1.0 && this_wt <= 1.0) {  // absolute value <= 1
            this_wt = 1.0 - this_wt * this_wt;
            this_wt *= this_wt;
            wt[my_count] = this_wt;
            wtx[my_count++] = this_wt * x2[i];
        }
    }
    
    // Sum of my_count values
    if (my_count == 1) {  // Border case: only one value to sum
        return wtx[0] / wt[0];
        
    } else if (my_count > 0) {
        return
          std::reduce(wtx.begin(), wtx.end(), 0.0, std::plus<double>())
            / std::reduce(wt.begin(), wt.end(), 0.0, std::plus<double>());
        
    } else {  // No values to sum
        return NAN;
    }

}
