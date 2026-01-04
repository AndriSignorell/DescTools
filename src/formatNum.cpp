#include <Rcpp.h>
#include <iomanip>
#include <sstream>
#include <cmath>
using namespace Rcpp;

// Hilfsfunktion: Rundung auf 10er, 100er etc.
double round_to_power(double value, int power) {
  double factor = std::pow(10.0, -power);
  return std::round(value / factor) * factor;
}

// [[Rcpp::export]]
CharacterVector formatNum(NumericVector x,
                          Nullable<IntegerVector> digits = R_NilValue,
                          Nullable<IntegerVector> ldigits = R_NilValue,
                          Nullable<CharacterVector> big_mark = R_NilValue,
                          Nullable<CharacterVector> decimal_mark = R_NilValue,
                          int sci_big = 9999,
                          int sci_small = -9999) {
  
  int n = x.size();
  
  // digits: default = 7 signifikante Stellen
  IntegerVector dig(n);
  if (digits.isNotNull()) {
    IntegerVector dvec(digits);
    for (int i = 0; i < n; ++i) dig[i] = dvec[i % dvec.size()];
  } else {
    for (int i = 0; i < n; ++i) {
      if (NumericVector::is_na(x[i])) {
        dig[i] = 7;
      } else if (x[i] == 0) {
        dig[i] = 6;
      } else {
        double absx = std::abs(x[i]);
        int magnitude = (int)std::floor(std::log10(absx));
        dig[i] = std::max(0, 7 - magnitude - 1);
      }
    }
  }
  
  // ldigits: default = 0
  IntegerVector ldig = ldigits.isNotNull() ? IntegerVector(ldigits) : IntegerVector::create(0);
  
  // big_mark: default = ""
  CharacterVector bmark = big_mark.isNotNull() ? CharacterVector(big_mark) : CharacterVector::create("");
  
  // decimal_mark: default = getOption("OutDec")
  CharacterVector dmark;
  if (decimal_mark.isNotNull()) {
    dmark = CharacterVector(decimal_mark);
  } else {
    Environment base("package:base");
    Function getOption = base["getOption"];
    dmark = CharacterVector::create(as<std::string>(getOption("OutDec")));
  }
  
  int nd = dig.size();
  int nl = ldig.size();
  int nb = bmark.size();
  int ndm = dmark.size();
  
  CharacterVector result(n);
  
  for (int i = 0; i < n; ++i) {
    double val = x[i];
    if (NumericVector::is_na(val)) {
      result[i] = NA_STRING;
      continue;
    }
    
    int d = dig[i % nd];
    int l = ldig[i % nl];
    std::string bm = as<std::string>(bmark[i % nb]);
    std::string dm = as<std::string>(dmark[i % ndm]);
    
    double abs_val = std::abs(val);
    std::ostringstream ss;
    
    bool use_sci = false;
    if (abs_val > 0) {
      double log10val = std::log10(abs_val);
      use_sci = (log10val >= sci_big) || (log10val < sci_small);
    }
    
    if (use_sci) {
      // Wissenschaftliche Notation
      ss << std::scientific << std::setprecision(std::max(d, 0)) << val;
      std::string sci_str = ss.str();
      
      // OutDec ersetzen
      if (dm != ".") {
        size_t dot = sci_str.find('.');
        if (dot != std::string::npos) {
          sci_str.replace(dot, 1, dm);
        }
      }
      result[i] = sci_str;
      continue;
    }
    
    // Normale Formatierung mit Rundung
    double rounded_val;
    int effective_digits;
    
    if (d >= 0) {
      rounded_val = std::round(val * std::pow(10.0, d)) / std::pow(10.0, d);
      effective_digits = d;
    } else {
      rounded_val = round_to_power(val, d);
      effective_digits = 0;
    }
    
    ss << std::fixed << std::setprecision(effective_digits) << std::abs(rounded_val);
    std::string num = ss.str();
    
    size_t dot_pos = num.find('.');
    std::string int_part = dot_pos != std::string::npos ? num.substr(0, dot_pos) : num;
    std::string dec_part = dot_pos != std::string::npos ? num.substr(dot_pos + 1) : "";
    
    if ((int)int_part.length() < l) {
      int_part = std::string(l - int_part.length(), '0') + int_part;
    }
    
    std::string grouped;
    int len = int_part.length();
    for (int j = 0; j < len; ++j) {
      grouped += int_part[j];
      if ((len - j - 1) % 3 == 0 && j != len - 1) {
        grouped += bm;
      }
    }
    
    std::string full = grouped;
    if (effective_digits > 0) {
      full += dm + dec_part;
    }
    
    if (l == 0) {
      // erase leading digits if ldigits == 0
      if (full.rfind("0" + dm, 0) == 0) {
        full.erase(0, 1);            // "0.23" -> ".23"
      }
    }
    
    if (rounded_val < 0) {
      full = "-" + full;
    }
    
    result[i] = full;
  }
  
  return result;
}
