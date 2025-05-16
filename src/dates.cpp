
#define STRICT_R_HEADERS
#include <Rcpp.h>

using namespace Rcpp;


// ************************ Week ***************

// [[Rcpp::export]]
IntegerVector isoWeek(DateVector x) {
  
  int n = x.size();
  IntegerVector weeks(n);

  for (int i = 0; i < n; i++) {
    
    if (R_IsNA(x[i])) {
      weeks[i] = NA_INTEGER;
      continue;
    }
    
    Date curr_d = x[i];

    int days_n = curr_d.getDate(); // Number of days since 1970-01-01
    int wday = (days_n + 4) % 7;   // 0 = Sunday, 1 = Monday, ..., 6 = Saturday
    
    Date z = curr_d + (3 - (wday + 6) % 7);
    
    Date jan1(z.getYear(), 1, 1);
    weeks[i] = 1 + (z - jan1) / 7;
  }

  return weeks;
  
}



// Helper function: Compute day of the year (1-based)
int Yearday(int year, int month, int day) {
  
  static const int daysBeforeMonth[2][12] = {
    { 0,  31,  59,  90, 120, 151, 181, 212, 243, 273, 304, 334 }, // Normal year
    { 0,  31,  60,  91, 121, 152, 182, 213, 244, 274, 305, 335 }  // Leap year
  };
  
  bool isLeap = (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
  
  return daysBeforeMonth[isLeap][month - 1] + day;
  
}



// [[Rcpp::export]]
IntegerVector usWeek(DateVector x) {
  
  int n = x.size();
  IntegerVector weeks(n);
  
  for (int i = 0; i < n; i++) {
    
    if (R_IsNA(x[i])) {
      weeks[i] = NA_INTEGER;
      continue;
    }
    
    Date curr_d = x[i];

    // Calculate US week number (weeks start on Sunday)
    // this does not work:
    //   weeks[i] = curr_d.getYearday() % 7 + 1;
    // so we get a helper function YearDay 
    weeks[i] = (Yearday(curr_d.getYear(), curr_d.getMonth(), curr_d.getDay()) - 1) / 7 + 1;

  }
  
  return weeks;
}



// ************************ Year ***************


// [[Rcpp::export]]
IntegerVector isoYear(DateVector x) {
  
  int n = x.size();
  IntegerVector res(n);
  
  for (int i = 0; i < n; i++) {
    
    if (R_IsNA(x[i])) {
      res[i] = NA_INTEGER;
      continue;
    }

    Date curr_d = x[i];

    int days_n = curr_d.getDate(); // Number of days since 1970-01-01
    int wday = (days_n + 4) % 7;   // 0 = Sunday, 1 = Monday, ..., 6 = Saturday
    
    Date z = curr_d + (3 - (wday + 6) % 7);
    
    Date jan1(z.getYear(), 1, 1);
    int iw = 1 + (z - jan1) / 7;
    
    int m = curr_d.getMonth();
    int y = curr_d.getYear();
    
    // ISO-Jahr bestimmen
    if (iw == 1 && m == 12) {
      res[i] = y + 1;
    } else if ((iw == 53 || iw == 52) && m == 1) {
      res[i] = y - 1;
    } else {
      res[i] = y;
    }
  }
  
  return res;
  
}


// ************************ YearWeek ***************


// [[Rcpp::export]]
IntegerVector isoYearweek(DateVector x) {
  
  int n = x.size();
  IntegerVector res(n);
  
  for (int i = 0; i < n; i++) {
    
    if (R_IsNA(x[i])) {
      res[i] = NA_INTEGER;
      continue;
    }
    
    Date curr_d = x[i];
    
    int days_n = curr_d.getDate(); // Number of days since 1970-01-01
    int wday = (days_n + 4) % 7;   // 0 = Sunday, 1 = Monday, ..., 6 = Saturday
    
    Date z = curr_d + (3 - (wday + 6) % 7);
    
    Date jan1(z.getYear(), 1, 1);
    int iw = 1 + (z - jan1) / 7;
    
    int m = curr_d.getMonth();
    int y = curr_d.getYear();
    
    // ISO-Jahr bestimmen
    if (iw == 1 && m == 12) {
      res[i] = y + 1;
    } else if ((iw == 53 || iw == 52) && m == 1) {
      res[i] = y - 1;
    } else {
      res[i] = y;
    }
    
    res[i] = res[i] * 100 + iw;
  }
  
  return res;
  
}



// [[Rcpp::export]]
IntegerVector usYearweek(DateVector x) {
  
  int n = x.size();
  IntegerVector res(n);
  
  for (int i = 0; i < n; i++) {
    
    if (R_IsNA(x[i])) {
      res[i] = NA_INTEGER;
      continue;
    }

    Date curr_d = x[i];

    int y = curr_d.getYear();
    int m = curr_d.getMonth();
    int d = curr_d.getDay();
    
    int w = (Yearday(y, m, d) - 1) / 7 + 1;
    
    res[i] = y * 100 + w;
  }
  
  return res;
  
}



// ************************ Yearmonth ***************



// [[Rcpp::export]]
IntegerVector usYearmonth(DateVector x) {
  
  int n = x.size();
  IntegerVector res(n);
  
  for (int i = 0; i < n; i++) {
    Date curr_d = x[i];
    res[i] = curr_d.getYear() * 100 + curr_d.getMonth();
  }
  
  return res;
  
}


// [[Rcpp::export]]
IntegerVector isLeapYearDate(DateVector x) {
  
  int n = x.size();
  int y = 0;
  IntegerVector res(n);
  
  for (int i = 0; i < n; i++) {
    Date curr_d = x[i];
    y = curr_d.getYear(); 
    res[i] = (y % 4 == 0 && y % 100 != 0) || (y % 400 == 0);
    
  }
  
  return res;
  
}


// [[Rcpp::export]]
IntegerVector isLeapYearInt(IntegerVector x) {
  
  int n = x.size();
  IntegerVector res(n);
  
  for (int i = 0; i < n; i++) {
    res[i] = (x[i] % 4 == 0 && x[i] % 100 != 0) || (x[i] % 400 == 0);
  }
  
  return res;
  
}





