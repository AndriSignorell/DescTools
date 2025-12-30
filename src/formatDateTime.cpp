#include <Rcpp.h>
#include <ctime>
#include <vector>
#include <string>
#include <clocale>
#include <memory>
#include <cctype>

using namespace Rcpp;

// --------------------------------------------------
// Token definition
// --------------------------------------------------
struct Token {
  std::string key;
  std::string strftime;
  bool strip_zero;
  bool manual;
};

// LONGEST match first!
const std::vector<Token> TOKENS = {
  {"dddd","%A",false,false},
  {"ddd","%a",false,false},
  {"MMMM","%B",false,false},
  {"MMM","%b",false,false},
  
  {"yyyy","",false,true},
  {"yy","%y",false,false},
  {"y","",true,true},
  
  {"dd","%d",false,false},
  {"d","%d",true,false},
  {"MM","%m",false,false},
  {"M","%m",true,false},
  
  {"HH","%H",false,false},
  {"H","%H",true,false},
  
  {"tt","%p",false,false},
  {"t","%p",true,false},
  
  {"hh","%I",false,false},
  {"h","%I",true,false},
  
  {"mm","%M",false,false},
  {"m","%M",true,false},
  {"ss","%S",false,false},
  {"s","%S",true,false}
};

// --------------------------------------------------
// helpers
// --------------------------------------------------
inline std::string strip_zero(const std::string& x) {
  if (x.size() > 1 && x[0] == '0')
    return x.substr(1);
  return x;
}

inline std::tm make_tm(time_t tt) {
  std::tm tm;
#ifdef _WIN32
  localtime_s(&tm, &tt);
#else
  localtime_r(&tt, &tm);
#endif
  return tm;
}

inline std::string eval_token(
    const Token& tk,
    const std::tm& tm,
    bool is_date
) {
  // manual year handling
  if (tk.manual) {
    int year = tm.tm_year + 1900;
    if (tk.key == "yyyy")
      return std::to_string(year);
    if (tk.key == "y")
      return std::to_string(year % 100);
  }
  
  // time tokens on Date -> zero
  if (is_date &&
      (tk.key == "H" || tk.key == "HH" ||
      tk.key == "h" || tk.key == "hh" ||
      tk.key == "m" || tk.key == "mm" ||
      tk.key == "s" || tk.key == "ss" ||
      tk.key == "t" || tk.key == "tt")) {
    return tk.strip_zero ? "0" : "00";
  }
  
  char buf[64];
  std::strftime(buf, sizeof(buf), tk.strftime.c_str(), &tm);
  std::string val(buf);
  
  // t = first letter of AM/PM
  if (tk.key == "t") {
    return val.substr(0, 1);
  }
  
  if (tk.strip_zero)
    val = strip_zero(val);
  
  return val;
}

// --------------------------------------------------
// Locale guard (RAII)
// --------------------------------------------------
class LocaleGuard {
public:
  explicit LocaleGuard(const std::string& loc) {
    const char* old = std::setlocale(LC_TIME, nullptr);
    old_ = old ? old : "";
    std::setlocale(LC_TIME, loc.c_str());
  }
  ~LocaleGuard() {
    if (!old_.empty())
      std::setlocale(LC_TIME, old_.c_str());
  }
private:
  std::string old_;
};


// [[Rcpp::export]]
CharacterVector formatDateTime(
    SEXP x,
    std::string fmt,
    bool strict = true,
    std::string locale = "current"
) {
  
  R_xlen_t n = Rf_length(x);
  CharacterVector out(n);
  
  bool is_date = Rf_inherits(x, "Date");
  double* xp = REAL(x);
  
  // --------------------------
  // locale handling
  // --------------------------
  std::unique_ptr<LocaleGuard> lg;
  if (locale != "current") {
    lg.reset(new LocaleGuard(locale == "C" ? "C" : locale));
  }
  
  // --------------------------
  // strict pre-check: 12h needs AM/PM
  // --------------------------
  if (strict) {
    bool uses_12h =
      fmt.find("h")  != std::string::npos ||
      fmt.find("hh") != std::string::npos;
    
    bool has_ampm =
      fmt.find("t")  != std::string::npos ||
      fmt.find("tt") != std::string::npos;
    
    if (uses_12h && !has_ampm) {
      stop("12-hour format ('h' or 'hh') requires 't' or 'tt' (AM/PM designator)");
    }
  }
  
  // --------------------------
  // vectorized formatting
  // --------------------------
  for (R_xlen_t i = 0; i < n; ++i) {
    
    if (ISNA(xp[i])) {
      out[i] = NA_STRING;
      continue;
    }
    
    time_t tt;
    if (is_date) {
      // Date: days since 1970-01-01
      tt = static_cast<time_t>(xp[i]) * 86400;
    } else {
      // POSIXct: seconds since 1970-01-01
      tt = static_cast<time_t>(xp[i]);
    }
    
    std::tm tm = make_tm(tt);
    
    std::string res;
    for (size_t pos = 0; pos < fmt.size();) {
      
      // ---- CORRECT yyy rejection (token-level) ----
      if (strict &&
          fmt.compare(pos, 3, "yyy") == 0 &&
          fmt.compare(pos, 4, "yyyy") != 0) {
        stop("Invalid format token 'yyy'. Did you mean 'yyyy'?");
      }
      
      bool matched = false;
      
      for (const auto& tk : TOKENS) {
        if (fmt.compare(pos, tk.key.size(), tk.key) == 0) {
          res += eval_token(tk, tm, is_date);
          pos += tk.key.size();
          matched = true;
          break;
        }
      }
      
      if (!matched) {
        if (strict && std::isalpha(static_cast<unsigned char>(fmt[pos]))) {
          stop("Unknown format token starting at '%c'", fmt[pos]);
        }
        res.push_back(fmt[pos]);
        pos++;
      }
    }
    
    out[i] = res;
  }
  
  return out;
}
