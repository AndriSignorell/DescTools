// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// ConDisPairs
List ConDisPairs(IntegerMatrix x);
RcppExport SEXP _DescTools_ConDisPairs(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(ConDisPairs(x));
    return rcpp_result_gen;
END_RCPP
}
// ConDisPairsXY
List ConDisPairsXY(NumericVector x, NumericVector y);
RcppExport SEXP _DescTools_ConDisPairsXY(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(ConDisPairsXY(x, y));
    return rcpp_result_gen;
END_RCPP
}
// tbrm
double tbrm(const std::vector<double>& x, double C);
RcppExport SEXP _DescTools_tbrm(SEXP xSEXP, SEXP CSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<double>& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type C(CSEXP);
    rcpp_result_gen = Rcpp::wrap(tbrm(x, C));
    return rcpp_result_gen;
END_RCPP
}
// compute_LCM
long long compute_LCM(long long int a, long long int b);
RcppExport SEXP _DescTools_compute_LCM(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< long long int >::type a(aSEXP);
    Rcpp::traits::input_parameter< long long int >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_LCM(a, b));
    return rcpp_result_gen;
END_RCPP
}
// compute_GCD
long long compute_GCD(long long int a, long long int b);
RcppExport SEXP _DescTools_compute_GCD(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< long long int >::type a(aSEXP);
    Rcpp::traits::input_parameter< long long int >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_GCD(a, b));
    return rcpp_result_gen;
END_RCPP
}
// divs
IntegerVector divs(int x);
RcppExport SEXP _DescTools_divs(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(divs(x));
    return rcpp_result_gen;
END_RCPP
}
// n_pow_sum
List n_pow_sum(NumericVector x);
RcppExport SEXP _DescTools_n_pow_sum(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(n_pow_sum(x));
    return rcpp_result_gen;
END_RCPP
}
// conv_DecToBin
std::vector< std::string > conv_DecToBin(std::vector< int > n);
RcppExport SEXP _DescTools_conv_DecToBin(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector< int > >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(conv_DecToBin(n));
    return rcpp_result_gen;
END_RCPP
}
// isoWeek
IntegerVector isoWeek(DateVector x);
RcppExport SEXP _DescTools_isoWeek(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DateVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(isoWeek(x));
    return rcpp_result_gen;
END_RCPP
}
// usWeek
IntegerVector usWeek(DateVector x);
RcppExport SEXP _DescTools_usWeek(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DateVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(usWeek(x));
    return rcpp_result_gen;
END_RCPP
}
// isoYear
IntegerVector isoYear(DateVector x);
RcppExport SEXP _DescTools_isoYear(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DateVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(isoYear(x));
    return rcpp_result_gen;
END_RCPP
}
// isoYearweek
IntegerVector isoYearweek(DateVector x);
RcppExport SEXP _DescTools_isoYearweek(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DateVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(isoYearweek(x));
    return rcpp_result_gen;
END_RCPP
}
// usYearweek
IntegerVector usYearweek(DateVector x);
RcppExport SEXP _DescTools_usYearweek(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DateVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(usYearweek(x));
    return rcpp_result_gen;
END_RCPP
}
// usYearmonth
IntegerVector usYearmonth(DateVector x);
RcppExport SEXP _DescTools_usYearmonth(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DateVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(usYearmonth(x));
    return rcpp_result_gen;
END_RCPP
}
// isLeapYearDate
IntegerVector isLeapYearDate(DateVector x);
RcppExport SEXP _DescTools_isLeapYearDate(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DateVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(isLeapYearDate(x));
    return rcpp_result_gen;
END_RCPP
}
// isLeapYearInt
IntegerVector isLeapYearInt(IntegerVector x);
RcppExport SEXP _DescTools_isLeapYearInt(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(isLeapYearInt(x));
    return rcpp_result_gen;
END_RCPP
}
// top_n
Rcpp::List top_n(Rcpp::NumericVector x, int n);
RcppExport SEXP _DescTools_top_n(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(top_n(x, n));
    return rcpp_result_gen;
END_RCPP
}
// bottom_n
Rcpp::List bottom_n(Rcpp::NumericVector x, int n);
RcppExport SEXP _DescTools_bottom_n(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(bottom_n(x, n));
    return rcpp_result_gen;
END_RCPP
}
// top_i
IntegerVector top_i(NumericVector v, unsigned int n);
RcppExport SEXP _DescTools_top_i(SEXP vSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(top_i(v, n));
    return rcpp_result_gen;
END_RCPP
}
// bottom_i
IntegerVector bottom_i(NumericVector v, unsigned int n);
RcppExport SEXP _DescTools_bottom_i(SEXP vSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(bottom_i(v, n));
    return rcpp_result_gen;
END_RCPP
}
// fastMode
SEXP fastMode(SEXP x, bool narm);
RcppExport SEXP _DescTools_fastMode(SEXP xSEXP, SEXP narmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type narm(narmSEXP);
    rcpp_result_gen = Rcpp::wrap(fastMode(x, narm));
    return rcpp_result_gen;
END_RCPP
}
// fastModeX
SEXP fastModeX(SEXP x, bool narm);
RcppExport SEXP _DescTools_fastModeX(SEXP xSEXP, SEXP narmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type narm(narmSEXP);
    rcpp_result_gen = Rcpp::wrap(fastModeX(x, narm));
    return rcpp_result_gen;
END_RCPP
}
// formatNum
CharacterVector formatNum(NumericVector x, Nullable<IntegerVector> digits, Nullable<IntegerVector> ldigits, Nullable<CharacterVector> big_mark, Nullable<CharacterVector> decimal_mark, int sci_big, int sci_small);
RcppExport SEXP _DescTools_formatNum(SEXP xSEXP, SEXP digitsSEXP, SEXP ldigitsSEXP, SEXP big_markSEXP, SEXP decimal_markSEXP, SEXP sci_bigSEXP, SEXP sci_smallSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Nullable<IntegerVector> >::type digits(digitsSEXP);
    Rcpp::traits::input_parameter< Nullable<IntegerVector> >::type ldigits(ldigitsSEXP);
    Rcpp::traits::input_parameter< Nullable<CharacterVector> >::type big_mark(big_markSEXP);
    Rcpp::traits::input_parameter< Nullable<CharacterVector> >::type decimal_mark(decimal_markSEXP);
    Rcpp::traits::input_parameter< int >::type sci_big(sci_bigSEXP);
    Rcpp::traits::input_parameter< int >::type sci_small(sci_smallSEXP);
    rcpp_result_gen = Rcpp::wrap(formatNum(x, digits, ldigits, big_mark, decimal_mark, sci_big, sci_small));
    return rcpp_result_gen;
END_RCPP
}
// dgompertz_work
Rcpp::NumericVector dgompertz_work(const Rcpp::NumericVector& x, const Rcpp::NumericVector& shape, const Rcpp::NumericVector& rate, const bool log);
RcppExport SEXP _DescTools_dgompertz_work(SEXP xSEXP, SEXP shapeSEXP, SEXP rateSEXP, SEXP logSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type shape(shapeSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type rate(rateSEXP);
    Rcpp::traits::input_parameter< const bool >::type log(logSEXP);
    rcpp_result_gen = Rcpp::wrap(dgompertz_work(x, shape, rate, log));
    return rcpp_result_gen;
END_RCPP
}
// pgompertz_work
Rcpp::NumericVector pgompertz_work(const Rcpp::NumericVector& q, const Rcpp::NumericVector& shape, const Rcpp::NumericVector& rate, const bool lower_tail, const bool give_log);
RcppExport SEXP _DescTools_pgompertz_work(SEXP qSEXP, SEXP shapeSEXP, SEXP rateSEXP, SEXP lower_tailSEXP, SEXP give_logSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type q(qSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type shape(shapeSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type rate(rateSEXP);
    Rcpp::traits::input_parameter< const bool >::type lower_tail(lower_tailSEXP);
    Rcpp::traits::input_parameter< const bool >::type give_log(give_logSEXP);
    rcpp_result_gen = Rcpp::wrap(pgompertz_work(q, shape, rate, lower_tail, give_log));
    return rcpp_result_gen;
END_RCPP
}
// check_gompertz
Rcpp::LogicalVector check_gompertz(const Rcpp::NumericVector& shape, const Rcpp::NumericVector& rate);
RcppExport SEXP _DescTools_check_gompertz(SEXP shapeSEXP, SEXP rateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type shape(shapeSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type rate(rateSEXP);
    rcpp_result_gen = Rcpp::wrap(check_gompertz(shape, rate));
    return rcpp_result_gen;
END_RCPP
}
// hlqest
double hlqest(NumericVector x);
RcppExport SEXP _DescTools_hlqest(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(hlqest(x));
    return rcpp_result_gen;
END_RCPP
}
// hl2qest
double hl2qest(NumericVector x, NumericVector y);
RcppExport SEXP _DescTools_hl2qest(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(hl2qest(x, y));
    return rcpp_result_gen;
END_RCPP
}
