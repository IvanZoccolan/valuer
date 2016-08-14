// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// calc_account
NumericVector calc_account(const NumericVector& spot, double fee, double barrier);
RcppExport SEXP valuer_calc_account(SEXP spotSEXP, SEXP feeSEXP, SEXP barrierSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const NumericVector& >::type spot(spotSEXP);
    Rcpp::traits::input_parameter< double >::type fee(feeSEXP);
    Rcpp::traits::input_parameter< double >::type barrier(barrierSEXP);
    __result = Rcpp::wrap(calc_account(spot, fee, barrier));
    return __result;
END_RCPP
}
