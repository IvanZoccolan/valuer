// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// calc_account
NumericVector calc_account(const NumericVector& spot, const NumericVector& ben, double fee, double barrier, const NumericVector& penalty);
RcppExport SEXP valuer_calc_account(SEXP spotSEXP, SEXP benSEXP, SEXP feeSEXP, SEXP barrierSEXP, SEXP penaltySEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const NumericVector& >::type spot(spotSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type ben(benSEXP);
    Rcpp::traits::input_parameter< double >::type fee(feeSEXP);
    Rcpp::traits::input_parameter< double >::type barrier(barrierSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type penalty(penaltySEXP);
    __result = Rcpp::wrap(calc_account(spot, ben, fee, barrier, penalty));
    return __result;
END_RCPP
}
