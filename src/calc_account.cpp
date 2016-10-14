
/*Copyright 2016 Ivan Zoccolan

This file is part of valuer.

Valuer is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Valuer is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

A copy of the GNU General Public License is available at
https://www.R-project.org/Licenses/ and included in the R distribution
(in directory share/licenses).
*/

#include <Rcpp.h>
using namespace Rcpp;


//' Calculates the account
//'
//' @param spot \code{numeric} vector with the VA reference fund values
//' @param ben \code{numeric} vector with the living benefit cash flow
//' @param fee  \code{numeric} scalar with the fee
//' @param barrier \code{numeric} scalar with the state-dependent barrier
//' @param penalty \code{numeric} scalar with the surrender penalty
//' @export
// [[Rcpp::export]]
NumericVector calc_account(const NumericVector& spot, const NumericVector& ben, double fee, double barrier, double penalty) {

  int n = spot.size();
  NumericVector account(n);

  double temp = spot[0];
  double p = 1 - penalty;

  for (int i = 0; i < n; ++i){

   if (temp > 0)
    account[i] = temp;
   else account[i] = 0;

   if (account[i] <= barrier){
     temp =  account[i] * ((spot[i+1] / spot[i]) - fee) - ben[i];
    } else {
     temp = account[i] * (spot[i+1] / spot[i]) - ben[i];
   };
  };

  for (int i = 0; i < n - 1; i++){
    account[i] = p * account[i];
  };

  return account;

}
