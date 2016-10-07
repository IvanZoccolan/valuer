
/*Copyright 2016 Ivan Zoccolan

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <Rcpp.h>
using namespace Rcpp;


//' Calculates the account
//'
//' @param spot \code{numeric} vector with the VA reference fund values
//' @param fee  \code{numeric} scalar with the fee
//' @param barrier \code{numeric} scalar with the state-dependent barrier
//' @param penalty \code{numeric} scalar with the surrender penalty
//' @export
// [[Rcpp::export]]
NumericVector calc_account(const NumericVector& spot, double fee, double barrier, double penalty) {

  int n = spot.size();
  NumericVector account(n);

  double temp = spot[0];
  double p = 1 - penalty;

  for (int i = 0; i < n; ++i){

   if (temp > 0)
    account[i] = temp;
   else account[i] = 0;

   if (account[i] <= barrier){
     temp =  account[i] * ((spot[i+1] / spot[i]) - fee);
    } else {
     temp = account[i] * (spot[i+1] / spot[i]);
   };
  };

  for (int i = 0; i < n - 1; i++){
    account[i] = p * account[i];
  };

  return account;

}
