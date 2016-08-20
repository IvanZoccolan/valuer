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
