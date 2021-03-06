# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Calculates the account
#'
#' @param spot \code{numeric} vector with the VA reference fund values
#' @param ben \code{numeric} vector with the living benefit cash flow
#' @param fee  \code{numeric} scalar with the fee
#' @param barrier \code{numeric} scalar with the state-dependent barrier
#' @param penalty \code{numeric} vector with the surrender penalty
#' @export
#' @useDynLib valuer, .registration = TRUE
#' @importFrom Rcpp sourceCpp
calc_account <- function(spot, ben, fee, barrier, penalty) {
    .Call(`_valuer_calc_account`, spot, ben, fee, barrier, penalty)
}

