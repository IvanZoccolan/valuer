
#'Monte Carlo pricing routine.
#'
#'@description
#'Estimates the price of an european derivative by means of
#'Monte Carlo simulation.
#'@details
#'This function is a simple
#'\href{https://en.wikipedia.org/wiki/Monte_Carlo_method}{Monte Carlo} routine
#'to estimate the value of an european derivative.
#'It's a porting from C++ code by M. Joshi  published in his
#'C++ Design Patterns and Derivatives Pricing
#'@export
#'@param the_option A vanilla_option object.
#'@param spot A numeric scalar representing the initial
#'value of the underlying asset.
#'@param vol A constant_parameters object representing
#' the volatility of the underlying asset.
#'@param int_rate A constant_parameters object representing
#' the spot interest rate.
#'@param path_num An integer scalar  representing the number of
#' simulated paths used for the Monte Carlo simulation.
#'@param statistics_gatherer  A statistics_mean object where the
#'results of the simulation are stored.
#'@examples
#'the_gatherer <- mc_gatherer$new()
#'
#'vol_param <- constant_parameters$new(0.2)
#'
#'r_param <- constant_parameters$new(0.01)
#'
#'call_payoff <- payoff_call$new(100)
#'
#'call_option <- vanilla_option$new(call_payoff, 1)
#'
#'monte_carlo(call_option, spot = 100, vol_param, r_param, path_num = 1e4,
#'the_gatherer)
#'
#'the_gatherer$get_results()


monte_carlo <- function(the_option, spot, vol, int_rate, path_num,
                        statistics_gatherer){

  expiry <- the_option$get_expiry()

  variance <- vol$integral_square(0, expiry)

  root_variance <- sqrt(variance)

  ito_correction <- -0.5 * variance

  discounting <- exp(-int_rate$integral(0, expiry))

  moved_spot <- spot * exp(int_rate$integral(0, expiry) + ito_correction)

  this_gaussian <- rnorm(n = path_num)

  this_spot <- moved_spot * exp(root_variance * this_gaussian)

  this_payoff <- the_option$option_payoff(this_spot)

  statistics_gatherer$dump_result(this_payoff * discounting)
}


