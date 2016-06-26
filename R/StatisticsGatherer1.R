#Defines the statistics gatherer class
#Porting to R of C++ code by M. Joshi
#Ivan Zoccolan 18/6/2016


#The class will decide how part of the algorithm will behave. This is the strategy pattern.
#So the statistics gatherer object will be changed its state by the Monte Carlo algorithm and must retain #the change at each step.
#In R we will need to use classes with reference semantics such as Reference Classes or R6

#This class defines the interface only. Implementation is demanded to subclasses inheriting this one.


gatherer <- R6Class("gatherer",
  public = list( dump_result = function(result) {},
                 get_results = function() {})

)


statistics_mean <- R6Class("statistics_mean", inherit = gatherer,

                  public = list(

                    initialize = function(){ private$values <- 0.0 },

                    dump_result = function(result){

                      if ( inherits(result, "numeric")) private$values <- result
                      else stop("result must be a numeric vector")
                    },

                    get_results = function() { mean(private$values) },

                    path_done = function() { length(private$values) }

                  ),
                  private = list(
                    values = "numeric"
                  )
                )





#Using the statistics gatherer

#' Estimates the price of a derivative by means of Monte Carlo.
#'
#' @description
#' \code{monte_carlo} estimates the price of a derivative by means of Monte Carlo simulation.
#'
#' @details
#' This function is a simple Monte Carlo \url{https://en.wikipedia.org/wiki/Monte_Carlo_method} routine
#' to estimate the value of an european derivative.
#' It's a porting from C++ code by M. Joshi published in his C++ Design Patterns and Derivatives Pricing
#'
#'@param the_option A vanilla_option object.
#'@param spot A numeric scalar representing the initial value of the underlying asset.
#'@param vol A constant_parameters object representing the volatility of the underlying asset.
#'@param int_rate A constant_parameters object representing the spot interest rate.
#'@param path_num An integer scalar  representing the number of simulated paths used for the Monte Carlo simulation.
#'@param statistics_gatherer  A statistics_mean object where the results of the simulation are stored.
#'@examples
#'gatherer_mean <- statistics_mean$new()
#'
#'vol_param <- constant_parameters$new(0.2)
#'
#'r_param <- constant_parameters$new(0.01)
#'
#'call_payoff <- payoff_call$new(100)
#'
#'call_option <- vanilla_option$new(call_payoff, 1)
#'
#'monte_carlo(call_option, spot = 100, vol_param, r_param, path_num = 1e4, gatherer_mean)
#'
#'system.time(monte_carlo(call_option, spot = 100, vol_param, r_param, path_num = 1e6, gatherer_mean))


monte_carlo <- function(the_option, spot, vol, int_rate, path_num, statistics_gatherer){

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


