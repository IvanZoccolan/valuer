#Extends the base exotic engine by implementing its get_one_path() method.
#get_one_path() will pull a sample path from a risk-neutral geometric Brownian
#motion process.It also changes the base class initialize method  to get
#parameters objects with the volatility and continuous dividend rate.

#' Exotic option pricing with GBM
#' @description
#' Class for a  pricing engine of a path dependent derivative
#' product with an underlying asset modeled as a geometric Brownian motion.
#' @docType class
#' @importFrom R6 R6Class
#' @importClassesFrom timeDate timeDate
#' @importFrom timeDate timeDate timeSequence
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#' \describe{
#'   \item{\code{new}}{Constructor method. It takes as
#'    arguments: \code{product} a \code{\link{path_dependent}} object,
#'    \code{interest} a \code{\link{parameters}} object carrying the
#'    interest rate, \code{spot} a \code{numeric} positive scalar which
#'    is the initial value of the underlying asset, \code{volatility} a
#'    \code{parameters} object carrying the volatility of the underlying
#'     asset and \code{dividends} a \code{parameters} object carrying the
#'     continuous dividend payout rate}
#'   \item{\code{get_one_path}}{Returns a \code{numeric}
#'    vector with a simulated path of the product underlying asset modeled
#'     as a risk neutral geometric Brownian motion process.}
#'   \item{\code{run_simulation}}{Runs the Monte Carlo simulation and stores
#'    the results in a \code{\link{gatherer}} object. Takes as arguments a
#'    \code{\link{gatherer}} object to store the results and an
#'    \code{integer} scalar with the number of paths to simulate.}
#'   \item{\code{discount_one_path}}{Discounts a simulated path of the
#'   underlying asset. It takes as argument \code{spot_values} a
#'   \code{numeric} vector of simulated values of the underlying asset,
#'   calculates the cash flows and takes their present value given the
#'   discount factors. Returns a \code{numeric} scalar with the present
#'   value of the discounted product cash flows.}
#'   \item{\code{discounts}}{\code{\link{R6Class}} active binding which
#'   calculates the discount factors. It is called during the initialize
#'   method given the interest rate is constant with this implementation.}
#'   }
#' @examples
#' prod_time <- timeDate::timeSequence(from="2016-07-09", to="2017-07-09")
#' prod <- path_dependent_asian$new(prod_time)
#' r <- constant_parameters$new(0.01)
#' spot <- 100
#' vol <- constant_parameters$new(0.2)
#' div <- constant_parameters$new(0.0)
#' engine <- exotic_bs_engine$new(prod, r, spot, vol, div)
#' the_gatherer <- mc_gatherer$new()
#' no_of_paths <- 1e3
#' engine$run_simulation(the_gatherer, no_of_paths)
#' the_gatherer$get_results()

exotic_bs_engine <- R6::R6Class("exotic_bs_engine", inherit= exotic_engine,
 public = list(
  initialize = function(product, interest, spot, volatility, dividends){
   if(!missing(product))
    if (inherits(product, "path_dependent")){
     private$the_product <- product
     private$times <- product$get_times()
     private$no_time_intervals <- length(private$times) - 1
     private$drifts <- numeric(private$no_time_intervals)
     private$standard_deviations <- numeric(private$no_time_intervals)
     private$variates <- numeric(private$no_time_intervals)
    } else stop(error_msg_1_("product", "path_dependent"))
    else stop(error_msg_1_("product", "path_dependent"))

   if(!missing(interest))
    if(inherits(interest, "parameters")){
     private$r <- interest
    } else stop(error_msg_1_("interest", "parameters"))
    else stop(error_msg_1_("interest", "parameters"))

   if(!missing(spot))
    if (is_positive_scalar(spot)){
     private$spot <- spot
    } else stop(error_msg_3_("spot"))
   else stop(error_msg_3_("spot"))

   if(!missing(volatility) & !missing(dividends)){
    if(inherits(volatility, "parameters") & inherits(dividends, "parameters")){
     for (j in seq(private$no_time_intervals)){
      this_variance <- volatility$integral_square(private$times[j],
                                                   private$times[j+1])
      private$drifts[j] <- interest$integral(private$times[j],
                                             private$times[j+1])
              - dividends$integral(private$times[j], private$times[j+1])
              - 0.5 * this_variance
              private$standard_deviations[j] <- sqrt(this_variance)
     }
    } else stop(error_msg_2("parameters"))
   } else stop(error_msg_2("parameters"))
   private$discount_factors <- self$discounts
  },
  get_one_path = function(){
   private$variates <- rnorm(private$no_time_intervals)
   current_log_spot <- private$drifts +
                        private$standard_deviations * private$variates
   current_log_spot <- cumsum(current_log_spot)
   c(spot, spot*exp(current_log_spot))
  },
  discount_one_path = function(spot_values){
   these_cash_flows <- private$the_product$cash_flows(spot_values)
   sum(these_cash_flows * private$discount_factors)
  }
 ),
 private = list(
  #A timeDate object with the product time-line
  times = "timeDate",
  #A vector holding the drifts
  drifts = "numeric",
  #A numeric vector holding the standard_deviations
  standard_deviations = "numeric",
  #A numeric vector holding the simulated standard normal values
  variates = "numeric",
  # A numeric scalar holding the initial value of the underlying asset
  spot = "numeric",
  #A numeric vector holding the number of time intervals
  #times[j] - times[j - 1]
  no_time_intervals = "numeric",
  #A numeric vector holding the discount factors
  discount_factors = "numeric"
 )
)

