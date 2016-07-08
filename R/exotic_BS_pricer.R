#Extends the base exotic engine by implementing its get_one_path() method.
#get_one_path() will pull a sample path from a risk-neutral geometric Brownian motion process.
#It also changes the base class initialize method  to get parameters objects
#with the volatility and continuous dividend rate.

#' Class for a  pricing engine of a path dependent derivative.
#' @description Class for a  pricing engine of a path dependent derivative
#' product with an underlying asset modeled as a geometric Brownian motion.
#' @docType class
#' @importFrom R6 R6Class
#' @importClassesFrom timeDate timeDate
#' @importFrom timeDate timeDate timeSequence
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @field the_product A \code{\link{path_dependent}} object with the product the engine will price.
#' @field r A \code{\link{parameters}} object with the interest rate.
#' @section Methods:
#' \describe{
#'   \item{\code{new}}{Constructor method which takes as argument a \code{\link{path_dependent}} object with the product and a \code{\link{parameters}} object with the interest rate}
#'   \item{\code{get_one_path}}{Returns a \code{numeric} vector with a simulated path of the product underlying asset modeled as a risk neutral geometric Brownian motion process.}
#'   \item{\code{run_simulation}}{Runs the Monte Carlo simulation and stores the results in a \code{\link{gatherer}} object. Takes as arguments a \code{\link{gatherer}} object to store the results and an \code{integer} scalar with the number of paths to simulate.}
#'   \item{\code{discount_one_path}}{discounts a cash flow path}
#'   \item{\code{discounts}}{\code{\link{R6Class}} active binding which calculates the discount factors.}
#'   }

exotic_bs_engine <- R6Class("exotic_bs_engine", inherit= exotic_engine,
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
        } else stop(error_msg_1("path_dependent"))
      else stop(error_msg_1("path_dependent"))

      if(!missing(interest))
        if(inherits(interest, "parameters")){
          private$r <- interest
        } else stop(error_msg_1("parameters"))
      else stop(error_msg_1("parameters"))

      if(!missing(spot))
         if (is_positive_scalar(spot)){
           private$log_spot <- log(spot)
         } else stop(error_msg_3)
       else stop(error_msg_3)

      if(!missing(volatility) & !missing(dividends)){
        if (inherits(volatility, "parameters") & inherits(dividends, "parameters")){
            for (j in seq(no_time_intervals)){
              this_variance <- volatility$integral_square(times[j], times[j+1])
              private$drifts[j] <- interest$integral(times[j], times[j+1])
              - dividends$integral(times[j], times[j+1])
              - 0.5 * this_variance
              private$standard_deviations[j] <- sqrt(this_variance)
            }
          }else stop(error_msg_2("parameters"))
      } else stop(error_msg_2("parameters"))
    },

    get_one_path = function(){

      private$variates <- rnorm(private$no_time_intervals)
      current_log_spot <- private$log_spot
      current_log_spot <- private$drifts + current_log_spot
      current_log_spot <- private$standard_deviations * private$variates
                          + current_log_spot
      exp(current_log_spot)

    }
  ),
  private = list(
    times = "timeDate",
    drifts = "numeric",
    standard_deviations = "numeric",
    variates = "numeric",
    log_spot = "numeric",
    no_time_intervals = "numeric"
  )
)

