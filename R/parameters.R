#Defines a base parameter class

parameters <- R6::R6Class("parameters",
 public = list(
  integral = function(t1, t2) {},
  integral_square = function(t1, t2) {}
 )
)



#' Constant parameter class
#' @description
#' Class providing a constant parameter object with methods to calculate
#' the integral of the parameter and  the squared parameter over a time span.
#' @docType class
#' @examples
#' r <- constant_parameters$new(0.01)
#' #Over the full year (365 days) the integral should evaluate to 0.01
#' r$integral(timeDate("2016-07-09"), timeDate("2017-07-09"))
#' #Over the full year the integral square should evaluate to 0.001
#' r$integral_square(timeDate("2016-07-09"), timeDate("2017-07-09"))
#' @importFrom R6 R6Class
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#' \describe{
#'   \item{\code{integral}}{Calculates the integral given the initial
#'    and final times. The arguments are two \code{\link{timeDate}}
#'    object with the initial  and final times.
#'    It returns a \code{numeric} scalar with the integral}
#'   \item{\code{integral_square} (\code{public})}{Calculates the integral
#'    of the squared constant parameter given the initial and final times.
#'    The arguments are two \code{\link{timeDate}} object with the initial
#'    and final times. It returns a \code{numeric} scalar with the integral}
#'   \item{\code{get} (\code{public})}{get the constant}
#' }

constant_parameters <- R6::R6Class("constant_parameters",
 inherit = parameters,
 public = list(
  initialize = function(const, t_span){
   if (!missing(t_span)){
    if (is_positive_integer(t_span)) private$time_span <- t_span
    else stop(error_msg_4("t_span"))
   } else private$time_span <- 365

   if(!missing(const)){
    if (is_numeric_scalar(const)){
     private$constant <- const / private$time_span
     private$constant_square <- const ^ 2 / private$time_span
    } else stop(error_msg_5("const"))
   } else {
    private$constant <- 0.0
    private$constant_square <- 0.0
   }
  },
  integral = function(t1, t2){
   as.numeric((t2 - t1) * private$constant)
  },
  integral_square = function(t1, t2){
   as.numeric((t2 - t1) * private$constant_square)
  },
  get = function() private$constant
 ),
 private = list(
  #A positive integer which stores the time span (in days).
  time_span = "numeric",
  #A numeric scalar which stores the constant parameter.
  constant = "numeric",
  #A numeric scalar which stores the square of the constant parameter
  constant_square = "numeric"
 )
)

