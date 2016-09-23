
#Financial derivatives payoffs

#Defines a base payoff class

payoff <- R6::R6Class("payoff",
 public = list(
  initialize = function(){},
  get = function() {},
  set = function() {}),
 private = list()
)


#' Call option payoff
#' @description
#' Class providing a call option payoff.
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#' \describe{
#'  \item{\code{new}}{Constructor method}
#'  \item{\code{get}}{Get the strike price}
#'  \item{\code{set}}{Set the stike price}
#'  \item{\code{payoff}}{Calculates the payoff given the argument
#'   \code{spot} which is a \code{numeric} vector or scalar with
#'   the spot prices}
#' }
#'@usage
#'callpayoff <- payoff_call$new(100)
#'@examples
#'callpayoff <- payoff_call$new(100)
#'
#'callpayoff$payoff(c(130, 120))
#'
#'#The [] operator was overridden to calculate the payoff
#'#with a function style:
#'
#'callpayoff[c(130, 120)]


payoff_call <- R6::R6Class("payoff_call", inherit = payoff,
 public = list(
  initialize = function(x){
   if (!missing(x)) {
    if (is_numeric_scalar(x)){
      private$strike <- x
    } else stop(error_msg_5("x"))
   } else private$strike <- 0.0
  },
  get = function() private$strike,
  set = function(x) {
   if (is_numeric_scalar(x)){
    private$strike <- x
   } else stop(error_msg_5("strike"))
  },
  payoff = function(spot){
    sapply(spot, function(s) max(s - private$strike, 0.0))
  }
 ),
 private = list(
  #numeric scalar to save the strike price
  strike = "numeric"
 )
)

#Overrides the [ ] operator
#'@export
"[.payoff_call" <- function(x, i, j=missing){
  sapply(i, function(spot) max(spot - x$get(), 0.0))
}



#Life insurance payoffs

#Define a base guarantee payoff which will be inherited by specialized classes
#This is not exported nor should be instantiated.

#' Generic guarantee payoff class
#' @description
#' Class providing an interface for guarantee payoff objects.
#' This class shouldn't be instantiated but used as base class
#' for more specialized implementations such as a roll-up or
#' ratchet payoff classes.
#' @docType class
#' @importFrom R6 R6Class
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#'  \describe{
#'   \item{\code{new} (\code{public})}{Initialize method.
#'   The argument is a non negative scalar with the premium.}
#'   \item{\code{set_premium} (\code{public})}{Stores the
#'   premium in a private field. The argument is a non negative scalar}
#'   \item{\code{get_premium} (\code{public})}{Returns the premium
#'   as non negative scalar}
#'   \item{\code{get_payoff} (\code{public})}{Gets a zero payoff in this
#'    base class.The arguments are a \code{numeric} vector with
#'    the amounts and a vector of \code{\link{timeDate}}
#'    objects to calculate the payoff}
#' }

payoff_guarantee <- R6::R6Class("payoff_guarantee",
 public = list(
  initialize = function(premium){
   if (!missing(premium))
    if (is_not_negative_scalar(premium)) private$the_premium <- premium
    else stop(error_msg_7("premium"))
   else private$the_premium <- 100
  },
  set_premium = function(premium){
   if (!missing(premium))
    if (is_not_negative_scalar(premium)) private$the_premium <- premium
    else stop(error_msg_7("premium"))
   else private$the_premium <- 100
  },
  get_premium = function() private$the_premium,
  get_payoff = function(amount, t) return(0)
 ),
 private = list(
  #numeric scalar which stores the premium
  the_premium = "numeric"
 )
)


#' Roll-up of premiums payoff class
#' @description
#' Class providing a roll-up of premiums payoff object.
#' Provides methods to get/set the base premium \eqn{P}{P}
#'and set the roll-up rate  \eqn{\delta} passed as a
#'\code{\link{constant_parameters}} object. There is a method
#'to calculate the payoff following the formula
#'\deqn{\max(A, G_t^P)}{max(A, G_t)}  where \eqn{G_t}{G_t}
#' is given by \deqn{Pe^{\delta(t_2 - t_1)}}{G_t = Pexp(\delta(t_2 - t_1))}
#' @docType class
#' @usage
#' rollup <- payoff_rollup$new(premium, rate)
#' @examples
#' rate <- constant_parameters$new(0.01)
#' premium <- 100
#' rollup <- payoff_rollup$new(premium, rate)
#' t1 <- timeDate::timeDate("2016-01-01")
#' t2 <- timeDate::timeDate("2016-12-31")
#' rollup$get_payoff(c(120,100), c(t1,t2))
#' @importFrom R6 R6Class
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#'  \describe{
#'   \item{\code{new}}{Initialize method.
#'    The arguments are a non negative scalar with the premium and a
#'    \code{\link{constant_parameters}} object with the roll-up rate.}
#'   \item{\code{set_premium}}{Stores the premium in a
#'    private field. The argument is a non negative scalar}
#'   \item{\code{get_premium}}{Returns the premium
#'   as non negative scalar}
#'   \item{\code{set_rate}}{Sets the roll-up rate into a private
#'   field. The argument is a \code{\link{constant_parameters}} object}
#'   \item{\code{get_payoff}}{Gets the payoff.
#'    The arguments are a \code{numeric} vector
#'    with the amounts and a vector of \code{\link{timeDate}} objects
#'    with the start and end dates to calculate the roll-up amount
#'    (see \bold{Examples})}
#'}


payoff_rollup <- R6::R6Class("payoff_rollup", inherit = payoff_guarantee,
 public = list(
  initialize = function(premium, rate){
   if (!missing(premium))
    if (is_not_negative_scalar(premium)) private$the_premium <- premium
    else stop(error_msg_7("premium"))
   else private$the_premium <- 100

   if (!missing(rate))
    if (inherits(rate, "constant_parameters")) private$the_rate <- rate
    else stop(error_msg_1("constant_parameters"))
   else private$the_rate <- constant_parameters$new(0)
  },
  set_rate = function(rate){
   if (!missing(rate))
    if (inherits(rate, "constant_parameters")) private$the_rate <- rate
    else stop(error_msg_1("constant_parameters"))
   else private$the_rate <- constant_parameters$new(0)
  },
  get_payoff = function(amount, t){
   guarantee <- private$the_premium*exp(private$the_rate$integral(t[1],t[2]))
   sapply(amount, function(i) max(i, guarantee))
  }
 ),
 private = list(
  #Stores the roll-up rate passed
  #as a constant_parameters object
  the_rate = "constant_parameters"
 )
)
