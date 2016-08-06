

payoff <- R6::R6Class("payoff", public = list(initialize = function(){},
                                        get = function() {},
                                        set = function() {}),
                                        private = list()
                                        )


payoff_call <- R6::R6Class("payoff_call", inherit = payoff,
                      public = list(
                        initialize = function(x){
                          if (!missing(x)) {
                            if (is_numeric_scalar(x)){
                              private$strike <- x
                              } else stop("strike must be a numeric scalar")
                          } else private$strike <- 0.0
                        },
                        get = function() {
                          private$strike
                          },
                        set = function(x) {
                          if (is_numeric_scalar(x)){
                            private$strike <- x
                            } else stop("strike must be numeric scalar")
                        }
                      ),
                      private = list(
                        strike = "numeric"
                      )
)

#Overrides the [ operator


"[.payoff_call" <- function(x, i, j=missing){
            sapply(i, function(spot) max(spot - x$get(), 0.0))
          }

#Examples

# callpayoff <- payoff_call$new(100)

# inherits(callpayoff, "payoff")

# callpayoff$get()

# callpayoff$set(120)

# callpayoff$get()

# callpayoff[c(130, 120)]

# callpayoff$set("test")

# call <- payoff_call$new("call")

# system.time(callpayoff[c(130, 120)])

# spots <- 120 * rnorm(n = 1e4, mean = 1, sd = 0.2)

# system.time(callpayoff[spots])


#Implements a vanilla option

vanilla_option <- R6::R6Class("vanilla_option", public = list(
                         initialize = function(the_payoff, expiry) {

                           if (!missing(the_payoff) ){
                             if (inherits(the_payoff, "payoff")){
                               private$pay_off <- the_payoff$clone( )
                             } else stop("the_payoff must be a payoff object")
                           } else private$pay_off <- payoff_call$new(100)

                           if (!missing(expiry)){
                             if (is_positive_scalar(expiry)) {
                               private$priv_expiry <- expiry
                               } else stop("Expiry must be a positive numeric scalar")
                           } else private$priv_expiry <- 1
                         },
                         get_expiry = function(){
                           private$priv_expiry
                         },
                         option_payoff = function(spot){
                           if (inherits(spot, "numeric")) private$pay_off[spot]
                           else stop("Method's parameter must be a numeric vector")
                           }
                         ),

                        private = list(
                          priv_expiry = "numeric",
                          pay_off = "payoff"
                        )
                  )

#Always use clone(deep = TRUE) with vanilla_option objects as it has a field which is a R6 object
#see https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html#cloning-objects

#Examples

# option <- vanilla_option$new(callpayoff, 3)

# option$get_expiry()

# option$option_payoff(c(170, 180))

# option  <- vanilla_option$new(3)
# option <- vanilla_option$new("pippo")

# option <- vanilla_option$new( )

# option <- vanilla_option$new("pippo", 8)

# option1 <- option

# callpayoff$get()

# callpayoff$set(90)

# option$option_payoff(c(170, 180))


#Defines a parameter class

parameters <- R6::R6Class("parameters", public = list(
  integral = function(t1, t2) {},
  integral_square = function(t1, t2) {}
  )
)



#' Class providing a constant parameter object.
#' @description
#' Class providing a constant parameter object with methods to calculate the integral of the parameter and  the squared parameter over a time span.
#' @docType class
#' @usage
#' r <- constant_parameters$new(0.01)
#' #Over the full year (365 days) the integral should evaluate to 0.01
#' r$integral(timeDate("2016-07-09"), timeDate("2017-07-09"))
#' #Over the full year the intergral square should evaluate to 0.001
#' r$integral_square(timeDate("2016-07-09"), timeDate("2017-07-09"))
#' @importFrom R6 R6Class
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @field constant (\code{private}) A \code{numeric} which stores the constant parameter.
#' @field constant_square (\code{private} A \code{numeric} which stores the square of the constant parameter
#' @field time_span (\code{private}) A positive integer which stores the time span (in days).
#' @section Methods:
#' \describe{
#'   \item{\code{integral} (\code{public})}{Calculates the integral given the initial and final times. The arguments are two \code{\link{timeDate}} object with the initial  and final times. It returns a \code{numeric} scalar with the integral}
#'   \item{\code{integral_square} (\code{public})}{Calculates the integral of the squared constant parameter given the initial and final times. The arguments are two \code{\link{timeDate}} object with the initial  and final times. It returns a \code{numeric} scalar with the integral}
#'   }

constant_parameters <- R6::R6Class("constant_parameters", public = list(
    initialize = function(const, t_span){
      if (!missing(t_span)){
        if (is_positive_integer(t_span)) private$time_span <- t_span
        else stop(error_msg_4("t_span"))
      } else private$time_span <- 365

      if (!missing(const)){
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
  }
 ),
  private = list(
    time_span = "numeric",
    constant = "numeric",
    constant_square = "numeric"
 ),
 inherit = parameters
)

