#Implemetation of VA products. The  base class will be path_dependent.
#Each VA rider will be a subclass.

#Roll-up an ratchet payoff objects are implemented

#Define a base guarantee payoff which will be inherited by specialized classes
#This is not exported nor should be instantiated.

#' Generic guarantee payoff class
#' @description
#' Class providing an interface for guarantee payoff objects. This class shouldn't be instantiated but used as
#' base class for more specialized implementations such as a roll-up or ratchet payoff classes.
#' @docType class
#' @importFrom R6 R6Class
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @field the_premium (\code{private}) A \code{numeric} which stores the premium.
#' @section Methods:
#'  \describe{
#'   \item{\code{new} (\code{public})}{Initialize method. The argument is  a non negative scalar
#'    with the premium.}
#'   \item{\code{set_premium} (\code{public})}{Sets the the_premium private field.
#'    The argument is a non negative scalar}
#'   \item{\code{get_premium} (\code{public})}{Returns the premium as non negative scalar}
#'   \item{\code{get_payoff} (\code{public})}{Gets a zero payoff in this base class.The arguments are a \code{numeric} vector
#'    with the amounts and two \code{\link{timeDate}} objects with the start and end dates
#'    to calculate the roll-up amount. }
#'}





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
                               get_premium = function(){private$the_premium},
                               get_payoff = function(amount, t1, t2){
                                 return(0)
                               }

                             ),
                             private = list(
                               the_premium = "numeric"
                             )
                  )








#' Roll-up of premiums payoff class
#' @description
#' Class providing a roll-up of premiums payoff object. Provides methods to get/set the base premium \eqn{P}{P} and set the roll-up rate  \eqn{\delta} passed as a \code{\link{constant_parameters}} object. There is a method to calculate the payoff following the formula \deqn{\max(A, G_t^P)}{max(A, G_t)}  where \eqn{G_t}{G_t} is given by \deqn{Pe^{\delta(t_2 - t_1)}}{G_t = Pexp(\delta(t_2 - t_1))}
#' @docType class
#' @usage
#' rate <- constant_parameters$new(0.01)
#' premium <- 100
#' rollup <- payoff_rollup$new(premium, rate)
#' t1 <- timeDate::timeDate("2016-01-01")
#' t2 <- timeDate::timeDate("2016-12-31")
#' rollup$get_payoff(c(120,100), t1,t2)
#' @importFrom R6 R6Class
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @field the_premium (\code{private}) A \code{numeric} which stores the premium.
#' @field the_rate (\code{private}) A \code{\link{constant_parameters}} object which stores the roll-up rate.
#' @section Methods:
#'  \describe{
#'   \item{\code{new} (\code{public})}{Initialize method. The arguments are a non negative scalar
#'    with the premium and a \code{\link{constant_parameters}} object with the roll-up rate.}
#'   \item{\code{set_premium} (\code{public})}{Sets the the_premium private field.
#'    The argument is a non negative scalar}
#'   \item{\code{get_premium} (\code{public})}{Returns the premium as non negative scalar}
#'   \item{\code{set_rate} (\code{public})}{Sets the roll-up rate private field.
#'    The argument is a \code{\link{constant_parameters object}}}
#'   \item{\code{get_payoff} (\code{public})}{Gets the payoff. The arguments are a \code{numeric} vector
#'    with the amounts and two \code{\link{timeDate}} objects with the start and end dates
#'    to calculate the roll-up amount (see \bold{Usage})}
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
                      get_payoff = function(amount, t1, t2){
                        guarantee <- private$the_premium*exp(private$the_rate$integral(t1,t2))
                        sapply(amount, function(i) max(i, guarantee))
                      }

                    ),
                    private = list(
                      the_rate = "constant_parameters"

                    )
                  )






#Define a base class for a product which will be inherited by specialized classes
#This is  exported but should not be instantiated.

#' Generic Variable Annuity  product class
#' @description  Class providing an interface for a generic VA product object. It inherits from \code{\link{path_dependent}}. \cr This class shouldn't be instantiated but used as base class for implementing  products with contract riders such as GMAB, GMIB, etc.
#' It supports a simple state-dependent fee structure with a single barrier. \cr
#' See \bold{References} for more details on variable annuities.
#' @docType class
#' @importFrom R6 R6Class
#' @importClassesFrom timeDate timeDate
#' @importFrom timeDate timeDate timeSequence
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @field the_payoff (\code{private}) A \code{\link{payoff_guarantee}} object which stores the type of payoff
#' (e.g: roll-up, ratchet, etc).
#' @field the_fee (\code{private}) A positive scalar with the annual VA contract fee.
#' @field the_barrier (\code{private}) A positive scalar with the barrier for state-dependent fees.
#' @field the_penalty (\code{private}) A scalar with the withdrawal penalty.
#' Must be between 0 and 1.
#' @section Methods:
#' \describe{
#'   \item{\code{new}}{Constructor method which takes as arguments:
#'   \describe{
#'     \item{\code{payoff}}{ \code{\link{payoff_guarantee}} object with the type of payoff (e.g: roll-up, ratchet)}
#' \item{\code{prod_times}}{ \code{\link{timeSequence}} object with the product timeline}
#' \item{\code{fee}}{ positive scalar with the fee. If not provided defaults to 0.02}
#' \item{\code{barrier}}{ positive scalar with the barrier. If not provided defaults to 0}
#' \item{\code{penalty}}{ scalar between 0 and 1 with the withdrawal penalty. If not provided defaults to \code{Inf}}
#' }
#' }
#'   \item{\code{get_times}}{get method for the product time-line. Retuns a \code{\link{timeDate}} object}
#'   \item{\code{max_number_cfs}}{ returns an \code{integer} with the maximun number of cash flows the product can generate}
#'   \item{\code{cash_flow_times}}{retuns a \code{\link{timeDate}} object with the possible cash flow times. Within this base class the method simply returns the product time-line.}
#'   \item{\code{cash_flows}}{returns a \code{\link{cash_flows}} object with the cash flows of the product. It takes as argument \code{spot_values} a \code{numeric} vector which holds the values of the underlying asset this method will calculate the cash flows from}
#'   }
#' @references
#' \enumerate{
#' \item{\cite{ Bacinello A.R., Millossovich P., Olivieri A., Pitacco  E.,
#' "Variable annuities: a unifying valuation approach." In: Insurance: Mathematics and Economics 49 (2011), pp. 285-297.
#' }}
#' \item{\cite{Bernard C., Hardy M. and Mackay A. "State-dependent fees for variable
#'  annuity guarantees." In: Astin Bulletin 44 (2014), pp. 559-585.}}
#'  }
#'@usage
#'#Sets up the payoff as a roll-up with roll-up rate 1%
#'
#'rate <- constant_parameters$new(0.01)
#'
#'premium <- 100
#'rollup <- payoff_rollup$new(premium, rate)
#'
#'#Five years time-line
#'times <- timeDate::timeSequence(from="2016-01-01", to="2020-12-31")
#'
#'fee <- 0.02
#'barrier <- 200
#'penalty <- 0.1
#'
#'#It shouldn't be instantiated as it's a generic contract base class.
#'va <- va_product$new(rollup, times, fee, barrier, penalty)


va_product <- R6::R6Class("va_product",  inherit = path_dependent,
          public = list(
           initialize = function(payoff, prod_times, fee, barrier, penalty){

            if (!missing(payoff))
             if (inherits(payoff, "payoff_guarantee")) private$the_payoff <- payoff
             else stop(error_msg_1("payoff_guarantee"))
            else stop("Please provide a guarantee payoff object\n")

            if (!missing(prod_times))
              if (are_dates(prod_times)) private$times <- prod_times
              else stop(error_msg_1("timeSequence"))
                #The default is a five year time sequence.
            else private$times <- timeDate::timeSequence(from="2016-01-01", to="2020-12-31")

            if (!missing(fee))
             if(is_positive_scalar(fee)) private$the_fee <- fee
             else stop(error_msg_3)
            else private$the_fee <- 0.02

            if (!missing(barrier))
             if (is_positive_scalar(barrier)) private$the_barrier <- barrier
             else stop(error_msg_3)
            else private$the_barrier <- Inf

            if (!missing(penalty))
              if (is_between(penalty,0,1)) private$the_penalty <- penalty
              else stop(error_msg_8("penalty"))
            else private$the_penalty <- 0


           }

         ),
         private = list(
           the_payoff = "payoff_guarantee",
           the_fee = "numeric",
           the_barrier = "numeric",
           the_penalty = "numeric"

         )
        )



#' @useDynLib valuer
#' @importFrom Rcpp evalCpp sourceCpp

GMAB <- R6::R6Class("GMAB", inherit = va_product,
          public = list(

            max_number_cfs = function(){

                length(private$time_index) - 1
            },

            cash_flows_times = function(){
              private$time_index[-1]
            },

            cash_flows = function(spot_values, death_time){

              if (death_time >= tail(private$time_index, 1))
                  delivery <- tail(private$time_index, 1)
              else    delivery <- death_time





            }

          )
 )








