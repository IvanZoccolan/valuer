
######################### DESIGN COMMENTS ######################################
#Implementation of VA products
#
#The path_dependent class is the base class of a VA product
#A new path_dependent subclass is needed for each VA contract rider
#For example we have a specialized GMAB path_dependent class for VA with
#GMAB rider, etc.
#A base class for riders is needed to make a standard interface as each product
#has to store the same info such as fee, barrier (for state-dependent fees),
#penalties for withdrawals, type of guarantee payoff (rollup / ratchet).
#A roll-up payoff or ratchet payoff object will be passed into the initialize
#of the product.
#The cash_flows method returns all possible  cash_flows, so withdrawal in case
#the insured surrenders the contract or if there's a GMWB rider
#as well as other living / death benefits depending on the riders.
#The engine will use the cash_flows depending if we're doing static or mixed
#The cash flows will be saved in private field of the engine.
#To calculate the cash flows we need first to calculate the account of the
#insured. The formula which calculates the account cannot be vectored since
#each value depends on the previous one.
#So for performance reasons it is implemented in C++ and interfaced with Rcpp
#This is the calc_account function.
#A simple state-dependent fee structure with a single barrier is implemented
#in the calc_account function.
#Two public methods will return the times of survival benefit payments
#and the possible surrender times.
#A public method returns the survival benefit at any given time.
##########################DESIGN COMMENTS END###################################


#Defines a base class for a product which will be inherited by
#specialized classes. This is  exported but should not be instantiated.

#' Generic Variable Annuity  product class
#' @description  Class providing an interface for a generic VA product object.
#' This class shouldn't be instantiated but used as base class for
#' implementing  products with contract riders such as GMAB, GMIB, etc.
#' It supports a simple state-dependent fee structure with a single barrier.\cr
#' See \bold{References} for a description of variable annuities life
#' insurance products, their guarantees and fee structures.
#' @docType class
#' @importFrom R6 R6Class
#' @importClassesFrom timeDate timeDate
#' @importFrom timeDate timeDate timeSequence
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#'  \describe{
#'   \item{\code{new}}{Constructor method}
#'   \item{\code{get_times}}{get method for the product time-line.
#'    Returns a \code{\link{timeDate}} object}
#'   \item{\code{get_age}}{get method for the age of the insured}
#'   \item{\code{set_age}}{set method for the age of the insured}
#'   \item{\code{get_barrier}}{get method for the state-dependent fee barrier.
#'    Returns a positive scalar with the barrier}
#'   \item{\code{set_barrier}}{set method for the state-dependent fee barrier.
#'     Argument must be a positive scalar.}
#'   \item{\code{set_penalty}}{set method for the penalty applied in case of
#'    surrender. Argument must be a scalar between 0 and 1.}
#'   \item{\code{get_penalty}}{get method for the penalty applied in case of
#'     surrender. It returns a scalar between 0 and 1.}
#'   \item{\code{set_fee}}{set method for the contract fee. The argument is
#'      a \code{\link{constant_parameters}} object with the fee.}
#'   \item{\code{survival_benefit_times}}{returns a \code{numeric} vector with
#'    the survival benefit time indexes.}
#'   \item{\code{surrender_times}}{returns a \code{numeric} vector with the
#'    surrender time indexes. Takes as argument a string with the frequency
#'    of the decision if surrendering the contract,  e.g. "3m"
#'    corresponds to a surrender decision taken every 3 months.}
##'  \item{\code{times_in_yrs}}{returns the product time-line in
#'    fraction of year}
#'   \item{\code{max_number_cfs}}{returns an \code{integer} with the maximun
#'    number of cash flows the product can generate}
#'   \item{\code{cash_flow_times}}{returns a \code{\link{timeDate}} object
#'    with the possible cash flow times.}
#'   \item{\code{cash_flows}}{returns a \code{numeric} vector with the
#'    cash flows of the product. It takes as argument \code{spot_values} a
#'    \code{numeric} vector which holds the values of the underlying fund this
#'    method will calculate the cash flows from}
#'   \item{\code{survival_benefit}}{Returns a numeric scalar corresponding to
#'    the survival benefit.
#'    The arguments are \code{spot_values} vector which holds the values of
#'    the underlying fund and \code{t} the time index of the survival benefit.
#'    The function will return 0 if there's no survival benefit at the
#'    specified time}
#'   \item{\code{get_premium}}{Returns the premium as non negative scalar}
#' }
#' @references
#' \enumerate{
#' \item{[BMOP2011]}{ \cite{Bacinello A.R., Millossovich P., Olivieri A.,
#'  Pitacco  E., "Variable annuities: a unifying valuation approach."
#' In: Insurance: Mathematics and Economics 49 (2011), pp. 285-297.
#' }}
#' \item{[BHM2014]}{ \cite{Bernard C., Hardy M. and Mackay A. "State-dependent
#' fees for variable annuity guarantees." In: Astin Bulletin 44 (2014),
#' pp. 559-585.}}
#' }
#'@usage
#'va <- va_product$new(rollup, times, age, fee, barrier, penalty)


va_product <- R6::R6Class("va_product",  inherit = path_dependent,
 public = list(
  initialize = function(payoff, prod_times, age, fee, barrier, penalty, ...){
   if (!missing(payoff))
    if (inherits(payoff, "payoff_guarantee")) private$the_payoff <- payoff
    else stop(error_msg_1("payoff_guarantee"))
   else stop("Please provide a guarantee payoff object\n")

   if (!missing(prod_times))
    if (are_dates(prod_times)){
     private$times <- prod_times
     # Normalizes the product time line into year fractions
     private$times_yrs <- yr_fractions(prod_times)
     } else stop(error_msg_1_("prod_times", "timeSequence"))
   #The default is a five year time sequence.
   else {
    t <- timeDate::timeSequence(from="2016-01-01", to="2020-12-31")
    private$times <- t
    private$times_yrs <- yr_fractions(t)
   }
   if(!missing(age))
    if (is_positive_integer(age))
     private$the_age <- age
    else stop(error_msg_4("age"))
   else private$the_age <- 60
   if (!missing(fee))
    if(inherits(fee, "constant_parameters")) private$the_fee <- fee
    else stop(error_msg_1_("fee", "constant_parameters"))
   else private$the_fee <- constant_parameters$new(0.02, 365)

   if (!missing(barrier))
    if (is_positive_scalar(barrier)) private$the_barrier <- barrier
    else stop(error_msg_3_("barrier"))
   else private$the_barrier <- Inf

   if (!missing(penalty))
    if (is_between(penalty,0,1)) private$the_penalty <- penalty
    else stop(error_msg_8("penalty"))
   else private$the_penalty <- 0
  },
  get_age = function() private$the_age,
  set_age = function(age){
   if(!missing(age))
    if (is_positive_integer(age))
     private$the_age <- age
    else stop(error_msg_4("age"))
   else private$the_age <- 60
  },
  get_barrier = function() private$the_barrier,
  set_barrier = function(barrier) {
   if (!missing(barrier))
    if (is_positive_scalar(barrier)) private$the_barrier <- barrier
    else stop(error_msg_3_("barrier"))
   else private$the_barrier <- Inf
  },
  get_penalty = function() private$the_penalty,
  set_penalty = function(penalty) {
   if (!missing(penalty))
    if (is_between(penalty, 0, 1)) private$the_penalty <- penalty
    else stop(error_msg_8("penalty"))
  else private$the_penalty <- 0
  },
  set_fee = function(fee){
    if (!missing(fee))
      if(inherits(fee, "constant_parameters")) private$the_fee <- fee
      else stop(error_msg_1_("fee", "constant_parameters"))
      else private$the_fee <- constant_parameters$new(0.02, 365)
  },
  times_in_yrs = function() private$times_yrs,
  get_premium = function() private$the_payoff$get_premium(),
  survival_benefit_times = function(){},
  surrender_times = function(){}
 ),
 private = list(
  #payoff_guarantee object which stores the type of payoff
  the_payoff = "payoff_guarantee",
  #A posite scalar with the age of the insured
  the_age = "numeric",
  #A positive scalar with the annual VA contract fee.
  the_fee = "constant_parameters",
  #A positive scalar with the barrier for state-dependent fees.
  the_barrier = "numeric",
  #A scalar with the withdrawal penalty.
  # Must be between 0 and 1.
  the_penalty = "numeric",
  #A numeric vector with the product time-line
  #in fraction of years
  times_yrs = "numeric"
 )
)


#' Variable Annuity with GMAB guarantee
#' @description
#' Class for VA with Guaranteed Minimum Accumulation Benefit (GMAB).
#' It supports a simple state-dependent fee structure with a single barrier.\cr
#' See \bold{References} for a description of variable annuities life
#' insurance products, their guarantees and fee structures.
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom Rcpp evalCpp sourceCpp
#' @importFrom timeDate timeDate timeSequence
#' @importClassesFrom timeDate timeDate
#' @useDynLib valuer
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#'  \describe{
#'   \item{\code{new}}{Constructor method}
#'   \item{\code{get_times}}{get method for the product time-line.
#'    Returns a \code{\link{timeDate}} object}
#'   \item{\code{get_age}}{get method for the age of the insured}
#'   \item{\code{set_age}}{set method for the age of the insured}
#'   \item{\code{get_barrier}}{get method for the state-dependent fee barrier.
#'    Returns a positive scalar with the barrier}
#'   \item{\code{set_barrier}}{set method for the state-dependent fee barrier.
#'    Argument must be a positive scalar.}
#'   \item{\code{set_penalty}}{set method for the penalty applied in case of
#'    surrender. Argument must be a scalar between 0 and 1.}
#'   \item{\code{get_penalty}}{get method for the penalty applied in case of
#'     surrender. It returns a scalar between 0 and 1.}
#'   \item{\code{set_fee}}{set method for the contract fee. The argument is
#'      a \code{\link{constant_parameters}} object with the fee.}
#'   \item{\code{survival_benefit_times}}{returns a \code{numeric} vector with
#'    the survival benefit time indexes.}
#'   \item{\code{surrender_times}}{returns a \code{numeric} vector with the
#'    surrender time indexes. Takes as argument a string with the frequency
#'    of the decision if surrendering the contract,  e.g. "3m"
#'    corresponds to a surrender decision taken every 3 months.}
#'   \item{\code{times_in_yrs}}{returns the product time-line in
#'    fraction of year}
#'   \item{\code{max_number_cfs}}{returns an \code{integer} with the maximun
#'    number of cash flows the product can generate}
#'   \item{\code{cash_flow_times}}{returns a \code{\link{timeDate}} object
#'    with the possible cash flow times.}
#'   \item{\code{cash_flows}}{returns a \code{numeric} vector with the
#'    cash flows of the product. It takes as argument \code{spot_values} a
#'    \code{numeric} vector which holds the values of the underlying fund this
#'     method will calculate the cash flows from}
#'   \item{\code{survival_benefit}}{Returns a numeric scalar corresponding to
#'    the survival benefit.
#'    The arguments are \code{spot_values} vector which holds the values of
#'    the underlying fund and \code{t} the time index of the survival benefit.
#'    The function will return 0 if there's no survival benefit at the
#'    specified time}
#'   \item{\code{get_premium}}{Returns the premium as non negative scalar}
#' }
#' @references
#' \enumerate{
#' \item{[BMOP2011]}{ \cite{Bacinello A.R., Millossovich P., Olivieri A.,
  #'  Pitacco  E., "Variable annuities: a unifying valuation approach."
  #' In: Insurance: Mathematics and Economics 49 (2011), pp. 285-297.
  #' }}
#' \item{[BHM2014]}{ \cite{Bernard C., Hardy M. and Mackay A. "State-dependent
  #' fees for variable annuity guarantees." In: Astin Bulletin 44 (2014),
  #' pp. 559-585.}}
#' }
#'@usage
#'contract <- GMAB$new(rollup, times, age, fee, barrier, penalty)
#'@examples
#'#Sets up the payoff as a roll-up of premiums with roll-up rate 1%
#'
#'rate <- constant_parameters$new(0.01)
#'
#'premium <- 100
#'rollup <- payoff_rollup$new(premium, rate)
#'
#'#Five years time-line
#'times <- timeDate::timeSequence(from="2016-01-01", to="2020-12-31")
#'
#'age <- 60
#'# A constant fee of 2% per year (365 days)
#'fee <- constant_parameters$new(0.02)
#'
#'#Barrier for a state-dependent fee. The fee will be applied only if
#'#the value of the account is below the barrier
#'barrier <- 200
#'
#'#Withdrawal penalty applied in case the insured surrenders the contract
#'penalty <- 0.01
#'
#'#Sets up a VA contract with GMAB guarantee. The guaranteed miminum
#'#is the roll-up of premiums with rate 1%
#'contract <- GMAB$new(rollup, times, age, fee, barrier, penalty)



GMAB <- R6::R6Class("GMAB", inherit = va_product,
 public = list(
  max_number_cfs = function() length(private$times),

  cash_flow_times = function(death_time){
   delivery <- which(private$times == death_time)
   if (length(delivery) != 0)
    res <- private$times[1:delivery]
   else res <- private$times
   res
  },
  survival_benefit_times = function() length(private$times),
  surrender_times = function(freq){
    surr_dates <- timeDate::periods(private$times, freq, freq)$to
    surr_idx <- sapply(surr_dates, function(x) which(x == times))
    c(1, head(surr_idx, -1))
  },
  cash_flows = function(spot_values, death_time){
   fee <- private$the_fee$get()
   barrier <- private$the_barrier
   penalty <- private$the_penalty
   len <- length(spot_values)

   if (death_time < length(private$times)){
    out <- calc_account(spot_values[1:death_time], fee, barrier, penalty)
    out <- rep(out, length.out=len)
    out[(death_time+1):len] <- 0
   } else {
    t0 <- head(private$times, 1)
    t1 <- tail(private$times, 1)
    out <- calc_account(spot_values, fee, barrier, penalty)
    #GMAB living benefit
    last <- length(out)
    out[last] <- private$the_payoff$get_payoff(out[last], c(t0, t1))
   }
   out
  },
  survival_benefit = function(spot_values, t){
    last <- length(private$times)
    if (t == last){
      fee <- private$the_fee$get()
      barrier <- private$the_barrier
      penalty <- private$the_penalty
      t0 <- head(private$times, 1)
      t1 <- tail(private$times, 1)
      out <- calc_account(spot_values, fee, barrier, penalty)
      out <-private$the_payoff$get_payoff(out[last], c(t0, t1))
    } else out <- 0
    out
  }
 )
)


#' Variable Annuity with GMAB and GMDB guarantees
#' @description
#' Class for a VA with Guaranteed Minimum Accumulation Benefit (GMAB)
#' and Guaranteed Minimun Accumulation Benefit (GMDB).
#' It supports a simple state-dependent fee structure with a single barrier.\cr
#' See \bold{References} for a description of variable annuities life
#' insurance products, their guarantees and fee structures.
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom Rcpp evalCpp sourceCpp
#' @importFrom timeDate timeDate timeSequence
#' @importClassesFrom timeDate timeDate
#' @useDynLib valuer
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#'  \describe{
#'   \item{\code{new}}{Constructor method}
#'   \item{\code{get_times}}{get method for the product time-line.
#'    Returns a \code{\link{timeDate}} object}
#'   \item{\code{get_age}}{get method for the age of the insured}
#'   \item{\code{set_age}}{set method for the age of the insured}
#'   \item{\code{get_barrier}}{get method for the state-dependent fee barrier.
#'    Returns a positive scalar with the barrier}
#'   \item{\code{set_barrier}}{set method for the state-dependent fee barrier.
#'     Argument must be a positive scalar.}
#'   \item{\code{set_penalty}}{set method for the penalty applied in case of
#'    surrender. Argument must be a scalar between 0 and 1.}
#'   \item{\code{get_penalty}}{get method for the penalty applied in case of
#'     surrender. It returns a scalar between 0 and 1.}
#'   \item{\code{set_fee}}{set method for the contract fee. The argument is
#'      a \code{\link{constant_parameters}} object with the fee.}
#'   \item{\code{survival_benefit_times}}{returns a \code{numeric} vector with
#'    the survival benefit time indexes.}
#'   \item{\code{surrender_times}}{returns a \code{numeric} vector with the
#'    surrender time indexes. Takes as argument a string with the frequency
#'    of the decision if surrendering the contract,  e.g. "3m"
#'    corresponds to a surrender decision taken every 3 months.}
#'   \item{\code{times_in_yrs}}{returns the product time-line in
#'    fraction of year}
#'   \item{\code{max_number_cfs}}{returns an \code{integer} with the maximun
#'    number of cash flows the product can generate}
#'   \item{\code{cash_flow_times}}{returns a \code{\link{timeDate}} object
#'    with the possible cash flow times.}
#'   \item{\code{cash_flows}}{returns a \code{numeric} vector with the
#'    cash flows of the product. It takes as argument \code{spot_values} a
#'    \code{numeric} vector which holds the values of the underlying fund this
#'    method will calculate the cash flows from}
#'   \item{\code{survival_benefit}}{Returns a numeric scalar corresponding to
#'    the survival benefit.
#'    The arguments are \code{spot_values} vector which holds the values of
#'    the underlying fund and \code{t} the time index of the survival benefit.
#'    The function will return 0 if there's no survival benefit at the
#'    specified time}
#'   \item{\code{get_premium}}{Returns the premium as non negative scalar}
#' }
#' @references
#' \enumerate{
#' \item{[BMOP2011]}{ \cite{Bacinello A.R., Millossovich P., Olivieri A.,
#' Pitacco  E., "Variable annuities: a unifying valuation approach."
#' In: Insurance: Mathematics and Economics 49 (2011), pp. 285-297.
#' }}
#' \item{[BHM2014]}{ \cite{Bernard C., Hardy M. and Mackay A. "State-dependent
#' fees for variable annuity guarantees." In: Astin Bulletin 44 (2014),
#' pp. 559-585.}}
#' }
#'@usage
#'contract <- GMAB_GMDB$new(rollup, times, age, fee, barrier, penalty, rollup)
#'@examples
#'#Sets up the payoff as a roll-up of premiums with roll-up rate 1%
#'
#'rate <- constant_parameters$new(0.01)
#'
#'premium <- 100
#'rollup <- payoff_rollup$new(premium, rate)
#'
#'#Five years time-line
#'times <- timeDate::timeSequence(from="2016-01-01", to="2020-12-31")
#'#Age of the insured
#'age <- 60
#'# A constant fee of 2% per year (365 days)
#'fee <- constant_parameters$new(0.02, 365)
#'
#'#Barrier for a state-dependent fee. The fee will be applied only if
#'#the value of the account is below the barrier
#'barrier <- 200
#'
#'#Withdrawal penalty applied in case the insured surrenders the contract.
#'penalty <- 0.01
#'#Sets up the GMAB + GMDB with the same payoff for survival and death
#'#benefits
#'contract <- GMAB_GMDB$new(rollup, times, age, fee, barrier, penalty, rollup)

GMAB_GMDB <- R6::R6Class("GMAB_GMDB", inherit = GMAB,
 public = list(
  initialize = function(payoff, prod_times, age, fee, barrier, penalty,
                        death_payoff){
   super$initialize(payoff, prod_times, age, fee, barrier, penalty)
   if (!missing(death_payoff))
     if (inherits(death_payoff, "payoff_guarantee"))
     private$the_death_payoff <- death_payoff
     else stop(error_msg_1_("death_payoff", "payoff_guarantee"))
   else stop("Please provide a guarantee payoff object\n")
   },
  cash_flows = function(spot_values, death_time){
   fee <- private$the_fee$get()
   barrier <- private$the_barrier
   penalty <- private$the_penalty
   len <- length(spot_values)
   t0 <- head(private$times, 1)
   if (death_time < length(private$times)){
    out <- calc_account(spot_values[1:death_time], fee, barrier, penalty)
    #GMDB death benefit
    last <- length(out)
    t1 <- private$times[death_time]
    out[last] <- private$the_death_payoff$get_payoff(out[last], c(t0, t1))
    out <- rep(out, length.out=len)
    out[(death_time+1):len] <- 0
   } else {
    t1 <- tail(private$times, 1)
    out <- calc_account(spot_values, fee, barrier, penalty)
    #GMAB living benefit
    last <- length(out)
    out[last] <- private$the_payoff$get_payoff(out[last], c(t0, t1))
   }
   out
  }
 ),
 private = list(
  #A payoff_guarantee object which stores the type
  #of payoff (e.g: roll-up, ratchet, etc) for the death benefit.
  the_death_payoff = "payoff_guarantee"
 )
)




