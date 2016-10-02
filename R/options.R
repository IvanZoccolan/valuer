#Implements a vanilla option


#' Vanilla option
#' @description
#' Class for a vanilla option supporting different types of payoffs
#' @docType class
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @examples
#' #Defines the payoff of a call option
#' call_payoff <- payoff_call$new(100)
#' #Defines the call option
#' call_option <- vanilla_option$new(the_payoff = call_payoff, expiry = 1)
#' #Calculates the payoff of the call corresponding to two spot prices
#' call_option$option_payoff(c(120, 130))
#' @section Methods:
#' \describe{
#'  \item{\code{new}}{Constructor method which takes as argument a
#'   \code{the_payoff} a \code{payoff} object with the payoff and
#'   \code{expiry} a positive scalar with the expiry time.}
#'  \item{\code{get_expiry}}{get method for the expiry time.}
#'  \item{\code{option_payoff}}{returns a \code{numeric} vector with the
#'   payoff. It takes as argument \code{spot}
#'   a \code{numeric} vector with the values of the underlying asset
#'   this method will calculate the payoff from.}
#' }


vanilla_option <- R6::R6Class("vanilla_option",
 public = list(
  initialize = function(the_payoff, expiry) {
   if (!missing(the_payoff) ){
    if (inherits(the_payoff, "payoff")){
     private$pay_off <- the_payoff$clone( )
    } else stop(error_msg_1_("the_payoff", "payoff"))
   } else private$pay_off <- payoff_call$new(100)
   if (!missing(expiry)){
    if (is_positive_scalar(expiry)) {
      private$priv_expiry <- expiry
    } else stop(error_msg_3_("Expiry"))
   } else private$priv_expiry <- 1
  },
  get_expiry = function() private$priv_expiry,
  option_payoff = function(spot){
   if (inherits(spot, "numeric")) private$pay_off[spot]
   else stop(error_msg_9("spot"))
  }
 ),
 private = list(
  #positive scalar with the expiry time of the option
  priv_expiry = "numeric",
  #payoff object with the payoff
  pay_off = "payoff"
 )
)



#' Path dependent derivative product
#' @description
#' Base class which defines the interface for a path dependent derivative
#' product. This class should be inherited by subclasses implementing its
#' methods. Objects of this class should not be instantiated.
#' @docType class
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#' \describe{
#'  \item{\code{new}}{Constructor method which takes as argument a
#'   \code{\link{timeDate}} object with the product time-line.
#'   If no argument is specified it gets initialized with a
#'   year long daily sequence}
#'  \item{\code{get_times}}{get method for the product time-line.
#'   Returns a \code{\link{timeDate}} object}
#'  \item{\code{max_number_cfs}}{ returns an \code{integer} with the
#'   maximun number of cash flows the product can generate}
#'  \item{\code{cash_flow_times}}{retuns a \code{\link{timeDate}} object
#'   with the possible cash flow times. Within this base class the method
#'   simply returns the product time-line.}
#'  \item{\code{cash_flows}}{returns a \code{numeric} vector with
#'   the cash flows of the product. It takes as argument \code{spot_values}
#'   a \code{numeric} vector which holds the values of the underlying asset
#'   this method will calculate the cash flows from}
#' }

path_dependent <- R6::R6Class("path_dependent",
 public = list(
  initialize = function(the_times){
    if (!missing(the_times))
      if (inherits(the_times, "timeDate")) private$times <- the_times
      else stop(error_msg_1_("the_times", "timeDate"))
    else private$times <- timeDate::timeSequence("2016-01-01", "2016-12-31")
  },
  get_times = function() private$times,
  max_number_cfs = function() length(private$times),
  cash_flow_times = function() private$times,
  cash_flows = function(spot_values) spot_values
 ),
 private = list(
  #timeDate object to store the product time-line
  times = "timeDate"
 )
)


#'Arithmetic Asian option class
#' @description
#'Class which defines an arithmetic asian option.
#' @docType class
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#' \describe{
#'  \item{\code{new}}{Constructor method which takes as argument a
#'   \code{\link{timeDate}} object with the product time-line,
#'   a \code{\link{timeDate}} object with the date of delivery and
#'   a \code{payoff} object with the payoff. If the time line
#'   isn't specified it gets initialized with a year long daily sequence.
#'   If the delivery date is not specified it is set as the last date of the
#'   time line. If the payoff is not specified it will be set with  a call
#'   type payoff with strike 100.}
#'  \item{\code{get_times}}{get method for the product time-line. Retuns a
#'   \code{\link{timeDate}} object}
#'  \item{\code{max_number_cfs}}{returns an \code{integer} with the maximun
#'   number of cash flows the product can generate}
#'  \item{\code{cash_flow_times}}{retuns a \code{\link{timeDate}} object with
#'    the possible cash flow times. It returns the first date and the
#'    delivery date of the arithmetic asian option}
#'  \item{\code{cash_flows}}{returns a \code{numeric} vector with the
#'   cash flows of the product. It takes as argument \code{spot_values}
#'   a \code{numeric} vector which holds the values of the underlying asset
#'   this method will calculate the cash flows from}
#' }

path_dependent_asian <- R6::R6Class("path_dependent_asian",
 inherit = path_dependent,
 public = list(
  initialize = function(the_times, delivery_, payoff_){
   #Initialize the private field times
   #times must be a timeDate sequence
   #If not given it will be set as the one year sequence starting 2016-01-01
   if (!missing(the_times)){
    if (are_dates(the_times)) private$times <- the_times
    else stop(error_msg_1_("the_times", "timeDate"))
   } else private$times <- timeDate::timeSequence("2016-01-01", "2016-12-31")
   #Initializes the private field delivery_time
   #The delivery_time should be contained into the the_times sequence
   #If not given it will be set as the  last day of the private times field
   if(!missing(delivery_)){
    if (is_date(delivery_)){
     if (any(delivery_ == the_times)) private$delivery_time <- delivery_
     else stop(error_msg_6)
    } else stop(error_msg_1_("delivery", "timeDate"))
   } else private$delivery_time <- tail(private$times, 1)
   #Initializes the private field the_payoff
   #Must be an object of class payoff
   #If not given it is set as a call payoff with strike 100
   if(!missing(payoff_)){
    if(is_payoff(payoff_)) private$the_payoff <- payoff_
    else stop(error_msg_1("payoff"))
   } else private$the_payoff <- payoff_call$new(100)
  },
  max_number_cfs = function() 2L,
  cash_flow_times = function() c(private$times[1], private$delivery_time),
  cash_flows = function(spot_values){
   mean_spot <- mean(spot_values)
   cash_flow_amounts <- c(0,private$the_payoff[mean_spot])
   cash_flow_amounts
  }
 ),
 private = list(
  #timeDate object to storethe delivery time.
  delivery_time = "timeDate",
  #payoff object storing the payoff that will be payed at delivery time
  the_payoff = "payoff"
 )
)

