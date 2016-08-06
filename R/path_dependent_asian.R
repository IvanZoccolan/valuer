

#'Arithmetic Asian option class
#' @description Class which defines an arithmetic asian option. Given it's a path dependent
#'derivative product it inherits from the path_dependent base class.
#' @docType class
#' @importFrom R6 R6Class
#' @importClassesFrom timeDate timeDate
#' @importFrom timeDate timeDate timeSequence
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @field time_index A \code{\link{timeDate}} object storing the product time-line.
#' @field delivery_time A \code{\link{timeDate}} object storing the delivery time.
#' @field the_payoff A \code{\link{payoff}} object storing the payoff that will be payed at delivery time
#' @section Methods:
#' \describe{
#'   \item{\code{new}}{Constructor method which takes as argument a \code{\link{timeDate}} object with the product time-line,
#'   a \code{\link{timeDate}} object with the date of delivery and a \code{\link{payoff}} object with the payoff. If the time line isn't specified it gets initialized with a year long daily sequence. If the delivery date is not specified it is set as the last date of the time line. If the payoff is not specified it will be set with  a call type payoff with strike 100.}
#'   \item{\code{get_times}}{get method for the product time-line. Retuns a \code{\link{timeDate}} object}
#'   \item{\code{max_number_cfs}}{ returns an \code{integer} with the maximun number of cash flows the product can generate}
#'   \item{\code{cash_flow_times}}{retuns a \code{\link{timeDate}} object with the possible cash flow times. It returns the first date and the  delivery date of the arithmetic asian option}
#'   \item{\code{cash_flows}}{returns a \code{\link{cash_flows}} object with the cash flows of the product. It takes as argument \code{spot_values} a \code{numeric} vector which holds the values of the underlying asset this method will calculate the cash flows from}
#'   }

path_dependent_asian <- R6::R6Class("path_dependent_asian", inherit = path_dependent,
  public = list(
    initialize = function(the_times, delivery_, payoff_){

      #Initialize the private field times
      #times must be a timeDate sequence
      #If not given it will be set as the one year sequence starting 2016-01-01
      if (!missing(the_times)){
        if (are_dates(the_times)) private$times <- the_times
        else stop(error_msg_1("timeDate"))
      } else private$times <- timeDate::timeSequence("2016-01-01", "2016-12-31")

      #Initializes the private field delivery_time
      #The delivery_time should be contained into the the_times sequence
      #If not given it will be set as the  last day of the private times field

      if(!missing(delivery_)){
        if (is_date(delivery_)){
          if (any(delivery_ == the_times)) private$delivery_time <- delivery_
          else stop(error_msg_6)
        } else stop(error_msg_1("timeDate"))
      } else private$delivery_time <- tail(private$times, 1)

      #Initializes the private field the_payoff
      #Must be an object of class payoff
      #If not given it is set as a call payoff with strike 100

      if(!missing(payoff_)){
        if(is_payoff(payoff_)) private$the_payoff <- payoff_
        else stop(error_msg_1("payoff"))
      }else private$the_payoff <- payoff_call$new(100)

    },


    max_number_cfs = function() {2L},

    cash_flow_times = function() {c(private$times[1], private$delivery_time)},

    cash_flows = function(spot_values){

      mean_spot <- mean(spot_values)
      cash_flow_times <- c(private$times[1], private$delivery_time)
      cash_flow_amounts <- c(0,private$the_payoff[mean_spot])
      cash_flows$new(cash_flow_times, cash_flow_amounts)

    }
  ),

  private = list(
    delivery_time = "timeDate",
    the_payoff = "payoff"

  )
)
