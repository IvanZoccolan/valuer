#Base classes for a pricing engine of exotic (path dependent) options.
#Porting of C++ code by M. Joshi from C++ Design Patterns and Derivatives Pricing


#' Class providing a cash flow object with methods to get/set the cash flow amount.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @return Object of \code{\link{R6Class}} with methods to get/set the cash flow amount.
#' @format \code{\link{R6Class}} object.
#' @examples
#' tt <- timeDate::Sys.timeDate()
#' cash <- cash_flow$new(tt, 50.89)
#' cash$get_amount()
#' cash$set_amount(60.00)
#' cash$get_amount()
#' @field amount \code{numeric} Stores the amount of the cash flow.
#' @field time_index \code{\link{timeDate}} Stores the time index of the cash flow.
#' @section Methods:
#' \describe{
#'   \item{\code{get_amount}}{get method for the amount of the cash flow}
#'
#'   \item{\code{set_amount}}{set method for the amount of the cash flow }
#'   }



cash_flow <- R6::R6Class("cash_flow", public = list(
  get_amount = function(){private$amount},
  set_amount = function(the_amount){
    if(!missing(the_amount))
      if (is_not_negative_scalar(the_amount)) private$amount <- the_amount
      else stop("argument must be a non-negative scalar\n")
    else private$amount <- 0
  },
  initialize = function(time, the_amount){
    if (!missing(time)){
      if (inherits(time, "timeDate")) private$time_index <- time
      else stop("argument must be a timeDate object\n")
    } else private$time_index <- timeDate::Sys.timeDate()

    if(!missing(the_amount)){
      if (inherits(the_amount, "numeric")) private$amount <- the_amount
      else stop("argument must be numeric\n")
    } else private$amount <- 0.
  }
  ),
 private = list(
  amount = "numeric",
  time_index = "timeDate"
 )
)


#' Class which defines  the interface for a  path dependent derivative product
#' @description Base class which defines the interface for a path dependent derivative
#' product. This class should be imported by subclasses implementing its methods.
#' Objects of this class should not be instantiated.
#' @docType class
#' @importFrom R6 R6Class
#' @importClassesFrom timeDate timeDate
#' @importFrom timeDate timeDate timeSequence
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @field time_index A \code{\link{timeDate}} object storing the cash flow times .
#' @section Methods:
#' \describe{
#'   \item{\code{get_times}}{get method for the cash flow times}
#'   \item{\code{max_number_cfs}}{returns the maximun number of cash flows the product can generate}
#'   \item{\code{cash_flow_times}}{retuns the possible cash flow times of the product}
#'   \item{\code{cash_flows}}{returns the cash flows of the product}
#'   }

path_dependent <- R6::R6Class("path_dependent", public = list(
    initialize = function(the_times){
      if (!missing(the_times))
        if (inherits(the_times, "timeDate")) private$times <- the_times
        else stop("argument must be a timeDate object\n")
      else private$times <- timeDate::Sys.timeDate()
    },
    get_times = function(){private$times},
    max_number_cfs = function(){},
    cash_flow_times = function(){},
    cash_flows = function(spot_values, generated_flows){}),
  private = list(
    times = "timeDate"
  )
)
