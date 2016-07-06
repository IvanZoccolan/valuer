#Base classes for a pricing engine of exotic (path dependent) options.
#Porting of C++ code by M. Joshi from C++ Design Patterns and Derivatives Pricing


#' Class providing a cash flow object with methods to get/set the cash flow amounts.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @return Object of \code{\link{R6Class}} with methods to get/set the cash flow amounts.
#' @format \code{\link{R6Class}} object.
#' @examples
#' tt <- timeDate::Sys.timeDate()
#' cash <- cash_flows$new(tt, 50.89)
#' cash$get_amounts()
#' cash$set_amounts(60.00)
#' cash$get_amounts()
#' @field amounts \code{numeric} Stores the amounts of the cash flows.
#' @field time_index \code{\link{timeDate}} Stores the time-line of the cash flows.
#' @section Methods:
#' \describe{
#'   \item{\code{get_amounts}}{get method for the amounts of the cash flow. Returns a \code{numeric} scalar or vector}
#'
#'   \item{\code{set_amounts}}{set method for the cash flow amounts }
#'   }



cash_flows <- R6::R6Class("cash_flows", public = list(
  get_amounts = function(){private$amounts},
  set_amounts = function(the_amounts){
    if(!missing(the_amounts))
      if (inherits(the_amounts, "numeric")) private$amounts <- the_amounts
      else stop("argument must be numeric\n")
    else private$amounts <- numeric(0)
  },
  initialize = function(time, the_amounts){
    if (!missing(time)){
      if (inherits(time, "timeDate")) private$time_index <- time
      else stop("argument must be a timeDate object\n")
    } else private$time_index <- timeDate::Sys.timeDate()

    if(!missing(the_amounts)){
      if (inherits(the_amounts, "numeric")) private$amounts <- the_amounts
      else stop("argument must be numeric\n")
    } else private$amounts <- 0
  }
  ),
 private = list(
  amounts = "numeric",
  time_index = "timeDate"
 )
)


#' Class which defines  the interface for a  path dependent derivative product
#' @description Base class which defines the interface for a path dependent derivative
#' product. This class should be inherited by subclasses implementing its methods.
#' Objects of this class should not be instantiated.
#' @docType class
#' @importFrom R6 R6Class
#' @importClassesFrom timeDate timeDate
#' @importFrom timeDate timeDate timeSequence
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @field time_index A \code{\link{timeDate}} object storing the product time-line .
#' @section Methods:
#' \describe{
#'   \item{\code{new}}{Constructor method which takes as argument a \code{\link{timeDate}} object with the product time-line. If no argument is specified it gets initialized with a year long daily sequence}
#'   \item{\code{get_times}}{get method for the product time-line. Retuns a \code{\link{timeDate}} object}
#'   \item{\code{max_number_cfs}}{ returns an \code{integer} with the maximun number of cash flows the product can generate}
#'   \item{\code{cash_flow_times}}{retuns a \code{\link{timeDate}} object with the possible cash flow times. Within this base class the method simply returns the product time-line.}
#'   \item{\code{cash_flows}}{returns a \code{\link{cash_flows}} object with the cash flows of the product}
#'   }

path_dependent <- R6::R6Class("path_dependent", public = list(
    initialize = function(the_times){
      if (!missing(the_times))
        if (inherits(the_times, "timeDate")) private$times <- the_times
        else stop("argument must be a timeDate object\n")
      else private$times <- timeDate::timeSequence("2016-01-01", "2016-12-31")
    },
    get_times = function() private$times,
    max_number_cfs = function() length(private$times),
    cash_flow_times = function() private$times,
    cash_flows = function(spot_values){
      tt  <- self$cash_flow_times()
      cash_flows$new(tt, numeric(length(tt)))
    }
    ),
  private = list(
    times = "timeDate"
  )
)


exotic_engine <- R6Class("exotic_engine",
  public = list(
    initialize = function(product, interest){
      if(!missing(product))
        if (inherits(product, "path_dependent")) {
          private$the_product <- product
          private$these_cash_flows <- product$cash_flows()
        } else stop("argument must be a path_dependent object")
      else stop("argument must be a path_dependent object")

      if(!missing(interest))
        if(inherits(interest, "parameters")) {
          private$r <- interest
          cf_times <- product$cash_flow_times()
          t0 <- cf_times[1]
          log_discounts <- sapply(cf_times, function(t) -interest$integral(t0, t))
          private$discounts <- exp(log_discounts)
        } else stop("argument must be a parameters object")
      else stop("argument must be a parameters object")
    },
    get_one_path = function(){},
    do_simulation = function(the_gatherer, number_of_paths){},
    do_one_path = function(){}
  ),
  private = list(
    the_product = "path_dependent",
    r = "parameters",
    discounts = "numeric",
    these_cash_flows = "cash_flows"
  )
)
