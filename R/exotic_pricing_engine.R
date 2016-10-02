#Base classes for a pricing engine of an exotic (path dependent)
#derivative product. It follows the ideas and framework shown in
#C++ Design Patterns and Derivatives Pricing by M. Joshi.


#' Pricing engine of a path dependent derivative
#' @description
#' Base class which defines the interface of a pricing engine of
#' a path dependent european derivative product. This class should
#' be inherited by subclasses implementing its methods and not be
#' instantiated directly.
#' @docType class
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#' \describe{
#'  \item{\code{new}}{Constructor method which takes as argument a
#'   \code{\link{path_dependent}} object with the product and a
#'   \code{parameters} object with the interest rate}
#'  \item{\code{get_one_path}}{Returns a \code{numeric}
#'   vector with a simulated path of the product underlying asset. This
#'   method needs to be implemented by subclasses.}
#'  \item{\code{run_simulation}}{Runs the Monte Carlo
#'   simulation and stores the results in a \code{gatherer} object.
#'   Takes as arguments a \code{gatherer} object to store
#'   the results and an \code{integer} scalar with the number of paths
#'   to simulate.}
#'  \item{\code{discount_one_path}}{Discounts a simulated path of the
#'   underlying asset. It takes as argument \code{spot_values} a
#'   \code{numeric} vector of simulated values of the underlying asset,
#'   calculates the cash flows and takes their present value given the
#'   discount factors. Returns a \code{numeric} scalar with the present
#'   value of the discounted product cash flows.}
#'  \item{\code{discounts}}{\code{\link{R6Class}} active binding which
#'   calculates the discount factors.}
#' }



exotic_engine <- R6::R6Class("exotic_engine",
 public = list(
  initialize = function(product, interest){
   if(!missing(product))
    if (inherits(product, "path_dependent")){
     private$the_product <- product
    } else stop(error_msg_1_("product", "path_dependent"))
   else stop(error_msg_1_("product", "path_dependent"))

   if(!missing(interest))
    if(inherits(interest, "parameters")){
     private$r <- interest
     } else stop(error_msg_1_("interest", "parameters"))
   else stop(error_msg_1_("interest", "parameters"))
  },
  get_one_path = function(){},
  run_simulation = function(the_gatherer, number_of_paths){
   res <- sapply(seq(number_of_paths), function(index){
   spot_values <- self$get_one_path()
   this_value <- self$discount_one_path(spot_values)
   })
   the_gatherer$dump_result(res)
  },
  discount_one_path = function(spot_values){
   these_cash_flows <- private$the_product$cash_flows(spot_values)
   sum(these_cash_flows * self$discounts)
   }
  ),
  active = list(
   discounts  = function(){
    cf_times <- private$the_product$cash_flow_times()
    t0 <- cf_times[1]
    log_discounts <- sapply(cf_times, function(t) -private$r$integral(t0, t))
    exp(log_discounts)
   }
  ),
  private = list(
   #path_dependent object with the product the engine will price
   the_product = "path_dependent",
   #parameters object with the interest rate
    r = "parameters"
  )
)
