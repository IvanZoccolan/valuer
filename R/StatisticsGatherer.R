#Defines the statistics gatherer class
#The statistics gatherer object will be changed its state by functions
#and other object methods and must retain the change at each step.
#In R we will need to use classes with reference semantics such as
#Reference Classes or R6


#This class defines the interface only.
#Implementation will be done in subclasses inheriting this one.


gatherer <-  R6::R6Class("gatherer",
  public = list( dump_result = function(result) {},
                 get_results = function() {})

)


#'Monte Carlo gatherer
#' @description Class which defines a gatherer for the Monte Carlo simulated
#' values. It has methods to return the Monte Carlo estimate and Monte Carlo
#' Standard Error of the estimate as well as a convergence table.
#' @usage
#' the_gatherer <- mc_gatherer$new()
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#' \describe{
#'   \item{\code{new}}{Constructor method}
#'   \item{\code{dump_results}}{Saves the argument \code{result} which is
#'   a numeric vector of simulated values}
#'   \item{\code{get_results}}{Returns the  Monte Carlo estimate and the
#'   (estimated) Monte Carlo Standard Error of the estimate}
#'   \item{\code{convergence_table}}{Returns the convergence table}
#' }
#'
mc_gatherer  <- R6::R6Class("mc_gatherer", inherit = gatherer,
 public = list(
  initialize = function(){ private$values <- 0.0 },
  dump_result = function(result){
   if (inherits(result, "numeric")) private$values <- result
   else stop("result must be a numeric vector\n")
  },
  get_results = function(){
   data.frame(mean = mean(private$values),
              se  =  sd(private$values) / sqrt(length(private$values))
   )},
  convergence_table = function(){
   if(!is.null(private$conv_table)) private$conv_table
   else {
    cum_mean <- cumsum(private$values) / seq_along(private$values)
    cum_se <- cumsd(private$values) / sqrt(seq_along(private$values))
    private$conv_table <- data.frame(mean = cum_mean, se = cum_se)
    private$conv_table
   }
  },
  path_done = function() length(private$values)
 ),
 private = list(
  values = "numeric",
  conv_table = NULL
 )
)

#Plot function for the mc_gatherer class.
#It prints the convergence graph at 95% confidence level.

#'@export

plot.mc_gatherer <- function(the_gatherer){
 table <- the_gatherer$convergence_table()
 df <- data.frame(No_of_simulations = seq(the_gatherer$path_done()),
        lower = table$mean - 1.96 * table$se, mean = table$mean,
        upper = table$mean + 1.96 * table$se)
 df <- tail(df, -1)
 p <-  ggplot2:::ggplot(df, ggplot2::aes(x = No_of_simulations, y = mean,
                                         colour = "Estimates"))
 p <- p + ggplot2::labs(x = "Number of simulated paths", y = "")
 p <- p + ggplot2::geom_line(ggplot2::aes(y = lower, colour = "Bounds"))
 p <- p + ggplot2::geom_line(ggplot2::aes(y = upper, colour = "Bounds"))
 p <- p + ggplot2::geom_line() + ggplot2::labs(colour ="")
 p <- p + ggplot2::ggtitle("Monte Carlo convergence graph at 95% level")
 p <- p + ggplot2::theme_bw()
 p

}

