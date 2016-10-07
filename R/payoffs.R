#Copyright 2016 Ivan Zoccolan

#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#A copy of the GNU General Public License is available at
#https://www.R-project.org/Licenses/ and included in the R distribution
#(in directory share/licenses).


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
  initialize = function(premium, ...){
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
  get_payoff = function(amount, t, ...) return(0)
 ),
 private = list(
  #numeric scalar which stores the premium
  the_premium = "numeric"
 )
)


#' Roll-up of premiums payoff class
#' @description
#' Class providing a roll-up of premium payoff object. The payoff is the
#' maximum between the account value and the roll-up of the premium
#' at a given rate.
#' @docType class
#' @examples
#' rate <- constant_parameters$new(0.01)
#' premium <- 100
#' rollup <- payoff_rollup$new(premium, rate)
#' t1 <- timeDate::timeDate("2016-01-01")
#' t2 <- timeDate::timeDate("2016-12-31")
#' rollup$get_payoff(c(120,100), c(t1,t2))
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
  get_payoff = function(amount, t, ...){
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


#' Ratchet payoff class
#' @description
#' Class providing a ratchet payoff object.
#' The payoff will be the highest account value recorded at
#' some specified times.
#' @docType class
#' @examples
#' freq <- "1m"
#' premium <- 100
#' ratchet <- payoff_ratchet$new(premium, freq)
#' t1 <- timeDate::timeDate("2016-01-01")
#' t2 <- timeDate::timeDate("2016-12-31")
#' account <- 120 * rnorm(365)
#' ratchet$get_payoff(c(120,100), c(t1,t2), account)
#' @export
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#'  \describe{
#'   \item{\code{new}}{Initialize method.
#'    The arguments are a non negative scalar with the premium and the
#'    ratchet frequency. Allowed units for the frequency are "m" for
#'     4 weeks, "w" for weeks, "d" for days}
#'   \item{\code{set_premium}}{Stores the premium in a
#'    private field. The argument is a non negative scalar}
#'   \item{\code{get_premium}}{Returns the premium
#'    as non negative scalar}
#'   \item{\code{set_freq}}{Sets the ratchet frequency.
#'    Allowed units for the frequency are "m" for
#'    4 weeks, "w" for weeks, "d" for days}
#'   \item{\code{get_payoff}}{Gets the payoff.
#'    The arguments are a \code{numeric} vector
#'    with the amounts, a vector of \code{\link{timeDate}} objects
#'    with the start and end dates for the ratchet and a \code{numeric}
#'    vector with the account values. (see \bold{Examples})}
#'}


payoff_ratchet <- R6::R6Class("payoff_ratchet", inherit = payoff_guarantee,
 public = list(
  initialize = function(premium, ratchet_freq = "1m"){
    super$initialize(premium)
    units <- c("m", "w", "d")
    freq_unit = gsub("[ 0-9]", "", ratchet_freq, perl = TRUE)
    if (freq_unit %in% units)
     private$freq <- ratchet_freq
    else stop(error_msg_10())
  },
  set_freq = function(ratchet_freq) {
    units <- c("m", "w", "d")
    freq_unit = gsub("[ 0-9]", "", ratchet_freq, perl = TRUE)
    if (freq_unit %in% units)
      private$freq <- ratchet_freq
    else stop(error_msg_10())
  },
  get_freq = function(ratchet_freq) private$freq,
  get_payoff = function(amount, t, amounts){
    t <- timeDate::timeSequence(from = t[1], to = t[2])
    freq <- private$freq
    ratchet_dates <- timeDate::periods(t, freq, freq)$to
    ratchet_idx <- sapply(ratchet_dates, function(x) which(x == t))
    ratchet_idx <- c(1, head(ratchet_idx, -1))
    guarantee <- max(amounts[ratchet_idx])
    sapply(amount, function(i) max(i, guarantee))
  }
 ),
 private = list(
   freq = "1m"
 )
)
