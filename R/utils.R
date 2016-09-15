
#Utility functions

is_numeric_scalar <- function(x, ...) tryCatch(inherits(x, "numeric") & isTRUE(length(x) == 1),
                                          error = function(e) FALSE)

is_positive_scalar <- function(x, ...) {
  tryCatch(inherits(x, "numeric") & isTRUE(length(x) == 1) &
    isTRUE(x > 0), error = function(e) FALSE)
}

is_not_negative_scalar <- function(x, ...){
  tryCatch(inherits(x, "numeric") & isTRUE(length(x) == 1) &
    isTRUE(x >= 0), error = function(e) FALSE)
}

is_integer <- function(x, ...){

  if (tryCatch((inherits(x, "numeric")), error = function(e) FALSE))
    return(isTRUE(x == round(x)))
  else FALSE

}

is_positive_integer <- function(x, ...){

  if (tryCatch((inherits(x, "numeric")), error = function(e) FALSE))
    return(isTRUE(x == round(x)) & isTRUE(x > 0))
  else FALSE

}

are_dates <- function(dates, ...) tryCatch(inherits(dates, "timeDate"), error = function(e) FALSE)

is_date <- function(date, ...) tryCatch((inherits(date, "timeDate") & (length(date) == 1)),
                                   error = function(e) FALSE )

same_length <- function(x,y, ...) tryCatch(isTRUE(length(x) == length(y)), error = function(e) FALSE)

is_payoff <- function(x, ...) tryCatch(inherits(x, "payoff"), error = function(e) FALSE)

is_between <- function(x,lower,upper, ...) tryCatch(isTRUE(x >= lower) & isTRUE(x <= upper), error = function(e) FALSE)

#Error messages

error_msg_1 <- function(object) paste("argument must be a ", object, " object\n")
error_msg_2 <- function(object) paste("arguments must be ", object, " objects\n")
error_msg_3 <- "argument must be a positive scalar\n"
error_msg_4 <- function(arg) paste("argument", arg, "must be a positive integer\n")
error_msg_5 <- function(arg) paste("argument", arg, "must be a numeric scalar\n")
error_msg_6 <- "The delivery time should be one of the possible product dates\n"
error_msg_7 <- function(arg) paste("argument", arg, "must be a non negative scalar\n")
error_msg_8 <- function(arg) paste("argument", arg, "must be between 0 and 1\n")




#' Normalizes a timeDate sequence into year fractions
#'@param times A \code{\link{timeDate}} sequence

yr_fractions <- function(times){

 t_periods <- timeDate::periods(times, period = "12m", by="12m")
 t_diffs <- as.numeric(timeDate::difftimeDate(t_periods$to, t_periods$from)) + 1
 ind <- seq_along(t_diffs)
 out <- sapply(ind, function(i) i - 1 + seq(t_diffs[i]) / t_diffs[i])
 c(0, unlist(out))

}


#Takes square root if positive otherwise returns zero.
#To be used with mean reverting squared root processes (CIR SDE)
sq <- function(x){y = 0;if(x>0){y = sqrt(x);};return(y);}

#Deterministic intensity of mortality ( Weibull )
mu <-  function(t, x, c1, c2) {(c1^(-c2))*c2*((x + t)^(c2 -1))}


