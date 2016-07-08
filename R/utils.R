
#Utility functions

is_numeric_scalar <- function(x) inherits(x, "numeric") & isTRUE(length(x) == 1)

is_positive_scalar <- function(x) {
  inherits(x, "numeric") & isTRUE(length(x) == 1) &
    isTRUE(x > 0)
}

is_not_negative_scalar <- function(x){
  inherits(x, "numeric") & isTRUE(length(x) == 1) &
    isTRUE(x >= 0)
}

same_length <- function(x,y) isTRUE(length(x) == length(y))

#Error messages

error_msg_1 <- function(object) paste("argument must be a ", object, " object\n")
error_msg_2 <- function(object) paste("arguments must be ", object, " objects\n")
error_msg_3 <- "argument must be a positive scalar\n"

