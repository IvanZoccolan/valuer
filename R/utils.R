
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
