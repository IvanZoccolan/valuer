#Reimplementation of payoff class, vanilla options and statistics gatherer with R6.
#Author: Ivan Zoccolan


payoff <- R6::R6Class("payoff", public = list(initialize = function(){},
                                        get = function() {},
                                        set = function() {}),
                                        private = list()
                                        )


payoff_call <- R6::R6Class("payoff_call", inherit = payoff,
                      public = list(
                        initialize = function(x){
                          if (!missing(x)) {
                            if (is_numeric_scalar(x)){
                              private$strike <- x
                              } else stop("strike must be a numeric scalar")
                          } else private$strike <- 0.0
                        },
                        get = function() {
                          private$strike
                          },
                        set = function(x) {
                          if (is_numeric_scalar(x)){
                            private$strike <- x
                            } else stop("strike must be numeric scalar")
                        }
                      ),
                      private = list(
                        strike = "numeric"
                      )
)

#Overrides the [ operator


"[.payoff_call" <- function(x, i, j=missing){
            sapply(i, function(spot) max(spot - x$get(), 0.0))
          }

#Examples

# callpayoff <- payoff_call$new(100)

# inherits(callpayoff, "payoff")

# callpayoff$get()

# callpayoff$set(120)

# callpayoff$get()

# callpayoff[c(130, 120)]

# callpayoff$set("test")

# call <- payoff_call$new("call")

# system.time(callpayoff[c(130, 120)])

# spots <- 120 * rnorm(n = 1e4, mean = 1, sd = 0.2)

# system.time(callpayoff[spots])


#Implements a vanilla option

vanilla_option <- R6::R6Class("vanilla_option", public = list(
                         initialize = function(the_payoff, expiry) {

                           if (!missing(the_payoff) ){
                             if (inherits(the_payoff, "payoff")){
                               private$pay_off <- the_payoff$clone( )
                             } else stop("the_payoff must be a payoff object")
                           } else private$pay_off <- payoff_call$new(100)

                           if (!missing(expiry)){
                             if (is_positive_scalar(expiry)) {
                               private$priv_expiry <- expiry
                               } else stop("Expiry must be a positive numeric scalar")
                           } else private$priv_expiry <- 1
                         },
                         get_expiry = function(){
                           private$priv_expiry
                         },
                         option_payoff = function(spot){
                           if (inherits(spot, "numeric")) private$pay_off[spot]
                           else stop("Method's parameter must be a numeric vector")
                           }
                         ),

                        private = list(
                          priv_expiry = "numeric",
                          pay_off = "payoff"
                        )
                  )

#Always use clone(deep = TRUE) with vanilla_option objects as it has a field which is a R6 object
#see https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html#cloning-objects

#Examples

# option <- vanilla_option$new(callpayoff, 3)

# option$get_expiry()

# option$option_payoff(c(170, 180))

# option  <- vanilla_option$new(3)
# option <- vanilla_option$new("pippo")

# option <- vanilla_option$new( )

# option <- vanilla_option$new("pippo", 8)

# option1 <- option

# callpayoff$get()

# callpayoff$set(90)

# option$option_payoff(c(170, 180))


#Defines a parameter class

parameters <- R6::R6Class("parameters", public = list(
  integral = function(t1, t2) {},
  integral_square = function(t1, t2) {}
  )
)

constant_parameters <- R6::R6Class("constant_parameters", public = list(
  initialize = function(const){
    if (!missing(const)){
      if (is_numeric_scalar(const)){
        private$constant <- const
        private$constant_square <- const ^ 2
      } else stop("Constant must be a numeric scalar")
    } else {
       private$constant <- 0.0
       private$constant_square <- 0.0
     }
   },
  integral = function(t1, t2){
    (t2 - t1) * private$constant
  },
  integral_square = function(t1, t2){
    (t2 - t1) * private$constant_square
  }
 ),
  private = list(
    constant = 0.0,
    constant_square = 0.0
 ),
 inherit = parameters
)


#Examples

#r <- constant_parameters$new(0.02)

#r$integral(0, 10)

#r$integral_square(0, 10)

#r <- constant_parameters$new()

#r$integral(0, 1)

#r <- constant_parameters$new(c(1, 2))

#r <- constant_parameters$new("pippo")
