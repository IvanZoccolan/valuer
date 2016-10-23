

GMWB <- R6::R6Class("GMWB", inherit = va_product,
 public = list(
  initialize = function(payoff, t0, t1, age, fee, barrier, penalty, type, freq){

   if (!missing(payoff))
    if (inherits(payoff, "payoff_GMWB")){
     super$initialize(payoff, t0 = t0, t1 = t1, age = age, fee = fee,
                      barrier = barrier, penalty = penalty)
    } else stop(error_msg_1_("payoff", "payoff_GMWB"))
   else stop(error_msg_1_("payoff", "payoff_GMWB"))

   if(!missing(type))
    if (is_identical_to_any(type, c("Wa", "Wb", "Wc")))
      private$type <- type
     else stop("type must be either \"Wa\" or \"Wb\" or \"Wc\"")
   else private$type <- "Wa"

   if(is_identical_to_any(private$type,  "Wc")){
     max_age <- 120
     private$times <- timeDate::timeSequence(private$t0,
                       length.out = 365 * (max_age - private$the_age))
     # Normalizes the product time line into year fractions
     private$times_yrs <- yr_fractions(private$times)
     private$t <- tail(private$times, 1)
   }
   #Check on freq units
   if (!missing(freq)){
    units <- c("m")
    freq_unit = gsub("[ 0-9]", "", freq, perl = TRUE)
    if (!(freq_unit %in% units)) stop("Allow unit for argument freq is 'm'")
    else private$freq <- freq
   } else private$freq <- "12m"

  },
  get_freq = function() private$freq,
  set_freq = function(freq) {
    units <- c("m")
    freq_unit = gsub("[ 0-9]", "", freq, perl = TRUE)
    if (!(freq_unit %in% units)) stop("Allow unit for argument freq is 'm'")
    else private$freq <- freq
  },
  survival_benefit_times = function( ){
   freq <- private$freq
   surv_dates <- timeDate::periods(private$times, freq, freq)$to
   surv_dates[length(surv_dates)] <- private$times[length(private$times)]
   surv_idx <- vector(mode = "numeric", length = length(surv_dates))
   for (i in seq_along(surv_dates))
     surv_idx[i] <- which(surv_dates[i] == private$times)
   c(1, surv_idx)
  },
  surrender_times = function(freq = "3m"){
   #Check on freq units
   units <- c("m", "w", "d")
   freq_unit = gsub("[ 0-9]", "", freq, perl = TRUE)
   if (!(freq_unit %in% units)) stop(error_msg_10())
   surr_dates <- timeDate::periods(private$times, freq, freq)$to
   surr_dates[length(surr_dates)] <- private$times[length(private$times)]
   surr_idx <- vector(mode = "numeric", length = length(surr_dates))
   for (i in seq_along(surr_dates))
    surr_idx[i] <- which(surr_dates[i] == private$times)
   head(surr_idx, -1)
  },
  cash_flows = function(spot_values, death_time, discounts){
   fee <- private$the_fee$get()
   barrier <- private$the_barrier
   penalty <- private$the_penalty
   len <- length(spot_values)
   times <- self$survival_benefit_times()
   if (private$type == 'Wa'){
     ben <- rep(0, len)
     ben[head(times, -1)] <-  private$the_payoff$get_payoff()
     out <- calc_account(spot_values, ben, fee, barrier, penalty)
     if (death_time < len){
      remaining_payments <- discounts * ben[death_time : len]
      out[death_time] <- max(out[death_time], remaining_payments)
     }
    } else {
      if (death_time < len){
       ben <- rep(0, death_time)
       ben[head(times, -1)] <-  private$the_payoff$get_payoff()
       out <- calc_account(spot_values[1 : death_time], ben, fee, barrier, penalty)
       out <- rep(out, length.out=len)
       out[death_time] <- out[death_time]
       out[(death_time + 1) : len] <- 0
      } else {
       ben <- rep(0, len)
       ben[head(times, -1)] <- private$the_payoff$get_payoff()
       out <- calc_account(spot_values, ben, fee, barrier, penalty)
      }
    }
   out
  },
  survival_benefit = function(spot_values, death_time, time){
   times <- self$survival_benefit_times()

   if(isTRUE(time %in% times) & time < death_time)
    out <- private$the_payoff$get_payoff()
   else out <- 0

   if(time == tail(times, 1) & time < death_time)
    if(isTRUE(private$type %in% c("Wa", "Wb"))){
     fee <- private$the_fee$get()
     barrier <- self$get_barrier()
     penalty <- self$get_penalty()
     ben <- rep(0, length(spot_values))
     ben[head(times, -1)] <-  private$the_payoff$get_payoff()
     out <- calc_account(spot_values, ben, fee, barrier, penalty)
     out <- tail(out, 1)
   }
   out
  }
 ),
 private = list(
  #Type of the withdrawal benefit
  type = "Wa",
  freq = "y"
 )
)
