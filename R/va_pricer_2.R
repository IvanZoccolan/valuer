


#'@export

va_sde_engine <- R6::R6Class("va_sde_engine", inherit = va_engine, public = list(
  initialize = function(product, financial_parms, financial_setModel,
                        mortality_parms, mortality_setModel){
    super$initialize(product)
    private$financial_parms <- financial_parms
    private$financial_model <- try(yuima::setModel(
     drift = financial_setModel$drift,
     diffusion = financial_setModel$diffusion,
     jump.coeff =financial_setModel$jump.coeff,
     measure = financial_setModel$measure,
     measure.type = financial_setModel$measure.type,
     solve.variable = financial_setModel$solve.variable
     )
    )

    private$mortality_parms <- mortality_parms
    private$mortality_model <- try(yuima::setModel(
     drift = mortality_setModel$drift,
     diffusion = mortality_setModel$diffusion,
     jump.coeff =mortality_setModel$jump.coeff,
     measure = mortality_setModel$measure,
     measure.type = mortality_setModel$measure.type,
     solve.variable = mortality_setModel$solve.variable
     )
    )
   #
   no_time_intervals <- length(private$the_product$get_times()) - 1
   private$samp <- yuima::setSampling(
    Terminal = tail(private$the_product$times_in_yrs(), 1),
    n = no_time_intervals)
  },
  simulate_financial_paths = function(npaths, ind = c(1,3)){
   len <-  length(private$the_product$get_times())
   data <- lapply(seq(npaths), function(idx) private$simulate_financial_path(ind))
   #Saves the fund
   private$fund <- t(vapply(seq(npaths), function(idx) {
    data[[idx]]$fund
    }, FUN.VALUE = vector("numeric", len)))
   private$r <- t(vapply(seq(npaths), function(idx) {
     data[[idx]]$r
   }, FUN.VALUE = vector("numeric", len)))
   #Saves the discounts
   private$discounts <- t(vapply(seq(npaths), function(idx) {
    exp(-cumsum(c(0, head(data[[idx]]$r, -1))* private$samp@delta))
    }, FUN.VALUE = vector("numeric", len)))
  },
  get_fund = function(i) private$fund[i, ],
  get_discount = function(i,j) private$discounts[i, j],
  simulate_mortality_paths = function(npaths){
   len <-  length(private$the_product$get_times())
   data <- lapply(seq(npaths), function(idx)
                     private$simulate_mortality_path())
   private$mu <- t(vapply(seq(npaths), function(idx) {
     data[[idx]]$mu
   }, FUN.VALUE = vector("numeric", len)))
   private$mu_integrals <- t(vapply(seq(npaths), function(idx) {
     data[[idx]]$mu_integral
   }, FUN.VALUE = vector("numeric", len)))
  },
  death_time = function(i){
    ind <- which(private$mu_integrals[i, ] > rexp(1))
    if (length(ind) != 0)
     res <- min(ind)
    else res <- length(private$the_product$get_times())
    res
  }
 ),
 private = list(
  financial_model = "yuima.model-class",
  financial_parms = "list",
  mortality_model = "yuima.model-class",
  mortality_parms ="list",
  samp = "yuima.sampling-class",
  simulate_financial_path = function(ind = c(1, 3)){
   zoo_paths <- yuima::simulate(private$financial_model,
    xinit = private$financial_parms$xinit,
    sampling = private$samp,
    true.parameter = private$financial_parms)
   data_paths <- yuima::get.zoo.data(zoo_paths)
   data.frame(r = as.numeric(data_paths[[ind[1]]]),
             fund = exp(as.numeric(data_paths[[ind[2]]])))
  },
  fund = "matrix",
  r = "matrix",
  discounts = "matrix",
  simulate_mortality_path = function(){
    zoo_paths <- yuima::simulate(private$mortality_model,
     xinit = private$mortality_parms$xinit,
     sampling = private$samp,
     true.parameter = private$mortality_parms)
    mu <- as.numeric(yuima::get.zoo.data(zoo_paths)[[1]])
    dt <- private$samp@delta
    mu_ <- head(mu, -1)
    mu_integrals <- cumsum(c(0, mu_ * dt))
    data.frame(mu = mu, mu_integrals = mu_integrals)
  },
  mu = "matrix",
  mu_integrals = "matrix",
  bases = function(paths, time, degree){
   res <- orthopolynom::laguerre.polynomials(degree, normalized = TRUE)
   x <- private$fund[paths, time]
   r <- private$r[paths, time]
   mu <- private$mu[paths, time]
   #Normalizes to avoid underflows in calculating
   #the exponential below.
   x <- x / private$the_product$get_premium()

   x <- sapply(seq_along(res), function(i){
     exp(-0.5 * x) * (as.function(res[[i]])(x))
    })
   r <- sapply(seq_along(res), function(i){
      exp(-0.5 * r) * (as.function(res[[i]])(r))
    })
   mu <- sapply(seq_along(res), function(i){
      exp(-0.5 * mu) * (as.function(res[[i]])(mu))
    })
   cbind(x, r, mu)
  }
 )
)


