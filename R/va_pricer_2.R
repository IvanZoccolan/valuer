


#'@export

va_sde_engine <- R6::R6Class("va_sde_engine", inherit = va_engine,
 public = list(
  initialize = function(product, financial_parms, mortality_parms){

    if (!requireNamespace("yuima", quietly = TRUE))
      stop("This class needs the yuima pkg. Please install it.",
           call. = FALSE)
    super$initialize(product)

    private$financial_parms <- financial_parms
    private$financial_model <- do.call(yuima::setModel,
     financial_parms[[2]])

    private$mortality_parms <- mortality_parms
    private$mortality_model <- do.call(yuima::setModel,
     mortality_parms[[2]])

   #
   no_time_intervals <- length(private$the_product$get_times()) - 1
   private$samp <- yuima::setSampling(
    Terminal = tail(private$the_product$times_in_yrs(), 1),
    n = no_time_intervals)
  },
  simulate_financial_paths = function(npaths){
   ind <- private$financial_parms[[3]]
   #Builds parameter list for yuima::simulate
   parms <- list(object = private$financial_model,
     xinit = private$financial_parms[[1]]$xinit,
     sampling = private$samp,
     true.parameter =  private$financial_parms[[1]])

   len <-  length(private$the_product$get_times())
   #Sets storage for fund, interest rate(r) and
   #discount factors simulated paths
   private$fund <- matrix(NA, npaths, len)
   private$r <- matrix(NA, npaths, len)
   private$discounts <- matrix(NA, npaths, len)
   #Simulates the underlying fund spot prices, the interest rate paths
   #and calculates the discount factors paths
   for (i in seq(npaths)){
     zoo_paths <- do.call(yuima::simulate, parms)
     data_paths <- yuima::get.zoo.data(zoo_paths)
     private$fund[i, ] <- exp(as.numeric(data_paths[[ind[2]]]))
     private$r[i, ] <- as.numeric(data_paths[[ind[1]]])
     private$discounts[i, ] <- exp(-cumsum(c(0, head(private$r[i, ], -1)) *                                           private$samp@delta))
   }

  },
  get_fund = function(i) private$fund[i, ],
  get_discount = function(i,j) private$discounts[i, j],
  get_int = function(i) private$mu_integrals[i, ],
  simulate_mortality_paths = function(npaths){

   ind <- private$mortality_parms[[3]]
   #Sets the arguments to call yuima::simulate
   parms <- list(
   object = private$mortality_model,
   xinit = private$mortality_parms[[1]]$xinit,
   sampling = private$samp,
   true.parameter = private$mortality_parms[[1]]
   )
   #Sets up storage for the intensity of mortality
   #and int of mort integrals paths
   len <-  length(private$the_product$get_times())
   private$mu <- matrix(NA, npaths, len)
   private$mu_integrals <- matrix(NA, npaths, len)
   #Sets up the intensity of mortality and
   #calculates the int of mortality integrals
   for(i in seq(npaths)){
    zoo_paths <- do.call(yuima::simulate, parms)
    private$mu[i, ] <- yuima::get.zoo.data(zoo_paths)[[ind]]
    dt <- private$samp@delta
    mu_ <- head(private$mu[i, ], -1)
    private$mu_integrals[i, ] <- cumsum(c(0, mu_ * dt))
   }
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
  fund = "matrix",
  r = "matrix",
  discounts = "matrix",
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


