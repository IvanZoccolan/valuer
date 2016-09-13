
#Takes square root if positive otherwise returns zero.
#To be used with mean reverting squared root processes (CIR SDE)
sq <- function(x){y = 0;if(x>0){y = sqrt(x);};return(y);}

#Deterministic intensity of mortality ( Weibull )
mu <-  function(t, x, c1, c2) {(c1^(-c2))*c2*((x + t)^(c2 -1))}


financial_parms <- list(
  #Interest rate parameters
  a = 0.60, b = 0.05, sigma.r = 0.03,
  #Stochastic volatility parameters
  c = 1.50, d = 0.04, sigma.K = 0.40,
  #Fund
  rho.Sr = 0.00,
  rho.SK = -0.70,
  #Jump component
  lambda.Y = 0.50,
  mu.Y = 0.00,
  sigma.Y = 0.07,

  #Initial values of processes
  xinit = c(r0 = 0.05, K0 = 0.04, Y0 = log(100)) #P = 100
)

financial_setModel <- list(
 solve.variable = c("r", "K", "S"),

 drift = c("a * (b - r)", "c * (d - K)", "r - 0.5 * K -lambda.Y * mu.Y"),

 diffusion = matrix(c("sigma.r * sq(r)", "0", "rho.Sr * sq(K)",
                 "0", "sigma.K * sq(K)", "rho.SK * sq(K)",
                 "0", "0", "sqrt(1 - (rho.SK^2) - (rho.Sr^2)) * sq(K)"), 3,3),

 jump.coeff = matrix(c("0","0","1"), 3, 1),

 measure = list(intensity = 0.5, df = list("dnorm(z, 0.00, 0.07)")),

 measure.type = "CP"

)


mortality_parms <- list(
  x = 40,
  e = 0.5,
  #
  sigma.mu = 0.03,
  #
  c1 = 83.70,
  #
  c2 = 8.30,
  xinit =mu(0, x = 40 , c1 = 83.70, c2 = 8.30)
  )

mortality_setModel <- list(
 drift="e * (mu(t, mortality_parms$x, mortality_parms$c1, mortality_parms$c2) - m)",
 diffusion="sigma.mu * sq(m)",
 jump.coeff = "1",
 measure = list(intensity = 0.10, df = list("dexp(z, 100)")),
 measure.type = "CP",
 solve.variable=c("m")
)


