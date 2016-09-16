#'BBM2010 financial risk factors
#'@description
#'List of parameters to initialize a va_sde_engine object to
#'simulate the interest rate, volatility and log price processes
#'according to the stochastic differential equations specified
#'in BBM2010 - See References.
#'@usage
#'engine <- va_sde_engine$new(contract, financials_BBM2010, mortality_BBM2010)
#'
#'@format A list with elements:
#'\describe{
#' \item{[[1]]}{List of parameters for \code{\link[yuima]{simulate}}}
#' \item{[[2]]}{List of parameters for \code{\link{setModel}}}
#' \item{[[3]]}{Vector with indices indicating the interest rate and log price
#' in solve.variable \code{\link{setModel}}}
#'}
#'@references
#'\describe{
#' \item{[BBM2010]}{
#'  \cite{Bacinello A.R., Biffis E. e Millossovich P.
#'        "Regression-based algorithms
#'         for life insurance contracts with surrender guarantees".
#'         In: Quantitative Finance 10.9 (2010), pp. 1077-1090.}
#'        }
#'  }
"financials_BBM2010"



#'BBM2010 demographic risk factors
#'@description
#'List of parameters to initialize a va_sde_engine object to
#'simulate the intensity of mortality process
#'according to the stochastic differential equation specified
#'in BBM2010 - See References.
#'@usage
#'engine <- va_sde_engine$new(contract, financials_BBM2010, mortality_BBM2010)
#'
#'@format A list with elements:
#'\describe{
#' \item{[[1]]}{List of parameters for \code{\link[yuima]{simulate}}}
#' \item{[[2]]}{List of parameters for \code{\link{setModel}}}
#' \item{[[3]]}{Vector with indices indicating the intensity of mortality
#' in solve.variable \code{\link{setModel}}}
#'}
#'@references
#'\describe{
#' \item{[BBM2010]}{
#'  \cite{Bacinello A.R., Biffis E. e Millossovich P.
#'        "Regression-based algorithms
#'         for life insurance contracts with surrender guarantees".
#'         In: Quantitative Finance 10.9 (2010), pp. 1077-1090.}
#'        }
#'  }
"mortality_BBM2010"


#'BMOP2011 financial risk factors
#'@description
#'List of parameters to initialize a va_sde_engine object to
#'simulate the interest rate, volatility and log price processes
#'according to the stochastic differential equations specified
#'in BMOP2011 - See References.
#'@usage
#'engine <- va_sde_engine$new(contract, financials_BMOP2011, mortality_BMOP2011)
#'
#'@format A list with elements:
#'\describe{
#' \item{[[1]]}{List of parameters for \code{\link[yuima]{simulate}}}
#' \item{[[2]]}{List of parameters for \code{\link{setModel}}}
#' \item{[[3]]}{Vector with indices indicating the interest rate and log price
#' in solve.variable \code{\link{setModel}}}
#'}
#'@references
#'\describe{
#' \item{[BMOP2011]}{
#'  \cite{Bacinello A.R., Millossovich P., Olivieri A. e Pitacco E.
#'        "Variable annuities: a unifying valuation approach."
#'         In: Insurance: Mathematics and Economics 49 (2011), pp. 285-297.}
#'       }
#'  }
"financials_BMOP2011"



#'BMOP2011 demographic risk factors
#'@description
#'List of parameters to initialize a va_sde_engine object to
#'simulate the intensity of mortality process
#'according to the stochastic differential equation specified
#'in BMOP2011 - See References.
#'@usage
#'engine <- va_sde_engine$new(contract, financials_BMOP2011, mortality_BMOP2011)
#'
#'@format A list with elements:
#'\describe{
#' \item{[[1]]}{List of parameters for \code{\link[yuima]{simulate}}}
#' \item{[[2]]}{List of parameters for \code{\link{setModel}}}
#' \item{[[3]]}{Vector with indices indicating the intensity of mortality
#' in solve.variable \code{\link{setModel}}}
#'}
#'@references
#'\describe{
#' \item{[BMOP2011]}{
#'  \cite{Bacinello A.R., Millossovich P., Olivieri A. e Pitacco E.
#'        "Variable annuities: a unifying valuation approach."
#'         In: Insurance: Mathematics and Economics 49 (2011), pp. 285-297.}
#'       }
#'  }
"mortality_BMOP2011"
