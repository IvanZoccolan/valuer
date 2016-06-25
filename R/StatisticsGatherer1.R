#Defines the statistics gatherer class
#Porting to R of C++ code by M. Joshi 
#Ivan Zoccolan 18/6/2016


#The class will decide how part of the algorithm will behave. This is the strategy pattern.
#So the statistics gatherer object will be changed its state by the Monte Carlo algorithm and must retain #the change at each step. 
#In R we will need to use classes with reference semantics such as Reference Classes or R6

#This class defines the interface only. Implementation is demanded to subclasses inheriting this one.

library(R6)

StatisticsMC <- R6Class("StatisticsMC",
  
  public = list( DumpResult = function(result) {}, GetResultSoFar = function() {})
  
)
  

StatisticsMean <- R6Class("StatisticsMean", inherit = StatisticsMC,
              
                  public = list(
                    
                    initialize = function(){ private$values = 0.0 },
                    
                    DumpResult = function(result){
                      
                      if ( inherits(result, "numeric")) private$values <- result
                      else stop("result must be a numeric vector")
                    },
                    
                    GetResultSoFar = function() { mean(private$values) },
                    
                    PathDone = function() { length(private$values) }
                    
                  ),
                  private = list(
                    values = 'numeric'
                  )
                )





#Using the statistics gatherer 

#' Estimates the price of a derivative by means of Monte Carlo.
#' 
#' @description 
#' \code{MonteCarlo} estimates the price of a derivative by means of Monte Carlo simulation.
#' 
#' @details
#' This function is a simple Monte Carlo \url{https://en.wikipedia.org/wiki/Monte_Carlo_method} routine
#' to estimate the value of an european derivative.
#' It's a porting from C++ code by M. Joshi published in his C++ Design Patterns and Derivatives Pricing 
#' 
#' @param TheOption A VanillaOption object.
#'  @param Spot A numeric scalar representing the initial value of the underlying asset.
#'  @param Vol A ConstantParameters object representing the volatility of the underlying asset.
#'  @param r A ConstantParameters object representing the spot interest rate.
#'  @param NumberOfPaths An integer scalar  representing the number of simulated paths used for the Monte Carlo simulation.
#' @param StatisticsGatherer  A StatisticsMean object where the results of the simulation are stored.
#' @examples 
#' gatherer <- StatisticsMean$new()
#'VolParam = ConstantParameters$new(0.2)
#'
#'rParam = ConstantParameters$new(0.01)
#'
#'
#'callPayOff = PayOffCall$new(100)
#'
#'
#'CallOption =VanillaOption$new(callPayOff, 1)
#
#'MonteCarlo(CallOption, Spot=100, VolParam, rParam, NumberOfPaths = 1e4, gatherer)



MonteCarlo <- function(TheOption, Spot, Vol, r, NumberOfPaths, StatisticsGatherer){
  
  Expiry = TheOption$GetExpiry()
  variance = Vol$IntegralSquare(0,Expiry)
  rootVariance = sqrt(variance)
  itoCorrection = -0.5*variance
  discounting = exp(-r$Integral(0, Expiry))
  
  movedSpot = Spot*exp(r$Integral(0, Expiry) + itoCorrection)

    
  thisGaussian <- rnorm(n = NumberOfPaths)
  thisSpot = movedSpot*exp(rootVariance*thisGaussian)
  thisPayOff = TheOption$OptionPayOff(thisSpot)
  StatisticsGatherer$DumpResult(thisPayOff*discounting)
} 



gatherer <- StatisticsMean$new()


VolParam = ConstantParameters$new(0.2)

rParam = ConstantParameters$new(0.01)


callPayOff = PayOffCall$new(100)

      
CallOption =VanillaOption$new(callPayOff, 1)

MonteCarlo(CallOption, Spot=100, VolParam, rParam, NumberOfPaths = 1e4, gatherer)

system.time(MonteCarlo(CallOption, Spot=100, VolParam, rParam, NumberOfPaths = 1e6, gatherer))