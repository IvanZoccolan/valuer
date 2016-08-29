
context("Tests of the va_engine class")

test_that("Inizialize works", {

rate <- constant_parameters$new(0.01)

premium <- 100
rollup <- payoff_rollup$new(premium, rate)

#Five years time-line
times <- timeDate::timeSequence(from="2016-01-01", to="2020-12-31")

# A constant fee of 2% per year (365 days)
fee <- constant_parameters$new(0.02, 365)

#Barrier for a state-dependent fee. The fee will be applied only if
#the value of the account is below the barrier
barrier <- 200

#Withdrawal penalty applied in case the insured surrenders the contract.
penalty <- 0.01

VA_GMAB <- GMAB$new(rollup, times, fee, barrier, penalty)

r <- constant_parameters$new(0.01)


expect_is(va_engine$new(VA_GMAB, interest=r), "va_engine")

engine <- va_engine$new(VA_GMAB, interest=r)


})
