context("tests with va_product class")

test_that("initialize works as expected", {

  rate <- constant_parameters$new(0.01)

  premium <- 100
  rollup <- payoff_rollup$new(premium, rate)

  #Five years time-line
  times <- timeDate::timeSequence(from="2016-01-01", to="2020-12-31")

  fee <- constant_parameters$new(0.01, 365)
  barrier <- 200
  penalty <- 0.1

  expect_is(va_product$new(rollup, times, fee, barrier, penalty), "va_product")
  expect_error(va_product$new("ciccio"), error_msg_1("payoff_guarantee"))
  expect_error(va_product$new(rollup, "ciccio"), error_msg_1("timeSequence"))
  expect_error(va_product$new(rollup, times, "ciccio"), error_msg_1("constant_parameters"))
  expect_error(va_product$new(rollup, times, fee, "ciccio"), error_msg_3)
  expect_error(va_product$new(rollup, times, fee, barrier, "ciccio"), error_msg_8("penalty"))

}
)


