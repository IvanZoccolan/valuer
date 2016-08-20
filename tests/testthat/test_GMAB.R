context("tests with GMAB product class")


test_that("initialize works as expected", {

  rate <- constant_parameters$new(0.01)

  premium <- 100
  rollup <- payoff_rollup$new(premium, rate)

  #Five years time-line
  times <- timeDate::timeSequence(from="2016-01-01", to="2020-12-31")

  fee <- constant_parameters$new(0.01, 365)
  barrier <- 200
  penalty <- 0.1

  expect_is(GMAB$new(rollup, times, fee, barrier, penalty), "GMAB")
  expect_error(GMAB$new("ciccio"), error_msg_1("payoff_guarantee"))
  expect_error(GMAB$new(rollup, "ciccio"), error_msg_1("timeSequence"))
  expect_error(GMAB$new(rollup, times, "ciccio"), error_msg_1("constant_parameters"))
  expect_error(GMAB$new(rollup, times, fee, "ciccio"), error_msg_3)
  expect_error(GMAB$new(rollup, times, fee, barrier, "ciccio"), error_msg_8("penalty"))

}
)


test_that("cash_flow and cash_flow_paths works as expected", {

  rate <- constant_parameters$new(0.01)

  premium <- 100
  rollup <- payoff_rollup$new(premium, rate)

  #Five years time-line
  times <- timeDate::timeSequence(from="2016-01-01", to="2020-12-31")
  dtime <- timeDate::timeDate("2021-01-01")

  fee <- constant_parameters$new(0.01, 365)
  barrier <- 200
  penalty <- 0.1

  VA_GMAB <- GMAB$new(rollup, times, fee, barrier, penalty)

  r <- constant_parameters$new(0.01)
  spot <- 100
  vol <- constant_parameters$new(0.2)
  div <- constant_parameters$new(0.0)

  engine <- exotic_bs_engine$new(VA_GMAB, r, spot, vol, div)
  values <- engine$get_one_path()

  expect_length(VA_GMAB$cash_flows(values, dtime), length(values))
  expect_length(VA_GMAB$cash_flows(values, times[456]), 456)
  expect_is(VA_GMAB$cash_flow_times(dtime), "timeDate")
}
)
