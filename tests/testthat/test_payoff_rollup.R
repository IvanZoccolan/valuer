context("tests with payoff_rollup")

test_that("initialize works", {
  rate <- constant_parameters$new(0.01)

  expect_is(payoff_rollup$new(100,rate), "payoff_rollup")
  expect_error(payoff_rollup$new("pippo"), error_msg_7("premium"))
  expect_error(payoff_rollup$new(100, "pippo"), error_msg_1("constant_parameters"))



})

test_that("get_payoff method works", {

  rate <- constant_parameters$new(0.01)
  tt <- timeDate::timeSequence(from="2016-07-09", to="2017-07-09")

  rollup <- payoff_rollup$new(100,rate)

  expect_equal(rollup$get_payoff(120,c(head(tt, 1), tail(tt, 1))), 120)
  expect_equal(rollup$get_payoff(c(120,130), c(head(tt, 1), tail(tt, 1))), c(120,130))
  expect_gt(rollup$get_payoff(100,c(head(tt, 1), tail(tt, 1))), 100)


})
