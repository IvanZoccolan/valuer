context("tests with cash_flows class")

test_that("initialize works as expected", {
  expect_is(cash_flows$new(timeDate::Sys.timeDate(), 67.89), "R6")

  tt <- timeDate::timeSequence("2016-01-01", "2016-12-31")
  amounts <- seq_along(tt) / 100
  expect_is(cash_flows$new(tt,amounts), "R6")

  expect_error(cash_flows$new("pippo"), "argument must be a timeDate object")
  expect_error(cash_flows$new(timeDate::Sys.timeDate(), "pippo"), "argument must be numeric")

  expect_error(cash_flows$new(tt, head(amounts)), "arguments must have the same length")

  }
)

test_that("get and set methods work as expected", {
  cash <- cash_flows$new(timeDate::Sys.timeDate(),45.00)
  expect_equal(cash$get_amounts(), 45)
  cash$set_amounts(50.67)
  expect_equal(cash$get_amounts(), 50.67)
  expect_error(cash$set_amounts("pippo"), "argument must be numeric")

})

