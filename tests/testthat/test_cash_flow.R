context("tests with cash_flow class")

test_that("initialize works as expected", {
  expect_is(cash_flow$new(timeDate::Sys.timeDate(), 67.89), "R6")
  expect_error(cash_flow$new("pippo"), "argument must be a timeDate object")
  expect_error(cash_flow$new(timeDate::Sys.timeDate(), "pippo"), "argument must be numeric")
  }
)

test_that("get and set methods work as expected", {
  cash <- cash_flow$new(timeDate::Sys.timeDate(),45.00)
  expect_equal(cash$get_amount(), 45)
  cash$set_amount(50.67)
  expect_equal(cash$get_amount(), 50.67)
  expect_error(cash$set_amount("pippo"), "argument must be a non-negative scalar")

})

