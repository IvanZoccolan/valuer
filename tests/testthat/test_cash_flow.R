context("tests with cash_flow class")

test_that("initialize works as expected", {
  expect_is(cash_flow$new(1, 67.89), "R6")
  expect_error(cash_flow$new("pippo"), "time should not be a negative scalar")
  expect_error(cash_flow$new(0, "pippo"), "the_amount should be a numeric vector")
  }
)

test_that("get and set methods work as expected", {
  cash <- cash_flow$new(0,45.00)
  expect_equal(cash$get_amount(), 45)
  cash$set_amount(50.67)
  expect_equal(cash$get_amount(), 50.67)
  expect_error(cash$set_amount("pippo"), "the_amount should be a not negative scalar")

})

