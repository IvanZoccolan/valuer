context("tests the exotic_engine base class")


test_that("initialize works as expected", {
  times <- timeDate::timeSequence(from = "2016-01-01", to="2016-07-06", format="%Y-%m-%d")
  prod <- path_dependent$new(times)
  r <- constant_parameters$new(0.01 / 365) #1% yearly constant interest rate
  expect_is(exotic_engine$new(prod, r), "R6")
  expect_error(exotic_engine$new("pippo"), "argument must be a path_dependent object")
  expect_error(exotic_engine$new(prod, "pippo"), "argument must be a parameters object")
  expect_error(exotic_engine$new(), "argument must be a path_dependent object")
  expect_error(exotic_engine$new(prod), "argument must be a parameters object")
})
