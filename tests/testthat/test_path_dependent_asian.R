context("tests with path_dependent_asian class")

test_that("initialize works as expected", {
 expect_is(path_dependent_asian$new(), "R6")

 tt <- timeDate::timeSequence(from = "2016-01-01", to = "2016-08-06")
 day <- tail(tt, 1)
 call <- payoff_call$new(120)
 expect_is(path_dependent_asian$new(tt,day,call), "path_dependent_asian")
 expect_error(path_dependent_asian$new("ciccio",day,call), error_msg_1("timeDate"))
 expect_error(path_dependent_asian$new(tt,"ciccio",call), error_msg_1("timeDate"))
 expect_error(path_dependent_asian$new(tt, day,"pay"), error_msg_1("payoff"))
}
)


test_that("cash_flows method works", {

  asian_digit <- path_dependent_asian$new()
  expect_is(asian_digit$cash_flows(c(120,130)), "cash_flows")
  expect_is(asian_digit$get_times(), "timeDate")

})
