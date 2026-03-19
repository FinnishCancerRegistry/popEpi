context("call_with_arg_list__")

testthat::test_that("call_with_arg_list__ works", {
  arg_list <- list(c(1:5, NA), na.rm = TRUE)
  exp <- do.call(base::mean, arg_list)
  obs <- call_with_arg_list__(base::mean, arg_list)
  testthat::expect_equal(obs, exp)
  obs <- call_with_arg_list__(mean, arg_list)
  testthat::expect_equal(obs, exp)
  obs <- call_with_arg_list__("mean", arg_list)
  testthat::expect_equal(obs, exp)
  obs <- call_with_arg_list__("base::mean", arg_list)
  testthat::expect_equal(obs, exp)
})