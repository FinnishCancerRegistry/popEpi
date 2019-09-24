




testthat::context("survival::Surv and popEpi::Surv harmony")




testthat::test_that("survival::Surv and popEpi::Surv's formals are in harmony",{
  
  popepi_formals <- as.list(formals(popEpi::Surv))
  survival_formals <- as.list(formals(survival::Surv))
  testthat::expect_identical(
    object = popepi_formals,
    expected = survival_formals
  )
  
})





