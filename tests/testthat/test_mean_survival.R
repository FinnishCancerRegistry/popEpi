context("mean survival congurence with Karri's old results")

test_that("old article results agree with meansurv's results", {
  if (dir.exists("P:/CR/Tilastotiimi_CR0020/programs/mean_survival_time/meansurv_comparison.R")) {
    source("P:/CR/Tilastotiimi_CR0020/programs/mean_survival_time/meansurv_comparison.R")
    ms <- c(sm2$est, sm$est, sm3$est.as)
    lt <- lt[[1]][,1]
    names(ms) <- names(lt)
    expect_equal(lt, ms, scale=1, tolerance = 0.5)
    rm(patients, sm, sm2, sm3, x, lt, BL, lifetime, envir = globalenv())
  }
  
})

