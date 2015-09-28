context("mean survival testing")

test_that("meansurv results the same as before", {
  ## note: used to check against the results of
  ## Seppä, Karri, and Timo Hakulinen. 
  ## "Mean and median survival times of cancer patients should be corrected 
  ## for informative censoring." 
  ## Journal of clinical epidemiology 62.10 (2009): 1095-1102.
  ## 
  ## age-group-specific estimates differed but randomly 
  sr <- copy(sire)
  sr$agegr <- cut(sr$dg_age, c(0,45,60,Inf), right=FALSE)
  x <- lexpand(sr[dg_date<ex_date], breaks=list(fot=seq(0,10,1/12)), pophaz=popmort)
  sma <- survmean(x, pophaz=popmort, by.vars="agegr", 
                  ext.breaks = list(fot = c(0:6/12, 0.75, 1:100), age = c(0, 125)))
  
  expect_equal(sma$est, c(34.368997, 21.589844, 7.895499), tol = 0.005, scale = 1)
  
})

