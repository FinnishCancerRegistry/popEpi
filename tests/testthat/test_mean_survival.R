context("mean survival testing")

test_that("meansurv results the same as before", {
  ## note: used to check against the results of
  ## Seppa, Karri, and Timo Hakulinen. 
  ## "Mean and median survival times of cancer patients should be corrected 
  ## for informative censoring." 
  ## Journal of clinical epidemiology 62.10 (2009): 1095-1102.
  ## 
  ## age-group-specific estimates differed but randomly 
  ## (since git ref e59d5c7 extremely small differences)
  
  sr <- copy(sire)[dg_date < ex_date, ]
  sr$agegr <- cut(sr$dg_age, c(0,45,60,Inf), right=FALSE)
  x <- lexpand(sr, birth  = bi_date, entry = dg_date, exit = ex_date,
               status = status %in% 1:2, pp = FALSE,
               breaks=list(fot=seq(0,10,1/12)), pophaz=popmort)
  sma <- survmean(x, pophaz=popmort, by.vars="agegr", 
                  ext.breaks = list(fot = c(0:6/12, 0.75, 1:100), age = c(0, 125)))
  
  ## values to test against computed on 2015-12-01;
  ## git ref: e59d5c7
  expect_equal(sma$est, c(34.369009,  21.589894, 7.901299), tol = 0.005, scale = 1)
  expect_equal(sma$exp, c(45.82442, 31.19623, 13.57836), tol = 0.005, scale = 1)
  
})

