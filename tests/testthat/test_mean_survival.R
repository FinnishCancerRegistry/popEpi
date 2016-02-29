context("mean survival testing")

test_that("survmean results the same as before", {
  ## note: used to check against the results of
  ## Seppa, Karri, and Timo Hakulinen. 
  ## "Mean and median survival times of cancer patients should be corrected 
  ## for informative censoring." 
  ## Journal of clinical epidemiology 62.10 (2009): 1095-1102.
  ## 
  ## age-group-specific estimates differed but randomly 
  ## (since git ref e59d5c7 quite small differences)
  
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

test_that("survmean_lex() agrees with old results", {
  
  library(Epi)
  library(survival)
  
  sr <- copy(sire)[dg_date < ex_date, ]
  sr$agegr <- cut(sr$dg_age, c(0,45,60,Inf), right=FALSE)
  
  x <- Lexis(entry = list(FUT = 0, AGE = dg_age, CAL = get.yrs(dg_date)),
             exit = list(CAL = get.yrs(ex_date)),
             data = sr,
             exit.status = factor(status, levels = 0:2,
                                  labels = c("alive", "canD", "othD")),
             merge = TRUE)

  ## observed survival
  pm <- copy(popEpi::popmort)
  names(pm) <- c("sex", "CAL", "AGE", "haz")
  sm <- survmean_lex(Surv(time = FUT, event = lex.Xst != "alive") ~ agegr,
                     pophaz = pm, data = x,
                     breaks = list(FUT = seq(0, 10, 1/12)))
  
  ## values to test against computed on 2015-02-29;
  ## git ref: 8b50a33a52cc98defaa5ddc8e8fc226288b832d1
  expect_equal(sm$est, c(34.089451,  21.544842, 7.916737), tol = 0.0005, scale = 1)
  expect_equal(sm$exp, c(34.50633, 22.39038, 11.00145), tol = 0.0005, scale = 1)
  
  
})



test_that("survmean_lex() agrees with results computed using pkg survival", {
  ## will only compute the mean survival time based on the cohort.
  ## will also compute expected extrapolation curve by hand.
  as.date.Date <- function(x, ...) {
    x <- as.integer(x) + 3653L
    as.date(x)
  }
  #### compute observed survivals
  library(survival)
  library(Epi)
  library(relsurv)
  
  BL <- list(fot= seq(0,15,1/12))
  sire2 <- sire[dg_date<ex_date, ]
  sire2$statusf <- factor(sire2$status, levels = 0:2, 
                          labels = c("alive", "canD", "othD"))
  
  x <- lexpand(sire2, 
               birth  = bi_date, entry = dg_date, exit = ex_date,
               status = statusf,
               breaks = NULL)
  popmort_sm <- setDT(copy(popEpi::popmort))
  setnames(popmort_sm, c("agegroup", "year"), c("age", "per"))
  sm <- survmean_lex(Surv(fot, event = lex.Xst) ~ 1, 
                     breaks = BL, ext.breaks = list(fot = seq(0,100,0.5)),
                     pophaz = popmort_sm, data = x)
  st <- survtab_lex(Surv(fot, event = lex.Xst) ~ 1, 
                    breaks = BL,
                    data = x, surv.type="surv.obs")
  setDT(x)
  setattr(x, "class", c("Lexis", "data.table", "data.frame"))
  setDT(st)
  
  fb <- setdiff(BL$fot, 0)
  su.km  <- survfit(Surv(time=fot, time2=fot+lex.dur, lex.Xst!="alive") ~ 1, data = x)
  su.km  <- summary(su.km, times = fb)
  su.km  <- cbind(data.table(time = su.km$time), data.table(su.km$surv))
  
  #### compute expected survivals for subjects surviving beyond observed curve
  BL <- list(fot = seq(0,100, 1/2))
  xe <- x[lex.dur >= max(fb)]
  xe[, age := age + max(fb)]
  xe[, per := per + max(fb)]
  xe[, lex.dur := max(BL$fot)]
  setattr(xe$age, "class", c("yrs", "numeric"))
  setattr(xe$per, "class", c("yrs", "numeric"))
  setattr(xe$per, "year.length", "approx")
  setattr(xe$age, "year.length", "approx")
  xe[, perdate := as.date.Date(as.Date.yrs(per))]
  xe[, agedate := as.integer(as.Date.yrs(per)-bi_date)]
  
  
  ## form ratetable
  # male
  pm <- setDT(copy(popEpi::popmort))
  # pm[, surv := 1L]
  pm[, surv := exp(-haz)]
  pm.m <- cast_simple(pm[sex==0], columns = 'year', rows = 'agegroup',  values='surv')
  pm.m[,agegroup := NULL]
  pm.m <- as.matrix(pm.m)
  # female
  pm.f <- cast_simple(pm[sex==1], columns = 'year', rows = 'agegroup',  values='surv')
  pm.f[,agegroup := NULL]
  pm.f <- as.matrix(pm.f)
  
  popm <- transrate(pm.m, pm.f, yearlim = c(1951, 2013), int.length = 1)
  
  pm[, surv := NULL]
  
  ## survival::survexp()
  su.exp <- survexp(~1, data = xe, ratetab = popm, method = "ederer", 
                rmap = list(sex = "female", year = perdate, 
                            age = agedate),
                times = BL$fot*365.242199)
  
  su.exp <- data.table(time = su.exp$time/365.242199, surv.exp = su.exp$surv)
  su.exp <- su.exp[time != 0L]
  su.exp[, time := time + max(su.km$time)]
  
  ## popEpi:::comp_e1()
  setnames(pm, c("year", "agegroup"), c("per", "age"))
  forceLexisDT(xe, breaks = NULL, allScales = c("fot", "per", "age"))
  e1 <- comp_e1(xe, breaks = BL, pophaz = pm, survScale = "fot")
  
  # plot(su.exp, ylim = c(0, 1), col = 1, xscale = 365.242199)
  # lines(I(c(1,surv.exp)) ~ I(BL$fot), col = "red", type = "s", data = e1)
  
  su.exp[, surv.exp := surv.exp * su.km[.N, V1]]
  su <- rbindlist(list(su.km, su.exp))
  
  st <- st[, .(fot = Tstop, surv.obs)]
  e1[, fot := fot + max(st$fot)+0.5]
  st <- rbindlist(list(st, e1))
  st[181:380, surv.obs := surv.obs*st[180L, surv.obs]]
  
  # plot(V1 ~ time, ylim = c(0, 1), col = 1, data = su, type = "l")
  # lines(surv.obs ~ fot, col = "red", type = "l", data = st)
  
  expect_equal(st$surv.obs, su$V1, scale = 1L, tolerance = 0.000475)
  
  st[, delta := fot - c(0, fot[-.N])]
  st[, l1 := c(1, surv.obs[-.N])]
  sm.st <- st[, sum((surv.obs+l1)/2L*delta)]
  
  su[, delta := time - c(0, time[-.N])]
  su[, l1 := c(1, V1[-.N])]
  sm.su <- su[, sum((V1+l1)/2L*delta)]
  
  expect_equal(sm.st, sm.su, scale = 1L, tolerance = 0.0571)
  expect_equal(sm.st, sm$est, scale = 1L)
})




