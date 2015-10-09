context("CIF's & surv.obs's congruence & comparison w/ survival::survfit")
library(survival)


test_that("surv.obs about the same as Kaplan-Meier & CIFs close to Aalen-Johansen", {
  
  BL <- list(fot= seq(0,19,1/12), per=c(2008,2013))
  x <- lexpand(sire[dg_date<ex_date, ], 
               birth  = bi_date, entry = dg_date, exit = ex_date,
               status = status,
               breaks=BL)
  st <- survtab(x, surv.type="cif.obs")
  setDT(x)
  setattr(x, "class", c("Lexis", "data.table", "data.frame"))
  setDT(st)
  
  test_that("CIFs and surv.obs sum to 1", {
    expect_equal(st[, CIF_1 + CIF_2 + surv.obs] ,  rep(1, times = st[,.N]), tolerance = 0.0001, scale=1)
  })
  
  sire2 <- copy(sire)
  sire2 <- lexpand(sire2[dg_date<ex_date, ], 
                   birth  = bi_date, entry = dg_date, exit = ex_date,
                   status = status %in% 1:2,
                   breaks=list(per = c(2008,2013)))
  
  fb <- setdiff(BL$fot, 0)
  su.km  <- survfit(Surv(time=fot, time2=fot+lex.dur, status!=0) ~ 1, data = sire2)
  su.km  <- summary(su.km, times = fb)
  su.km  <- cbind(data.table(time = su.km$time), data.table(su.km$surv))
  
  su.cif <- survfit(Surv(time=fot, time2=fot+lex.dur, event = factor(status))~1, data=sire2)
  su.cif <- summary(su.cif, times = fb)
  su.cif <- cbind(data.table(time = su.cif$time), data.table(su.cif$prev))
  
  expect_equal(st[, surv.obs] ,  su.km[, V1], tolerance = 0.0032, scale=1)
  
  expect_equal(su.cif$V1, st$CIF_1, tolerance = 0.0022, scale=1)
  expect_equal(su.cif$V2, st$CIF_2, tolerance = 0.0011, scale=1)
})


# custom status var -------------------------------------------------------

test_that("survtab works with factor status variable", {
  skip_on_cran()
  BL <- list(fot= seq(0,19,1/12), per=c(2008,2013))
  x <- lexpand(sire[dg_date<ex_date, ],  
               birth  = bi_date, entry = dg_date, exit = ex_date,
               status = status,
               breaks=BL)
  
  setDT(x)
  setattr(x, "class", c("Lexis", "data.table", "data.frame"))
  x[, lex.Xst := factor(lex.Xst, levels=as.character(0:2), labels = c("alive", "canDeath", "othDeath"))]
  x[, lex.Cst := factor(lex.Cst, levels=as.character(0:2), labels = c("alive", "canDeath", "othDeath"))]
  x[, agegr := cut(dg_age, 5, labels=F, right=F)]
  expect_message(st <- survtab(x, surv.type="cif.obs", by.vars="agegr"), "event.values was NULL, so chose alive as non-event value")
  expect_equal(length(intersect(names(st), c("CIF_canDeath", "CIF_othDeath"))), 2L)
})

