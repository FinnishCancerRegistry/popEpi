context("popEpi::survtab vs. relsurv::rs.surv")

# survtab vs. relsurv::rs.surv --------------------------------------------
test_that("relative survivals about the same as relsurv's", {
  
  library(survival)
  library(relsurv)
  
  # male
  pm <- copy(popmort)
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
  
  sire2 <- copy(sire)
  sire2[, Tstop  := as.integer(ex_date - dg_date)]
  sire2[, dg_age := as.integer(dg_date - bi_date)]
  
  
  ## rs.surv
  ## sex must be coded c(1,2) (male, female)
  sire2[, sex := 2L]
  rs.e2 <- rs.surv(Surv(Tstop, status!=0) ~ 1 + ratetable(age=dg_age, sex=sex, year=dg_date),
                   ratetable = popm, data = sire2, method = 'ederer2', type = "fleming-harrington", fin.date=ex_date)
  rs.pp <- rs.surv(Surv(Tstop, status!=0) ~ 1 + ratetable(age=dg_age, sex=sex, year=dg_date),
                   ratetable = popm, data = sire2, method = 'pohar-perme', type = "fleming-harrington", fin.date=ex_date)
  sire2[, sex := 1L]
  
  ## survtab
  fb <- 0:(19*12)/12
  x <- lexpand(sire, birth  = bi_date, entry = dg_date, exit = ex_date,
               status = status %in% 1:2,
               breaks=list(fot=fb), pophaz=pm)
  st <- survtab(x, surv.type="surv.rel", event.values=1:2, format=F, relsurv.method="pp")
  setDT(st)
  setDT(x)
  setattr(x, "class", c("Lexis", "data.table", "data.frame"))
  ## rs.surv
  fb <- fb[-1]
  fbd <- fb*365.242199
  
  su.e2 <- summary(rs.e2, times = fbd)
  su.e2 <- cbind(data.table(time = fb), data.table(su.e2$surv))
  su.pp <- summary(rs.pp, times = fbd)
  su.pp <- cbind(data.table(time = fb), data.table(su.pp$surv))
  
  
  
  expect_equal(st[, r.e2] ,  su.e2[, V1], tolerance = 0.005)
  expect_equal(st[, r.pp] ,  su.pp[, V1], tolerance = 0.005)
})

# relpois vs. relsurv::rsadd ---------------------------------------------

test_that("relpois congruent with relsurv::rsadd", {
  skip_on_cran()
  
  library(survival)
  library(relsurv)
  
  # male
  pm <- copy(popmort)
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
  
  sire2 <- copy(sire)
  sire2[, Tstop  := as.integer(ex_date - dg_date)]
  sire2[, dg_age := as.integer(dg_date - bi_date)]
  
  
  sire2[, agegr := cut(dg_age/365.25, breaks = c(0,45,70,Inf))]
  x <- lexpand(sire2, birth  = bi_date, entry = dg_date, exit = ex_date,
               status = status %in% 1:2,
               breaks=list(fot=0:5), pophaz=pm)
  rp <- relpois(x, formula = lex.Xst%in%1:2 ~ -1+FOT+agegr)
  setDT(x)
  setattr(x, "class", c("Lexis", "data.table", "data.frame"))
  sire2[, sex := 2L]
  rs <- relsurv::rsadd(Surv(Tstop, event = status %in% 1:2) ~ ratetable(age=dg_age, sex=sex, year=dg_date) + agegr,
                       int = 0:5,
                       ratetable = popm, data = sire2, method = "glm.poi")
  
  
  expect_equal(coef(rp)[1:5] ,  coef(rs)[3:7], tolerance = 0.055, scale=1, check.attributes=FALSE)
})


