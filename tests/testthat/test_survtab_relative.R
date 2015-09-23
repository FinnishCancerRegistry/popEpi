context("popEpi::survtab vs. relsurv::rs.surv")


# survtab vs. relsurv::rs.surv --------------------------------------------
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

test_that("relative survivals about the same as relsurv's", {
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
  x <- lexpand(sire, breaks=list(fot=fb), status = status, pophaz=pm)
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


# test using relsurv's data -----------------------------------------------
## something wrong with prepping for survtab somehow
## perhaps haz at wrong level
# library(relsurv)
# data(rdata)
# rdata <- data.table(rdata)
# data(slopop)
# 
# rs.e2 <- rs.surv(Surv(time,cens)~1+ratetable(age=age*365,sex=sex,year=year),method = "ederer2",
#                  ratetable=slopop,data=rdata)
# 
# rs.pp <- rs.surv(Surv(time,cens)~1+ratetable(age=age*365,sex=sex,year=year),method = "pohar-perme",
#                  ratetable=slopop,data=rdata)
# 
# sl <- as.data.table.ratetable(slopop)
# setnames(sl, c("age"), c("agegroup"))
# sl[, c("year", "agegroup") := lapply(.SD, as.integer), .SDcols = c("year", "agegroup")]
# 
# rdata[, sex := factor(sex, levels = 1:2, labels = c("male", "female"))]
# rdata[, Tstop := time / 365.242199]
# rdata[, dg_date := as.Date(year, origin = "1960-01-01")]
# 
# rdata[, ex_date := dg_date + time]
# rdata[, bi_date := dg_date - age*365.242199]
# 
# x <- lexpand(rdata, status = cens, entry.status = 0, breaks = list(fot = seq(0,15,1/12)), pophaz = sl)
# st <- survtab(x, event.values = 0, relsurv.method ="pp")
# 
# 
# st[, Dstop := Tstop*365.242199]
# plot(rs.e2, mark.time = FALSE)
# lines(r.e2 ~ Dstop, data = st, col = "red")



test_that("relative survivals about the same as relsurv's", {
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
  x <- lexpand(sire, breaks=list(fot=fb), status = status, pophaz=pm)
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




# period method ----------------------------------------------------------
## in relsurv since 2.0.6
## I don't think relsurv's results are correct
# test_that("relsurv:::rs.period and survtab have very similar results", {
#   
#   pb <- c(as.Date("2000-01-01"), as.Date("2005-01-01"))
#   
#   sire2[, sex := 2L]
#   #   sire3 <- lexpand(sire2, fot = c(0,5), per = pb)
#   rs.e2 <- relsurv:::rs.period(Surv(Tstop, status!=0) ~ 1 + ratetable(age=dg_age, sex=sex, year=dg_date), 
#                                winst = pb[1], winfin = pb[2],
#                                ratetable = popm, data = sire2, method = 'ederer2', type = "fleming-harrington")
#   
#   rs.pp <- relsurv:::rs.period(Surv(Tstop, status!=0) ~ 1 + ratetable(age=dg_age, sex=sex, year=dg_date), 
#                                winst = pb[1], winfin = pb[2],
#                                ratetable = popm, data = sire2, method = 'pohar-perme', type = "fleming-harrington")
#   sire2[, sex := 1L]
#   
#   ## survtab
#   fb <- seq(0, 5, 1/24)
#   x <- lexpand(sire, breaks=list(fot=fb, per = pb), status = status, pophaz=pm)
#   st <- survtab(x, surv.type="surv.rel", event.values=1:2, format=F, relsurv.method="pp")
#   rm(x); gc()
#   
#   
#   ## rs.surv
#   fb <- fb[-1]
#   fbd <- fb*365.242199
#   
#   su.e2 <- summary(rs.e2, times = fbd)
#   su.e2 <- cbind(data.table(time = su.e2$time), data.table(su.e2$surv))
#   su.pp <- summary(rs.pp, times = fbd)
#   su.pp <- cbind(data.table(time = su.pp$time), data.table(su.pp$surv))
#   
#   st[, Dstop := fbd]
#   fbd <- intersect(fbd, su.pp$time)
#   
#   expect_equal(st[Dstop %in% fbd, r.e2] ,  su.e2[, V1], tolerance = 0.005)
#   expect_equal(st[Dstop %in% fbd, r.pp] ,  su.pp[, V1], tolerance = 0.005)
#   
#   ## plot
#   plot(st, y = "r.e2")
#   lines(su.e2$V1 ~ I(su.e2$time/365.242199), col = "red")
#   
#   plot(st, y = "r.pp")
#   lines(su.pp$V1 ~ I(su.pp$time/365.242199), col = "red")
#   
# })


## using relsurv's data
#   library(relsurv)
#   data(rdata)
#   data(slopop)
#   fit <- relsurv:::rs.period(Surv(time,cens)~sex+ratetable(age=age*365,sex=sex,year=year),ratetable=slopop,data=rdata,winst=as.date("1Jan1982"),winfin=as.date("1Jan1987"))
#   plot(fit, mark.time = FALSE)
#   
#   rdata <- data.table(rdata)
#   sl <- as.data.table.ratetable(slopop)
#   setnames(sl, c("age"), c("agegroup"))
#   sl[, c("year", "agegroup") := lapply(.SD, as.integer), .SDcols = c("year", "agegroup")]
#   
#   rdata[, sex := factor(sex, levels = 1:2, labels = c("male", "female"))]
#   rdata[, Tstop := time / 365.242199]
#   rdata[, dg_date := as.Date(year, origin = "1960-01-01")]
#   
#   rdata[, ex_date := dg_date + time]
#   rdata[, bi_date := dg_date - age*365.242199]
#   
#   x <- lexpand(rdata, status = cens, entry.status = 0, breaks = list(per = c(1982, 1987), fot = seq(0,19,1/12)), pophaz = sl)
#   st <- survtab(x, event.values = 0, relsurv.method ="pp", by = "sex")
#   
#   
#   st[, Dstop := Tstop*365.242199]
#   plot(fit, mark.time = FALSE)
#   lines(r.e2 ~ Dstop, data = st[sex == "male"], col = "red")
#   lines(r.e2 ~ Dstop, data = st[sex == "female"], col = "red")



# relpois vs. relsurv::rsadd ---------------------------------------------
sire2[, agegr := cut(dg_age/365.25, breaks = c(0,45,70,Inf))]
x <- lexpand(sire2, breaks=list(fot=0:5), status = status, pophaz=pm)
rp <- relpois(x, formula = lex.Xst%in%1:2 ~ -1+FOT+agegr)
setDT(x)
setattr(x, "class", c("Lexis", "data.table", "data.frame"))
sire2[, sex := 2L]
rs <- relsurv::rsadd(Surv(Tstop, event = status %in% 1:2) ~ ratetable(age=dg_age, sex=sex, year=dg_date) + agegr,
                     int = 0:5,
                     ratetable = popm, data = sire2, method = "glm.poi")


test_that("relpois congruent with relsurv::rsadd", {
  expect_equal(coef(rp)[1:5] ,  coef(rs)[3:7], tolerance = 0.055, scale=1, check.attributes=FALSE)
})


# 
# 
# 
# rp <- relpois(x, formula = lex.Xst%in%1:2 ~ -1+agegr:FOT)
# 
# rs <- relsurv::rsadd(Surv(fot*365.24, (fot+lex.dur)*365.24, event = lex.Xst %in% 1:2) ~ 
#                        ratetable(age=age*365.24, sex=sex, year=per*365.24) + agegr:cut(fot, 0:5, right=F),
#                      int = c(0,5),
#                      ratetable = popm, data = x, method = "glm.poi")


