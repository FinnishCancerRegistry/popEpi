## Copied from test_mean_survival: 
## source: git changeset 64d8e79cacb01b2cd104760206b7c9af469133a3
## note: contains logic for computing also popEpi-based extrapolated
## survival curve and mean survival, but those results are not saved.
## kept here because why not.
## last computed this on changeset 7842fb3eebee37cff7aca2b3261749da5d1cadd7

library("data.table")
library("Epi")
library("relsurv")
library("git2r")


#### compute observed survivals
BL <- list(fot= seq(0,15,1/24))
eBL <- list(fot = unique(c(BL$fot, seq(15, 115,0.5))))
sire2 <- sire[dg_date<ex_date, ]
sire2$statusf <- factor(sire2$status, levels = 0:2, 
                        labels = c("alive", "canD", "othD"))

x <- lexpand(sire2, 
             birth  = bi_date, entry = dg_date, exit = ex_date,
             status = statusf,
             breaks = NULL)
popmort_sm <- setDT(copy(popEpi::popmort))
setnames(popmort_sm, c("agegroup", "year"), c("age", "per"))
sm <- survmean(Surv(fot, event = lex.Xst) ~ 1, 
               breaks = BL, e1.breaks = eBL,
               pophaz = popmort_sm, data = x)
st <- survtab(Surv(fot, event = lex.Xst) ~ 1, 
              breaks = BL,
              data = x, surv.type="surv.obs")
setDT(x)
setattr(x, "class", c("Lexis", "data.table", "data.frame"))
setDT(st)

fb <- setdiff(BL$fot, 0)
su.km  <- survfit(Surv(time=fot, time2=fot+lex.dur, lex.Xst!="alive") ~ 1, 
                  data = x)
su.km  <- summary(su.km, times = fb)
su.km  <- data.table("time" = su.km$time, "est" = su.km$surv)

stopifnot(
  all.equal(
    su.km[time <= 15][["est"]], st[["surv.obs"]], tol = 0.001
  )
)

#### compute expected survivals for all subjects
BL <- list(fot = seq(0,100, 1/2))
xe <- copy(x)
setattr(xe$age, "class", c("yrs", "numeric"))
setattr(xe$per, "class", c("yrs", "numeric"))
setattr(xe$per, "year.length", "approx")
setattr(xe$age, "year.length", "approx")
xe[, "perdate" := as.Date.yrs(per)]
xe[, "agedate" := as.integer(as.Date.yrs(per)-as.Date(bi_date))]


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
empty_list <- list(fot = NULL, per = NULL, age = NULL)
forceLexisDT(xe, breaks = empty_list, allScales = c("fot", "per", "age"))
e1 <- comp_e1(xe, breaks = BL, pophaz = pm, survScale = "fot")


st <- st[, .(fot = Tstop, surv.obs)]
nrow_obs <- copy(nrow(st))
nrow_exp <- copy(nrow(e1))

setnames(e1, "surv.exp", "surv.obs")
st <- rbind(st, e1)
st[(nrow_obs+1):.N, "fot" := fot + st[nrow_obs, fot]]
st[(nrow_obs+1):.N, "surv.obs" := surv.obs*st[nrow_obs, surv.obs]]


su.exp[, surv.exp := surv.exp * su.km[.N, est]]
su <- rbindlist(list(su.km, su.exp))

su[, "est_type" := c(rep("observed", nrow_obs), rep("extrapolated", nrow_exp))]
setnames(su, c("time"), c("Tstop"))
setcolorder(su, c("est_type", "Tstop", "est"))


saveRDS(su, file = "tests/testthat/survmean_test_data_01.rds")

