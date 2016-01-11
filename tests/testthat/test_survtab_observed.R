context("CIF's & surv.obs's congruence & comparison w/ survival::survfit")

test_that("surv.obs about the same as Kaplan-Meier & CIFs close to Aalen-Johansen", {
  library(survival)
  BL <- list(fot= seq(0,19,1/12), per=c(2008,2013))
  sire2 <- sire[dg_date<ex_date, ]
  sire2$status <- factor(sire2$status, levels = 0:2, 
                         labels = c("alive", "canD", "othD"))
  
  x <- lexpand(sire2, 
               birth  = bi_date, entry = dg_date, exit = ex_date,
               status = status,
               breaks=BL)
  st <- survtab_lex(Surv(fot, event = lex.Xst) ~ 1, data = x, surv.type="cif.obs")
  setDT(x)
  setattr(x, "class", c("Lexis", "data.table", "data.frame"))
  setDT(st)
  
  test_that("CIFs and surv.obs sum to 1", {
    expect_equal(st[, CIF_canD + CIF_othD + surv.obs] ,  rep(1, times = st[,.N]), tolerance = 0.0001, scale=1)
  })
  
  x <- lexpand(sire2, 
               birth  = bi_date, entry = dg_date, exit = ex_date,
               status = status,
               breaks = BL["per"])
  
  fb <- setdiff(BL$fot, 0)
  su.km  <- survfit(Surv(time=fot, time2=fot+lex.dur, lex.Xst!="alive") ~ 1, data = x)
  su.km  <- summary(su.km, times = fb)
  su.km  <- cbind(data.table(time = su.km$time), data.table(su.km$surv))
  
  su.cif <- survfit(Surv(time=fot, time2=fot+lex.dur, lex.Xst)~1, data=x)
  su.cif <- summary(su.cif, times = fb)
  su.cif <- cbind(data.table(time = su.cif$time), data.table(su.cif$prev))
  
  expect_equal(st[, surv.obs] ,  su.km[, V1], tolerance = 0.0032, scale=1)
  
  expect_equal(su.cif$V1, st$CIF_canD, tolerance = 0.0022, scale=1)
  expect_equal(su.cif$V2, st$CIF_othD, tolerance = 0.0011, scale=1)
})


# custom status var -------------------------------------------------------

test_that("survtab_lex status argument works as expected", {
  library(survival)
  skip_on_cran()
  BL <- list(fot= seq(0,19,1/12), per=c(2008,2013))
  sr <- sire[dg_date < ex_date, ]
  
  sr$statusf <- factor(sr$status, 0:2, labels = c("alive", "canD", "othD"))
  sr$statusb <- as.integer(sr$status %in% 1:2)
  
  st <- NULL
  x <- lexpand(sr, birth  = bi_date, entry = dg_date, 
               exit = ex_date, status = status)
  st <- try(survtab_lex(Surv(fot, lex.Xst) ~ 1, data = x, surv.type = "surv.obs", 
                    breaks = list(fot = 0:5)), silent = TRUE)
  expect_equal(class(st)[1L], "try-error")
  
  st <- NULL
  x <- lexpand(sr, birth  = bi_date, entry = dg_date, 
               exit = ex_date, status = statusf)
  st <- survtab_lex(Surv(fot, lex.Xst) ~ 1, data = x, surv.type = "surv.obs", 
                    breaks = list(fot = 0:5))
  expect_equal(class(st)[1L], "survtab")
  
  st <- NULL
  x <- lexpand(sr, birth  = bi_date, entry = dg_date, 
               exit = ex_date, status = statusb)
  st <- survtab_lex(Surv(fot, lex.Xst) ~ 1, data = x, surv.type = "surv.obs", 
                    breaks = list(fot = 0:5))
  expect_equal(class(st)[1L], "survtab")
  
})

