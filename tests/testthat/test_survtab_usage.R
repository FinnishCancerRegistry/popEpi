context("survtab usage")



test_that("Dates and frac. yrs produce congruent results", {
  skip_on_cran()
  library(Epi)
  library(survival)
  
  x <- data.table(popEpi::sire)
  x <- x[dg_date<ex_date]
  
  ## phony group variable
  set.seed(1L)
  x$group <- rbinom(nrow(x), 1, 0.5)
  
  
  ## yrs
  xy <- Lexis(entry = list(FUT = 0, AGE = dg_age, CAL = get.yrs(dg_date)), 
              exit = list(CAL = get.yrs(ex_date)), 
              data = x,
              exit.status = factor(status, levels = 0:2, 
                                   labels = c("alive", "canD", "othD")), 
              merge = TRUE)
  
  ## dates
  xd <- Lexis(entry = list(FUT = 0L, AGE = dg_date-bi_date, CAL = dg_date),
              exit = list(CAL = ex_date),
              data = x,
              exit.status = factor(status, levels = 0:2, 
                                   labels = c("alive", "canD", "othD")), 
              merge = TRUE)
  yd <- 365.242199
  BLy <- list(FUT = seq(0, 5, 1/4))
  BLd <- lapply(BLy, function(el) el * yd)
  
  pmy <- data.table(popEpi::popmort)
  setnames(pmy, c("year", "agegroup"), c("CAL", "AGE"))
  
  pmd <- data.table(pmy)
  pmd[, CAL := as.Date(paste0(CAL, "-01-01"))]
  pmd[, AGE := AGE * yd]
  pmd[, haz := haz/yd]
  
  #### hazard method
  ## observed survival & Ederer II
  
  sty <- survtab(Surv(FUT, lex.Xst) ~ group, data = xy, 
                     surv.type = "surv.rel", relsurv.method = "e2",
                     surv.method = "hazard",
                     breaks = BLy, pophaz = pmy)
  
  std <- survtab(Surv(FUT, lex.Xst) ~ group, data = xd, 
                     surv.type = "surv.rel", relsurv.method = "e2",
                     surv.method = "hazard",
                     breaks = BLd, pophaz = pmd)    
  
  expect_equal(sty$surv.obs.lo, std$surv.obs.lo, scale = 1L, tolerance = 0.0005)
  expect_equal(sty$surv.obs, std$surv.obs, scale = 1L, tolerance = 0.0005)
  expect_equal(sty$surv.obs.hi, std$surv.obs.hi, scale = 1L, tolerance = 0.0005)
  expect_equal(sty$r.e2.lo, std$r.e2.lo, scale = 1L, tolerance = 0.0005)
  expect_equal(sty$r.e2, std$r.e2, scale = 1L, tolerance = 0.0005)
  expect_equal(sty$r.e2.hi, std$r.e2.hi, scale = 1L, tolerance = 0.0005)
  
  ## pohar perme
  
  sty <- survtab(Surv(FUT, lex.Xst) ~ group, data = xy, 
                     surv.type = "surv.rel", relsurv.method = "pp",
                     surv.method = "hazard",
                     breaks = BLy, pophaz = pmy)
  
  std <- survtab(Surv(FUT, lex.Xst) ~ group, data = xd, 
                     surv.type = "surv.rel", relsurv.method = "pp",
                     surv.method = "hazard",
                     breaks = BLd, pophaz = pmd)    
  
  expect_equal(sty$r.pp.lo, std$r.pp.lo, scale = 1L, tolerance = 0.0005)
  expect_equal(sty$r.pp, std$r.pp, scale = 1L, tolerance = 0.0005)
  expect_equal(sty$r.pp.hi, std$r.pp.hi, scale = 1L, tolerance = 0.0005)
  
  #### lifetable method
  ## observed survival & Ederer II
  
  sty <- survtab(Surv(FUT, lex.Xst) ~ group, data = xy, 
                     surv.type = "surv.rel", relsurv.method = "e2",
                     surv.method = "lifetable",
                     breaks = BLy, pophaz = pmy)
  
  std <- survtab(Surv(FUT, lex.Xst) ~ group, data = xd, 
                     surv.type = "surv.rel", relsurv.method = "e2",
                     surv.method = "lifetable",
                     breaks = BLd, pophaz = pmd)    
  
  expect_equal(sty$surv.obs.lo, std$surv.obs.lo, scale = 1L, tolerance = 0.0005)
  expect_equal(sty$surv.obs, std$surv.obs, scale = 1L, tolerance = 0.0005)
  expect_equal(sty$surv.obs.hi, std$surv.obs.hi, scale = 1L, tolerance = 0.0005)
  expect_equal(sty$r.e2.lo, std$r.e2.lo, scale = 1L, tolerance = 0.0005)
  expect_equal(sty$r.e2, std$r.e2, scale = 1L, tolerance = 0.0005)
  expect_equal(sty$r.e2.hi, std$r.e2.hi, scale = 1L, tolerance = 0.0005)
  
  ## pohar perme
  
  sty <- survtab(Surv(FUT, lex.Xst) ~ group, data = xy, 
                     surv.type = "surv.rel", relsurv.method = "pp",
                     surv.method = "lifetable",
                     breaks = BLy, pophaz = pmy)
  
  std <- survtab(Surv(FUT, lex.Xst) ~ group, data = xd, 
                     surv.type = "surv.rel", relsurv.method = "pp",
                     surv.method = "lifetable",
                     breaks = BLd, pophaz = pmd)    
  
  expect_equal(sty$r.pp.lo, std$r.pp.lo, scale = 1L, tolerance = 0.0005)
  expect_equal(sty$r.pp, std$r.pp, scale = 1L, tolerance = 0.0005)
  expect_equal(sty$r.pp.hi, std$r.pp.hi, scale = 1L, tolerance = 0.0005)
  
})



test_that("hazard and lifetable produce congruent results", {
  skip_on_cran()
  library(Epi)
  library(survival)
  
  x <- data.table(popEpi::sire)
  x <- x[dg_date<ex_date]
  
  ## phony group variable
  set.seed(1L)
  x$group <- rbinom(nrow(x), 1, 0.5)
  
  
  xy <- Lexis(entry = list(FUT = 0, AGE = dg_age, CAL = get.yrs(dg_date)), 
              exit = list(CAL = get.yrs(ex_date)), 
              data = x,
              exit.status = factor(status, levels = 0:2, 
                                   labels = c("alive", "canD", "othD")), 
              merge = TRUE)
  
  
  pmy <- data.table(popEpi::popmort)
  setnames(pmy, c("year", "agegroup"), c("CAL", "AGE"))
  BL <- list(FUT = seq(0, 5, 1/12))
  
  sth <- survtab(Surv(FUT, lex.Xst) ~ group, data = xy,
                 pophaz = pmy, breaks = BL,
                 surv.type = "surv.rel", surv.method = "hazard")
  
  stl <- survtab(Surv(FUT, lex.Xst) ~ group, data = xy,
                 pophaz = pmy, breaks = BL,
                 surv.type = "surv.rel", surv.method = "lifetable")
  
  expect_equal(sth$r.e2, stl$r.e2, scale = 1, tol = 0.0003415)
  expect_equal(sth$r.e2.lo, stl$r.e2.lo, scale = 1, tol = 0.000354)
  expect_equal(sth$r.e2.hi, stl$r.e2.hi, scale = 1, tol = 0.00033)
  
  expect_equal(sth$surv.obs, stl$surv.obs, scale = 1, tol = 0.00002575)
  expect_equal(sth$surv.obs.lo, stl$surv.obs.lo, scale = 1, tol = 0.000027)
  expect_equal(sth$surv.obs.hi, stl$surv.obs.hi, scale = 1, tol = 0.000025)
  
})

