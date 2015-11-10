context("laggre")

test_that("laggre leaves original data untouched", {
  
  x <- sire[1:100,]
  BL <- list(fot= seq(0,20,1/12), age= c(0:100, Inf), per= c(1960:2014))
  x <- lexpand(x, birth = bi_date, entry = dg_date, exit = ex_date,
               status = status %in% 1:2, breaks=BL)
  
  ## scramble order
  set.seed(1L)
  x <- x[sample(x = .N, size = .N, replace = FALSE)]
  setattr(x, "breaks", BL)
  setattr(x, "time.scales", c("fot", "per", "age"))
  
  xor <- copy(x)
  
  ag1 <- laggre(x, aggre = list(gender = factor(sex, 1, "f"), sex, surv.int = fot, per, agegr = age))
  
  expect_identical(x, xor)
})

test_that("laggre and lexpand produce the same results", {
  # skip_on_cran()
  
  sr <- popEpi::sire[dg_date < ex_date,][1:1000,]
  
  BL <- list(fot= seq(0,20,1/12), age= c(0:100, Inf), per= c(1960:2014))
  x <- lexpand(sr, birth  = bi_date, entry = dg_date, exit = ex_date,
               status = status %in% 1:2, breaks=BL)
  if (!is.data.table(x)) setDF2DT(x)
  
  e <- quote(list(gender = factor(sex, 1, "f"), sex, surv.int = fot, per, agegr = age))
  v <- c("gender", "sex", "sex", "surv.int", "per", "agegr")
  
  x2 <- laggre(x, aggre = e, substituted = T, verbose = FALSE)
  x3 <- laggre(x, aggre = e, substituted = T, type = "full", verbose = FALSE)
  x4 <- lexpand(sr, birth  = bi_date, entry = dg_date, exit = ex_date,
                status = status %in% 1:2, aggre.type = "non-empty",
                breaks=BL, aggre = list(gender = factor(sex, 1, "f"), sex, surv.int = fot, per, agegr = age))
  x5 <- lexpand(sr, birth  = bi_date, entry = dg_date, exit = ex_date,
                status = status %in% 1:2, aggre.type = "cartesian",
                breaks=BL, aggre = list(gender = factor(sex, 1, "f"), sex, surv.int = fot, per, agegr = age))
  
  x[, fot := cutLow(fot, BL$fot)]
  x[, age := cutLow(age, BL$age)]
  x[, per := cutLow(per, BL$per)]
  
  x <- x[, list(pyrs = sum(lex.dur), obs = sum(lex.Xst)), keyby = e]
  x <- x[pyrs > 0 & !is.na(pyrs)]
  
  if (!is.data.table(x2)) setDF2DT(x2)
  if (!is.data.table(x3)) setDF2DT(x3)
  if (!is.data.table(x4)) setDF2DT(x4)
  if (!is.data.table(x5)) setDF2DT(x5)
  
  setkeyv(x, v)
  setkeyv(x2, v)
  setkeyv(x3, v)
  setkeyv(x4, v)
  setkeyv(x5, v)
  
  expect_equal(x2$pyrs, x$pyrs, tolerance = 1e-05)
  expect_equal(x2$from0to1, x$obs, tolerance = 1e-05)
  
  expect_equal(sum(x2$pyrs), sum(x3$pyrs), tolerance = 1e-05)
  expect_equal(sum(x2$from0to1), sum(x3$from0to1), tolerance = 1e-05)
  
  expect_equal(sum(x2$pyrs), sum(x4$pyrs), tolerance = 1e-05)
  expect_equal(sum(x2$from0to1), sum(x4$from0to1), tolerance = 1e-05)
  
  expect_equal(x3$pyrs, x5$pyrs, tolerance = 1e-05)
  expect_equal(x3$from0to0, x5$from0to0, tolerance = 1e-05)
  expect_equal(sum(x3$from0to1), sum(x5$from0to1), tolerance = 1e-05)
  
  expect_equal(x2$pyrs, x4$pyrs, tolerance = 1e-05)
  expect_equal(x2$from0to0, x4$from0to0, tolerance = 1e-05)
  expect_equal(sum(x2$from0to1), sum(x4$from0to1), tolerance = 1e-05)
})


test_that("laggre's aggre argument works flexibly", {
  # skip_on_cran()
  
  library(Epi)
  BL <- list(fot = 0:5, per = c(1995,2015))
  for (cond in c(FALSE, TRUE)) {
    x <- Lexis(data = sire[dg_date < ex_date,][1:500, ], entry = list(fot = 0, age = dg_age, per = get.yrs(dg_date)),
               exit = list(per = get.yrs(ex_date)), exit.status = status, 
               entry.status = 0)
    x <- splitLexis(x, breaks = BL$fot, time.scale = "fot")
    x <- splitLexis(x, breaks = BL$per, time.scale = "per")
    x$agegr <- cut(x$dg_age, 2)
    if (cond) {
      setDT(x)
      setattr(x, "class", c("Lexis", "data.table", "data.frame"))
      setattr(x, "breaks", BL)
    }
    
    a <- laggre(x, aggre = list(agegr = cut(dg_age, 2), sex, fot, per = per), type = "unique")
    b <- laggre(x, aggre = c("agegr", "sex", "fot", "per"), type = "unique")
    
    expect_equal(a, b)
    
    
    a <- laggre(x, aggre = cut(dg_age, 2), type = "unique")
    setnames(a, "cut", "agegr")
    attr(a, "aggreVars")$by <- "agegr"
    b <- laggre(x, aggre = c("agegr"), type = "unique")
    c <- laggre(x, aggre = list(agegr = cut(dg_age, 2)), type = "unique")
    
    expect_equal(a, b)
    expect_equal(b, c)
  }
  
  
})


