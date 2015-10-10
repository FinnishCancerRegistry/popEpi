context("laggre")

test_that("laggre produces meaningful results", {
  # skip_on_cran()
  
  BL <- list(fot= seq(0,20,1/12), age= c(0:100, Inf), per= c(1960:2014))
  x <- lexpand(sire, birth  = bi_date, entry = dg_date, exit = ex_date,
               status = status %in% 1:2,
               breaks=BL)
  e <- quote(list(gender = factor(sex, 1, "f"), sex, surv.int = fot, per, agegr = age))
  v <- c("gender", "sex", "sex", "surv.int", "per", "agegr")
  
  x2 <- laggre(x, aggre = e, substituted = T)
  x3 <- laggre(x, aggre = e, substituted = T, type = "full")
  x4 <- lexpand(sire, birth  = bi_date, entry = dg_date, exit = ex_date,
                status = status %in% 1:2, aggre.type = "unique",
                breaks=BL, aggre = list(gender = factor(sex, 1, "f"), sex, surv.int = fot, per, agegr = age))
  
  setDT(x)
  x[, fot := cutLow(fot, BL$fot)]
  x[, age := cutLow(age, BL$age)]
  x[, per := cutLow(per, BL$per)]
  
  x <- x[, list(pyrs = sum(lex.dur), obs = sum(lex.Xst)), keyby = e]
  x <- x[pyrs > 0 & !is.na(pyrs)]
  
  setkeyv(x, v)
  setkeyv(x2, v)
  setkeyv(x3, v)
  setkeyv(x4, v)
  
  expect_equal(x2$pyrs, x$pyrs, tolerance = 1e-05)
  expect_equal(x2$from0to1, x$obs, tolerance = 1e-05)
  
  expect_equal(sum(x2$pyrs), sum(x3$pyrs), tolerance = 1e-05)
  expect_equal(sum(x2$from0to1), sum(x3$from0to1), tolerance = 1e-05)
  
  expect_equal(sum(x2$pyrs), sum(x4$pyrs), tolerance = 1e-05)
  expect_equal(sum(x2$from0to1), sum(x4$from0to1), tolerance = 1e-05)
})