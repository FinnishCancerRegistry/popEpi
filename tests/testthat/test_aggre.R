context("laggre")

test_that("laggre produces meaningful results", {
  skip_on_cran()
  
  BL <- list(fot= seq(0,20,1/12), age= c(0:100, Inf), per= c(1960:2014))
  x <- lexpand(sire, birth  = bi_date, entry = dg_date, exit = ex_date,
               status = status %in% 1:2,
               breaks=BL)
  e <- quote(list(surv.int = fot, per, agegr = age))
  v <- c("surv.int", "per", "agegr")
  
  x2 <- laggre(x, aggre = e, substituted = T)
  x2 <- laggre(x, aggre = e, substituted = T, type = "full")
  setkeyv(x2, v)
  
  setDT(x)
  x[, fot := cutLow(fot, BL$fot)]
  x[, age := cutLow(age, BL$age)]
  x[, per := cutLow(per, BL$per)]
  
  x <- x[, list(pyrs = sum(lex.dur), obs = sum(lex.Xst)), keyby = e]
  x <- x[pyrs > 0 & !is.na(pyrs)]
  
  setkeyv(x, v)
  
  expect_equal(x2$pyrs, x$pyrs, tolerance = 1e-05)
  expect_equal(x2$from0to1, x$obs, tolerance = 1e-05)
  
})