context("makeWeightsDT")


test_that("makeWeightsDT works with various weights & adjust spesifications", {
  skip("makeWeightsDT tests not yet implemented")
  sibr <- popEpi::sibr[1:100]
  sibr[1:50, sex := 0L]
  dt <- lexpand(sibr, birth = bi_date, entry = dg_date, exit = ex_date,
                status = status %in% 1:2, aggre = list(sex, dg_age = cutLow(dg_age, 0:125)))
  print <- quote(list(sex))
  print <- evalPopArg(dt, print)
  
  adjust <- quote(list(dg_age = cutLow(dg_age, c(0, seq(15,85,5), Inf))))
  adjust <- evalPopArg(dt, adjust)
  expr <- quote(list(pyrs = sum(pyrs), from0to1 = sum(from0to1)))
  dt <- dt[, eval(expr), keyby = cbind(print, adjust)]
  wdt <- ICSS[, list(dg_age = c(0, seq(15,85,5)), weights = ICSS3)]
  wli <- list(dg_age = ICSS$ICSS3)
  
  DT <- makeWeightsDT(dt, weights = wdt)
  
  list(dg_age = cutLow(dg_age))
  
})