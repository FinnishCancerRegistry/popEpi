context("makeWeightsDT")


test_that("makeWeightsDT works with various weights & adjust spesifications", {
  skip_on_cran()
  sibr <- popEpi::sibr[1:100]
  sibr[1:50, sex := 0L]
  
  dt <- lexpand(sibr, birth = bi_date, entry = dg_date, exit = ex_date,
                status = status %in% 1:2, aggre = list(sex, dg_age = popEpi:::cutLow(dg_age, 0:125)))
  
  print <- quote(list(gender = sex))
  prdt <- evalPopArg(dt, print)
  
  adjust <- quote(list(agegr = popEpi:::cutLow(dg_age, c(0, seq(15,85,5), Inf))))
  addt <- evalPopArg(dt, adjust)
  
  expr <- quote(list(pyrs = sum(pyrs), from0to1 = sum(from0to1)))
  dt <- dt[, eval(expr), keyby = cbind(prdt, addt)]
  
  wdt <- ICSS[, list(agegr = c(0, seq(15,85,5)), weights = ICSS3)]
  
  
  expr <- quote(list(pyrs = pyrs, from0to1 = from0to1))
  
  ## works with weights data.frame?
  ## Intention of test: wdt contains more levels of agegr than data;
  ## weights need to be scaled to one correctly and only the original
  ## rows in data kept.
  DT <- makeWeightsDT(dt, values = expr, weights = wdt, 
                      print = "gender", adjust = "agegr")
  expect_equal(nrow(DT), 24L)
  expect_equal(sum(dt$pyrs), sum(DT$pyrs))
  expect_equal(sort(intersect(wdt$weights/sum(wdt$weights), unique(DT$weights))),
               sort(unique(DT$weights)))
  expect_equal(DT[, sum(pyrs), keyby = agegr], dt[, sum(pyrs), keyby = agegr], check.attributes = FALSE)
  expect_equal(DT[, sum(pyrs), keyby = gender], dt[, sum(pyrs), keyby = gender], check.attributes = FALSE)
  
  ## weights and adjust only the same length thanks to custom.levels
  ## intention: test that custom.levels works as intended (repeats empty levels),
  ## and that weights as a list works
  wli <- list(agegr = ICSS[, list(agegr = sum(ICSS3)), 
                           by = list(cut(age, breaks = c(0,seq(15,85,5)), right = FALSE)) ]$agegr)
  
  DT <- makeWeightsDT(dt, values = expr, weights = wli, 
                      print = "gender", adjust = "agegr", 
                      custom.levels = list(agegr = c(0,seq(15,85,5))))
  
  expect_equal(nrow(DT), 32L)
  expect_equal(sort(intersect(wli$agegr/sum(wli$agegr), unique(DT$weights))),
               sort(unique(DT$weights)))
  expect_equal(DT[agegr>=30L, sum(pyrs), keyby = agegr], dt[, sum(pyrs), keyby = agegr], check.attributes = FALSE)
  expect_equal(DT[, sum(pyrs), keyby = gender], dt[, sum(pyrs), keyby = gender], check.attributes = FALSE)
  
})