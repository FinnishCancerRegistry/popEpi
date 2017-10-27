context("Attributes of data split by popEpi funs")





test_that("popEpi splitters produce correct attributes", {
  
  library("Epi")
  library("data.table")
  
  sire <- setDT(copy(popEpi::sire))
  sire[, "dg_yrs" := get.yrs(dg_date, "actual")]
  sire[, "ex_yrs" := get.yrs(ex_date, "actual")]
  sire[, "bi_yrs" := get.yrs(bi_date, "actual")]
  sire[, "id" := 1:.N]
  
  sire <- Lexis(
    data = sire[dg_date < ex_date], 
    entry = list(fot=0, per=dg_yrs, age=dg_age),
    exit = list(per=ex_yrs),
    merge = TRUE, 
    exit.status = 1L, entry.status = 0L
  )
  BL <- list(age = c(0, 50), fot = c(0, 5))
  sm_1 <- splitMulti(sire, breaks = BL["fot"], drop = TRUE)
  sm_1 <- splitMulti(sm_1, breaks = BL["age"], drop = TRUE)
  
  sm_2 <- splitMulti(sire, breaks = BL, drop = TRUE)
  
  sl <- splitLexisDT(sire, breaks = BL$fot, timeScale = "fot", drop = TRUE)
  sl <- splitLexisDT(sl, breaks = BL$age, timeScale = "age", drop = TRUE)
  
  lp <- lexpand(data.table(sire)[, .(bi_yrs, dg_yrs, ex_yrs, status)], 
                birth = bi_yrs, entry = dg_yrs, exit = ex_yrs, status = status,
                breaks = BL)
  
  lapply(list(sm_1, sm_2, sl, lp), function(lex) {
    expect_identical(
      attr(lex, "breaks"),
      list(fot = c(0, 5), per = NULL, age = c(0, 50))
    )
    expect_identical(
      attr(lex, "time.scales"),
      c("fot", "per", "age")
    ) 
  })
  
})





