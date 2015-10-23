context("utility functions")

test_that("subsetting in ltable works and ltable has no side effects", {
  skip_on_cran()
  
  sr <- popEpi::sire[1:100, ]
  set.seed(1L)
  sr[, sex := rbinom(.N, 1, prob = 0.5)]
  sr[c(1, 50), sex := NA]
  
  setkeyv(sr, "bi_date")
  old_sr <- copy(sr)
  
  lt1 <- ltable(sr, by = "sex", subset = sex == 0, na.rm = TRUE)
  lt2 <- ltable(sr, by = "sex", subset = sex == 1, na.rm = TRUE)
  lt3 <- ltable(sr, by = "sex", na.rm = TRUE)
  
  expect_equal(lt3$obs, c(lt1[1, ]$obs, lt2[2, ]$obs))
  expect_true(all.equal(sr, old_sr))
  
})




test_that("evalPopArg produces intended results",{
  set.seed(1L)
  dt <- data.table(a = rbinom(10, 100, 0.25), b = 1:2, c = 1:5)
  
  tf <- function(x=dt, arg) {
    
    as <- substitute(arg)
    byTab <- evalPopArg(x, arg = as)
    
    x[, list(sum = sum(a)), by = byTab]
    
  }
  
  ## symbol
  t1 <- tf(arg=b)
  
  ## name string
  t2 <- tf(arg="b")
  
  expect_equal(t1$sum, c(127, 131))
  expect_equal(t1, t2)
  
  ## list of symbols / expressions
  t3 <- tf(arg=list(b, c))
  
  ## name strings
  t4 <- tf(arg=c("b", "c"))
  
  ## object containing name strings
  byVars <- c("b", "c")
  t5 <- tf(arg=byVars)
  
  expect_equal(t4$sum, c(22,24,26,31,21, 31,32,27,26,18))
  expect_equal(t4, t3)
  expect_equal(t4, t5)
  
  ## list of symbols / expressions
  t6 <- tf(arg=list(var1 = b,c, cut(c,3)))
  expect_equal(names(t6), c("var1", "c", "cut", "sum"))
  
  
  ## NULL object
  byVars <- NULL
  t7 <- tf(arg=byVars)
  t8 <- tf(arg=NULL)
  expect_equal(t7, t8)
  
  ## a list of predetermined values
  byList <- as.list(dt[, list(b, var1 = c)])
  t9 <- tf(arg=byList)
  
  ## list without any names
  byList <- list(dt$b, dt$c)
  t10<- tf(arg=byList)
  
  ## partially named list
  byList <- list(var1 = dt$b, dt$c)
  t11<- tf(arg=byList)
  
  expect_equal(t9$sum, t10$sum)
  expect_equal(t10$sum, t11$sum)
  expect_equal(names(t11), c("var1", "BV2", "sum"))
  
  
  t12 <- tf(arg=list(V0=dt$b, dt$c))
  byList <- list(V0 = dt$b, dt$c)
  t13 <- tf(arg=byList)
  expect_equal(t12, t13)
  
})
