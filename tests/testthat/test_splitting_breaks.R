context("breaks attributes resulting from splitting")




test_that("splitMulti produces intended breaks list", {
  
  x <- data.table(popEpi::sibr)
  
  x <- lexpand(x, entry = "dg_date", exit = "ex_date", birth = "bi_date", 
               status = "status")
  
  BL <- list(fot = 0:10, per = 1990:2000, age = seq(0,100, 10))
  xx <- splitMulti(x, breaks = BL, drop = TRUE)
  
  BL2 <- list(fot = 2:8, per = 1991:1999, age = seq(50,70, 10))
  xxx <- splitMulti(xx, breaks = BL, drop = TRUE)
  
  expect_equal(breaks(xx, "fot"), BL$fot)
  expect_equal(breaks(xx, "per"), BL$per)
  expect_equal(breaks(xx, "age"), BL$age)
  
  expect_equal(breaks(xxx, "fot"), BL2$fot)
  expect_equal(breaks(xxx, "per"), BL2$per)
  expect_equal(breaks(xxx, "age"), BL2$age)
})



test_that("splitLexisDT produces intended breaks list", {
  
  x <- data.table(popEpi::sibr)
  
  x <- lexpand(x, entry = "dg_date", exit = "ex_date", birth = "bi_date", 
               status = "status")
  
  br <- 0:10
  xx <- splitLexisDT(x, breaks = br, timeScale = "fot", drop = TRUE)
  
  br2 <- 2:8
  xxx <- splitLexisDT(xx, breaks = br, timeScale = "fot", drop = TRUE)
  
  expect_equal(breaks(xx, "fot"), br)
  expect_equal(breaks(xxx, "fot"), br2)
})





