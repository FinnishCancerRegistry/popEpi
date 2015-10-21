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


