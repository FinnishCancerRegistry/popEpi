context("test_lexis_crop.R")

testthat::test_that("lexis_crop works", {
  dt <- popEpi::Lexis_dt(
    entry = list(ts_fut = 0.0, ts_cal = 2001.56, ts_age = 51.64),
    exit = list(ts_cal = 2006.32),
    entry.status = 0L,
    exit.status = 0L
  )
  dtc <- lexis_crop(
    lexis = data.table::copy(dt),
    breaks = list(ts_cal = c(2001, 2005), ts_age = c(52, 60))
  )
  testthat::expect_equal(dtc[["ts_age"]], 52)
  testthat::expect_equal(dtc[["ts_cal"]] + dtc[["lex.dur"]], 2005)

  dtc <- lexis_crop(
    lexis = data.table::copy(dt),
    breaks = list(ts_cal = c(1995, 2000), ts_age = c(52, 60))
  )
  testthat::expect_true(is.na(dtc[["lex.dur"]]))

  dtc <- lexis_crop(
    lexis = data.table::copy(dt),
    breaks = list(ts_cal = c(2020, 2025), ts_age = c(52, 60))
  )
  testthat::expect_true(is.na(dtc[["lex.dur"]]))
})
