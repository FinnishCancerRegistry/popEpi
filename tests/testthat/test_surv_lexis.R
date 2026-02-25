testthat::context("surv_lexis")

testthat::test_that("surv_lexis & survival produce comparable results", {
  sire <- test_make_sire__()
  bl <- list(ts_fut = seq(0, 5, 1 / 12))
  obs <- popEpi:::surv_lexis(
    dt = sire,
    breaks = bl,
    aggre_by = "my_stratum",
    aggre_ts_col_nms = "ts_fut",
    subset = NULL,
    estimators = "S_pch"
  )
  exp <- test_survfit_dt__(
    data = sire,
    t = bl[["ts_fut"]],
    stratum_col_nms = "my_stratum"
  )
  testthat::expect_true(all.equal(
    obs[["S_pch_est"]],
    exp[["est"]],
    scale = 1L,
    tolerance = 0.001
  ))
  testthat::expect_true(all.equal(
    obs[["S_pch_se"]],
    exp[["se"]],
    scale = 1L,
    tolerance = 0.001
  ))
})

testthat::test_that("surv_lexis & relsurv produce comparable results", {
  sire <- test_make_sire__()
  bl <- list(ts_fut = seq(0, 5, 1 / 12))
  obs <- popEpi:::surv_lexis(
    dt = sire,
    breaks = bl,
    merge_dt = test_make_pm__(),
    merge_dt_by = c("sex", "ts_cal", "ts_age"),
    aggre_by = "my_stratum",
    aggre_ts_col_nms = "ts_fut",
    subset = NULL,
    estimators = c("RS_e2_pch", "NS_pp_pch")
  )
  exp <- test_relsurv_dt__(
    data = sire,
    stratum_col_nms = "my_stratum",
    t = bl[["ts_fut"]],
    method = "ederer2"
  )
  testthat::expect_true(
    max(abs(obs[["RS_e2_pch_est"]] - exp[["est"]])) < 0.026
  )
  testthat::expect_true(
    max(abs(obs[["RS_e2_pch_se"]] - exp[["se"]])) < 0.001
  )
  exp <- test_relsurv_dt__(
    data = sire,
    stratum_col_nms = "my_stratum",
    t = bl[["ts_fut"]],
    method = "pohar-perme"
  )
  testthat::expect_true(
    max(abs(obs[["NS_pp_pch_est"]] - exp[["est"]])) < 0.028
  )
  testthat::expect_true(
    max(abs(obs[["NS_pp_pch_se"]] - exp[["se"]])) < 0.001
  )
})
