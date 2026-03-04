testthat::context("surv_lexis_S_exp_e1_pch_est")

# testthat::test_that("surv_lexis_S_exp_e1_pch_est & survival in harmony", {
#   sire <- test_make_sire__()
#   bl <- list(ts_fut = seq(0, 5, 1 / 12))
#   pm <- test_make_pm__()
#   sire <- sire[
#     data.table::year(sire[["dg_date"]]) <= max(pm[["ts_cal"]] - 5)
#   ]
#   obs <- popEpi::surv_lexis(
#     lexis = sire,
#     breaks = bl,
#     aggre_by = "my_stratum",
#     aggre_ts_col_nms = "ts_fut",
#     merge_dt = pm,
#     merge_dt_by = setdiff(names(pm), "h_exp"),
#     estimators = "S_exp_e1_pch"
#   )
#   exp <- test_survexp_dt__(
#     data = sire,
#     t = bl[["ts_fut"]],
#     stratum_col_nms = "my_stratum"
#   )
#   # these are rather different.
#   testthat::expect_true(all.equal(
#     obs[["S_exp_e1_pch_est"]],
#     exp[["est"]],
#     scale = 1L,
#     tolerance = 0.033
#   ))
# })
