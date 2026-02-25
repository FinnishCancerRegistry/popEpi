test_make_pm__ <- function() {
  pm <- data.table::copy(popEpi::popmort)
  data.table::setnames(
    pm,
    c("year", "agegroup", "haz"),
    c("ts_cal", "ts_age", "h_exp")
  )
  data.table::setkeyv(pm, c("sex", "ts_cal", "ts_age"))
  pm <- c(
    list(pm),
    lapply(101:110, function(age) {
      sub_pm <- pm[
        pm[["ts_age"]] == max(pm[["ts_age"]])
      ]
      sub_pm[
        j = "ts_age" := age
      ]
      sub_pm[]
    })
  )
  pm <- data.table::rbindlist(pm)
  return(pm[])
}

test_make_column_icss_ag__ <- function(age) {
  cut(
    age,
    breaks = c(0, 60, 70, 80, Inf),
    right = FALSE,
    labels = c("0-59", "60-69", "70-79", "80+")
  )
}

test_make_standard_weight_dt__ <- function() {
  return(popEpi::ICSS[
    j = list(
      weight = as.double(sum(.SD[["ICSS1"]]))
    ),
    keyby = list(
      icss_ag = test_make_column_icss_ag__(popEpi::ICSS[["age"]])
    )
  ][])
}

test_make_sire__ <- function() {
  sire <- popEpi::sire[dg_date < ex_date, ]
  set.seed(1337)
  sire[j = "my_stratum" := sample(2L, size = .N, replace = TRUE)]
  data.table::setkeyv(sire, c("dg_date", "ex_date"))
  sire <- popEpi::Lexis_dt(
    data = sire,
    entry = list(
      ts_cal = round(get.yrs(dg_date), 4),
      ts_age = round(
        get.yrs(dg_date) - get.yrs(bi_date),
        4
      ),
      ts_fut = 0.0
    ),
    duration = round(get.yrs(ex_date), 4) -
      round(get.yrs(dg_date), 4),
    entry.status = 0L,
    exit.status = status
  )
  data.table::setkeyv(sire, c("lex.id", "ts_cal", "ts_age", "ts_fut"))
  data.table::set(
    x = sire,
    j = "icss_ag",
    value = test_make_column_icss_ag__(sire[["dg_age"]])
  )
  data.table::set(
    x = sire,
    j = c("individual_weight", "dg_age_group"),
    value = list(
      popEpi::surv_individual_weights(
        dt = sire,
        standard_weight_dt = test_make_standard_weight_dt__(),
        observed_weight_dt = NULL
      ),
      cut(
        sire[["dg_age"]],
        breaks = c(0, 60, 70, 80, Inf),
        labels = FALSE,
        right = TRUE
      )
    )
  )
  # sire <- sire[
  #   j = .SD[as.integer(round(seq.int(from = 1L, to = .N, length.out = 30)))],
  #   by = "icss_ag"
  # ]

  # sire[, "rs_dur"  := as.integer(sire$ex_date - sire$dg_date)]
  sire[, "rs_ts_age"  := as.integer(sire$dg_date - sire$bi_date)]
  sire[, "rs_ts_cal"  := as.integer(sire$dg_date)]
  sire[, "rs_sex"  := as.integer(sire$sex + 1L)]
  return(sire[])
}

test_survfit_dt__ <- function(data, stratum_col_nms = NULL, t, ...) {
  requireNamespace("survival")
  out <- data[
    j = {
      fit <- survival::survfit(
        formula = Surv(lex.dur, lex.Xst != 0) ~ 1,
        data = .SD,
        ...
      )
      t <- setdiff(t, 0.0)
      fit <- summary(fit, times = t)
      out <- data.table::data.table(
        t = t,
        est = fit[["surv"]],
        se = fit[["std.err"]]
      )
      out[]
    },
    keyby = eval(stratum_col_nms)
  ]
  return(out[])
}

test_relsurv_ratetable__ <- function() {
  # male
  pm <- data.table::data.table(popEpi::popmort)
  pm[, "surv" := exp(-pm$haz)]
  pm_m <- cast_simple(
    pm[pm$sex == 0],
    columns = 'year', rows = 'agegroup',  values='surv'
  )
  pm_m[, "agegroup" := NULL]
  pm_m <- as.matrix(pm_m)
  # female
  pm_f <- cast_simple(
    pm[pm$sex == 1],
    columns = 'year', rows = 'agegroup',  values='surv'
  )
  pm_f[, "agegroup" := NULL]
  pm_f <- as.matrix(pm_f)

  rt <- relsurv::transrate(
    pm_m, pm_f, yearlim = c(1951, 2013), int.length = 1
  )
  data.table::setattr(
    rt,
    "dimid",
    c("rs_ts_age", "rs_sex", "rs_ts_cal")
  )
  return(rt)
}

test_relsurv_dt__ <- function(
  data, stratum_col_nms = NULL, t, ...
) {
  requireNamespace("relsurv")
  rt <- test_relsurv_ratetable__()
  rs_ts_age <- rs_sex <- rs_ts_cal <- NULL # for R CMD CHECK
  year_mult <- 365.2425
  out <- data[
    j = {
      rs_fit <- relsurv::rs.surv(
        Surv(lex.dur * year_mult, lex.Xst != 0) ~ 1,
        ratetable = rt,
        data = .SD,
        type = "fleming-harrington",
        rmap = list(age = rs_ts_age, sex = rs_sex, year = rs_ts_cal),
        precision = 30,
        ...
      )
      t <- setdiff(t, 0.0)
      rs_summary <- summary(rs_fit, t = t * year_mult)
      rs_dt <- data.table::data.table(
        t = t,
        est = rs_summary[["surv"]],
        se = rs_summary[["std.err"]]
      )
      rs_dt[]
    },
    keyby = eval(stratum_col_nms)
  ]
  return(out[])
}
