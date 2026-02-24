prev_survival_dt_expand__ <- function(
  survival_dt,
  survival_dt_by,
  ts_fut_col_nm,
  ts_fut_ceiling = NULL,
  n_target_intervals = 1e3L
) {
  n_original_intervals <- data.table::uniqueN(survival_dt[[ts_fut_col_nm]])
  m <- as.integer(n_target_intervals / n_original_intervals)
  out <- data.table::setDT(data.table::copy(survival_dt))
  data.table::setkeyv(
    out,
    c(setdiff(survival_dt_by, ts_fut_col_nm), ts_fut_col_nm)
  )
  ts_fut_breaks <- sort(unique(survival_dt[[ts_fut_col_nm]]))
  if (is.null(ts_fut_ceiling)) {
    ts_fut_ceiling <- ts_fut_breaks[length(ts_fut_breaks)] + diff(utils::tail(
      ts_fut_breaks, 2L
    ))
  }
  ts_fut_breaks <- c(ts_fut_breaks, ts_fut_ceiling)
  data.table::set(
    x = out,
    j = "interval_width",
    value = local({
      iw <- diff(ts_fut_breaks)
      rep(iw, times = nrow(out) / length(iw))
    })
  )
  data.table::set(
    x = out,
    j = "cumulative_hazard",
    value = -log(out[["survival"]])
  )
  out[
    #' @importFrom data.table :=
    j = "hazard" := diff(c(0.0, .SD[["cumulative_hazard"]])) /
      .SD[["interval_width"]],
    by = setdiff(survival_dt_by, ts_fut_col_nm)
  ]

  out <- out[
    i = rep(seq_len(nrow(survival_dt)), each = m)
  ]
  data.table::set(
    x = out,
    j = c(ts_fut_col_nm, "interval_width"),
    value = local({
      ts_fut_values <- unique(unlist(lapply(
        seq_len(length(ts_fut_breaks) - 1L),
        function(i) {
          seq(ts_fut_breaks[i], ts_fut_breaks[i + 1L], length.out = m + 1L)
        }
      )))
      list(
        ts_fut_values[-length(ts_fut_values)],
        diff(ts_fut_values)
      )
    })
  )
  data.table::set(
    x = out,
    j = "cumulative_hazard",
    value = out[["hazard"]] * out[["interval_width"]]
  )
  out[
    #' @importFrom data.table := .SD
    j = "cumulative_hazard" := lapply(.SD, cumsum),
    .SDcols = "cumulative_hazard",
    by = setdiff(survival_dt_by, ts_fut_col_nm)
  ]
  data.table::set(
    x = out,
    j = "survival",
    value = exp(-out[["cumulative_hazard"]])
  )
  data.table::set(
    x = out,
    j = setdiff(names(out), c(survival_dt_by, "survival")),
    value = NULL
  )
  return(out[])
}

prev_lexis <- function(
  dt,
  subset = NULL,
  breaks = list(
    ts_cal = 2009.99,
    ts_fut = c(0L, 1L, 5L, 10L),
    ts_age = c(seq(0L, 85L, 5L), 120L)
  ),
  survival_dt_by = NULL,
  survival_dt = NULL,
  survival_dt_harmonisers = NULL,
  aggre_stratum_dt = NULL,
  conf_methods = "log-log",
  conf_lvls = 0.95,
  weights = NULL
) {
  stopifnot(
    inherits(breaks, "list"),
    length(breaks) == 3,
    data.table::uniqueN(names(breaks)) == 3,
    is.data.frame(dt),
    names(breaks) %in% names(dt),
    breaks[[2]][1] == 0
  )
  subset <- handle_arg_subset()
  lexis_dt <- dt[
    (subset),
    unique(c(
      "lex.id", names(breaks), "lex.dur", "lex.Cst", "lex.Xst",
      survival_dt_by,
      names(aggre_stratum_dt)
    ))
  ]
  prev_dt <- data.table::data.table(
    dt_row_no = rep(seq_len(nrow(lexis_dt)), each = length(breaks[[1]])),
    index_time_point = rep(breaks[[1]], times = sum(subset))
  )
  data.table::set(
    x = prev_dt,
    j = c("entry", "exit", "died"),
    value = data.table::setDF(list(
      lexis_dt[[names(breaks)[1]]],
      lexis_dt[[names(breaks)[1]]] + lexis_dt[["lex.dur"]],
      lexis_dt[["lex.Cst"]] != lexis_dt[["lex.Xst"]]
    ))[prev_dt[["dt_row_no"]], ]
  )
  data.table::set(
    x = prev_dt,
    j = "exit_before_time_point",
    value = prev_dt[["exit"]] < prev_dt[["index_time_point"]]
  )
  data.table::set(
    x = prev_dt,
    j = "died_before_time_point",
    value = prev_dt[["died"]] & prev_dt[["exit_before_time_point"]]
  )
  prev_dt <- prev_dt[
    i = !prev_dt[["died_before_time_point"]],
    #' @importFrom data.table .SD
    j = .SD,
    .SDcols = setdiff(names(prev_dt), "died_before_time_point")
  ]
  survival_dt <- data.table::setDT(as.list(survival_dt))
  data.table::set(
    x = survival_dt,
    j = c("H", "survival_ts_fut"),
    value = list(
      -log(survival_dt[["S"]]),
      survival_dt[[names(breaks)[2]]]
    )
  )
  browser()
  survival_dt[
    #' @importFrom data.table := .SD
    j = "h_pch" := diff(c(0.0, .SD[["H"]])) / .SD[["d"]],
    by = eval(setdiff(names(survival_dt_by), names(breaks)[2]))
  ]
  data.table::set(
    x = prev_dt,
    j = c("observed_duration", "unobserved_duration"),
    value = list(
      prev_dt[["exit"]] - prev_dt[["entry"]],
      prev_dt[["index_time_point"]] - prev_dt[["exit"]]
    )
  )
  data.table::set(
    x = prev_dt,
    j = "total_duration",
    value = prev_dt[["observed_duration"]] + prev_dt[["unobserved_duration"]]
  )
  data.table::setnames(
    x = prev_dt,
    old = "index_time_point",
    new = names(breaks)[1]
  )
  data.table::set(
    x = prev_dt,
    j = c(names(breaks)[2:3], "lex.dur"),
    value = list(
      lexis_dt[[names(breaks)[2]]][prev_dt[["dt_row_no"]]] +
        prev_dt[["total_duration"]],
      lexis_dt[[names(breaks)[3]]][prev_dt[["dt_row_no"]]] +
        prev_dt[["total_duration"]],
      0.0
    )
  )
  data.table::set(
    x = prev_dt,
    j = setdiff(survival_dt_by, names(prev_dt)),
    value = lapply(setdiff(survival_dt_by, names(prev_dt)), function(col_nm) {
      lexis_dt[[col_nm]][prev_dt[["dt_row_no"]]]
    })
  )
  lexis_set__(dt = prev_dt, lexis_ts_col_nms = names(breaks))
  browser()
  lexis_merge_survival(
    dt = prev_dt,
    survival_dt = survival_dt,
    survival_dt_by = survival_dt_by,
    survival_dt_harmonisers = survival_dt_harmonisers,
    ts_fut_col_nm = names(breaks)[1]
  )
  prev_dt[
    i = prev_dt[["exit_before_time_point"]] == TRUE,
    #' @importFrom data.table := .SD
    j = "surv_exit" := exp(
      -(.SD[["H"]] - .SD[["h_pch"]] *
        (.SD[["prevalence_time_point_ts_fut"]] - .SD[["surv_ts_fut"]]))
    )
  ]

  browser()

  # for each prevalence time point,
  # - use S = 1 if person still in follow-up
  # - use S = 0 if person has died before this time point
  # - use S \in [0, 1] if person was lost to follow-up before this time point:
  #   + take S_i for the index time point and divide by S_x at the exit time
  #     point. S = S_i / S_x. alternatively S = exp(-(H_i - H_x)).
  #   + slight problem is the fact that survival is defined for intervals and
  #     we must take a survival value at a time point. a time point near the
  #     start of the interval should be almost the same as the survival at the
  #     start of the interval for instance. this could be handled for instance
  #     via S_i = exp(-(H - h_pch * (t_i - start))) where t_i is the index time
  #     point and start is the interval start point.
  work_dt <- surv_split__(
    dt = dt,
    breaks = breaks[1],
    subset = subset,
    merge = intersect(
      names(dt),
      union(names(aggre_stratum_dt), survival_dt_by)
    )
  )
  data.table::setkeyv(
    work_dt,
    c(names(aggre_stratum_dt), "lex.id", names(breaks))
  )
  data.table::set(
    x = survival_dt,
    j = c("interval_start", "interval_stop"),
    value = list(
      survival_dt[[names(breaks)[2]]],
      survival_dt[[names(breaks)[2]]] + survival_dt[["d"]]
    )
  )
  lexis_merge(
    dt = work_dt,
    merge_dt = survival_dt,
    merge_dt_by = survival_dt_by,
    merge_dt_harmonisers = survival_dt_harmonisers
  )
  browser()
  data.table::setDT(work_dt)
  work_dt <- work_dt[
    i = work_dt[[names(breaks)[1]]] == breaks[[1]],
    j = .SD,
    .SDcols = intersect(
      names(work_dt),
      c(
        "lex.id",
        names(breaks),
        names(aggre_stratum_dt)
      )
    )
  ]
  data.table::set(
    x = work_dt,
    j = names(breaks)[2:3],
    value = list(
      breaks[[2]][cut(
        x = work_dt[[names(breaks)[2]]],
        breaks = breaks[[2]],
        right = FALSE,
        labels = FALSE
      ) + 1L],
      breaks[[3]][cut(
        x = work_dt[[names(breaks)[3]]],
        breaks = breaks[[3]],
        right = FALSE,
        labels = FALSE
      )]
    )
  )
  aggre_stratum_dt <- local({
    add_stratum_dt <- data.table::CJ(
      ts_cal = breaks[[1]],
      ts_fut = breaks[[2]][-1],
      ts_age = breaks[[3]][-length(breaks[[3]])]
    )
    data.table::setnames(
      x = add_stratum_dt,
      new = names(breaks)
    )
    if (is.null(aggre_stratum_dt)) {
      big_stratum_dt <- add_stratum_dt
    } else {
      big_stratum_dt <- aggre_stratum_dt[
        rep(seq_len(nrow(aggre_stratum_dt)), each = nrow(add_stratum_dt))
      ]
      data.table::set(
        x = big_stratum_dt,
        j = names(add_stratum_dt),
        value = add_stratum_dt[
          rep(seq_len(nrow(add_stratum_dt)), times = nrow(aggre_stratum_dt))
        ]
      )
    }
    data.table::setkeyv(big_stratum_dt, names(big_stratum_dt))
    big_stratum_dt
  })

  work_dt <- work_dt[
    #' @importFrom data.table .N .SD
    j = list(
      record_count = .N,
      subject_count = data.table::uniqueN(.SD[["lex.id"]])
    ),
    .SDcols = "lex.id",
    #' @importFrom data.table .EACHI
    keyby = names(aggre_stratum_dt),
    nomatch = 0L
  ]
  work_dt <- work_dt[
    i = aggre_stratum_dt,
    on = names(aggre_stratum_dt)
  ]
  data.table::set(
    x = work_dt,
    i = which(is.na(work_dt[["record_count"]])),
    j = c("record_count", "subject_count"),
    value = list(0L, 0L)
  )
  data.table::setorderv(
    x = work_dt,
    cols = c(
      setdiff(names(aggre_stratum_dt), names(breaks)[2]),
      names(breaks)[2]
    ),
    order = c(rep(1L, ncol(aggre_stratum_dt) - 1L), -1L)
  )
  work_dt[
    j = c("record_count", "subject_count") := lapply(.SD, cumsum),
    .SDcols = c("record_count", "subject_count"),
    keyby = setdiff(names(aggre_stratum_dt), names(breaks)[2])
  ]
  data.table::setkeyv(work_dt, names(aggre_stratum_dt))
  return(work_dt[])
}
