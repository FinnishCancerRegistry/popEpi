surv_interval <- function(
  lexis,
  break_lo,
  break_hi,
  ts_col_nm,
  merge = FALSE
) {
  assert_is_arg_lexis(lexis, dt = FALSE)
  stopifnot(
    ts_col_nm %in% Epi::timeScales(lexis),
    identical(storage.mode(break_lo), storage.mode(lexis[[ts_col_nm]])),
    identical(storage.mode(break_hi), storage.mode(lexis[[ts_col_nm]]))
  )
  merge <- handle_arg_merge(merge, lexis)
  lexis_col_nms <- c(
    "lex.id",
    "lex.dur",
    "lex.Cst",
    "lex.Xst"
  )
  work_dt <- data.table::setDT(as.list(lexis)[lexis_col_nms])
  ts_dt <- data.table::setDT(as.list(lexis)[attr(lexis, "time.scales")])
  data.table::set(
    x = ts_dt,
    j = ts_col_nm,
    value = pmax(lexis[[ts_col_nm]], break_lo)
  )
  ts_stop_col_nm <- paste0(ts_col_nm, "_stop")
  data.table::set(
    x = work_dt,
    j = ts_stop_col_nm,
    value = lexis[[ts_col_nm]] + lexis[["lex.dur"]]
  )
  data.table::set(
    x = work_dt,
    j = "in_interval",
    value = work_dt[[ts_stop_col_nm]] >= break_lo &
      lexis[[ts_col_nm]] < break_hi
  )
  data.table::set(
    x = work_dt,
    j = "end_in_interval",
    value = data.table::fifelse(
      work_dt[["in_interval"]],
      work_dt[[ts_stop_col_nm]] <= break_hi,
      FALSE
    )
  )
  data.table::set(
    x = work_dt,
    j = ts_stop_col_nm,
    value = pmin(work_dt[[ts_stop_col_nm]], break_hi)
  )
  data.table::set(
    x = work_dt,
    j = "lex.dur",
    value = work_dt[[ts_stop_col_nm]] - ts_dt[[ts_col_nm]]
  )
  local({
    offset <- ts_dt[[ts_col_nm]] - lexis[[ts_col_nm]]
    lapply(
      setdiff(attr(lexis, "time.scales"), ts_col_nm),
      function(ts_col_nm_) {
        data.table::set(
          x = ts_dt,
          j = ts_col_nm_,
          value = lexis[[ts_col_nm_]] + offset
        )
      }
    )
    NULL
  })
  data.table::set(
    x = work_dt,
    j = "lex.Xst",
    value = data.table::fifelse(
      work_dt[["end_in_interval"]],
      lexis[["lex.Xst"]],
      lexis[["lex.Cst"]]
    )
  )

  data.table::set(
    x = work_dt,
    j = names(ts_dt),
    value = ts_dt
  )
  if (length(merge) > 0) {
    data.table::set(
      x = work_dt,
      j = merge,
      value = as.list(lexis)[merge]
    )
  }
  work_dt <- subset(
    work_dt,
    subset = work_dt[["in_interval"]],
    select = c(
      "lex.id",
      attr(lexis, "time.scales"),
      "lex.dur",
      "lex.Cst", "lex.Xst",
      merge
    )
  )
  return(work_dt[])
}

surv_interpolate <- function(
  estimates,
  ts_fut_stops,
  ts_fut_stop_value,
  estimate_start_value = 1.0,
  method = c("linear", "geometric_mean", "hazard")[1]
) {
  stopifnot(
    estimate_start_value %in% 0:1,
    method %in% c("linear", "geometric_mean", "hazard")
  )
  ts_fut_breaks <- c(0.0, ts_fut_stops)
  interval_idx <- cut(
    ts_fut_stop_value,
    breaks = ts_fut_breaks,
    labels = FALSE,
    right = FALSE
  )
  interval_start <- ts_fut_breaks[interval_idx]
  interval_stop <- ts_fut_breaks[interval_idx + 1L]
  estimates <- c(estimate_start_value, estimates)
  interval_start_estimate <- estimates[interval_idx]
  interval_stop_estimate <- estimates[interval_idx + 1L]
  # first we take conditional survival
  conditional_stop_estimate <- interval_stop_estimate / interval_start_estimate
  interval_width <- interval_stop - interval_start
  ts_fut_stop_value_in_interval <- (ts_fut_stop_value - interval_start)
  w <- ts_fut_stop_value_in_interval / interval_width
  # we interpolate it.
  # these methods in fact produce exactly the same result --- the linear
  # interpolation.
  out <- switch(
    method,
    "linear" = (1 - w) * estimate_start_value + 
      w * conditional_stop_estimate,
    "geometric_mean" = conditional_stop_estimate ^ w,
    "hazard" = exp(-(
      -log(conditional_stop_estimate) * w
    ))
  )
  # then we un-conditionalise it
  out <- out * interval_start_estimate
  # the result is interpolated unconditional survival at ts_fut_stop_value
  return(out)
}
