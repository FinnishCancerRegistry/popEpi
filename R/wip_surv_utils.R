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
      "lex.Cst",
      "lex.Xst",
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
    method %in% c("linear", "geometric_mean", "hazard"),
    ts_fut_stops[1] != 0.0,
    length(estimates) == length(ts_fut_stops),
    !duplicated(ts_fut_stops),
    isTRUE(all.equal(ts_fut_stops, sort(ts_fut_stops)))
  )
  # I added the extra interval ]-1.0, 0.0] to get an estimate for ts_fut = 0.0.
  interval_dt <- data.table::setDT(list(
    ts_fut_start = c(-1.0, 0.0, ts_fut_stops[-length(ts_fut_stops)]),
    ts_fut_stop = c(0.0, ts_fut_stops),
    est_start = c(1.0, 1.0, estimates[-length(estimates)]),
    est_stop = c(1.0, estimates)
  ))
  data.table::setkeyv(interval_dt, c("ts_fut_start", "ts_fut_stop"))
  out <- data.table::setDT(list(
    ts_fut = ts_fut_stop_value,
    interval = cut(
      ts_fut_stop_value,
      breaks = c(
        interval_dt[["ts_fut_start"]][1L],
        interval_dt[["ts_fut_stop"]]
      ),
      # labels = FALSE,
      right = TRUE
    )
  ))
  data.table::set(out, j = "interval_id", value = as.integer(out[["interval"]]))
  data.table::set(
    x = out,
    j = c("ts_fut_start", "ts_fut_stop", "est_start", "est_stop"),
    value = list(
      interval_dt[["ts_fut_start"]][out[["interval_id"]]],
      interval_dt[["ts_fut_stop"]][out[["interval_id"]]],
      interval_dt[["est_start"]][out[["interval_id"]]],
      interval_dt[["est_stop"]][out[["interval_id"]]]
    )
  )
  data.table::set(
    x = out,
    j = "interval_width",
    value = out[["ts_fut_stop"]] - out[["ts_fut_start"]]
  )
  data.table::set(
    x = out,
    # distance from the start of the interval to the interpolation point
    j = "delta",
    value = out[["ts_fut"]] - out[["ts_fut_start"]]
  )
  data.table::set(
    x = out,
    # distance from the start of the interval to the interpolation point
    j = "w",
    value = out[["delta"]] / out[["interval_width"]]
  )
  # we interpolate it.
  # these methods in fact produce exactly the same result --- the linear
  # interpolation.
  data.table::set(
    x = out,
    j = "est_cond",
    value = out[["est_stop"]] / out[["est_start"]]
  )
  data.table::set(
    x = out,
    j = "est_interpolated",
    value = switch(
      method,
      "linear" = (1 - out[["w"]]) *
        out[["est_start"]] +
        out[["w"]] * out[["est_stop"]],
      "geometric_mean" = out[["est_start"]] * out[["est_cond"]]^out[["w"]],
      "hazard" = out[["est_start"]] *
        exp(-(-log(out[["est_cond"]]) * out[["w"]]))
    )
  )
  if (any(out[["est_interpolated"]] < 0)) {
    browser()
  }
  # the result is interpolated unconditional survival at ts_fut_stop_value
  return(out[["est_interpolated"]])
}

surv_collapse_ts_1d_eval_test_expr__ <- function(
  test_expr,
  dt,
  subset_idx = NULL,
  call_env = NULL
) {
  if (is.null(call_env)) {
    call_env <- parent.frame(2L)
  }
  test_result <- tryCatch(
    eval(test_expr, dt, call_env),
    error = function(e) e
  )
  dt_expr <- substitute(dt[j = test_expr], list(test_expr = test_expr))
  if (!is.null(subset_idx)) {
    dt_expr[["i"]] <- subset_idx
  }
  eval_env <- new.env(parent = call_env)
  eval_env[["dt"]] <- dt
  test_result <- tryCatch(eval(dt_expr, eval_env), error = function(e) e)
  if (inherits(test_result, "error")) {
    stop(
      "Test expression ",
      deparse1(test_expr),
      " resulted in an error: ",
      test_expr[["message"]]
    )
  } else if (
    !storage.mode(test_result) %in% c("logical", "double", "integer")
  ) {
    stop(
      "Test expression ",
      deparse1(test_expr),
      " did not evaluate into ",
      "(?storage.mode) logical, numeric, nor integer. ",
      "Instead it evaluated to `storage.mode(result) = ",
      deparse1(storage.mode(test_result), "`")
    )
  } else if (storage.mode(test_result) %in% c("double", "integer")) {
    test_result <- test_result > 0
  }
  test_result[is.na(test_result)] <- FALSE
  return(any(test_result))
}
