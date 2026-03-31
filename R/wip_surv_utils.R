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

surv_collapse_1d__ <- function(
  dt,
  ts_col_nm,
  value_col_nms,
  test_expr = NULL,
  mandatory_breaks = NULL
) {
  stopifnot(
    data.table::is.data.table(dt),
    paste0(ts_col_nm, c("_start", "_stop")) %in% names(dt),
    !duplicated(dt[[paste0(ts_col_nm, "_start")]]),
    value_col_nms %in% names(dt),
    data.table::key(dt)[1] == "box_id",
    is.null(test_expr) || is.language(test_expr)
  )
  if (is.null(test_expr)) {
    test_col_nms <- intersect(
      c(
        "t_at_risk",
        names(dt)[grepl("^t_", names(dt))],
        "n_at_risk_eff",
        names(dt)[grepl("^n_", names(dt))]
      ),
      names(dt)
    )
    if (length(test_col_nms) == 0) {
      stop(
        "Could not automatically determine value column to test whether ",
        "intervals should be combined. If you see this, add `t_at_risk` to ",
        "the columns created by `lexis_split_merge_aggregate_by_stratum`."
      )
    }
    test_expr <- substitute(
      col > 0,
      list(col = str2lang(test_col_nms[1]))
    )
  }
  ts_start_col_nm <- paste0(ts_col_nm, "_start")
  ts_stop_col_nm <- paste0(ts_col_nm, "_stop")
  ts_breaks <- c(dt[[ts_start_col_nm]], dt[[ts_stop_col_nm]][nrow(dt)])
  test_result <- tryCatch(
    eval(test_expr, dt, parent.frame(1L)),
    error = function(e) e
  )
  if (inherits(test_result, "error")) {
    return(dt[])
  }
  if (
    !storage.mode(test_result) %in% c("logical", "double", "integer")
  ) {
    stop(
      "Test expression ", deparse1(test_expr), " did not evaluate into ",
      "(?storage.mode) logical, numeric, nor integer. ",
      "Instead it evaluated to `storage.mode(result) = ",
      deparse1(storage.mode(test_result), "`")
    )
  }
  if (storage.mode(test_result) %in% c("double", "integer")) {
    test_result <- test_result > 0
  }
  if (anyNA(test_result)) {
    browser()
  }
  test_result[is.na(test_result)] <- FALSE
  if (all(test_result)) {
    return(dt[])
  }
  out <- rep(NA_integer_, length(ts_breaks))
  out[1L] <- 1L
  out_pos <- 1L
  ts_fut_id_lo <- 1L
  ts_fut_id_hi <- 1L
  while (TRUE) {
    if (ts_fut_id_hi >= length(ts_breaks)) {
      break
    }
    break_lo <- ts_breaks[ts_fut_id_lo]
    break_hi <- ts_breaks[ts_fut_id_hi + 1L]
    can_combine_next <- ts_fut_id_hi + 1L <= length(ts_breaks) &&
      !break_hi %in% mandatory_breaks
    can_combine_previous <- ts_fut_id_lo > 1 &&
      !break_lo %in% mandatory_breaks
    can_combine <- can_combine_next || can_combine_previous
    do_combine <- can_combine &&
      !any(test_result[ts_fut_id_lo:ts_fut_id_hi])

    if (do_combine) {
      if (can_combine_next) {
        ts_fut_id_lo <- ts_fut_id_lo
        ts_fut_id_hi <- ts_fut_id_hi + 1L
      } else if (can_combine_previous) {
        ts_fut_id_lo <- out[out_pos - 1L]
        ts_fut_id_hi <- ts_fut_id_hi
      }
    } else {
      if (out_pos > 1 && ts_fut_id_lo == out[out_pos - 1L]) {
        # combined with previous interval
        # out_pos <- out_pos # no need to run this...
      } else {
        out_pos <- out_pos + 1L
      }
      out[out_pos - 1L] <- ts_fut_id_lo
      out[out_pos] <- ts_fut_id_hi
      ts_fut_id_lo <- ts_fut_id_hi <- ts_fut_id_hi + 1L
    }
  }
  new_ts_breaks <- ts_breaks[union(out[!is.na(out)], length(ts_breaks))]
  ts_id_col_nm <- paste0(ts_col_nm, "_id")
  data.table::set(
    x = dt,
    j = ts_id_col_nm,
    value = cut(
      x = dt[[ts_start_col_nm]],
      breaks = new_ts_breaks,
      right = FALSE,
      labels = FALSE
    )
  )
  ts_id_col_nms <- setdiff(names(dt)[grepl("_id$", names(dt))], "box_id")
  data.table::set(
    x = dt,
    j = c(ts_start_col_nm, ts_stop_col_nm, "box_id"),
    value = list(
      new_ts_breaks[-length(new_ts_breaks)][dt[[ts_id_col_nm]]],
      new_ts_breaks[-1][dt[[ts_id_col_nm]]],
      cumsum(!duplicated(dt, by = ts_id_col_nms))
    )
  )
  dt <- dt[
    #' @importFrom data.table .SD
    j = lapply(.SD, sum, na.rm = TRUE),
    .SDcols = value_col_nms,
    keyby = setdiff(names(dt), value_col_nms)
  ]
  return(dt[])
}
