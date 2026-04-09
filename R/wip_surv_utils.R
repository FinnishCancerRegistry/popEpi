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

surv_collapse_1d_eval_test_expr__ <- function(
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
      "Test expression ", deparse1(test_expr), " resulted in an error: ",
      test_expr[["message"]]
    )
  } else if (
    !storage.mode(test_result) %in% c("logical", "double", "integer")
  ) {
    stop(
      "Test expression ", deparse1(test_expr), " did not evaluate into ",
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
      sum(col),
      list(col = str2lang(test_col_nms[1]))
    )
  }
  ts_start_col_nm <- paste0(ts_col_nm, "_start")
  ts_stop_col_nm <- paste0(ts_col_nm, "_stop")

  br_dt <- data.table::setDT(list(
    br = c(dt[[ts_start_col_nm]], dt[[ts_stop_col_nm]][nrow(dt)])
  ))
  data.table::set(
    x = br_dt,
    j = "in_mandatory_breaks",
    value =  br_dt[["br"]] %in% mandatory_breaks
  )
  collapsed_grp_ids <- rep(NA_integer_, nrow(dt))
  collapsed_grp_ids[1L] <- collapsed_grp_id <- 1L
  interval_id_lo <- 1L
  interval_id_hi <- 1L
  while (TRUE) {
    need_to_combine <- !surv_collapse_1d_eval_test_expr__(
      test_expr = test_expr,
      dt = dt,
      subset_idx = substitute(
        interval_id_lo:interval_id_hi,
        list(interval_id_lo = interval_id_lo, interval_id_hi = interval_id_hi)
      ),
      call_env = parent.frame(1L)
    )
    if (need_to_combine) {
      can_combine_with_next <- interval_id_hi < nrow(dt) &&
        !br_dt[["in_mandatory_breaks"]][interval_id_lo]
      can_combine_with_previous <- interval_id_lo > 1 &&
        !br_dt[["in_mandatory_breaks"]][interval_id_hi + 1L]
      can_combine <- can_combine_with_next || can_combine_with_previous
      # can combine and need to combine
      do_combine <- can_combine
    } else {
      do_combine <- FALSE
    }
    if (do_combine) {
      # lets collapse with next or previous and try again
      if (can_combine_with_next) {
        interval_id_lo <- interval_id_lo
        interval_id_hi <- interval_id_hi + 1L
      } else {
        interval_id_lo <- which(collapsed_grp_ids == collapsed_grp_id - 1L)[1L]
        interval_id_hi <- interval_id_hi
      }
    } else {
      # done with this (potentially collapsed) group
      collapsed_grp_ids[interval_id_lo:interval_id_hi] <- collapsed_grp_id
      collapsed_grp_id <- collapsed_grp_id + 1L
      interval_id_lo <- interval_id_hi <- interval_id_hi + 1L
    }
    if (interval_id_hi > nrow(dt)) {
      collapsed_grp_ids[is.na(collapsed_grp_ids)] <- collapsed_grp_id
      break
    }
  }
  collapsed_grp_ids <- cumsum(!duplicated(collapsed_grp_ids))
  ts_id_col_nm <- paste0(ts_col_nm, "_id")
  dt <- data.table::setDT(as.list(dt))
  data.table::set(
    x = dt,
    j = ts_id_col_nm,
    value = collapsed_grp_ids
  )
  ts_id_col_nms <- setdiff(names(dt)[grepl("_id$", names(dt))], "box_id")
  data.table::set(
    x = dt,
    j = "box_id",
    value = cumsum(!duplicated(dt, by = ts_id_col_nms))
  )
  data.table::set(
    x = dt,
    j = c(ts_start_col_nm, ts_stop_col_nm),
    value = lapply(c(ts_start_col_nm, ts_stop_col_nm), function(col_nm) {
      data.table::copy(dt[[col_nm]])
    })
  )
  dt[
    j = c(ts_start_col_nm, ts_stop_col_nm) := list(
      .SD[[ts_start_col_nm]][1L],
      .SD[[ts_stop_col_nm]][.N]
    ),
    by = ts_id_col_nm
  ]
  dt <- dt[
    #' @importFrom data.table .SD
    j = lapply(.SD, sum, na.rm = TRUE),
    .SDcols = value_col_nms,
    keyby = setdiff(names(dt), value_col_nms)
  ]
  return(dt[])
}
