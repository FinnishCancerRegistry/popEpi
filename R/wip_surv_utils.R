
surv_breaks_rule_based <- function(
  lexis,
  ts_fut_nm = "ts_fut",
  breaks = NULL,
  mandatory_breaks = 0:5,
  combination_test_expr = quote(sum(lex.dur) == 0)
) {
  call_env <- parent.frame(1L)
  if (is.null(breaks)) {
    breaks <- lexis[
      i = lexis[["lex.Cst"]] != lexis[["lex.Xst"]],
      j = .SD[[ts_fut_nm]] + .SD[["lex.dur"]],
      .SDcols = c(ts_fut_nm, "lex.dur")
    ]
    breaks <- sort(unique(round(breaks, 10)))
    breaks <- union(0L, breaks)
    storage.mode(breaks) <- storage.mode(lexis[["lex.dur"]])
  }
  out <- rep(NA_integer_, length(breaks))
  out[1L] <- 1L
  out_pos <- 1L
  break_lo_pos <- 1L
  break_hi_pos <- 2L
  while (TRUE) {
    if (break_hi_pos > length(breaks)) {
      break
    }
    break_lo <- breaks[break_lo_pos]
    break_hi <- breaks[break_hi_pos]
    can_combine_next <- break_hi_pos + 1L <= length(breaks) &&
      !break_hi %in% mandatory_breaks
    can_combine_previous <- break_lo_pos > 1 &&
      !break_lo %in% mandatory_breaks
    can_combine <- can_combine_next || can_combine_previous
    do_combine <- can_combine && local({
      work_dt <- surv_interval(
        lexis = lexis,
        break_lo = break_lo,
        break_hi = break_hi,
        ts_col_nm = ts_fut_nm,
        merge = FALSE
      )
      eval(combination_test_expr, envir = work_dt, enclos = call_env)
    })

    if (do_combine) {
      if (can_combine_next) {
        break_lo_pos <- break_lo_pos
        break_hi_pos <- break_hi_pos + 1L
      } else if (can_combine_previous) {
        break_lo_pos <- out[out_pos - 1L]
        break_hi_pos <- break_hi_pos
      }
    } else {
      if (out_pos > 1 && break_lo_pos == out[out_pos - 1L]) {
        # combined with previous interval
        # out_pos <- out_pos # no need to run this...
      } else {
        out_pos <- out_pos + 1L
      }
      out[out_pos - 1L] <- break_lo_pos
      out[out_pos] <- break_hi_pos
      break_lo_pos <- break_hi_pos
      break_hi_pos <- break_lo_pos + 1L
    }
  }
  out <- breaks[union(out[!is.na(out)], length(breaks))]
  return(out)
}


surv_interval <- function(
  lexis,
  break_lo,
  break_hi,
  ts_col_nm,
  merge = FALSE
) {
  assert_is_arg_lexis(lexis, dt = FALSE)
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
    lapply(setdiff(attr(lexis, "time.scales"), ts_col_nm), function(ts_col_nm_) {
      data.table::set(
        x = ts_dt,
        j = ts_col_nm_,
        value = lexis[[ts_col_nm_]] + offset
      )
    })
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
