lexis_dt_set__ <- function(lexis, lexis_ts_col_nms = NULL) {
  if (inherits(lexis, "Lexis")) {
    data.table::setDT(lexis)
    data.table::setattr(lexis, "class", c("Lexis", "data.table", "data.frame"))
    return(lexis[])
  }
  if (is.null(lexis_ts_col_nms)) {
    stop("Internal error: internal function lexis_dt_set__ ",
         "cannot automatically determine lexis_ts_col_nms. Please ",
         "complain to the maintainer if you see this.")
  }
  stopifnot(
    is.data.frame(lexis),
    lexis_ts_col_nms %in% names(lexis)
  )

  attrs <- list(
    time.scales = lexis_ts_col_nms,
    time.since = rep("", length(lexis_ts_col_nms)),
    breaks = structure(
      lapply(lexis_ts_col_nms, function(x) NULL),
      names = lexis_ts_col_nms
    ),
    class = c("Lexis", "data.table", "data.frame")
  )
  data.table::setDT(lexis)
  for (attr_nm in names(attrs)) {
    data.table::setattr(lexis, name = attr_nm, value = attrs[[attr_nm]])
  }
  return(invisible(lexis[]))
}

lexis_dt__ <- function(lexis, lexis_ts_col_nms = NULL) {
  if (inherits(lexis, "Lexis")) {
    out <- data.table::setDT(as.list(lexis))
    data.table::setattr(out, "class", c("Lexis", "data.table", "data.frame"))
    return(out[])
  }
  if (is.null(lexis_ts_col_nms)) {
    stop("Internal error: internal function lexis_dt__ ",
         "cannot automatically determine lexis_ts_col_nms. Please ",
         "complain to the maintainer if you see this.")
  }
  lexis_dt_set__(out, lexis_ts_col_nms = lexis_ts_col_nms)
  return(out)
}

surv_split__ <- function(
  lexis,
  breaks,
  subset = NULL,
  merge = TRUE
) {
  assert_is_arg_lexis(lexis, dt = FALSE)
  if (nrow(lexis) == 0) {
    return(lexis[])
  }
  lexis_ts_col_nms <- Epi::timeScales(lexis)
  lexis_col_nms <- c(
    "lex.id", lexis_ts_col_nms, "lex.dur", "lex.Cst", "lex.Xst"
  )
  if (isTRUE(merge)) {
    merge <- setdiff(names(lexis), lexis_col_nms)
  } else if (isFALSE(merge)) {
    merge <- character(0L)
  }
  out <- data.table::setDT(as.list(lexis)[union(lexis_col_nms, merge)])
  subset <- handle_arg_subset(dataset_nm = "lexis")
  if (any(!subset)) {
    out <- out[(subset), ]
  }
  lexis_dt_set__(lexis = out, lexis_ts_col_nms = lexis_ts_col_nms)
  out <- popEpi::splitMulti(
    data = out,
    breaks = breaks,
    drop = TRUE,
    merge = TRUE,
    verbose = FALSE
  )
  data.table::setkeyv(out, c("lex.id", lexis_ts_col_nms))
  return(out[])
}


surv_box_dt__ <- function(
  breaks
) {
  stopifnot(
    inherits(breaks, "list"),
    !is.null(names(breaks)),
    !duplicated(names(breaks))
  )
  split_ts_col_nms <- names(breaks)
  box_dt <- lapply(split_ts_col_nms, function(ts_col_nm) {
    seq_len(length(breaks[[ts_col_nm]]) - 1L)
  })
  box_dt <- do.call(data.table::CJ, box_dt, quote = TRUE)
  id_col_nms <- paste0(split_ts_col_nms, "_id")
  data.table::setnames(box_dt, id_col_nms)
  data.table::set(
    x = box_dt,
    j = "box_id",
    value = seq_len(nrow(box_dt))
  )
  start_col_nms <- paste0(split_ts_col_nms, "_start")
  data.table::set(
    x = box_dt,
    j = start_col_nms,
    value = lapply(split_ts_col_nms, function(ts_col_nm) {
      breaks[[ts_col_nm]][box_dt[[paste0(ts_col_nm, "_id")]]]
    })
  )
  stop_col_nms <- paste0(split_ts_col_nms, "_stop")
  data.table::set(
    x = box_dt,
    j = stop_col_nms,
    value = lapply(split_ts_col_nms, function(ts_col_nm) {
      breaks[[ts_col_nm]][box_dt[[paste0(ts_col_nm, "_id")]] + 1L]
    })
  )
  data.table::setcolorder(
    box_dt,
    c(
      "box_id", id_col_nms,
      paste0(
        rep(split_ts_col_nms, each = 2L),
        rep(c("_start", "_stop"), times = length(split_ts_col_nms))
      )
    )
  )
  data.table::setkeyv(box_dt, names(box_dt))
  return(box_dt[])
}

surv_box_id__ <- function(
  lexis,
  box_dt
) {
  assert_is_arg_lexis(lexis, dt = TRUE)
  start_col_nms <- sort(names(box_dt)[grepl("_start$", names(box_dt))])
  split_ts_col_nms <- sub("_start$", "", start_col_nms)
  merge_dt <- data.table::setDT(as.list(box_dt)[start_col_nms])
  data.table::setnames(merge_dt, split_ts_col_nms)
  data.table::set(
    x = merge_dt,
    j = "box_id",
    value = box_dt[["box_id"]]
  )
  breaks <- lapply(split_ts_col_nms, function(ts_col_nm) {
    union(
      box_dt[[paste0(ts_col_nm, "_start")]],
      box_dt[[paste0(ts_col_nm, "_stop")]][nrow(box_dt)]
    )
  })
  names(breaks) <- split_ts_col_nms
  lexis <- lexis_merge(
    lexis = lexis,
    merge_dt = merge_dt,
    merge_dt_by = split_ts_col_nms,
    merge_dt_harmonisers = structure(lapply(seq_along(breaks), function(i) {
      substitute({
        breaks <- BR
        idx <- cut( # nolint
          x = COL + 0.5 * lex.dur,
          breaks = breaks,
          right = FALSE,
          labels = FALSE
        )
        breaks[idx]
      }, list(COL = parse(text = names(breaks)[i])[[1]], BR = breaks[[i]]))
    }), names = names(breaks))
  )
  return(invisible(lexis[]))
}

lexis_crop <- function(lexis, breaks) {
  assert_is_arg_lexis(lexis, dt = FALSE)
  delay_entry <- do.call(pmax, lapply(names(breaks), function(ts_col_nm) {
    entry <- lexis[[ts_col_nm]]
    cropped_entry <- min(breaks[[ts_col_nm]])
    pmax(entry, cropped_entry) - entry
  }))
  earlify_exit <- do.call(pmax, lapply(names(breaks), function(ts_col_nm) {
    exit <- lexis[[ts_col_nm]] + lexis[["lex.dur"]]
    cropped_exit <- max(breaks[[ts_col_nm]])
    exit - pmin(exit, cropped_exit)
  }))
  data.table::set(
    x = lexis,
    j = Epi::timeScales(lexis),
    value = lapply(Epi::timeScales(lexis), function(ts_col_nm) {
      lexis[[ts_col_nm]] + delay_entry
    })
  )
  data.table::set(
    x = lexis,
    j = "lex.dur",
    value = lexis[["lex.dur"]] - delay_entry - earlify_exit
  )
  data.table::set(
    x = lexis,
    i = which(lexis[["lex.dur"]] < 0),
    j = c(Epi::timeScales(lexis), "lex.dur"),
    value = NA
  )
  return(invisible(lexis[]))
}

lexis_immortalise <- function(lexis, breaks) {
  assert_is_arg_lexis(lexis, dt = FALSE)
  max_by_ts <- lapply(breaks, max)
  pmin_data <- lapply(names(max_by_ts), function(ts_col_nm) {
    max_by_ts[[ts_col_nm]] - lexis[[ts_col_nm]]
  })
  names(pmin_data) <- names(max_by_ts)
  data.table::set(
    x = lexis,
    j = "lex.dur",
    value = do.call(pmin, pmin_data, quote = TRUE)
  )
  data.table::set(
    x = lexis,
    j = "lex.Xst",
    value = lexis[["lex.Cst"]]
  )
  return(invisible(lexis[]))
}

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
