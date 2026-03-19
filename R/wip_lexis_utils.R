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

lexis_attr_nms__ <- function() {
  c("time.scales", "time.since", "breaks")
}

lexis_to_lexis_dt__ <- function(
  lexis,
  subset = NULL,
  select = NULL
) {
  stopifnot(
    inherits(lexis, "Lexis"),

    inherits(subset, c("NULL", "logical"))
  )
  select <- handle_arg_select(select, lexis)
  out <- data.table::setDT(as.list(lexis)[select])
  if (is.logical(subset)) {
    subset[is.na(subset)] <- FALSE
    if (!all(subset)) {
      out <- out[(subset), ]
    }
  }
  attr_nms <- lexis_attr_nms__()
  attr_nms <- intersect(attr_nms, names(attributes(lexis)))
  attrs <- lapply(attr_nms, attr, x = lexis)
  names(attrs) <- attr_nms
  attrs[["class"]] <- c("Lexis", "data.table", "data.frame")
  for (attr_nm in names(attrs)) {
    data.table::setattr(out, attr_nm, attrs[[attr_nm]])
  }
  if (data.table::is.data.table(lexis)) {
    if (all(data.table::key(lexis) %in% names(out))) {
      data.table::setkeyv(out, data.table::key(lexis))
    }
  }
  return(out[])
}

lexis_dt__ <- function(lexis, lexis_ts_col_nms = NULL) {
  if (inherits(lexis, "Lexis")) {
    return(lexis_to_lexis_dt__(lexis)[])
  }
  if (is.null(lexis_ts_col_nms)) {
    stop("Internal error: internal function lexis_dt__ ",
         "cannot automatically determine lexis_ts_col_nms. Please ",
         "complain to the maintainer if you see this.")
  }
  out <- data.table::setDT(as.list(lexis))
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


lexis_box_dt__ <- function(
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
  box_dt <- call_with_arg_list__(data.table::CJ, box_dt)
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

lexis_box_id__ <- function(
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

lexis_delay_entry <- function(lexis, ts_col_new_entry, ts_col_nm) {
  assert_is_arg_lexis(lexis, dt = FALSE)
  stopifnot(
    ts_col_nm %in% names(lexis),
    ts_col_nm %in% Epi::timeScales(lexis),
    length(ts_col_new_entry) == 1,
    identical(class(ts_col_new_entry), class(lexis[[ts_col_nm]]))
  )
  cannot_be_delayed <- lexis[[ts_col_nm]] > ts_col_new_entry |
    (lexis[[ts_col_nm]] + lexis[["lex.dur"]] <= ts_col_new_entry)
  # e.g. from ts_cal = 2001.5 to ts_col_rebase = 2002.3,
  # move_entry = 2002.3 - 2001.5 = 0.8
  # e.g. from ts_cal = 2002.5 to ts_col_rebase = 2002.3,
  # move_entry = -0.2
  move_entry <- ts_col_new_entry - lexis[[ts_col_nm]]
  move_entry[cannot_be_delayed] <- NA
  # e.g. ts_cal = 2001.5 -> 2002.3
  data.table::set(
    x = lexis,
    j = Epi::timeScales(lexis),
    value = lapply(Epi::timeScales(lexis), function(ts_col_nm) {
      lexis[[ts_col_nm]] + move_entry
    })
  )
  # e.g. lex.dur = 1.5 - 0.8 = 0.7
  data.table::set(
    x = lexis,
    j = "lex.dur",
    value = lexis[["lex.dur"]] - move_entry
  )
  return(invisible(lexis))
}

lexis_crop <- function(lexis, breaks) {
  assert_is_arg_lexis(lexis, dt = FALSE)
  delay_entry <- call_with_arg_list__(
    pmax, 
    lapply(names(breaks), function(ts_col_nm) {
      entry <- lexis[[ts_col_nm]]
      cropped_entry <- min(breaks[[ts_col_nm]])
      pmax(entry, cropped_entry) - entry
    })
  )
  earlify_exit <- call_with_arg_list__(
    pmax,
    lapply(names(breaks), function(ts_col_nm) {
      exit <- lexis[[ts_col_nm]] + lexis[["lex.dur"]]
      cropped_exit <- max(breaks[[ts_col_nm]])
      exit - pmin(exit, cropped_exit)
    })
  )
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
  copied_statuses <- FALSE
  na_idx <- which(lexis[["lex.dur"]] < 0)
  if (length(na_idx) > 0) {
    # this takes copies of the two columns which we modify below. although
    # now lexis will be modified, if lexis itself is a shallow copy then the
    # original is not modified.
    copied_statuses <- TRUE
    data.table::set(
      x = lexis,
      j = c("lex.Cst", "lex.Xst"),
      value = list(lexis[["lex.Cst"]], lexis[["lex.Xst"]])
    )
    data.table::set(
      x = lexis,
      i = na_idx,
      j = c(Epi::timeScales(lexis), "lex.dur", "lex.Cst", "lex.Xst"),
      value = NA
    )
  }
  earlify_exit_idx <- which(earlify_exit > 0)
  if (length(earlify_exit_idx) > 0) {
    if (!copied_statuses) {
      data.table::set(
        x = lexis,
        j = c("lex.Cst", "lex.Xst"),
        value = list(lexis[["lex.Cst"]], lexis[["lex.Xst"]])
      )
    }
    data.table::set(
      x = lexis,
      i = earlify_exit_idx,
      j = "lex.Xst",
      value = lexis[["lex.Cst"]][earlify_exit_idx]
    )
  }
  return(invisible(lexis[]))
}

lexis_immortalise <- function(lexis, breaks = NULL, crop = TRUE) {
  assert_is_arg_lexis(lexis, dt = FALSE)
  if (is.null(breaks)) {
    if (identical(class(lexis[["lex.dur"]]), "numeric")) {
      immortalise_to <- Inf
    } else {
      immortalise_to <- methods::as(
        .Machine$integer.max,
        class(lexis[["lex.dur"]])[1]
      )
    }
  } else {
    if (crop) {
      lexis_crop(lexis, breaks = breaks)
    }
    max_by_ts <- lapply(breaks, max)
    pmin_data <- lapply(names(max_by_ts), function(ts_col_nm) {
      max_by_ts[[ts_col_nm]] - lexis[[ts_col_nm]]
    })
    names(pmin_data) <- names(max_by_ts)
    immortalise_to <- call_with_arg_list__(pmin, pmin_data)
  }
  data.table::set(
    x = lexis,
    j = c("lex.dur", "lex.Cst"),
    value = list(
      immortalise_to,
      lexis[["lex.Cst"]]
    )
  )
  return(invisible(lexis[]))
}

lexis_subset <- function(
  lexis,
  breaks
) {
  assert_is_arg_lexis(lexis, dt = FALSE)
  assert_is_arg_breaks(breaks, lexis)

  tmp_dt <- data.table::setDT(list(keep = rep(TRUE, nrow(lexis))))
  tmp_dt_idx <- seq_len(nrow(tmp_dt))
  for (ts_col_nm in names(breaks)) {
    data.table::set(
      x = tmp_dt,
      i = tmp_dt_idx, # no copy
      j = "keep",
      value = tmp_dt[["keep"]] &
        (lexis[[ts_col_nm]] + lexis[["lex.dur"]]) >= min(breaks[[ts_col_nm]])
    )
    data.table::set(
      x = tmp_dt,
      i = tmp_dt_idx, # no copy
      j = "keep",
      value = tmp_dt[["keep"]] &
        lexis[[ts_col_nm]] < max(breaks[[ts_col_nm]])
    )
  }
  return(tmp_dt[["keep"]])
}

lexis_drop <- function(
  lexis,
  breaks,
  subset = NULL,
  select = NULL
) {
  assert_is_arg_lexis(lexis, dt = FALSE)
  assert_is_arg_breaks(breaks, lexis)
  subset <- handle_arg_subset(dataset_nm = "lexis", output_type = "logical")
  select <- handle_arg_select(select, lexis)

  subset <- subset & lexis_subset(lexis = lexis, breaks = breaks)
  if (data.table::is.data.table(lexis)) {
    out <- lexis[
      i = (subset),
      #' @importFrom data.table .SD
      j = .SD,
      .SDcols = select
    ]
  } else {
    out <- lexis[subset, select, drop = FALSE]
  }
  return(out[])
}
