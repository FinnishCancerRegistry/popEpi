lexis_set__ <- function(dt, ts_col_nms) {
  attr_nms <- c("time.scales", "time.since", "breaks")
  if (inherits(dt, "Lexis")) {
    attrs <- lapply(attr_nms, attr, x = dt)
  } else {
    attrs <- list(
      ts_col_nms,
      rep("", length(ts_col_nms)),
      structure(lapply(ts_col_nms, function(x) NULL), names = ts_col_nms)
    )
  }
  names(attrs) <- attr_nms
  data.table::setDT(dt)
  attrs <- c(attrs, list(class = c("Lexis", "data.table", "data.frame")))
  for (attr_nm in names(attrs)) {
    data.table::setattr(dt, name = attr_nm, value = attrs[[attr_nm]])
  }
  return(invisible(dt[]))
}

surv_split <- function(
  dt,
  breaks,
  ts_col_nms,
  merge = TRUE
) {
  if (nrow(dt) == 0) {
    return(dt[])
  }
  lexis_col_nms <- c("lex.id", ts_col_nms, "lex.dur", "lex.Cst", "lex.Xst")
  if (isTRUE(merge)) {
    merge <- setdiff(names(dt), lexis_col_nms)
  } else if (isFALSE(merge)) {
    merge <- character(0L)
  }
  out <- data.table::setDT(as.list(dt)[union(lexis_col_nms, merge)])
  lexis_set__(out, ts_col_nms = ts_col_nms)
  out <- popEpi::splitMulti(
    data = out,
    breaks = breaks,
    drop = TRUE,
    merge = TRUE,
    verbose = FALSE
  )
  data.table::setDT(out)
  dbc::assert_prod_output_is_data_table_with_required_names(
    x = out,
    required_names = c(lexis_col_nms, merge)
  )
  data.table::setkeyv(out, c("lex.id", ts_col_nms))
  return(out[])
}

surv_merge <- function(
  dt,
  merge_dt,
  merge_dt_by,
  merge_dt_harmonisers = NULL,
  ts_col_nms
) {
  calling_env <- parent.frame(1L)
  merge_ts_col_nms <- intersect(ts_col_nms, merge_dt_by)
  if (is.null(merge_dt_harmonisers)) {
    merge_dt_harmonisers <- lapply(merge_ts_col_nms, function(col_nm) {
      cut_breaks <- sort(unique(merge_dt[[col_nm]]))
      if (is.integer(cut_breaks) || is.double(cut_breaks)) {
        if (all(diff(cut_breaks) == 1)) {
          cut_breaks <- c(cut_breaks, cut_breaks[length(cut_breaks)] + 1L)
        } else {
          cut_breaks <- c(cut_breaks, cut_breaks[length(cut_breaks)] + 1e6L)
        }
      } else {
        stop(
          "Cannot automatically determine `merge_dt_harmonisers$", col_nm, "`;",
          "Please supply argument `merge_dt_harmonisers` yourself."
        )
      }
      substitute(
        {
          breaks <- CUT_BREAKS
          idx <- cut(
            x = COL + lex.dur / 2,
            breaks = breaks,
            right = FALSE, labels = FALSE
          )
          breaks[idx]
        },
        list(COL = parse(text = col_nm)[[1]], CUT_BREAKS = cut_breaks)
      )
    })
    names(merge_dt_harmonisers) <- merge_ts_col_nms
  }
  join_dt <- data.table::setDT(lapply(merge_dt_by, function(col_nm) {
    if (col_nm %in% names(merge_dt_harmonisers)) {
      eval(
        merge_dt_harmonisers[[col_nm]],
        envir = dt,
        enclos = calling_env
      )
    } else {
      dt[[col_nm]]
    }
  }))
  data.table::setnames(join_dt, merge_dt_by)
  merge_value_col_nms <- setdiff(names(merge_dt), merge_dt_by)
  data.table::set(
    x = dt,
    j = merge_value_col_nms,
    value = merge_dt[
      i = join_dt,
      on = names(join_dt),
      #' @importFrom data.table .SD
      j = .SD,
      .SDcols = merge_value_col_nms
    ]
  )
  for (merge_value_col_nm in merge_value_col_nms) {
    is_missing <- is.na(dt[[merge_value_col_nm]])
    if (any(is_missing)) {
      print(dt[is_missing, ])
      browser()
      stop("Merging `merge_dt` into split (subset of) `dt` produced NA values ",
           "in column `dt$", merge_value_col_nm, "`. ",
           "See the table printed above.")
    }
  }
  return(invisible(dt[]))
}

surv_aggregate_one_stratum__ <- function(
  sub_dt,
  interval_dt,
  aggre_values_expr,
  enclos_env
) {
  expr_obj_nms <- all.vars(aggre_values_expr)
  ts_col_nms <- attr(sub_dt, "time.scales")
  lapply(ts_col_nms, function(ts_col_nm) {
    add_col_nms <- unique(expr_obj_nms[
      grepl(sprintf("^%s_((lead)|(lag))[0-9]+$", ts_col_nm), expr_obj_nms)
    ])
    lapply(add_col_nms, function(add_col_nm) {
      settings <- list(type = "lead")
      if (grepl("lag", add_col_nm)) {
        settings[["type"]] <- "lag"
      }
      settings[["n"]] <- as.integer(sub("^[^0-9]+", "", add_col_nm))
      sub_dt[
        #' @importFrom data.table := .SD
        j = (add_col_nm) := .SD[[ts_col_nm]] - data.table::shift(
          x = .SD[[ts_col_nm]],
          n = settings[["n"]],
          type = settings[["type"]],
          fill = NA_integer_
        ),
        .SDcols = ts_col_nm,
        by = "lex.id"
      ]
    })
  })
  fut_ts_col_nm <- intersect(ts_col_nms, names(interval_dt))
  data.table::set(
    x = sub_dt,
    j = "interval_id",
    value = cut(
      x = sub_dt[[fut_ts_col_nm]],
      breaks = c(
        interval_dt[[fut_ts_col_nm]],
        interval_dt[[fut_ts_col_nm]][[nrow(interval_dt)]] +
          interval_dt[["interval_width"]][[nrow(interval_dt)]]
      ),
      right = FALSE,
      labels = FALSE
    )
  )
  agg_expr <- substitute(
    sub_dt[
      j = EXPR,
      keyby = "interval_id"
    ],
    list(
      EXPR = aggre_values_expr
    )
  )
  eval_env <- new.env(parent = enclos_env)
  eval_env[["sub_dt"]] <- sub_dt
  eval_env[["interval_dt"]] <- interval_dt
  out <- eval(agg_expr, envir = eval_env)
  out <- out[
    i = interval_dt,
    on = "interval_id"
  ]
  data.table::setcolorder(
    out,
    c(names(interval_dt), setdiff(names(out), names(interval_dt)))
  )
  data.table::setkeyv(out, setdiff(names(interval_dt), "interval_width"))
  if (nrow(sub_dt) == 0) {
    data.table::set(
      x = out,
      j = setdiff(names(out), names(interval_dt)),
      value = NA
    )
  }
  return(out[])
}

breaks_list_to_interval_dt <- function(breaks, ts_nms = NULL) {
  if (is.null(ts_nms)) {
    ts_nms <- names(breaks)
  }
  breaks <- lapply(breaks, function(x) sort(unique(x)))
  interval_dt <- do.call(
    data.table::CJ,
    lapply(breaks[ts_nms], function(x) {
      x[-length(x)]
    })
  )
  data.table::set(
    interval_dt,
    j = c("interval_id", "interval_width"),
    value = list(
      seq_len(nrow(interval_dt)),
      do.call(pmin, do.call(
        data.table::CJ,
        lapply(breaks[ts_nms], function(x) {
          x[-1]
        })
      ) - interval_dt)
    )
  )
  data.table::setcolorder(
    interval_dt, c("interval_id", "interval_width", ts_nms)
  )
  data.table::setkeyv(interval_dt, c("interval_id", ts_nms))

  return(interval_dt[])
}

surv_split_merge_aggregate <- function(
  dt,
  breaks = list(ts_cal = 2000:2025, ts_age = 0:100, ts_fot = seq(0, 5, 1 / 12)),
  merge_dt = data.table::data.table(
    sex = 0L,
    ts_cal = 2000L,
    ts_age = 0L,
    haz = 0.010
  ),
  merge_dt_by = c("sex", "ts_cal", "ts_age"),
  merge_dt_harmonisers = NULL,
  aggre_stratum_dt = data.table::CJ(sex = 0:1, study_arm = 0:1),
  aggre_ts_col_nms = "ts_fot",
  aggre_values = quote(list(
    total_subject_time = sum(lex.dur),
    n_events = sum(lex.Xst != 0)
  ))
) {
  aggre_values_expr <- substitute(aggre_values)
  if (identical(aggre_values_expr[[1]], quote(quote))) {
    aggre_values_expr <- eval(aggre_values_expr)
  }
  ts_col_nms <- attr(dt, "time.scales")

  interval_dt <- breaks_list_to_interval_dt(breaks, aggre_ts_col_nms)
  surv_split_merge_aggregate_env <- environment()
  out <- dt[
    i = aggre_stratum_dt,
    on = names(aggre_stratum_dt),
    j = {
      if (.N == 0) {
        # for some reason .SD is a one-row data.table with all NA values
        sub_dt <- dt[0L, ]
      } else {
        sub_dt <- .SD
      }
      sub_dt <- surv_split(
        dt = sub_dt,
        breaks = breaks,
        ts_col_nms = ts_col_nms,
        merge = TRUE
      )
      surv_merge(
        sub_dt,
        merge_dt = merge_dt,
        merge_dt_by = merge_dt_by,
        ts_col_nms = ts_col_nms
      )
      out <- surv_aggregate_one_stratum__(
        sub_dt = sub_dt,
        interval_dt = interval_dt,
        aggre_values_expr = aggre_values_expr,
        enclos_env = surv_split_merge_aggregate_env
      )
      out
    },
    .SDcols = intersect(
      names(dt),
      c(
        names(breaks),
        merge_dt_by,
        all.vars(expr = aggre_values_expr),
        c("lex.id", "lex.dur", "lex.Cst", "lex.Xst")
      )
    ),
    #' @importFrom data.table .EACHI
    keyby = .EACHI
  ]
  # to clear Epi attributes
  out <- as.list(out)
  attributes(out) <- attributes(out)["names"]
  data.table::setDT(out)
  data.table::setkeyv(out, c(names(aggre_stratum_dt), "interval_id"))
  return(out[])
}

surv_split_merge_aggregate_by_interval <- function(
  dt,
  breaks = list(cal = 2000:2025, age = 0:100, fot = seq(0, 5, 1 / 12)),
  merge_dt_by = c("sex", "cal", "age"),
  merge_dt = data.table::data.table(
    sex = 0L,
    cal = 2000,
    age = 0,
    haz = 0.010
  ),
  aggre_stratum_dt = data.table::CJ(sex = 0:1, study_arm = 0:1),
  aggre_ts_col_nms = "fot",
  aggre_values = quote(list(
    total_subject_time = sum(duration),
    n = .N,
    n_events = sum(status == "dead"),
    w = sum(brenner_weight),
    c = sum(status == "lost to follow-up")
  )),
  n_processing_strata = data.table::getDTthreads() * 2L
) {
  aggre_values_expr <- substitute(aggre_values)
  if (identical(aggre_values_expr[[1]], quote(quote))) {
    aggre_values_expr <- eval(aggre_values_expr)
  }
  ts_col_nms <- attr(dt, "time.scales")

  breaks <- lapply(breaks, function(x) sort(unique(x)))
  interval_dt <- local({
    interval_breaks <- breaks[aggre_ts_col_nms]
    interval_dt_by_ts <- lapply(names(interval_breaks), function(ts_col_nm) {
      x <- interval_breaks[[ts_col_nm]]
      out <- data.table::data.table(
        lo = x[-length(x)],
        hi = x[-1L]
      )
      data.table::setnames(out, paste0(ts_col_nm, c("_lo", "_hi")))
      return(out[])
    })
    names(interval_dt_by_ts) <- names(interval_breaks)
    out <- do.call(data.table::CJ, lapply(interval_dt_by_ts, function(idt) {
      seq_len(nrow(idt))
    }))
    lapply(names(interval_dt_by_ts), function(ts_col_nm) {
      idt <- interval_dt_by_ts[[ts_col_nm]]
      indices <- out[[ts_col_nm]]
      data.table::set(
        out,
        j = names(idt),
        value = idt[indices, ]
      )
      data.table::set(
        out,
        j = ts_col_nm,
        value = NULL
      )
      NULL
    })
    out[]
  })

  # out <- local({
  #   out <- data.table::CJ(
  #     aggre_stratum_dt_row_no = seq_len(nrow(aggre_stratum_dt)),
  #     interval_dt_row_no = seq_len(nrow(interval_dt))
  #   )
  #   data.table::set(
  #     out,
  #     j = names(aggre_stratum_dt),
  #     value = aggre_stratum_dt[out[["aggre_stratum_dt_row_no"]]]
  #   )
  #   data.table::set(
  #     out,
  #     j = names(interval_dt),
  #     value = interval_dt[out[["interval_dt_row_no"]]]
  #   )
  #   data.table::set(
  #     out,
  #     j = c("aggre_stratum_dt_row_no", "interval_dt_row_no"),
  #     value = NULL
  #   )
  #   out[]
  # })
  processing_stratum_col_nm <- ".______PROCESSING_STRATUM_______."
  dt <- local({
    dt <- data.table::setDT(as.list(dt))
    data.table::set(
      dt,
      j = processing_stratum_col_nm,
      value = rep(seq_len(n_processing_strata), length.out = nrow(dt))
    )
    dt[]
  })
  out <- dt[
    j = {
      invisible(lapply(seq_len(nrow(interval_dt)), function(i) {
        sub_breaks <- lapply(names(breaks), function(ts_col_nm) {
          c(
            interval_dt[[paste0(ts_col_nm, "_lo")]][i],
            interval_dt[[paste0(ts_col_nm, "_hi")]][i]
          )
        })
        names(sub_breaks) <- names(breaks)
        sub_dt <- surv_split(
          dt = .SD,
          breaks = sub_breaks
        )
        if (nrow(sub_dt) == 0) {
          return(NULL)
        }
        surv_merge(
          sub_dt,
          merge_dt = merge_dt,
          merge_dt_by = merge_dt_by,
          ts_col_nms = ts_col_nms
        )
        agg_expr <- quote(sub_dt[
          j = "placeholder",
          keyby = eval(names(aggre_stratum_dt))
        ])
        agg_expr[["j"]] <- aggre_values_expr
        stratum_interval_value_dt <- eval(agg_expr)
        data.table::set(
          stratum_interval_value_dt,
          j = names(interval_dt),
          value = interval_dt[i, ]
        )
        join_col_nms <- c(names(aggre_stratum_dt), names(interval_dt))
        value_col_nms <- setdiff(names(stratum_interval_value_dt), join_col_nms)
        join_expr <- quote(out[
          i = stratum_interval_value_dt,
          on = join_col_nms,
          j = "placeholder"
        ])
        j_expr <- sprintf(
          "%s := list(%s)",
          deparse1(value_col_nms),
          paste0(
            value_col_nms, " + i.", value_col_nms
          )
        )
        join_expr[["j"]] <- parse(text = j_expr)[[1]]
        NULL
      }))
      0L
    },
    .SDcols = intersect(
      names(dt),
      c(
        names(aggre_stratum_dt),
        merge_dt_by,
        names(breaks),
        all.vars(expr = aggre_values_expr),
        c("lex.id", "lex.dur", "lex.Cst", "lex.Xst")
      )
    ),
    keyby = processing_stratum_col_nm
  ]

  return(out[])
}

surv_pohar_perme_weight__ <- function(
  dt,
  breaks,
  ts_col_nm,
  hazard_col_nm,
  method = c("survival interval", "subject subinterval")[1L]
) {
  work_dt <- data.table::setDT(list(
    lex.id = dt[["lex.id"]],
    lex.dur = dt[["lex.dur"]],
    "expected_hazard" = dt[[hazard_col_nm]]
  ))
  if (method == "subject subinterval") {
    data.table::set(
      x = work_dt,
      j = "H*(t_{i,j,k})",
      value = work_dt[["expected_hazard"]] * work_dt[["lex.dur"]]
    )
    work_dt[
      #' @importFrom data.table := .SD
      j = "H*(t_{i,j,k})" := lapply(.SD, cumsum),
      .SDcols = "H*(t_{i,j,k})",
      by = "lex.id"
    ]
    data.table::set(
      x = work_dt,
      j = "pp_weight",
      value = {
        # @codedoc_comment_block basicepistats:::surv_pohar_perme_weight__
        # The Pohar Perme weight is always the inverse of the expected survival
        # probability. The dataset these weights are computed in is allowd to
        # contain multiple subinterval within the survival interval for a given
        # subject. This can occur if e.g. the data are split both into survival
        # intervals and also by calendar year. Two methods are supported for
        # computing the weights:
        #
        # - `method = "subject subinterval"`: We compute the expected survival
        #   probability to the mid-point of each subinterval by subject.
        #   E.g. if the survival interval is
        #   `[0, 1[` and one subject has rows with `t_start = c(0.10, 0.20)`
        #   and `t_stop = c(0.20, 0.30)`, then we integrate expected survival
        #   probabilities from `t = 0.10` to the mid points `t = c(0.15, 0.25)`.
        # @codedoc_comment_block basicepistats:::surv_pohar_perme_weight__
        # note: interval `i` has breaks `[t_{i-1}, t_i[`.
        # hence e.g. `h*_{i,j,k}` is the expected hazard in interval `i` between
        # `[t_{i-1}, t_i[`.
        `H*(t_{i,j,k})` <- work_dt[["H*(t_{i,j,k})"]]
        `h*_{i,j,k}` <- work_dt[["expected_hazard"]]
        `u_{i,j,k}` <- work_dt[["lex.dur"]] / 2
        # note: m_{i,j,k} = t_{i,j,k} - u_{i,j,k}
        `H*(m_{i,j,k})` <- `H*(t_{i,j,k})` - `h*_{i,j,k}` * `u_{i,j,k}`
        # note: `w_{i,j,k}` = 1 / exp(-`H*(m_{i,j,k})`) = exp(`H*(m_{i,j,k})`)
        `w_{i,j,k}` <- exp(`H*(m_{i,j,k})`)
        `w_{i,j,k}`
      }
    )
  } else if (method == "survival interval") {
    # @codedoc_comment_block basicepistats:::surv_pohar_perme_weight__
    # - `method = "survival interval"`: The Pohar Perme weight is based on
    #   the expected survival probability at the middle of the survival
    #   interval. E.g. if the survival interval is
    #   `[0, 1[` and one subject has rows with `t_start = c(0.10, 0.20)`
    #   and `t_stop = c(0.20, 0.30)`, both rows get the same Pohar Perme weight
    #   based on integrating expected survival probability from `t = 0.10` to
    #   `t = 0.50`. This is the method proposed by Coviello et al (2015,
    #   https://doi.org/10.1177/1536867X1501500111)
    #   and validated by Sepp\u00e4 et al
    #   (2016, https://doi.org/10.1002/sim.6833) in simulated data.
    #   The following steps are performed:
    # @codedoc_comment_block basicepistats:::surv_pohar_perme_weight__
    data.table::set(
      x = work_dt,
      j = "survival_interval_id",
      value = cut(
        x = dt[[ts_col_nm]],
        breaks = breaks,
        right = FALSE,
        labels = FALSE
      )
    )
    join_dt <- data.table::setDT(list(
      lex.id = work_dt[["lex.id"]],
      survival_interval_id = work_dt[["survival_interval_id"]]
    ))
    data.table::set(
      x = work_dt,
      j = "h*_{i,j}",
      value = work_dt[["lex.dur"]] * work_dt[["expected_hazard"]]
    )
    work_dt <- work_dt[
      #' @importFrom data.table .SD
      j = lapply(.SD, sum),
      .SDcols = c("h*_{i,j}", "lex.dur"),
      by = c("lex.id", "survival_interval_id")
    ]
    data.table::set(
      x = work_dt,
      j = c("integration_start", "integration_stop", "h*_{i,j}"),
      value = list(
        # @codedoc_comment_block basicepistats:::surv_pohar_perme_weight__
        #   + Take as the start point of integration for survival interval i
        #     and subject j the entry point to follow-up in the survival interval.
        #     This is usually the start of the survival interval but late entries
        #     may start after that. E.g. for rows `t_start = c(0.10, 0.20)`
        #     the integration starts for survival interval i and subject j
        #     from `t = 0.10`.
        # @codedoc_comment_block basicepistats:::surv_pohar_perme_weight__
        dt[[ts_col_nm]][!duplicated(dt[["lex.id"]])],
        # @codedoc_comment_block basicepistats:::surv_pohar_perme_weight__
        #   + Take as the end point of integration for survival interval i
        #     and subject j the end of the survival interval i.
        #     E.g. `t = 1.0` for interval `[0.0, 1.0[`.
        # @codedoc_comment_block basicepistats:::surv_pohar_perme_weight__
        breaks[work_dt[["survival_interval_id"]] + 1L],
        # @codedoc_comment_block basicepistats:::surv_pohar_perme_weight__
        #   + Take as the expected hazard for subject j in survival interval i
        #     the weighted average of subinterval expected hazards, where
        #     the observed width of the subinterval is the weight. E.g.
        #     with `t_start = c(0.1, 0.2)` and `t_stop = c(0.2, 0.3)`
        #     we get `h*_{i,j}` as the average of `h*_{i,j,1}` and `h*_{i,j,2}`
        #     for subject j in survival interval `[0.00, 1.00[`.
        # @codedoc_comment_block basicepistats:::surv_pohar_perme_weight__
        work_dt[["h*_{i,j}"]] / work_dt[["lex.dur"]]
      )
    )
    data.table::set(
      x = work_dt,
      j = "l_{i,j}",
      value = work_dt[["integration_stop"]] - work_dt[["integration_start"]]
    )
    data.table::set(
      x = work_dt,
      j = "H*(t_{i,j})",
      value = work_dt[["h*_{i,j}"]] * work_dt[["l_{i,j}"]]
    )
    work_dt[
      #' @importFrom data.table := .SD
      j = "H*(t_{i,j})" := lapply(.SD, cumsum),
      .SDcols = "H*(t_{i,j})",
      by = "lex.id"
    ]
    data.table::set(
      x = work_dt,
      j = "pp_weight",
      value = {
        # @codedoc_comment_block basicepistats:::surv_pohar_perme_weight__
        #   + Now for subject j compute the cumulative expected hazard
        #     over survival intervals using the previously collected integration
        #     start and stop times. This results in values of `H*(t_{i,j})`
        #     for subject j and survival interval i, where this value is at the
        #     end of the interval. To get to the middle of the survival interval
        #     we subtract from `H*(t_{i,j})` the corresponding `h*_{i,j}`
        #     multiplied with half the width of that survival interval.
        #     This yields `H*(m_{i,j})`.
        # @codedoc_comment_block basicepistats:::surv_pohar_perme_weight__
        `H*(m_{i,j})` <- work_dt[["H*(t_{i,j})"]]
          - work_dt[["h*_{i,j}"]] * work_dt[["l_{i,j}"]] / 2
        # `w_{i,j}` = 1 / exp(-`H*(m_{i,j})`) = exp(`H*(m_{i,j})`)
        # @codedoc_comment_block basicepistats:::surv_pohar_perme_weight__
        #   + Then the Pohar Perme weight is simply
        #     `w_{i,j} = 1 / e^{-H*(m_{i,j}))} = e^H*(m_{i,j})`.
        # @codedoc_comment_block basicepistats:::surv_pohar_perme_weight__
        `w_{i,j}` <-  exp(`H*(m_{i,j})`)
        `w_{i,j}`
      }
    )
    # @codedoc_comment_block basicepistats:::surv_pohar_perme_weight__
    #   + With the weight computed for each subject in each survival interval
    #     (in which they are in follow-up), these weights are joined back to
    #     the subinterval level via a simple left join on subject ID and
    #     survival interval ID. That is, `w_{i,j,k} = w_{i,j}` for every k.
    # @codedoc_comment_block basicepistats:::surv_pohar_perme_weight__
    work_dt <- work_dt[
      i = join_dt,
      on = c("lex.id", "survival_interval_id"),
      #' @importFrom data.table .SD
      j = .SD,
      .SDcols = "pp_weight"
    ]
  } else {
    stop("No such method for computing Pohar Perme weights: \"", method, "\"")
  }
  return(work_dt[["pp_weight"]])
}

surv_split_merge_aggregate_by_row <- function(
  dt,
  breaks = list(cal = 2000:2025, age = 0:100, fot = seq(0, 5, 1 / 12)),
  merge_dt_by = c("sex", "cal", "age"),
  merge_dt = data.table::data.table(
    sex = 0L,
    cal = 2000,
    age = 0,
    haz = 0.010
  ),
  aggre_stratum_dt = data.table::CJ(sex = 0:1, study_arm = 0:1),
  aggre_ts_col_nms = "fot",
  aggre_values = quote(list(
    total_subject_time = sum(duration),
    n = .N,
    n_events = sum(status == "dead"),
    w = sum(brenner_weight),
    c = sum(status == "lost to follow-up")
  )),
  n_processing_strata = data.table::getDTthreads() * 2L
) {
  aggre_values_expr <- substitute(aggre_values)
  if (identical(aggre_values_expr[[1]], quote(quote))) {
    aggre_values_expr <- eval(aggre_values_expr)
  }

  breaks <- lapply(breaks, function(x) sort(unique(x)))
  interval_dt <- do.call(
    data.table::CJ,
    lapply(breaks[aggre_ts_col_nms], function(x) {
      x[-length(x)]
    })
  )
  data.table::setkeyv(dt, c(names(aggre_stratum_dt), names(breaks)))
  work_dt <- cbind(aggre_stratum_dt[1L, ], interval_dt[1L, ], interval_dt[1L, ])
  ts_lo_col_nms <- paste0(names(interval_dt), "_lo")
  ts_hi_col_nms <- paste0(names(interval_dt), "_hi")
  names(ts_lo_col_nms) <- names(ts_hi_col_nms) <- names(interval_dt)
  data.table::setnames(
    work_dt,
    c(
      names(aggre_stratum_dt),
      ts_lo_col_nms,
      ts_hi_col_nms
    )
  )
  lapply(seq_len(nrow(dt)), function(i) {
    dt_i <- dt[i, ]
    for (j in seq_len(nrow(interval_dt))) {
      data.table::set(
        work_dt,
        j = ts_lo_col_nms,
        value = lapply(names(interval_dt), function(ts_col_nm) {
          pmax(dt_i[[ts_col_nm]], interval_dt[[ts_lo_col_nms[ts_col_nm]]][j])
        })
      )
    }
  })

  return(NULL)
}

surv_estimate_expr_list__ <- list(
  lifetable_observed_survival = list(
    estimate = quote(cumprod(1 -  n_events / n_at_risk_effective)),
    standard_error = quote(
      lifetable_observed_survival_estimate *
        sqrt(cumsum(n_events / (n_at_risk_effective * (n_at_risk_effective - n_events))))
    )
  ),
  hazard_observed_survival = list(
    estimate = quote(
      exp(-cumsum(interval_width * hazard_observed))
    ),
    standard_error = quote(
      hazard_observed_survival_estimate *
        sqrt(cumsum((interval_width ^ 2) * n_events / (total_subject_time ^ 2)))
    )
  ),
  lifetable_expected_survival_ederer_ii = list(
    estimate = quote(
      cumprod(1 - n_events_expected_ederer_ii / n_at_risk_effective)
    ),
    standard_error = quote(
      rep(0.0, length(lifetable_expected_survival_estimate))
    )
  ),
  hazard_expected_survival_ederer_ii = list(
    estimate = quote(
      exp(-cumsum(interval_width * hazard_expected_ederer_ii))
    ),
    standard_error = quote(
      rep(0.0, length(hazard_expected_survival_ederer_ii))
    )
  ),
  lifetable_relative_survival_ederer_ii = list(
    estimate = quote(
      cumprod(1 - (n_events - n_events_expected_ederer_ii) / n_at_risk_effective)
    ),
    standard_error = quote(
      lifetable_observed_survival_standard_error / lifetable_expected_survival_ederer_ii_estimate
    )
  ),
  hazard_relative_survival_ederer_ii = list(
    estimate = quote(
      exp(-cumsum(interval_width * hazard_excess_ederer_ii))
    ),
    standard_error = quote(
      hazard_observed_survival_standard_error / hazard_expected_survival_ederer_ii_estimate
    )
  ),
  lifetable_net_survival_pohar_perme = list(
    estimate = quote(
      cumprod(1 - (n_events_pp - n_events_expected_pp) / n_in_follow_up_effective_pp)
    ),
    standard_error = quote(
      lifetable_net_survival_pohar_perme_estimate *
        sqrt(cumsum(n_events_pp_double_weighted / (n_at_risk_effective ^ 2)))
    )
  ),
  hazard_net_survival_pohar_perme = list(
    estimate = quote(
      exp(-cumsum(interval_width * hazard_excess_pp))
    ),
    standard_error = quote(
      hazard_net_survival_pohar_perme_estimate *
        sqrt(cumsum((interval_width ^ 2) * n_events_pp_double_weighted / (total_subject_time_pp ^ 2)))
    )
  ),
  # lifetable_observed_absolute_risk <- list(
  #   estimate = quote(
  #     cumprod()
  #   ),
  #   standard_error = quote(
  #     NA_real_
  #   )
  # ),
  # hazard_observed_absolute_risk <- list(
  #   estimate = quote(
  #     exp(-cumsum(hazard_observed))
  #   ),
  #   standard_error = quote(
  #     NA_real_
  #   )
  # ),
  # lifetable_expected_absolute_risk_ederer_i <- list(
  #   estimate = quote(
  #     1 - lifetable_expected_survival_ederer_i_estimate
  #   ),
  #   standard_error = quote(
  #     lifetable_observed_survival_standard_error
  #   )
  # ),
  # hazard_expected_absolute_risk_ederer_i <- list(
  #   estimate = quote(
  #     1 - hazard_expected_survival_ederer_i_estimate
  #   ),
  #   standard_error = quote(
  #     hazard_observed_survival_standard_error
  #   )
  # ),
  # lifetable_observed_extra_absolute_risk = list(
  #   estimate = quote({
  #     obs_abs_risk <- lifetable_observed_absolute_risk
  #     exp_abs_risk <- 1 - lifetable_expected_survival_estimate
  #     obs_abs_risk - exp_abs_risk
  #   }),
  #   standard_error = quote(
  #     lifetable_observed_absolute_risk_standard_error
  #   )
  # ),
  # hazard_observed_extra_absolute_risk = list(
  #   estimate = quote({
  #     obs_abs_risk <- hazard_observed_absolute_risk_estimate
  #     exp_abs_risk <- 1 - hazard_expected_survival_estimate
  #     obs_abs_risk - exp_abs_risk
  #   }),
  #   standard_error = quote(
  #     lifetable_observed_absolute_risk_standard_error
  #   )
  # ),
  lifetable_observed_survival_deficit = list(
    estimate = quote({
      obs_surv <- lifetable_observed_survival_estimate
      exp_surv <- lifetable_expected_survival_ederer_i_estimate
      exp_surv - obs_surv
    }),
    standard_error = quote(
      lifetable_observed_survival_standard_error
    )
  ),
  hazard_observed_survival_deficit = list(
    estimate = quote({
      obs_surv <- hazard_observed_survival_estimate
      exp_surv <- hazard_expected_survival_ederer_i_estimate
      exp_surv - obs_surv
    }),
    standard_error = quote(
      hazard_observed_survival_standard_error
    )
  )
)
make_surv_estimate_expr_list__ <- function(surv_estimate_expr_list) {
  utility_expr_list <- list(
    hazard_observed = quote(
      n_events / total_subject_time
    ),
    hazard_expected_ederer_ii = quote(
      n_events_expected_ederer_ii / total_subject_time
    ),
    hazard_excess_ederer_ii = quote(
      (n_events - n_events_expected_ederer_ii) / total_subject_time
    ),
    hazard_excess_pp = quote(
      (n_events_pp - n_events_expected_pp) / total_subject_time_pp
    )
  )
  for (utility_expr_nm in names(utility_expr_list)) {
    # e.g. expr = quote(exp(-cumsum(interval_width * hazard_observed)))
    expr <- utility_expr_list[[utility_expr_nm]]
    # e.g. expr_expr = quote(substitute(exp(-cumsum(interval_width * hazard_observed)), utility_expr_list))
    expr_expr <- substitute(
      substitute(expr, utility_expr_list),
      list(expr = expr)
    )
    # e.g. expr = quote(exp(-cumsum(interval_width * n_events / total_subject_time)))
    expr <- eval(expr_expr)
    utility_expr_list[[utility_expr_nm]] <- expr
  }
  for (estimator_nm in names(surv_estimate_expr_list)) {
    for (elem_nm in c("estimate", "standard_error")) {
      # e.g. expr = quote(lifetable_observed_survival_estimate)
      expr <- surv_estimate_expr_list[[estimator_nm]][[elem_nm]]
      # e.g. expr_expr = quote(substitute(lifetable_observed_survival_estimate, utility_expr_list))
      expr_expr <- substitute(
        substitute(expr, utility_expr_list),
        list(expr = expr)
      )
      # e.g. expr = quote(cumprod(1 - n_events / n_at_risk_effective))
      expr <- eval(expr_expr)
      utility_elem_nm <- paste0(
        estimator_nm,
        "_",
        elem_nm
      )
      utility_expr_list[[utility_elem_nm]] <- expr
      surv_estimate_expr_list[[estimator_nm]][[elem_nm]] <- expr
    }
  }
  return(surv_estimate_expr_list)
}
surv_estimate_expr_list__ <- make_surv_estimate_expr_list__(
  surv_estimate_expr_list__
)

surv_estimate_exprs <- function(type) {
  dbc::assert_vector_elems_are_in_set(
    type,
    set = names(surv_estimate_expr_list__)
  )
  out <- surv_estimate_expr_list__[type]
  return(out)
}
surv_estimate <- function(
  dt,
  stratum_col_nms = NULL,
  type = "hazard_observed_survival",
  conf_methods = "log-log"
) {
  call_env <- parent.frame(1L)
  if (is.character(type)) {
    expressions <- surv_estimate_exprs(type)
  } else {
    expressions <- type
  }
  for (estimator_name in names(expressions)) {
    for (element_name in names(expressions[[estimator_name]])) {
      add_col_nm <- sprintf("%s_%s", estimator_name, element_name)
      dt[
        #' @importFrom data.table := .SD
        j = (add_col_nm) := eval(
          expressions[[estimator_name]][[element_name]],
          envir = .SD,
          enclos = call_env
        ),
        by = stratum_col_nms
      ]
    }
  }
  return(dt[])
}

surv_lexis <- function(
  dt,
  breaks,
  merge_dt_by = NULL,
  merge_dt = NULL,
  aggre_stratum_dt = NULL,
  aggre_ts_col_nm = NULL,
  type = "hazard_observed_survival",
  conf_methods = "log-log",
  conf_lvls = 0.95,
  weight_dt = NULL
) {
  dt <- surv_split_merge_aggregate(
    dt = dt,
    breaks = breaks,
    merge_dt_by = merge_dt_by,
    merge_dt = merge_dt,
    aggre_stratum_dt = aggre_stratum_dt,
    aggre_ts_col_nms = aggre_ts_col_nm
  )
  do_adjust <- !is.null(weight_dt)
  surv_estimate(
    dt = dt,
    type = type,
    conf_methods = if (do_adjust) "none" else conf_methods
  )
  if (do_adjust) {
    estimate_col_nms <- intersect(
      names(dt),
      paste0(names(surv_estimate_expr_list__), "_estimate")
    )
    standard_error_col_nms <- sub(
      "_estimate$", "_standard_error", estimate_col_nms
    )
    data.table::set(
      dt,
      j = standard_error_col_nms,
      value = lapply(standard_error_col_nms, function(secn) {
        dt[[secn]] ^ 2
      })
    )
    variance_col_nms <- sub(
      "_estimate$", "_variance", estimate_col_nms
    )
    data.table::setnames(dt, standard_error_col_nms, variance_col_nms)
    stratum_col_nms <- setdiff(names(aggre_stratum_dt), names(weight_dt))
    adjust_stratum_col_nms <- c(
      stratum_col_nms, "interval_id", "interval_width", aggre_ts_col_nm
    )
    dta <- directadjusting::directly_adjusted_estimates(
      stats_dt = dt,
      stratum_col_nms = adjust_stratum_col_nms,
      stat_col_nms = estimate_col_nms,
      var_col_nms = variance_col_nms,
      adjust_col_nms = setdiff(names(weight_dt), "weight"),
      conf_methods = conf_methods,
      conf_lvls = conf_lvls,
      weights = weight_dt
    )
    data.table::set(
      dta,
      j = variance_col_nms,
      value = lapply(variance_col_nms, function(vcn) {
        sqrt(dta[[vcn]])
      })
    )
    data.table::setnames(dta, variance_col_nms, standard_error_col_nms)
    sum_col_nms <- setdiff(
      names(dt),
      c(
        adjust_stratum_col_nms,
        names(aggre_stratum_dt),
        names(weight_dt),
        estimate_col_nms,
        variance_col_nms,
        standard_error_col_nms
      )
    )
    data.table::set(
      x = dta,
      j = sum_col_nms,
      value = dt[
        i = dta,
        on = adjust_stratum_col_nms,
        #' @importFrom data.table .SD
        j = lapply(.SD, sum),
        .SDcols = sum_col_nms,
        #' @importFrom data.table .EACHI
        keyby = .EACHI
      ][j = .SD, .SDcols = sum_col_nms]
    )
    dt <- dta
  }
  return(dt[])
}

surv_crop <- function(dt, breaks) {
  # "crop", e.g.
  # breaks = list(ts_fut = seq(0, 5, 1 / 12), ts_cal = 2000:2010) means that
  # follow-up starts only at ts_cal = 2000 at the earliest (period analysis).
  min_by_ts <- lapply(breaks, min)
  offset <- do.call(pmax, lapply(names(breaks), function(ts_col_nm) {
    dt[[ts_col_nm]] - pmax(dt[[ts_col_nm]], min_by_ts[[ts_col_nm]])
  }))
  data.table::set(
    x = dt,
    j = names(breaks),
    value = lapply(names(breaks), function(ts_col_nm) {
      dt[[ts_col_nm]] + offset
    })
  )
  data.table::set(
    x = dt,
    j = "lex.dur",
    value = dt[["lex.dur"]] - offset
  )
  return(invisible(dt[]))
}

surv_make_immortal <- function(dt, breaks) {
  max_by_ts <- lapply(breaks, max)
  pmin_data <- lapply(names(max_by_ts), function(ts_col_nm) {
    max_by_ts[[ts_col_nm]] - dt[[ts_col_nm]]
  })
  names(pmin_data) <- names(max_by_ts)
  data.table::set(
    x = dt,
    j = "lex.dur",
    value = do.call(pmin, pmin_data, quote = TRUE)
  )
  data.table::set(
    x = dt,
    j = "lex.Xst",
    value = dt[["lex.Cst"]]
  )
  return(invisible(dt[]))
}

surv_rule_based_interval_breaks <- function(
  dt,
  ts_fut_nm = "ts_fut",
  breaks = NULL,
  mandatory_breaks = 0:5,
  combination_test_expr = quote(sum(lex.Xst != lex.Cst) == 0)
) {
  calling_env <- parent.frame(1L)
  if (is.null(breaks)) {
    breaks <- dt[
      i = dt[["lex.Cst"]] != dt[["lex.Xst"]],
      j = .SD[[ts_fut_nm]] + .SD[["lex.dur"]],
      .SDcols = c(ts_fut_nm, "lex.dur")
    ]
    breaks <- sort(unique(round(breaks, 10)))
    breaks <- union(0L, breaks)
    storage.mode(breaks) <- storage.mode(dt[["lex.dur"]])
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
        dt = dt,
        break_lo = break_lo,
        break_hi = break_hi,
        ts_col_nm = ts_fut_nm,
        merge = FALSE
      )
      eval(combination_test_expr, envir = work_dt, enclos = calling_env)
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

surv_estimate_ederer_i <- function(
  dt,
  breaks,
  interval_dt,
  merge_dt,
  merge_dt_by
) {
  # dt contains one row per lex.id.
  dt <- data.table::setDT(as.list(dt))
  surv_crop(dt = dt, breaks = breaks)
  drop <- dt[["lex.dur"]] <= 0.0
  if (any(drop)) {
    dt <- subset(dt, subset = !drop)
  }
  surv_make_immortal(dt = dt, breaks = breaks)
  aggre_ts_col_nm <- intersect(names(breaks), names(interval_dt))
  dt <- surv_split_merge_aggregate(
    dt = dt,
    breaks = breaks,
    merge_dt = merge_dt,
    merge_dt_by = merge_dt_by,
    # aggre_stratum_dt = data.table::data.table("__dummy__" = TRUE),
    aggre_stratum_dt = data.table::data.table(
      lex.id = sort(unique(dt[["lex.id"]])),
      key = "lex.id"
    ),
    aggre_ts_col_nms = aggre_ts_col_nm,
    aggre_values = quote(list(
      haz = sum(lex.dur * haz)
    ))
  )
  stop("above produces mostly NA values in haz.")
  # dt now contains survival-interval-specific hazard for each lex.id.
  data.table::setkeyv(dt, c("lex.id", aggre_ts_col_nm))
  dt[
    #' @importFrom data.table := .SD
    j = "ederer_i" := exp(-cumsum(.SD[["haz"]])),
    by = "lex.id"
  ]
  # dt now contains ederer_i expected survival curve per lex.id.
  dt <- dt[
    #' @importFrom data.table .SD
    j = list(ederer_i = mean(.SD[["ederer_i"]])),
    .SDcols = "ederer_i",
    keyby = eval(aggre_ts_col_nm)
  ]
  # dt now contains the overall average ederer_i expected survival curve.
  return(dt[["ederer_i"]])
}

surv_interval <- function(
  dt,
  break_lo,
  break_hi,
  ts_col_nm,
  merge = FALSE
) {
  dbc::assert_has_class(dt, required_class = "Lexis")
  lexis_col_nms <- c(
    "lex.id",
    "lex.dur",
    "lex.Cst",
    "lex.Xst"
  )
  work_dt <- data.table::setDT(as.list(dt)[lexis_col_nms])
  ts_dt <- data.table::setDT(as.list(dt)[attr(dt, "time.scales")])
  if (isTRUE(merge)) {
    merge <- setdiff(names(dt), c(names(work_dt), names(ts_dt)))
  } else if (isFALSE(merge)) {
    merge <- character(0L)
  } else {
    dbc::assert_vector_elems_are_in_set(
      x = merge,
      set = names(dt)
    )
  }
  data.table::set(
    x = ts_dt,
    j = ts_col_nm,
    value = pmax(dt[[ts_col_nm]], break_lo)
  )
  ts_stop_col_nm <- paste0(ts_col_nm, "_stop")
  data.table::set(
    x = work_dt,
    j = ts_stop_col_nm,
    value = dt[[ts_col_nm]] + dt[["lex.dur"]]
  )
  data.table::set(
    x = work_dt,
    j = "in_interval",
    value = work_dt[[ts_stop_col_nm]] >= break_lo &
      dt[[ts_col_nm]] < break_hi
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
    offset <- ts_dt[[ts_col_nm]] - dt[[ts_col_nm]]
    lapply(setdiff(attr(dt, "time.scales"), ts_col_nm), function(ts_col_nm_) {
      data.table::set(
        x = ts_dt,
        j = ts_col_nm_,
        value = dt[[ts_col_nm_]] + offset
      )
    })
    NULL
  })
  data.table::set(
    x = work_dt,
    j = "lex.Xst",
    value = data.table::fifelse(
      work_dt[["end_in_interval"]],
      dt[["lex.Xst"]],
      dt[["lex.Cst"]]
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
      value = as.list(dt)[merge]
    )
  }
  work_dt <- subset(
    work_dt,
    subset = work_dt[["in_interval"]],
    select = c(
      "lex.id",
      attr(dt, "time.scales"),
      "lex.dur",
      "lex.Cst", "lex.Xst",
      merge
    )
  )
  return(work_dt[])
}
