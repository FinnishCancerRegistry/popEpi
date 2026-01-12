lexis_set__ <- function(dt, lexis_ts_col_nms) {
  attr_nms <- c("time.scales", "time.since", "breaks")
  if (inherits(dt, "Lexis")) {
    attrs <- lapply(attr_nms, attr, x = dt)
  } else {
    attrs <- list(
      lexis_ts_col_nms,
      rep("", length(lexis_ts_col_nms)),
      structure(lapply(lexis_ts_col_nms, function(x) NULL), names = lexis_ts_col_nms)
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

#' @title Survival Time Statistics
#' @description
#' Functions used for estimation of various survival time statistics.
#' E.g. relative survival.
#' @name survival_revamp
NULL

surv_split <- function(
  dt,
  subset = NULL,
  breaks,
  merge = TRUE
) {
  stopifnot(
    inherits(dt, "Lexis")
  )
  if (nrow(dt) == 0) {
    return(dt[])
  }
  lexis_ts_col_nms <- attr(dt, "time.scales")
  lexis_col_nms <- c(
    "lex.id", lexis_ts_col_nms, "lex.dur", "lex.Cst", "lex.Xst"
  )
  if (isTRUE(merge)) {
    merge <- setdiff(names(dt), lexis_col_nms)
  } else if (isFALSE(merge)) {
    merge <- character(0L)
  }
  out <- data.table::setDT(as.list(dt)[union(lexis_col_nms, merge)])
  subset <- handle_arg_subset()
  if (any(!subset)) {
    out <- out[(subset), ]
  }
  lexis_set__(out, lexis_ts_col_nms = lexis_ts_col_nms)
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

#' @eval codedoc::pkg_doc_fun("popEpi::surv_merge", "survival_revamp")
#' @examples
#' # popEpi::surv_merge
#' lexis <- Epi::Lexis(
#'   entry = list(ts_fut = 0.0, ts_cal = 2010.3, ts_age = 56.8),
#'   exit = list(ts_cal = 2024.9999),
#'   entry.status = 0,
#'   exit.status = 0
#' )
#' lexis$sex <- 0L
#' lexis <- popEpi::splitMulti(
#'   data = lexis,
#'   breaks = list(ts_fut = seq(0, 3, 1 / 12))
#' )
#' my_merge_dt <- data.table::CJ(sex = 0:1, ts_age = 0:100, ts_cal = 2000:2025)
#' data.table::set(
#'   x = my_merge_dt,
#'   j = "merge_value",
#'   value = runif(nrow(my_merge_dt))
#' )
#' popEpi::surv_merge(
#'   dt = lexis,
#'   merge_dt = my_merge_dt,
#'   merge_dt_by = c("sex", "ts_age", "ts_cal")
#' )
#' stopifnot(
#'   "merge_value" %in% names(lexis),
#'   !is.na(lexis[["merge_value"]])
#' )
#' data.table::set(
#'   x = lexis,
#'   j = "merge_value",
#'   value = NULL
#' )
#' popEpi::surv_merge(
#'   dt = lexis,
#'   merge_dt = my_merge_dt,
#'   merge_dt_by = c("sex", "ts_age", "ts_cal"),
#'   merge_dt_harmonisers = list(
#'     ts_cal = quote(as.integer(ts_cal)),
#'     ts_age = quote(as.integer(ts_age))
#'   )
#' )
#' stopifnot(
#'   "merge_value" %in% names(lexis),
#'   !is.na(lexis[["merge_value"]])
#' )
#'
surv_merge <- function(
  dt,
  merge_dt,
  merge_dt_by,
  merge_dt_harmonisers = NULL
) {
  dbc::assert_is_identical(
    x = class(dt),
    y = c("Lexis", "data.table", "data.frame")
  )
  dbc::assert_vector_elems_are_in_set(
    x = merge_dt_by,
    set = names(dt)
  )
  dbc::assert_vector_elems_are_in_set(
    x = merge_dt_by,
    set = names(merge_dt)
  )
  # @codedoc_comment_block popEpi::surv_merge
  # `popEpi::surv_merge` can be used to merge additional information into
  # `Lexis` data, allowing the use of the `Lexis` time scales in the
  # merge. The typical use-case is to split `Lexis` data and then merge
  # population (expected) hazards to the subject-intervals.
  # `popEpi::surv_merge` performs the following steps:
  # 
  # @codedoc_comment_block popEpi::surv_merge
  call_env <- parent.frame(1L)
  lexis_ts_col_nms <- attr(dt, "time.scales")
  merge_ts_col_nms <- intersect(lexis_ts_col_nms, merge_dt_by)
  if (is.null(merge_dt_harmonisers)) {
    # @codedoc_comment_block popEpi::surv_merge
    # - If `is.null(merge_dt_harmonisers)`, `popEpi::surv_merge` attempts to
    #   automatically determine the harmonisers making use of `cut` by looking
    #   at the unique
    #   values of the time scale to merge by in `merge_dt` (e.g. calendar year
    #   in `ts_cal`):
    # @codedoc_comment_block popEpi::surv_merge
    merge_dt_harmonisers <- lapply(merge_ts_col_nms, function(col_nm) {
      cut_breaks <- sort(unique(merge_dt[[col_nm]]))
      if (is.integer(cut_breaks) || is.double(cut_breaks)) {
        if (all(diff(cut_breaks) == 1)) {
          # @codedoc_comment_block popEpi::surv_merge
          #   + If `merge_dt[[col_nm]]` contains numbers in increments of one and
          #     nothing else, we define the `cut` breaks as the unique values of
          #    `merge_dt[[col_nm]]` and as the ceiling
          #    `max(merge_dt[[col_nm]]) + 1L`. E.g.
          #    `1950:2021` for `merge_dt[[col_nm]]` containing unique values
          #    `1950:2020`.
          # @codedoc_comment_block popEpi::surv_merge
          cut_breaks <- c(cut_breaks, cut_breaks[length(cut_breaks)] + 1L)
        } else {
          # @codedoc_comment_block popEpi::surv_merge
          #   + If `merge_dt[[col_nm]]` contains numbers but they are not all in
          #     increments of one then
          #     we define `cut` breaks as the unique values of
          #     `merge_dt[[col_nm]]` and as the ceiling
          #     `max(merge_dt[[col_nm]]) + last_diff`. Where `last_diff` is the
          #     difference between the highest and second highest values. E.g.
          #     `c(1950, 1960, 1970:2020, 2021)` for `merge_dt[[col_nm]]`
          #     containing unique values `c(1950, 1960, 1970:2020)`.
          # @codedoc_comment_block popEpi::surv_merge
          last_diff <- diff(utils::tail(cut_breaks, 2L))
          cut_breaks <- c(
            cut_breaks,
            cut_breaks[length(cut_breaks)] + last_diff
          )
        }
      } else {
        # @codedoc_comment_block popEpi::surv_merge
        #   + If `merge_dt[[col_nm]]` does not contain numbers, an error is
        #     raised because we don't know how to automatically form a
        #     harmoniser.
        # @codedoc_comment_block popEpi::surv_merge
        stop(
          "Cannot automatically determine `merge_dt_harmonisers$", col_nm, "`;",
          "Please supply argument `merge_dt_harmonisers` yourself."
        )
      }
      # @codedoc_comment_block popEpi::surv_merge
      #    + With the `cut` breaks defined, the automatically created harmoniser
      #      becomes a `cut` call with arguments
      #      * `x = COL + lex.dur / 2`, where `COL` is the current column,
      #      * `breaks` as specified above,
      #      * `right = FALSE`, and
      #      * `labels = FALSE`.
      #    + This results in indices to the breaks, and the harmoniser returns
      #      break values at those indices. E.g. the cut results in
      #      `3` in `breaks = c(1950, 1960, 1970:2020, 2021)` and output is
      #      `1970` for every value of `COL + lex.dur / 2` in the interval
      #      `]1970, 1971]`.
      # @codedoc_comment_block popEpi::surv_merge
      substitute(
        {
          breaks <- CUT_BREAKS
          idx <- cut(
            x = COL + lex.dur / 2,
            breaks = breaks,
            right = FALSE,
            labels = FALSE
          )
          breaks[idx]
        },
        list(COL = parse(text = col_nm)[[1]], CUT_BREAKS = cut_breaks)
      )
    })
    names(merge_dt_harmonisers) <- merge_ts_col_nms
  }
  # @codedoc_comment_block popEpi::surv_merge
  # - Armed with either user-defined or automatically created
  #   `merge_dt_harmonisers`, they are each evaluated to create a temporary
  #   `data.table` with harmonised data from `dt`. This is performed via
  #   `eval` with `envir = dt` and `enclos = call_env` where `call_env` is the
  #   environment where `popEpi::surv_merge` was called. Of course if a column
  #   has no harmoniser at this point then it is used as-is. For instance there
  #   is no need to harmonise stratifying columns because they are not changed
  #   by splitting the `Lexis` data.
  # @codedoc_comment_block popEpi::surv_merge
  join_dt <- data.table::setDT(lapply(merge_dt_by, function(col_nm) {
    if (col_nm %in% names(merge_dt_harmonisers)) {
      eval(
        merge_dt_harmonisers[[col_nm]],
        envir = dt,
        enclos = call_env
      )
    } else {
      dt[[col_nm]]
    }
  }))
  data.table::setnames(join_dt, merge_dt_by)
  merge_value_col_nms <- setdiff(names(merge_dt), merge_dt_by)
  # @codedoc_comment_block popEpi::surv_merge
  # - Then we perform the actual merge between `merge_dt` and the harmonised
  #   data. This merges in data from `merge_dt` into every row of `dt` in-place.
  #   So `dt` is modified and no additional copy is taken for the sake of
  #   efficiency.
  # @codedoc_comment_block popEpi::surv_merge
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
  # @codedoc_comment_block popEpi::surv_merge
  # - Each merged-in column from `merge_dt` (all columns not in `merge_dt_by`)
  #   are inspected for missing values. If there are any, an error is raised.
  #   This usually occurs if `merge_dt` does not contain data for all data in
  #   `dt`. For instance it only covers years 1950-2020 but `dt` contains also
  #   data for 2021. This error helps you to spot those problems early instead
  #   of producing nonsense results downstream.
  # @codedoc_comment_block popEpi::surv_merge
  for (merge_value_col_nm in merge_value_col_nms) {
    is_missing <- is.na(dt[[merge_value_col_nm]])
    if (any(is_missing)) {
      print(dt[is_missing, ])
      stop("Merging `merge_dt` into split (subset of) `dt` produced NA values ",
           "in column `dt$", merge_value_col_nm, "`. ",
           "See the table printed above.")
    }
  }
  # @codedoc_comment_block popEpi::surv_merge
  # - `popEpi::surv_merge` returns `dt` invisibly after adding columns from
  #   `merge_dt` into `dt` in-place, without taking a copy.
  # @codedoc_comment_block popEpi::surv_merge
  return(invisible(dt[]))
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
  if (length(split_ts_col_nms) == 1) {
    data.table::set(
      x = box_dt,
      j = "interval_width",
      value = box_dt[[stop_col_nms]] - box_dt[[start_col_nms]]
    )
  }
  return(box_dt[])
}

surv_box_id__ <- function(
  dt,
  box_dt
) {
  start_col_nms <- sort(names(box_dt)[grepl("_start$", names(box_dt))])
  stop_col_nms <- sort(names(box_dt)[grepl("_stop$", names(box_dt))])
  split_ts_col_nms <- sub("_start$", "", start_col_nms)
  id_col_nms <- paste0(split_ts_col_nms, "_id")
  stopifnot(
    inherits(box_dt, "data.frame"),
    "box_id" %in% names(box_dt),
    id_col_nms %in% names(box_dt),
    identical(split_ts_col_nms, sub("_stop$", "", stop_col_nms)),
    inherits(dt, "data.frame"),
    split_ts_col_nms %in% names(dt),
    "lex.dur" %in% names(dt)
  )
  names(start_col_nms) <- names(stop_col_nms) <- split_ts_col_nms
  work_dt <- data.table::setDT(lapply(split_ts_col_nms, function(ts_col_nm) {
    breaks <- sort(union(
      box_dt[[start_col_nms[ts_col_nm]]],
      box_dt[[stop_col_nms[ts_col_nm]]]
    ))
    x <- dt[[ts_col_nm]] + dt[["lex.dur"]] * 0.5
    idx <- cut(
      x = x,
      breaks = breaks,
      right = FALSE,
      labels = FALSE
    )
    return(idx)
  }))
  data.table::setnames(work_dt, names(work_dt), id_col_nms)
  out <- box_dt[
    i = work_dt,
    on = id_col_nms,
    #' @importFrom data.table .SD
    j = .SD[["box_id"]],
    .SDcols = "box_id"
  ]
  return(out)
}

surv_aggregate_one_stratum__ <- function(
  sub_dt,
  box_dt,
  aggre_values_expr,
  enclos_env
) {
  expr_obj_nms <- all.vars(aggre_values_expr)
  lexis_ts_col_nms <- attr(sub_dt, "time.scales")
  sub_dt <- data.table::setDT(as.list(sub_dt))
  lapply(lexis_ts_col_nms, function(ts_col_nm) {
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
  data.table::set(
    x = sub_dt,
    j = "box_id",
    value = surv_box_id__(dt = sub_dt, box_dt = box_dt)
  )
  agg_expr <- substitute(
    sub_dt[
      j = EXPR,
      keyby = "box_id"
    ],
    list(
      EXPR = aggre_values_expr
    )
  )
  eval_env <- new.env(parent = enclos_env)
  eval_env[["sub_dt"]] <- sub_dt
  out <- eval(agg_expr, envir = eval_env)
  out <- out[
    i = box_dt,
    on = "box_id"
  ]
  data.table::setcolorder(
    out,
    c(names(box_dt), setdiff(names(out), names(box_dt)))
  )
  data.table::setkeyv(out, names(box_dt))
  if (nrow(sub_dt) == 0) {
    data.table::set(
      x = out,
      j = setdiff(names(out), names(box_dt)),
      value = NA
    )
  }
  return(out[])
}

#' @eval codedoc::pkg_doc_fun(
#'   "popEpi::surv_split_merge_aggregate_by_stratum",
#'   "survival_revamp"
#' )
surv_split_merge_aggregate_by_stratum <- function(
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
  aggre_by = data.table::CJ(sex = 0:1, study_arm = 0:1),
  aggre_ts_col_nms = "ts_fot",
  aggre_values = quote(list(
    total_subject_time = sum(lex.dur),
    n_events = sum(lex.Xst != lex.Cst)
  )),
  subset = NULL,
  optional_steps = NULL
) {
  eval_env <- environment()
  call_env <- parent.frame(1L)
  aggre_by <- handle_arg_by(by = aggre_by, dataset = dt)
  subset <- handle_arg_subset()
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
  # `popEpi::surv_split_merge_aggregate_by_stratum` can be used to split `Lexis`
  # (`[Epi::Lexis]`) data, merge something to it after the merge, and
  # then perform an aggregation step. The following steps are performed:
  #
  # - Call
  #   `optional_steps[["on_entry"]](eval_env = eval_env, call_env = call_env)`
  #   if that `optional_steps` element exists.
  #   `eval_env` is the temporary evaluation environment of
  #   `popEpi::surv_split_merge_aggregate_by_stratum` which contains all
  #   contains all the arguments of
  #   `popEpi::surv_split_merge_aggregate_by_stratum` and `call_env` is the environment
  #   where it was called.
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
  if ("on_entry" %in% names(optional_steps)) {
    optional_steps[["on_entry"]](eval_env = eval_env, call_env = call_env)
  }
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
  # - Call
  #   `on.exit(optional_steps[["on_exit"]](eval_env = eval_env, call_env = call_env))`
  #   if that `optional_steps` element exists.
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
  if ("on_exit" %in% names(optional_steps)) {
    on.exit(
      optional_steps[["on_exit"]](eval_env = eval_env, call_env = call_env)
    )
  }
  aggre_values_expr <- substitute(aggre_values)
  if (
    identical(aggre_values_expr[[1]], quote(quote)) ||
      identical(aggre_values_expr[[1]], quote(substitute))
  ) {
    aggre_values_expr <- eval(aggre_values_expr)
  }

  box_dt <- surv_box_dt__(breaks[aggre_ts_col_nms])
  lexis_ts_col_nms <- union(aggre_ts_col_nms, names(breaks))
  lexis_col_nms <- c(
    "lex.id", lexis_ts_col_nms, "lex.dur", "lex.Cst", "lex.Xst"
  )
  dt <- data.table::setDT(as.list(dt)[intersect(
    names(dt),
    c(
      lexis_col_nms,
      names(aggre_by),
      merge_dt_by,
      all.vars(expr = aggre_values_expr, functions = FALSE)
    )
  )])
  if (!all(subset)) {
    dt <- dt[(subset), ]
  }
  lexis_set__(dt = dt, lexis_ts_col_nms = lexis_ts_col_nms)
  out <- dt[
    i = aggre_by,
    on = names(aggre_by),
    j = {
      # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
      # - For each stratum in `aggre_by`:
      #   + Run
      #     `optional_steps[["stratum_on_entry"]](stratum_eval_env = stratum_eval_env, eval_env = eval_env, call_env = call_env)`
      #     if that `optional_steps` element exists.
      #     `stratum_eval_env` is the environment where the stratum-specific
      #     steps are performed.
      # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
      stratum_eval_env <- environment()
      if ("stratum_on_entry" %in% names(optional_steps)) {
        optional_steps[["stratum_on_entry"]](
          stratum_eval_env = stratum_eval_env,
          eval_env = eval_env,
          call_env = call_env
        )
      }
      if (.N == 0) {
        # for some reason .SD is a one-row data.table with all NA values
        sub_dt <- dt[0L, ]
      } else {
        sub_dt <- .SD
      }
      # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
      #   + Run
      #     `surv_split` on the subset of `dt` which contains data from the
      #     current stratum.
      # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
      sub_dt <- data.table::setDT(as.list(sub_dt))
      lexis_set__(dt = sub_dt, lexis_ts_col_nms = lexis_ts_col_nms)
      sub_dt <- surv_split(
        dt = sub_dt,
        breaks = breaks,
        merge = TRUE
      )
      # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
      #   + Run
      #     `optional_steps[["stratum_post_split"]](stratum_eval_env = stratum_eval_env, eval_env = eval_env, call_env = call_env)`
      #     if that `optional_steps` element exists.
      # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
      if ("stratum_post_split" %in% names(optional_steps)) {
        optional_steps[["stratum_post_split"]](
          stratum_eval_env = stratum_eval_env,
          eval_env = eval_env,
          call_env = call_env
        )
      }
      # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
      #   + Run
      #     `surv_merge` with `merge_dt`, `merge_dt_by`, and
      #     `merge_dt_harmonisers`.
      # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
      surv_merge(
        sub_dt,
        merge_dt = merge_dt,
        merge_dt_by = merge_dt_by,
        merge_dt_harmonisers = merge_dt_harmonisers
      )
      # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
      #   + Run
      #     `optional_steps[["stratum_post_merge"]](stratum_eval_env = stratum_eval_env, eval_env = eval_env, call_env = call_env)`
      #     if that `optional_steps` element exists.
      # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
      if ("stratum_post_merge" %in% names(optional_steps)) {
        optional_steps[["stratum_post_merge"]](
          stratum_eval_env = stratum_eval_env,
          eval_env = eval_env,
          call_env = call_env
        )
      }
      # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
      #   + Evaluate `aggre_values_expr` in the context of the split data
      #     with merged-in additional data. The enclosing environment is
      #     `call_env`. See `?eval`. This results in a `data.table` that
      #     contains one row per interval of `aggre_ts_col_nms`.
      # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
      out <- surv_aggregate_one_stratum__(
        sub_dt = sub_dt,
        box_dt = box_dt,
        aggre_values_expr = aggre_values_expr,
        enclos_env = call_env
      )
      # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
      #   + Run
      #     `optional_steps[["stratum_post_aggregation"]](stratum_eval_env = stratum_eval_env, eval_env = eval_env, call_env = call_env)`
      #     if that `optional_steps` element exists.
      # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
      if ("stratum_post_aggregation" %in% names(optional_steps)) {
        optional_steps[["stratum_post_aggregation"]](
          stratum_eval_env = stratum_eval_env,
          eval_env = eval_env,
          call_env = call_env
        )
      }
      out
    },
    #' @importFrom data.table .EACHI
    keyby = .EACHI
  ]
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
  # - After every stratum has been processed, set proper `data.table`
  #   attributes on the resulting big table and call `data.table::setkeyv`
  #   with `cols = c(names(aggre_by), "box_id")`.
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
  # to clear Epi attributes
  out <- as.list(out)
  attributes(out) <- attributes(out)["names"]
  data.table::setDT(out)
  data.table::setkeyv(out, c(names(aggre_by), "box_id"))
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
  # - Run
  #   `optional_steps[["post_aggregation"]](eval_env = eval_env, call_env = call_env)`
  #   if that `optional_steps` element exists.
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
  if ("post_aggregation" %in% names(optional_steps)) {
    optional_steps[["post_aggregation"]](
      stratum_eval_env = stratum_eval_env,
      eval_env = eval_env,
      call_env = call_env
    )
  }
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
  # - Return a big `data.table` with stratum columns as specified via
  #   `aggre_by` and value columns as specified via `aggre_values_expr`.
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
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
# 
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
  aggre_by = NULL,
  aggre_ts_col_nm = NULL,
  subset = NULL,
  type = "hazard_observed_survival",
  conf_methods = "log-log",
  conf_lvls = 0.95,
  weights = NULL
) {
  stopifnot(
    inherits(dt, "Lexis"),
    inherits(dt, "data.frame"),
    inherits(weights, c("NULL", "character", "data.frame"))
  )
  subset <- handle_arg_subset()
  dt <- surv_split_merge_aggregate_by_stratum(
    dt = dt,
    breaks = breaks,
    merge_dt_by = merge_dt_by,
    merge_dt = merge_dt,
    aggre_by = aggre_by,
    aggre_ts_col_nms = aggre_ts_col_nm,
    aggre_values = surv_aggre_expression(
      type = type,
      weight_col_nm = weights
    ),
    subset = subset
  )
  do_adjust <- !is.null(weights)
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
    stratum_col_nms <- setdiff(names(aggre_by), names(weight_dt))
    adjust_stratum_col_nms <- c(
      stratum_col_nms, "box_id", aggre_ts_col_nm
    )
    dta <- directadjusting::directly_adjusted_estimates(
      stats_dt = dt,
      stratum_col_nms = adjust_stratum_col_nms,
      stat_col_nms = estimate_col_nms,
      var_col_nms = variance_col_nms,
      adjust_col_nms = setdiff(names(weight_dt), "weight"),
      conf_methods = conf_methods,
      conf_lvls = conf_lvls,
      weights = weights
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
        names(aggre_by),
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
  call_env <- parent.frame(1L)
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

surv_estimate_ederer_i <- function(
  dt,
  breaks,
  ts_col_nm,
  merge_dt,
  merge_dt_by
) {
  dbc::assert_is_identical(
    x = class(dt),
    y = c("Lexis", "data.table", "data.frame")
  )
  dbc::assert_is_identical(
    x = data.table::key(dt)[1],
    y = "lex.id"
  )
  dbc::assert_atom_is_in_set(
    x = ts_col_nm,
    set = data.table::key(dt)
  )
  keep_col_nms <- unique(c(
    "lex.id",
    attr(dt, "time.scales"),
    "lex.dur", "lex.Cst", "lex.Xst",
    setdiff(names(merge_dt), "haz")
  ))
  work_dt <- data.table::setDT(as.list(dt)[keep_col_nms])
  surv_crop(dt = work_dt, breaks = breaks)
  keep <- work_dt[["lex.dur"]] > 0.0 & !duplicated(work_dt, by = "lex.id")
  work_dt <- subset(work_dt, subset = keep, select = keep_col_nms)
  surv_make_immortal(dt = work_dt, breaks = breaks)
  lex_id_dt <- data.table::setDT(list(lex.id = work_dt[["lex.id"]]))
  data.table::setkeyv(lex_id_dt, "lex.id")
  lexis_set__(
    dt = work_dt,
    lexis_ts_col_nms = attr(dt, "time.scales")
  )
  work_dt <- surv_split_merge_aggregate_by_stratum(
    dt = work_dt,
    breaks = breaks,
    merge_dt = merge_dt,
    merge_dt_by = merge_dt_by,
    aggre_by = lex_id_dt,
    aggre_ts_col_nms = ts_col_nm,
    aggre_values = quote(list(
      ederer_i = sum(lex.dur * haz)
    ))
  )
  data.table::set(
    x = work_dt,
    j = setdiff(names(work_dt), c("lex.id", "box_id", "ederer_i")),
    value = NULL
  )
  # work_dt now contains survival-interval-specific hazard for each lex.id.
  data.table::setkeyv(work_dt, c("lex.id", "box_id"))
  work_dt[
    #' @importFrom data.table := .SD
    j = "ederer_i" := lapply(.SD, cumsum),
    .SDcols = "ederer_i",
    by = "lex.id"
  ]
  data.table::set(
    x = work_dt,
    j = "ederer_i",
    value = exp(-work_dt[["ederer_i"]])
  )
  # work_dt now contains ederer_i expected survival curve per lex.id.
  work_dt <- work_dt[
    #' @importFrom data.table .SD
    j = lapply(.SD, mean),
    .SDcols = "ederer_i",
    keyby = "box_id"
  ]
  # work_dt now contains the overall average ederer_i expected survival curve.
  return(work_dt[["ederer_i"]])
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

assert_is_weight_dt <- function(
  x,
  x_nm = NULL,
  assertion_type = NULL,
  call = NULL
) {
  dbc::handle_args_inplace()
  dbc::assert_is_data_table_with_required_names(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    required_names = "weight"
  )
  dbc::assert_is_number_nonNA_gtezero_vector(
    x[["weight"]],
    x_nm = sprintf("%s[[\"weight\"]]", x_nm),
    call = call,
    assertion_type = assertion_type
  )
}

surv_individual_weights <- function(
  dt,
  standard_weight_dt,
  observed_weight_dt = NULL
) {
  # @codedoc_comment_block surv_individual_weights
  # Produce a vector of weights, one weight for each row in `dt`.
  # These weights have been called individual weights, Brenner weights,
  # (Brenner et al 2004, https://doi.org/10.1016/j.ejca.2004.07.007),
  # pre-weights, and maybe even others. A beloved child has many names.
  #
  # The idea of these weights is to weigh the individual contribution of each
  # person to the summary statistics from which the survival function estimates
  # themselves are produced. They pronounce the influence of under-represented
  # data and reduce the influence of over-represented data just as the more
  # conventional computation of weighted averages survival function estimates
  # does. However, in this individual weighting approach even a single event
  # can be included in the summary statistics as e.g. 0.80 or 1.20 events
  # for over and under-represented data respectively.
  #
  # This function makes producing these weights easier. It performs the
  # following steps:
  # @codedoc_comment_block surv_individual_weights
  stratum_col_nms <- setdiff(names(standard_weight_dt), "weight")
  if (is.null(observed_weight_dt)) {
    # @codedoc_comment_block surv_individual_weights
    # - If `is.null(observed_weight_dt)`, `observed_weight_dt` is computed
    #   by `surv_individual_weights` by simply counting the number of cases
    #   in each stratum in `standard_weight_dt`.
    # @codedoc_comment_block surv_individual_weights
    observed_weight_dt <- dt[
      i = standard_weight_dt,
      on = stratum_col_nms,
      j = list(weight = .N),
      #' @importFrom data.table .EACHI
      keyby = .EACHI
    ]
  } else {
    assert_is_weight_dt(observed_weight_dt)
  }
  assert_is_weight_dt(standard_weight_dt)
  # @codedoc_comment_block surv_individual_weights
  # - Merge `standard_weight_dt` and `observed_weight_dt` into one table.
  #   Scale the standard weights and the observed weights to sum into one
  #   (they are in separate columns). E.g. one stratum has
  #   `weight_standard = 0.5` and `weight_observed = 0.4`.
  # @codedoc_comment_block surv_individual_weights
  weight_dt <- merge(
    x = standard_weight_dt,
    y = observed_weight_dt,
    by = stratum_col_nms,
    suffixes = c("_standard", "_observed")
  )
  data.table::set(
    x = weight_dt,
    j = "weight_standard",
    value = weight_dt[["weight_standard"]] / sum(weight_dt[["weight_standard"]])
  )
  data.table::set(
    x = weight_dt,
    j = "weight_observed",
    value = weight_dt[["weight_observed"]] / sum(weight_dt[["weight_observed"]])
  )
  # @codedoc_comment_block surv_individual_weights
  # - Compute the individual weights as the standard weights divided by the
  #   observed weights. E.g. one stratum has
  #   `weight_brenner = 0.5 / 0.4 = 1.2`.
  # @codedoc_comment_block surv_individual_weights
  data.table::set(
    x = weight_dt,
    j = "weight_brenner",
    value = weight_dt[["weight_standard"]] / weight_dt[["weight_observed"]]
  )
  # @codedoc_comment_block surv_individual_weights
  # - Using left-join, produce a vector of length `nrow(dt)` where each row in
  #   `dt` gets an individual weight based on its stratum.
  # @codedoc_comment_block surv_individual_weights
  out <- weight_dt[
    i = dt,
    on = stratum_col_nms,
    j = .SD,
    .SDcols = "weight_brenner"
  ]
  # @codedoc_comment_block surv_individual_weights
  # - Return these weights as a vector.
  # @codedoc_comment_block surv_individual_weights
  return(out[["weight_brenner"]])
}
