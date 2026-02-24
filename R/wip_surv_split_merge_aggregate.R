SURV_AGGRE_EXPRS__ <- list(
  n_in_follow_up_at_interval_start = quote(
    sum(in_follow_up_at_interval_start)
  ),
  n_entered_late_during_interval = quote(
    sum(entered_late_during_interval)
  ),
  n_left_early_during_interval = quote(
    sum(left_early_during_interval)
  ),
  n_at_risk_eff = quote(
    sum((
      in_follow_up_at_interval_start +
        0.5 * (entered_late_during_interval & !left_early_during_interval) +
        0.25 * (entered_late_during_interval & left_early_during_interval) -
        0.5 * (!entered_late_during_interval & left_early_during_interval)
    ) * iw)
  ),
  n_events = quote(
    sum((lex.Xst != lex.Cst) * iw)
  ),
  t_at_risk = quote(
    sum(lex.dur * iw)
  ),
  n_events_exp_e2 = quote(
    sum(lex.dur * h_exp * iw)
  ),
  n_events_pp = quote(
    sum((lex.Xst != lex.Cst) * pp * iw)
  ),
  n_events_pp_double_weighted = quote(
    sum((lex.Xst != lex.Cst) * pp * pp * iw)
  ),
  n_events_exp_pp = quote(
    sum(lex.dur * h_exp * pp * iw)
  ),
  n_at_risk_eff_pp =  quote(
    sum((
      in_follow_up_at_interval_start +
        0.5 * (entered_late_during_interval & !left_early_during_interval) +
        0.25 * (entered_late_during_interval & left_early_during_interval) -
        0.5 * (!entered_late_during_interval & left_early_during_interval)
    ) * pp * iw)
  ),
  t_at_risk_pp = quote(
    sum(lex.dur * pp * iw)
  ),
  "n_events_[x, y]" = quote(
    sum((lex.Cst %in% x & lex.Xst %in% y) * iw)
  )
)
surv_aggre_exprs_table__ <- function() {
  dt <- data.table::data.table(
    "Name" = surv_estimate_expression_table_clean__(
      names(SURV_AGGRE_EXPRS__)
    ),
    "Expression" = vapply(
      SURV_AGGRE_EXPRS__,
      surv_estimate_expression_table_clean__,
      character(1L)
    )
  )
  return(dt)
}

SPLIT_LEXIS_COLUMN_EXPRS__ <- list(
  # tricky. ts_fut and box_id live in work_dt and box_dt has to be reachable
  # from stratum_eval_env.
  in_follow_up_at_interval_start = quote(
    ts_fut == box_dt[[ts_fut_start_col_nm]][box_id]
  ),
  entered_late_during_interval = quote(
    ts_fut > box_dt[[ts_fut_start_col_nm]][box_id]
  ),
  left_early_during_interval = quote(
    {
      out <- duplicated(lex.id, fromLast = TRUE) # nolint
      out[out] <- lex.Cst[out] == lex.Xst[out]
      out[out] <- round(ts_fut[out] + lex.dur[out], 10) <
        round(box_dt[[ts_fut_stop_col_nm]][box_id[out]], 10)
      out
    }
  ),
  pp = quote(
    {
      surv_pohar_perme_weight__(
        dt = work_dt,
        ts_fut_breaks = eval_env[["breaks"]][[ts_fut_col_nm]],
        ts_fut_col_nm = ts_fut_col_nm,
        hazard_col_nm = "h_exp"
      )
    }
  )
)
split_lexis_column_exprs_table__ <- function() {
  dt <- data.table::data.table(
    "Name" = surv_estimate_expression_table_clean__(
      names(SPLIT_LEXIS_COLUMN_EXPRS__)
    ),
    "Expression" = vapply(
      SPLIT_LEXIS_COLUMN_EXPRS__,
      surv_estimate_expression_table_clean__,
      character(1L)
    )
  )
  return(dt)
}

surv_aggregate_one_stratum__ <- function(
  dt_stratum_subset_split,
  box_dt,
  aggre_exprs,
  call_env,
  eval_env,
  stratum_eval_env
) {
  assert_is_arg_dt(dt_stratum_subset_split, lexis = TRUE)
  # `dt_stratum_subset_split` should be keyed by e.g.
  # c("lex.id", "ts_cal", "ts_fut") or
  # c("lex.id", "ts_fut") or c("lex.id", "ts_fut", "ts_cal") etc if
  # `box_ts_col_nms == "ts_fut"`. in particular `dt_stratum_subset_split`
  # must not be keyed
  # by anything weird before the time scales. after the time scales it can be
  # keyed by whatever.
  box_ts_col_nms <- names(box_dt)[grepl("_stop$", names(box_dt))]
  box_ts_col_nms <- sub("_stop$", "", box_ts_col_nms)
  lexis_ts_col_nms <- attr(dt_stratum_subset_split, "time.scales")
  if (nrow(dt_stratum_subset_split) > 0) {
    stopifnot(
      data.table::key(dt_stratum_subset_split)[1] == "lex.id",
      length(data.table::key(dt_stratum_subset_split)) >= length(box_ts_col_nms) + 1L,
      data.table::key(dt_stratum_subset_split)[2:(length(box_ts_col_nms) + 1L)] %in% union(
        box_ts_col_nms, lexis_ts_col_nms
      )
    )
  }
  assert_is_arg_box_dt(box_dt)
  assert_is_arg_aggre_exprs(aggre_exprs)
  stopifnot(
    is.environment(call_env),
    is.environment(eval_env),
    is.environment(stratum_eval_env)
  )

  work_dt <- data.table::setDT(as.list(dt_stratum_subset_split))
  data.table::setkeyv(work_dt, data.table::key(dt_stratum_subset_split))
  lexis_set__(
    dt = work_dt,
    lexis_ts_col_nms = Epi::timeScales(dt_stratum_subset_split)
  )
  # @codedoc_comment_block popEpi:::surv_aggregate_one_stratum__
  #    * First collect variables mentioned in `aggre_exprs` using `[all.vars]`.
  # @codedoc_comment_block popEpi:::surv_aggregate_one_stratum__
  expr_obj_nms <- unique(unlist(lapply(aggre_exprs, all.vars)))
  # lapply(lexis_ts_col_nms, function(ts_col_nm) {
  #   add_col_nms <- unique(expr_obj_nms[
  #     grepl(sprintf("^%s_((lead)|(lag))[0-9]+$", ts_col_nm), expr_obj_nms)
  #   ])
  #   lapply(add_col_nms, function(add_col_nm) {
  #     settings <- list(type = "lead")
  #     if (grepl("lag", add_col_nm)) {
  #       settings[["type"]] <- "lag"
  #     }
  #     settings[["n"]] <- as.integer(sub("^[^0-9]+", "", add_col_nm))
  #     work_dt[
  #       #' @importFrom data.table := .SD
  #       j = (add_col_nm) := .SD[[ts_col_nm]] - data.table::shift(
  #         x = .SD[[ts_col_nm]],
  #         n = settings[["n"]],
  #         type = settings[["type"]],
  #         fill = NA_integer_
  #       ),
  #       .SDcols = ts_col_nm,
  #       by = "lex.id"
  #     ]
  #   })
  # })
  surv_box_id__(dt = work_dt, box_dt = box_dt)
  add_expr_eval_env <- environment()
  local({
    ts_fut_col_nm <- eval_env[["aggre_ts_col_nms"]][
      length(eval_env[["aggre_ts_col_nms"]])
    ]
    substitute_env <- list(
      ts_fut = parse(text = ts_fut_col_nm)[[1]],
      ts_fut_col_nm = ts_fut_col_nm,
      ts_fut_start_col_nm = paste0(ts_fut_col_nm, "_start"),
      ts_fut_stop_col_nm = paste0(ts_fut_col_nm, "_stop")
    )

    # @codedoc_comment_block popEpi:::surv_aggregate_one_stratum__
    #    * There is a pre-defined table of expressions that are evaluated
    #      and added as new columns in the split data if they appear in any
    #      of the `aggre_exprs`. See below for the table.
    #
    #    * Table of expressions which create new columns into split data:
    #
    # ${paste0(knitr::kable(split_lexis_column_exprs_table__()), collapse = "\n")}
    # @codedoc_comment_block popEpi:::surv_aggregate_one_stratum__
    add_expr_list <- SPLIT_LEXIS_COLUMN_EXPRS__[intersect(
      expr_obj_nms,
      names(SPLIT_LEXIS_COLUMN_EXPRS__)
    )]
    lapply(names(add_expr_list), function(col_nm) {
      expr <- add_expr_list[[col_nm]]
      expr <- substitute(
        substitute(expr, substitute_env),
        list(expr = expr)
      )
      expr <- eval(expr, list(substitute_env = substitute_env))
      data.table::set(
        x = work_dt,
        j = col_nm,
        value = eval(expr, work_dt, add_expr_eval_env)
      )
      NULL
    })
    data.table::setDT(work_dt)
  })
  data.table::setkeyv(work_dt, c(data.table::key(work_dt), "box_id"))
  agg_expr <- substitute(
    dt_stratum_subset_split[
      j = EXPR,
      keyby = "box_id"
    ],
    list(
      EXPR = as.call(c(quote(list), aggre_exprs))
    )
  )
  env <- new.env(parent = call_env)
  env[["call_env"]] <- call_env
  env[["eval_env"]] <- eval_env
  env[["stratum_eval_env"]] <- stratum_eval_env
  env[["dt_stratum_subset_split"]] <- work_dt
  out <- eval(agg_expr, envir = work_dt, enclos = env)
  out <- out[
    i = box_dt,
    on = "box_id"
  ]
  data.table::setcolorder(
    out,
    c(names(box_dt), setdiff(names(out), names(box_dt)))
  )
  data.table::setkeyv(out, names(box_dt))
  if (nrow(work_dt) == 0) {
    data.table::set(
      x = out,
      j = setdiff(names(out), names(box_dt)),
      value = NA
    )
  }
  # @codedoc_comment_block popEpi:::surv_aggregate_one_stratum__
  #    * It can happen that e.g. a survival interval has absolutely no data in
  #      it. Especially in sparse data and with delayed entry. For the
  #      pre-specified aggregation expressions such as `n_events` we ensure
  #      that empty time scale boxes have value zero. Your custom aggregation
  #      expressions will result in `NA` values in empty time scale boxes.
  # @codedoc_comment_block popEpi:::surv_aggregate_one_stratum__
  lapply(intersect(names(out), names(SURV_AGGRE_EXPRS__)), function(col_nm) {
    data.table::set(
      x = out,
      i = which(is.na(out[[col_nm]])),
      j = col_nm,
      value = 0L
    )
    NULL
  })
  return(out[])
}

#' @eval codedoc::pkg_doc_fun(
#'   "popEpi::surv_split_merge_aggregate_by_stratum",
#'   "surv_functions"
#' )
surv_split_merge_aggregate_by_stratum <- function(
  dt,
  breaks,
  aggre_exprs,
  aggre_by = NULL,
  aggre_ts_col_nms = NULL,
  merge_dt = NULL,
  merge_dt_by = NULL,
  merge_dt_harmonisers = NULL,
  subset = NULL,
  weight_col_nm = NULL,
  optional_steps = NULL
) {
  # @codedoc_comment_block surv_arg_dt
  # - `surv_split_merge_aggregate_by_stratum`:
  #   A `Lexis` dataset (`[Epi::Lexis]`).
  # @codedoc_comment_block surv_arg_dt

  assert_is_arg_dt(dt, lexis = TRUE)
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum::breaks
  # @codedoc_insert_comment_block popEpi:::assert_is_arg_breaks
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum::breaks
  assert_is_arg_breaks(breaks, dt)
  assert_is_arg_merge_dt_and_merge_dt_by(
    merge_dt = merge_dt,
    merge_dt_by = merge_dt_by,
    dt = dt,
    mandatory = FALSE
  )
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum::aggre_by
  # @codedoc_insert_comment_block popEpi:::handle_arg_by
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum::aggre_by
  aggre_by <- handle_arg_by(by = aggre_by, dataset = dt)
  #' @param aggre_ts_col_nms `[NULL, character]` (default `NULL`)
  #'
  #' Names of `Lexis` time scales in `dt` by which to aggregate results.
  #'
  #' - `NULL`: Use `names(breaks)`.
  #' - `character`: Aggregate by these time scales. E.g. `"ts_fut"`.
  if (is.null(aggre_ts_col_nms)) {
    aggre_ts_col_nms <- names(breaks)
  } else {
    stopifnot(
      aggre_ts_col_nms %in% names(breaks)
    )
  }
  #' @param aggre_exprs `[character, list]` (no default)
  #'
  #' Defines what is aggregated within every stratum-interval
  #' defined by `aggre_ts_col_nms` and the time scales `aggre_ts_col_nms`.
  #' See **Details** for how and where the expressions are evaluated.
  #'
  #' Each element must be either named and an R expression (see e.g. `[quote]`)
  #' or a character string which identifies the aggregation to perform from
  #' a table of pre-defined expressions within `popEpi` (shown in **Details**).
  #' E.g. `c("n_events", "t_at_risk")`,
  #' `list("n_events", t_at_risk = quote(sum(lex.dur)))`.
  #'
  #' @param weight_col_nm `[NULL, character]` (default `NULL`)
  #'
  #' Name of weight column in `dt` if you want to perform individual weighting.
  #' See **Details** for where this comes into play.
  #'
  #' - `NULL`: No individual weighting is performed.
  #' - `character`: This is the name of the column. E.g. `"my_iw"`.
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
  # `popEpi::surv_split_merge_aggregate_by_stratum` can be used to split `Lexis`
  # (`[Epi::Lexis]`) data, merge something to it after the merge, and
  # then perform an aggregation step. The following steps are performed:
  #
  # - Handle `aggre_exprs` as follows:
  # @codedoc_insert_comment_block popEpi:::handle_arg_aggre_exprs
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
  aggre_exprs <- handle_arg_aggre_exprs(
    aggre_exprs = aggre_exprs,
    weight_col_nm = weight_col_nm
  )

  #' @param optional_steps `[NULL, list]` (default `NULL`)
  #'
  #' Optional steps to perform along the way.
  #'
  #' - `NULL`: No optional steps are performed.
  #' - `list`: Each named element is a function that is called in a specific
  #'   stage of the run. See **Details** for what functions are recognised.
  #'
  stopifnot(
    inherits(optional_steps, c("NULL", "list"))
  )
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum::subset
  # @codedoc_insert_comment_block popEpi:::handle_arg_subset
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum::subset
  subset <- handle_arg_subset()

  eval_env <- environment()
  call_env <- parent.frame(1L)
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
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

  lexis_ts_col_nms <- intersect(
    attr(dt, "time.scales"),
    c(aggre_ts_col_nms, names(breaks), merge_dt_by)
  )
  lexis_col_nms <- c(
    "lex.id", lexis_ts_col_nms, "lex.dur", "lex.Cst", "lex.Xst"
  )
  dt <- data.table::setDT(as.list(dt)[intersect(
    names(dt),
    c(
      lexis_col_nms,
      names(aggre_by),
      merge_dt_by,
      unlist(lapply(aggre_exprs, all.vars))
    )
  )])
  if (!all(subset)) {
    dt <- dt[(subset), ]
  }
  lexis_set__(dt = dt, lexis_ts_col_nms = lexis_ts_col_nms)

  box_dt <- surv_box_dt__(breaks[aggre_ts_col_nms])

  out <- quote(dt[
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
        dt_stratum_subset <- dt[0L, ]
      } else {
        dt_stratum_subset <- .SD
      }
      # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
      #   + Run
      #     `popEpi::splitMulti` on the subset of `dt` which contains data from
      #     the current stratum.
      # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
      dt_stratum_subset <- data.table::setDT(as.list(dt_stratum_subset))
      lexis_set__(dt = dt_stratum_subset, lexis_ts_col_nms = lexis_ts_col_nms)
      dt_stratum_subset_split <- surv_split__(
        dt = dt_stratum_subset,
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
      #     `merge_dt_harmonisers`, if `merge_dt` has been supplied.
      # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
      if (!is.null(merge_dt)) {
        surv_merge(
          dt = dt_stratum_subset_split,
          merge_dt = merge_dt,
          merge_dt_by = merge_dt_by,
          merge_dt_harmonisers = merge_dt_harmonisers
        )
      }
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
      #   + Evaluate `aggre_exprs` as follows:
      # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
      out <- surv_aggregate_one_stratum__(
        dt_stratum_subset_split = dt_stratum_subset_split,
        box_dt = box_dt,
        aggre_exprs = aggre_exprs,
        eval_env = eval_env,
        call_env = call_env,
        stratum_eval_env = stratum_eval_env
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
    .SDcols = names(dt),
    #' @importFrom data.table .EACHI
    keyby = .EACHI
  ])
  if (is.null(aggre_by)) {
    out[c("i", "on", "keyby")] <- NULL
  }
  out <- eval(out)
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
  # - After every stratum has been processed, set proper `data.table`
  #   attributes on the resulting big table and call `data.table::setkeyv`
  #   with `cols = c(names(aggre_by), "box_id")`. We store the metadata
  #   `list(stratum_col_nms, value_col_nms)` into the attribute named
  #   `surv_split_merge_aggregate_by_stratum_meta`, where
  #   `stratum_col_nms = names(aggre_by)`, `ts_col_nms = aggre_ts_col_nms`, and
  #   `value_col_nms` are the names of the columns resulting from `aggre_exprs`.
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
  # to clear Epi attributes
  out <- as.list(out)
  attributes(out) <- attributes(out)["names"]
  data.table::setDT(out)
  data.table::setkeyv(out, c(names(aggre_by), "box_id"))
  data.table::setattr(
    out,
    "surv_split_merge_aggregate_by_stratum_meta",
    list(
      stratum_col_nms = names(aggre_by),
      ts_col_nms = aggre_ts_col_nms,
      value_col_nms = setdiff(names(out), c(names(aggre_by), names(box_dt)))
    )
  )
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
  # - Run
  #   `optional_steps[["post_aggregation"]](eval_env = eval_env, call_env = call_env)`
  #   if that `optional_steps` element exists.
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
  if ("post_aggregation" %in% names(optional_steps)) {
    optional_steps[["post_aggregation"]](
      eval_env = eval_env,
      call_env = call_env
    )
  }
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
  # - Return a `data.table` with stratum columns as specified via
  #   `aggre_by` and value columns as specified via `aggre_exprs`.
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
  # @codedoc_comment_block return(popEpi::surv_split_merge_aggregate_by_stratum)
  # Returns a `data.table` with stratum columns as specified via
  # `aggre_by` and value columns as specified via `aggre_exprs`.
  # @codedoc_comment_block return(popEpi::surv_split_merge_aggregate_by_stratum)
  return(out[])
}
