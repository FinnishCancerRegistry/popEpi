surv_aggregate_one_stratum__ <- function(
  sub_dt,
  box_dt,
  aggre_expr,
  call_env,
  eval_env,
  stratum_eval_env
) {
  assert_is_arg_dt(sub_dt, lexis = TRUE)
  # `sub_dt` should be keyed by e.g. c("lex.id", "ts_cal", "ts_fut") or
  # c("lex.id", "ts_fut") or c("lex.id", "ts_fut", "ts_cal") etc if
  # `box_ts_col_nms == "ts_fut"`. in particular `sub_dt` must not be keyed
  # by anything weird before the time scales. after the time scales it can be
  # keyed by whatever.
  box_ts_col_nms <- names(box_dt)[grepl("_stop$", names(box_dt))]
  box_ts_col_nms <- sub("_stop$", "", box_ts_col_nms)
  lexis_ts_col_nms <- attr(sub_dt, "time.scales")
  stopifnot(
    data.table::key(sub_dt)[1] == "lex.id",
    length(data.table::key(sub_dt)) >= length(box_ts_col_nms) + 1L,
    data.table::key(sub_dt)[2:(length(box_ts_col_nms) + 1L)] %in% union(
      box_ts_col_nms, lexis_ts_col_nms
    )
  )
  assert_is_arg_box_dt(box_dt)
  assert_is_arg_aggre_expr(aggre_expr)
  stopifnot(
    is.environment(call_env),
    is.environment(eval_env),
    is.environment(stratum_eval_env)
  )

  expr_obj_nms <- all.vars(aggre_expr)
  work_dt <- data.table::setDT(as.list(sub_dt))
  data.table::setkeyv(work_dt, data.table::key(sub_dt))
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
      work_dt[
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
    x = work_dt,
    j = "box_id",
    value = surv_box_id__(dt = work_dt, box_dt = box_dt)
  )
  add_expr_eval_env <- environment()
  local({
    ts_fut_col_nm <- eval_env[["aggre_ts_col_nms"]][
      length(eval_env[["aggre_ts_col_nms"]])
    ]
    add_expr_list <- list(
      # tricky. ts_fut and box_id live in work_dt and box_dt has to be reachable
      # from stratum_eval_env.
      in_follow_up_at_interval_start = substitute(
        ts_fut == box_dt[[ts_fut_start_col_nm]][box_id],
        list(
          ts_fut = parse(text = ts_fut_col_nm)[[1]],
          ts_fut_start_col_nm = paste0(ts_fut_col_nm, "_start")
        )
      ),
      entered_late_during_interval = substitute(
        ts_fut > box_dt[[ts_fut_start_col_nm]][box_id],
        list(
          ts_fut = parse(text = ts_fut_col_nm)[[1]],
          ts_fut_start_col_nm = paste0(ts_fut_col_nm, "_start")
        )
      ),
      had_event_during_interval = quote(lex.Cst != lex.Xst),
      left_early_during_interval = substitute(
        {
          out <- duplicated(lex.id, fromLast = TRUE)
          out[out] <- lex.Cst[out] == lex.Xst[out]
          out[out] <- round(ts_fut[out] + lex.dur[out], 10) <
            round(box_dt[[ts_fut_stop_col_nm]][box_id[out]], 10)
          out
        },
        list(
          ts_fut = parse(text = ts_fut_col_nm)[[1]],
          ts_fut_stop_col_nm = paste0(ts_fut_col_nm, "_stop")
        )
      ),
      pp = substitute(
        {
          lexis_set__(dt = work_dt, lexis_ts_col_nms = lexis_ts_col_nms)
          out <- surv_pohar_perme_weight__(
            dt = work_dt,
            ts_fut_breaks = eval_env[["breaks"]][[
              eval_env[["aggre_ts_col_nms"]]
            ]],
            ts_fut_col_nm = ts_fut_col_nm,
            hazard_col_nm = hazard_col_nm
          )
          data.table::setDT(work_dt)
          out
        },
        list(
          ts_fut_col_nm = ts_fut_col_nm,
          hazard_col_nm = setdiff(
            names(eval_env[["merge_dt"]]),
            eval_env[["merge_dt_by"]]
          )[1]
        )
      )
    )
    lapply(intersect(names(add_expr_list), expr_obj_nms), function(col_nm) {
      data.table::set(
        x = work_dt,
        j = col_nm,
        value = eval(add_expr_list[[col_nm]], work_dt, add_expr_eval_env)
      )
      NULL
    })
    data.table::setDT(work_dt)
  })
  data.table::setkeyv(work_dt, c(data.table::key(work_dt), "box_id"))
  agg_expr <- substitute(
    sub_dt[
      j = EXPR,
      keyby = "box_id"
    ],
    list(
      EXPR = aggre_expr
    )
  )
  env <- new.env(parent = call_env)
  env[["call_env"]] <- call_env
  env[["eval_env"]] <- eval_env
  env[["stratum_eval_env"]] <- stratum_eval_env
  env[["sub_dt"]] <- work_dt
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
  return(out[])
}

#' @eval codedoc::pkg_doc_fun(
#'   "popEpi::surv_split_merge_aggregate_by_stratum",
#'   "surv_functions"
#' )
surv_split_merge_aggregate_by_stratum <- function(
  dt,
  breaks,
  merge_dt = NULL,
  merge_dt_by = NULL,
  merge_dt_harmonisers = NULL,
  aggre_by = NULL,
  aggre_ts_col_nms = NULL,
  aggre_expr,
  subset = NULL,
  optional_steps = NULL
) {
  # @codedoc_comment_block surv_arg_dt
  # - `surv_split_merge_aggregate_by_stratum`:
  #   A `Lexis` dataset (`[Epi::Lexis]`).
  # @codedoc_comment_block surv_arg_dt

  assert_is_arg_dt(dt, lexis = TRUE)
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
  }
  #' @param aggre_expr `[call]` (no default)
  #'
  #' A quoted (`[quote]`) R expression. When this is evaluated within a stratum,
  #' the desired summary statistics are produced for that stratum. One
  #' evaluation stratum is based on `aggre_by` and `aggre_ts_col_nms`, and might
  #' be e.g. `list(sex = 1, ag = 6, ts_fut_start = 3.0, ts_fut_stop = 4.0)`
  #' with `aggre_by = c("sex", "ag")` and `aggre_ts_col_nms = "ts_fut"`.
  #' The data
  #' for the stratum have been split and `merge_dt` has been merged when
  #' `aggre_expr` is evaluated. E.g.
  #' `quote(list(n_at_risk = sum(at_risk), n_events = sum(lex.Cst != lex.Xst)))`
  #' . See **Details** for what kinds of expressions are possible.
  #'
  #' - `NULL`: Use `names(breaks)`.
  #' - `character`: Aggregate by these time scales. E.g. `"ts_fut"`.
  stopifnot(
    aggre_ts_col_nms %in% names(breaks),
    is.language(aggre_expr),
    inherits(optional_steps, c("NULL", "list"))
  )
  subset <- handle_arg_subset()

  eval_env <- environment()
  call_env <- parent.frame(1L)
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
      all.vars(expr = aggre_expr, functions = FALSE)
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
        sub_dt <- dt[0L, ]
      } else {
        sub_dt <- .SD
      }
      # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
      #   + Run
      #     `popEpi::splitMulti` on the subset of `dt` which contains data from
      #     the current stratum.
      # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
      sub_dt <- data.table::setDT(as.list(sub_dt))
      lexis_set__(dt = sub_dt, lexis_ts_col_nms = lexis_ts_col_nms)
      sub_dt <- surv_split__(
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
      #     `merge_dt_harmonisers`, if `merge_dt` has been supplied.
      # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
      if (!is.null(merge_dt)) {
        surv_merge(
          dt = sub_dt,
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
      #   + Evaluate `aggre_expr` in the context of the split data
      #     with merged-in additional data. The enclosing environment is
      #     `call_env`. See `?eval`. This results in a `data.table` that
      #     contains one row per interval of `aggre_ts_col_nms`.
      # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
      out <- surv_aggregate_one_stratum__(
        sub_dt = sub_dt,
        box_dt = box_dt,
        aggre_expr = aggre_expr,
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
  #   `value_col_nms` are the names of the columns resulting from `aggre_expr`.
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
  #   `aggre_by` and value columns as specified via `aggre_expr`.
  # @codedoc_comment_block popEpi::surv_split_merge_aggregate_by_stratum
  # @codedoc_comment_block return(popEpi::surv_split_merge_aggregate_by_stratum)
  # Returns a `data.table` with stratum columns as specified via
  # `aggre_by` and value columns as specified via `aggre_expr`.
  # @codedoc_comment_block return(popEpi::surv_split_merge_aggregate_by_stratum)
  return(out[])
}
