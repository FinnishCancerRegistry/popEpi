lexis_split_column_expr_table_doc__ <- function() {
  dt <- get_internal_dataset("lexis_split_column_expr_table__")
  dt <- data.table::data.table(
    "Name" = doc_verb__(dt[["name"]]),
    "Explanation" = dt[["info"]],
    "Expression" = doc_verb__(dt[["expr"]])
  )
  return(dt)
}

lexis_aggregate_one_stratum__ <- function(
  lexis_stratum_subset_split,
  box_dt,
  aggre_exprs,
  split_lexis_column_exprs = NULL,
  call_env,
  eval_env,
  stratum_eval_env
) {
  assert_is_arg_lexis(lexis_stratum_subset_split, dt = FALSE)
  # `lexis_stratum_subset_split` should be keyed by e.g.
  # c("lex.id", "ts_cal", "ts_fut") or
  # c("lex.id", "ts_fut") or c("lex.id", "ts_fut", "ts_cal") etc if
  # `box_ts_col_nms == "ts_fut"`. in particular `lexis_stratum_subset_split`
  # must not be keyed
  # by anything weird before the time scales. after the time scales it can be
  # keyed by whatever.
  box_ts_col_nms <- names(box_dt)[grepl("_stop$", names(box_dt))]
  box_ts_col_nms <- sub("_stop$", "", box_ts_col_nms)
  lexis_ts_col_nms <- attr(lexis_stratum_subset_split, "time.scales")
  if (nrow(lexis_stratum_subset_split) > 0) {
    stopifnot(
      data.table::key(lexis_stratum_subset_split)[1] == "lex.id",
      length(data.table::key(lexis_stratum_subset_split)) >=
        length(box_ts_col_nms) + 1L,
      data.table::key(lexis_stratum_subset_split)[
        2:(length(box_ts_col_nms) + 1L)
      ] %in%
        union(box_ts_col_nms, lexis_ts_col_nms)
    )
  }
  assert_is_arg_box_dt(box_dt, ts_col_nms = box_ts_col_nms)
  assert_is_arg_aggre_exprs(aggre_exprs)
  assert_is_arg_split_lexis_column_exprs(split_lexis_column_exprs)
  stopifnot(
    is.environment(call_env),
    is.environment(eval_env),
    is.environment(stratum_eval_env)
  )

  work_dt <- lexis_to_lexis_dt__(lexis_stratum_subset_split)
  # @codedoc_comment_block popEpi:::lexis_aggregate_one_stratum__
  #        + First collect variables mentioned in `aggre_exprs` using
  #          `[all.vars]`.
  # @codedoc_comment_block popEpi:::lexis_aggregate_one_stratum__
  expr_obj_nms <- unique(unlist(lapply(aggre_exprs, all.vars)))
  lexis_box_id__(lexis = work_dt, box_dt = box_dt)
  add_expr_eval_env <- environment()
  local({
    ts_fut_col_nm <- utils::tail(box_ts_col_nms, 1)
    substitute_env <- list(
      ts_fut = parse(text = ts_fut_col_nm)[[1]],
      ts_fut_col_nm = ts_fut_col_nm,
      ts_fut_start_col_nm = paste0(ts_fut_col_nm, "_start"),
      ts_fut_stop_col_nm = paste0(ts_fut_col_nm, "_stop")
    )

    # @codedoc_comment_block popEpi:::lexis_aggregate_one_stratum__
    #        + There is a pre-defined table of expressions that are evaluated
    #          and added as new columns in the split data if they appear in any
    #          of the `aggre_exprs`. See below for the table.
    #
    #        + Table of expressions which create new columns into split data:
    #
    # ${paste0(knitr::kable(lexis_split_column_expr_table_doc__()), collapse = "\n")}
    # @codedoc_comment_block popEpi:::lexis_aggregate_one_stratum__
    lexis_split_column_expr_list__ <-
      get_internal_dataset("lexis_split_column_expr_list__")
    add_expr_list <- lexis_split_column_expr_list__[intersect(
      expr_obj_nms,
      names(lexis_split_column_expr_list__)
    )]
    add_expr_list <- c(add_expr_list, split_lexis_column_exprs)
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
      if (col_nm == "in_follow_up_at_interval_stop") {
        browser()
      }
      NULL
    })
    data.table::setDT(work_dt)
  })
  data.table::setkeyv(work_dt, c(data.table::key(work_dt), "box_id"))
  agg_expr <- substitute(
    lexis_stratum_subset_split[
      j = EXPR,
      keyby = "box_id"
    ],
    list(
      EXPR = as.call(c(quote(list), aggre_exprs))
    )
  )
  if (".SD" %in% all.vars(agg_expr)) {
    agg_expr[[".SD"]] <- quote(names(lexis_stratum_subset_split))
  }
  env <- new.env(parent = call_env)
  env[["call_env"]] <- call_env
  env[["eval_env"]] <- eval_env
  env[["stratum_eval_env"]] <- stratum_eval_env
  env[["lexis_stratum_subset_split"]] <- work_dt
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
  # @codedoc_comment_block popEpi:::lexis_aggregate_one_stratum__
  #        + It can happen that e.g. a survival interval has absolutely no data in
  #          it. Especially in sparse data and with delayed entry. For the
  #          pre-specified aggregation expressions such as `n_events` we ensure
  #          that empty time scale boxes have value zero. Your custom aggregation
  #          expressions will result in `NA` values in empty time scale boxes.
  # @codedoc_comment_block popEpi:::lexis_aggregate_one_stratum__
  lexis_aggre_expr_list__ <- get_internal_dataset("lexis_aggre_expr_list__")
  lapply(
    intersect(names(out), names(lexis_aggre_expr_list__)),
    function(col_nm) {
      data.table::set(
        x = out,
        i = which(is.na(out[[col_nm]])),
        j = col_nm,
        value = 0L
      )
      NULL
    }
  )
  return(out[])
}

#' @title Split, Merge, and Aggregate a `Lexis` Object
#' @description
#' Function(s) which split, merge, and aggregate a `Lexis` object in one go.
#' @eval codedoc::pkg_doc_fun(
#'   "popEpi::lexis_split_merge_aggregate_by_stratum"
#' )
#' @family Lexis_functions
#' @examples
#'
#' # popEpi::split_merge_aggregate_by_stratum
#' make_pm <- function() {
#'   pm <- data.table::copy(popEpi::popmort)
#'   data.table::setnames(
#'     pm,
#'     c("year", "agegroup", "haz"),
#'     c("ts_cal", "ts_age", "h_exp")
#'   )
#'   data.table::setkeyv(pm, c("sex", "ts_cal", "ts_age"))
#'   data.table::setkeyv(pm, c("sex", "ts_cal", "ts_age"))
#'   return(pm[])
#' }
#'
#' make_column_icss_ag <- function(age) {
#'   cut(
#'     age,
#'     breaks = c(0, 60, 70, 80, Inf),
#'     right = FALSE,
#'     labels = c("0-59", "60-69", "70-79", "80+")
#'   )
#' }
#'
#' make_standard_weight_dt <- function() {
#'   return(popEpi::ICSS[
#'     j = list(
#'       weight = as.double(sum(.SD[["ICSS1"]]))
#'     ),
#'     keyby = list(
#'       icss_ag = make_column_icss_ag(popEpi::ICSS[["age"]])
#'     )
#'   ][])
#' }
#'
#' make_sire <- function() {
#'   sire <- popEpi::sire
#'   sire <- sire[
#'     sire[["dg_date"]] < sire[["ex_date"]] &
#'       data.table::between(
#'         sire[["ex_date"]],
#'         as.Date("1999-01-01"),
#'         as.Date("2003-12-31"),
#'         incbounds = TRUE
#'       ) &
#'       (get.yrs(sire[["ex_date"]]) - get.yrs(sire[["bi_date"]])) < 100
#'   ]
#'   sire[j = "my_stratum" := sample(2L, size = nrow(sire), replace = TRUE)]
#'   sire <- sire[
#'     j = .SD[as.integer(seq(1L, .N, length.out = 50L))],
#'     keyby = "my_stratum"
#'   ]
#'   # you can also use popEpi::Lexis_dt
#'   sire <- Epi::Lexis(
#'     entry = list(
#'       ts_cal = popEpi::get.yrs(dg_date),
#'       ts_age = popEpi::get.yrs(dg_date) - popEpi::get.yrs(bi_date),
#'       ts_fut = 0.0
#'     ),
#'     duration = popEpi::get.yrs(ex_date) - popEpi::get.yrs(dg_date),
#'     entry.status = 0L,
#'     exit.status = status,
#'     data = sire
#'   )
#'   sire[["icss_ag"]] <- make_column_icss_ag(sire[["dg_age"]])
#'   sire[["individual_weight"]] <- popEpi::surv_individual_weights(
#'     df = sire,
#'     standard_weight_dt = wdt
#'   )
#'   sire
#' }
#'
#' pm <- make_pm()
#' wdt <- make_standard_weight_dt()
#' sire <- make_sire()
#'
#' # note that we use here 1-year survival intervals which are too wide for
#' # practical survival estimation. we also aggregate for period analysis
#' # for demonstration purposes.
#' bl <- list(ts_cal = c(1999, 2004), ts_fut = 0:5)
#'
#' # using some pre-defined aggregation expressions
#' agdt <- popEpi::lexis_split_merge_aggregate_by_stratum(
#'   lexis = sire,
#'   breaks = bl,
#'   aggre_exprs = c("n_events", "t_at_risk", "n_events_[0, 1]"),
#'   aggre_by = "my_stratum"
#' )
#' stopifnot(
#'   c("n_events", "t_at_risk", "n_events_[0, 1]") %in% names(agdt)
#' )
#'
#' # using some pre-defined aggregation expressions + using expected hazard.
#' agdt <- popEpi::lexis_split_merge_aggregate_by_stratum(
#'   lexis = sire,
#'   breaks = bl,
#'   aggre_exprs = c(
#'     "n_events", "t_at_risk", "n_events_[0, 1]",
#'     "n_events_exp_e2"
#'   ),
#'   aggre_by = "my_stratum",
#'   merge_dt = pm,
#'   merge_dt_by = c("sex", "ts_cal", "ts_age")
#' )
#'
#' # using custom expressions --- `h_exp` is merged from `merge_dt`
#' agdt <- popEpi::lexis_split_merge_aggregate_by_stratum(
#'   lexis = sire,
#'   breaks = bl,
#'   aggre_exprs = list(
#'     "n_events", "t_at_risk", "n_events_[0, 1]",
#'     "n_events_exp_e2",
#'     "my_n_events" = quote(sum(lex.Cst == 0 & lex.Xst != 0)),
#'     "my_n_events_exp_e2" = quote(sum(h_exp * lex.dur))
#'   ),
#'   aggre_by = "my_stratum",
#'   merge_dt = pm,
#'   merge_dt_by = c("sex", "ts_cal", "ts_age")
#' )
#' stopifnot(
#'   agdt[["my_n_events"]] == agdt[["n_events"]],
#'   all.equal(agdt[["my_n_events_exp_e2"]], agdt[["n_events_exp_e2"]])
#' )
#'
#' # using custom expressions --- some pre-defined variables which are added
#' # automatically
#' agdt <- popEpi::lexis_split_merge_aggregate_by_stratum(
#'   lexis = sire,
#'   breaks = bl,
#'   aggre_exprs = list(
#'     "n_events_pp",
#'     "my_n_events_pp" = quote(sum((lex.Cst == 0 & lex.Xst != 0) * pp)),
#'     "n_at_risk_eff",
#'     "my_n_at_risk_eff" = quote(sum(
#'       in_follow_up_at_interval_start +
#'         0.5 * entered_late_during_interval -
#'         0.5 * left_early_during_interval
#'     )),
#'     "my_n_at_risk_eff_2" = quote({
#'       # custom definition for detecting late entry. in this example we add
#'       # a larger tolerance than is used normally. you can find the normal
#'       # tolerance used in the table of expressions which create columns into
#'       # the split lexis dataset.
#'       ts_fut_floor <- eval_env[["box_dt"]][["ts_fut_start"]][box_id]
#'       distance_from_floor <- (ts_fut - ts_fut_floor)
#'       entered_late_during_interval <- distance_from_floor > 1e-3
#'       sum(
#'         in_follow_up_at_interval_start +
#'           0.5 * entered_late_during_interval -
#'           0.5 * left_early_during_interval
#'       )
#'     })
#'   ),
#'   aggre_by = "my_stratum",
#'   merge_dt = pm,
#'   merge_dt_by = c("sex", "ts_cal", "ts_age")
#' )
#' stopifnot(
#'   all.equal(agdt[["my_n_events_pp"]], agdt[["n_events_pp"]]),
#'   all.equal(agdt[["n_at_risk_eff"]], agdt[["my_n_at_risk_eff"]]),
#'   all.equal(agdt[["n_at_risk_eff"]], agdt[["my_n_at_risk_eff_2"]])
#' )
#'
#' # using custom expressions --- also a custom column at the split lexis level.
#' # this is for demonstration of what is possible and probably no-one would
#' # need such an h_exp_mean in a real application.
#' agdt <- popEpi::lexis_split_merge_aggregate_by_stratum(
#'   lexis = sire,
#'   breaks = bl,
#'   aggre_exprs = list(
#'     "my_stat" = quote(sum(lex.dur * h_exp_mean))
#'   ),
#'   aggre_by = "my_stratum",
#'   merge_dt = pm,
#'   merge_dt_by = c("sex", "ts_cal", "ts_age"),
#'   split_lexis_column_exprs = list(
#'     h_exp_mean = quote({
#'       dt <- data.table::setDT(list(lex.id = lex.id, h_exp = h_exp))
#'       h_exp_lag1 <- dt[
#'         j = list(lag1 = data.table::shift(h_exp, n = 1L, type = "lag")),
#'         by = "lex.id"
#'       ][["lag1"]]
#'       h_exp_lag1[is.na(h_exp_lag1)] <- h_exp[is.na(h_exp_lag1)]
#'       h_exp_lead1 <- dt[
#'         j = list(lead1 = data.table::shift(h_exp, n = 1L, type = "lead")),
#'         by = "lex.id"
#'       ][["lead1"]]
#'       h_exp_lead1[is.na(h_exp_lead1)] <- h_exp[is.na(h_exp_lead1)]
#'       (h_exp + h_exp_lag1 + h_exp_lead1) / 3
#'     })
#'   )
#' )
#' stopifnot(
#'   "my_stat" %in% names(agdt),
#'   !is.na(agdt[["my_stat"]])
#' )
#'
#' # this function was written for survival estimation but is is in fact more
#' # general. for instance we can look at a calendar year / age grid.
#' agdt <- popEpi::lexis_split_merge_aggregate_by_stratum(
#'   lexis = sire,
#'   breaks = list(ts_cal = 1999:2004, ts_age = seq(0, 100, 10)),
#'   aggre_exprs = list(
#'     "n_events_exp_e2"
#'   ),
#'   merge_dt = pm,
#'   merge_dt_by = c("sex", "ts_cal", "ts_age")
#' )
#' stopifnot(
#'   c("ts_cal_start", "ts_age_start") %in% names(agdt)
#' )
#' agdt_wide <- data.table::dcast(
#'   data = agdt,
#'   formula = ts_age_start ~ ts_cal_start,
#'   value.var = "n_events_exp_e2"
#' )
#' # print(round(agdt_wide, 2))
#' # Key: <ts_age_start>
#' #     ts_age_start  1999  2000  2001  2002  2003
#' #            <num> <num> <num> <num> <num> <num>
#' #  1:            0  0.00  0.00  0.00  0.00  0.00
#' #  2:           10  0.00  0.00  0.00  0.00  0.00
#' #  3:           20  0.00  0.00  0.00  0.00  0.00
#' #  4:           30  0.00  0.00  0.00  0.00  0.00
#' #  5:           40  0.00  0.00  0.00  0.00  0.00
#' #  6:           50  0.01  0.01  0.01  0.00  0.00
#' #  7:           60  0.04  0.04  0.03  0.01  0.02
#' #  8:           70  0.33  0.28  0.31  0.16  0.05
#' #  9:           80  1.36  1.04  0.89  0.48  0.20
#' # 10:           90  0.39  0.29  0.47  0.37  0.56
#'
#' # this is what happens when there is an empty interval
#' agdt <- popEpi::lexis_split_merge_aggregate_by_stratum(
#'   lexis = popEpi::Lexis_dt(
#'     entry = list(ts_fut = c(0.0, 3.5)),
#'     exit = list(ts_fut = c(1.5, 4.5)),
#'     entry.status = 0L,
#'     exit.status = 1L
#'   ),
#'   breaks = list(ts_fut = 0:5),
#'   aggre_exprs = list(
#'     "t_at_risk",
#'     "n_events",
#'     my_n_events = quote(sum(lex.Cst == 0 & lex.Xst != 0))
#'   )
#' )
#' stopifnot(
#'   identical(agdt[["t_at_risk"]], c(1.0, 0.5, 0.0, 0.5, 0.5)),
#'   identical(agdt[["n_events"]], c(0L, 1L, 0L, 0L, 1L)),
#'   identical(agdt[["my_n_events"]], c(0L, 1L, NA, 0L, 1L))
#' )
#'
#' # this is what happens when an empty stratum is requested
#' agdt <- popEpi::lexis_split_merge_aggregate_by_stratum(
#'   lexis = sire,
#'   aggre_by = data.table::data.table(sex = 0:2),
#'   breaks = list(ts_fut = 0:5),
#'   aggre_exprs = list(
#'     "t_at_risk",
#'     "my_stat" = quote(sum(lex.dur ^ 2)),
#'     "t_at_risk_squaredd" = quote(sum(lex.dur ^ 2))
#'   )
#' )
#' stopifnot(
#'   nrow(agdt) == length(0:2) * (length(0:5) - 1),
#'   agdt[["t_at_risk"]][!agdt[["sex"]] %in% sire[["sex"]]] == 0,
#'   is.na(agdt[["my_stat"]][!agdt[["sex"]] %in% sire[["sex"]]]),
#'   agdt[["t_at_risk_squared"]][!agdt[["sex"]] %in% sire[["sex"]]] == 0
#' )
#'
#' # and this is what happens when no records are in the dataset
#' agdt <- popEpi::lexis_split_merge_aggregate_by_stratum(
#'   lexis = sire,
#'   subset = rep(FALSE, nrow(sire)),
#'   aggre_by = data.table::data.table(sex = 0:2),
#'   breaks = list(ts_fut = 0:5),
#'   aggre_exprs = list(
#'     "t_at_risk",
#'     "my_stat" = quote(sum(lex.dur ^ 2))
#'   )
#' )
#' stopifnot(
#'   nrow(agdt) == length(0:2) * (length(0:5) - 1),
#'   agdt[["t_at_risk"]] == 0,
#'   is.na(agdt[["my_stat"]])
#' )
#'
#' # and this is what happens when no records are in the dataset
#' agdt <- popEpi::lexis_split_merge_aggregate_by_stratum(
#'   lexis = sire,
#'   subset = rep(FALSE, nrow(sire)),
#'   aggre_by = data.table::data.table(sex = 0:2),
#'   breaks = list(ts_fut = 0:5),
#'   aggre_exprs = list(
#'     "t_at_risk",
#'     "my_stat" = quote(sum(lex.dur ^ 2))
#'   )
#' )
#' stopifnot(
#'   nrow(agdt) == length(0:2) * (length(0:5) - 1),
#'   agdt[["t_at_risk"]] == 0,
#'   is.na(agdt[["my_stat"]])
#' )
lexis_split_merge_aggregate_by_stratum <- function(
  lexis,
  breaks,
  aggre_exprs,
  aggre_by = NULL,
  merge_dt = NULL,
  merge_dt_by = NULL,
  merge_optional_args = NULL,
  subset = NULL,
  weight_col_nm = NULL,
  split_lexis_column_exprs = NULL,
  breaks_collapse_args = NULL,
  optional_steps = NULL
) {
  #' @template param_lexis
  assert_is_arg_lexis(lexis, dt = FALSE)
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum::breaks
  # @codedoc_insert_comment_block popEpi:::assert_is_arg_breaks
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum::breaks
  assert_is_arg_breaks(breaks, lexis)
  assert_is_arg_merge_dt_and_merge_dt_by(
    merge_dt = merge_dt,
    merge_dt_by = merge_dt_by,
    dt = lexis,
    mandatory = FALSE
  )
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum::aggre_by
  # @codedoc_insert_comment_block popEpi:::handle_arg_by
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum::aggre_by
  aggre_by <- handle_arg_by(by = aggre_by, dataset = lexis)
  #' @param aggre_exprs `[character, list]` (no default)
  #'
  #' Defines what is aggregated within every stratum-box
  #' defined via `aggre_by` and `breaks`.
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
  #' Name of weight column in `lexis` if you want to perform individual
  #' weighting.
  #' See **Details** for where this comes into play.
  #'
  #' - `NULL`: No individual weighting is performed.
  #' - `character`: This is the name of the column. E.g. `"my_iw"`.
  assert_is_arg_weight_col_nm(weight_col_nm)
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
  # `popEpi::lexis_split_merge_aggregate_by_stratum` can be used to split `Lexis`
  # (`[Epi::Lexis]`) data, merge something to it after the merge, and
  # then perform an aggregation step. The following steps are performed:
  #
  # - Handle `aggre_exprs` as follows:
  # @codedoc_insert_comment_block popEpi:::handle_arg_aggre_exprs
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
  aggre_exprs <- handle_arg_aggre_exprs(
    aggre_exprs = aggre_exprs,
    weight_col_nm = weight_col_nm
  )

  #' @param split_lexis_column_exprs `[NULL, list]`
  #'
  #' Additional columns to create in `Lexis` data after splitting and before
  #' aggregation. Any column you create this way can be the used in an
  #' aggregation expression.
  #'
  #' - `NULL`: Don't create additional columns.
  #' - `list`: Each element is named and an R expression object, e.g.
  #'   `list(my_col = quote(my_function(lex.Cst, lex.Xst)))`.
  #'   See **Details** for more information on when and how these expressions
  #'   are evaluated.
  assert_is_arg_split_lexis_column_exprs(split_lexis_column_exprs)

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
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum::subset
  # @codedoc_insert_comment_block popEpi:::handle_arg_subset
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum::subset
  subset <- handle_arg_subset(dataset_nm = "lexis")
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
  # - If `weight_col_nm` was supplied, include in `subset` only records where
  #   `lexis[[weight_col_nm]] > 0`.
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
  if (!is.null(weight_col_nm)) {
    subset <- subset & lexis[[weight_col_nm]] > 0
  }

  eval_env <- environment()
  call_env <- parent.frame(1L)
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
  # - Call
  #   `optional_steps[["on_entry"]](eval_env = eval_env, call_env = call_env)`
  #   if that `optional_steps` element exists.
  #   `eval_env` is the temporary evaluation environment of
  #   `popEpi::lexis_split_merge_aggregate_by_stratum` which contains all
  #   contains all the arguments of
  #   `popEpi::lexis_split_merge_aggregate_by_stratum` and `call_env` is the environment
  #   where it was called.
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
  if ("on_entry" %in% names(optional_steps)) {
    optional_steps[["on_entry"]](eval_env = eval_env, call_env = call_env)
  }
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
  # - Call
  #   `on.exit(optional_steps[["on_exit"]](eval_env = eval_env, call_env = call_env))`
  #   if that `optional_steps` element exists.
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
  if ("on_exit" %in% names(optional_steps)) {
    on.exit(
      optional_steps[["on_exit"]](eval_env = eval_env, call_env = call_env)
    )
  }

  lexis_ts_col_nms <- intersect(
    Epi::timeScales(lexis),
    c(names(breaks), merge_dt_by)
  )
  lexis_col_nms <- c(
    "lex.id", lexis_ts_col_nms, "lex.dur", "lex.Cst", "lex.Xst"
  )
  lexis_dt <- lexis_to_lexis_dt__(
    lexis = lexis,
    select = intersect(
      c(
        lexis_col_nms,
        names(aggre_by),
        merge_dt_by,
        unlist(lapply(aggre_exprs, all.vars))
      ),
      names(lexis)
    )
  )
  lexis_crop(lexis = lexis_dt, breaks = breaks)
  subset <- subset & !is.na(lexis_dt[["lex.dur"]]) # due to crop

  #' @param merge_dt
  #' Passed to `[lexis_merge]`.
  #' @param merge_dt_by
  #' Passed to `[lexis_merge]`.
  #' @param merge_optional_args `[NULL, list]` (default `NULL`)
  #'
  #' Each element passed to `[lexis_merge]`.
  #' E.g. `list(merge_dt_harmonisers = my_harmonisers)`.
  stopifnot(
    inherits(merge_optional_args, c("NULL", "list")),
    names(merge_optional_args) %in% names(formals(popEpi::lexis_merge))
  )
  lexis_merge_arg_list <- as.list(merge_optional_args)
  lexis_merge_arg_list[c("merge_dt", "merge_dt_by")] <- list(
    merge_dt,
    merge_dt_by
  )

  box_dt <- lexis_box_dt__(breaks)
  split_ts_col_nm <- names(breaks)[length(breaks)]
  stratum_box_dt <- local({
    stratum_breaks <- breaks
    stratum_breaks[[split_ts_col_nm]] <- range(
      stratum_breaks[[split_ts_col_nm]]
    )
    lexis_box_dt__(stratum_breaks)
  })
  out_expr <- quote(lexis_dt[
    j = {
      # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
      # - For each stratum in `aggre_by`:
      #   + Run
      #     `optional_steps[["stratum_on_entry"]](stratum_eval_env = stratum_eval_env, eval_env = eval_env, call_env = call_env)`
      #     if that `optional_steps` element exists.
      #     `stratum_eval_env` is the environment where the stratum-specific
      #     steps are performed.
      # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
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
        lexis_stratum_subset <- lexis_dt[0L, ]
      } else {
        lexis_stratum_subset <- .SD
      }
      lexis_stratum_subset <- data.table::setDT(as.list(lexis_stratum_subset))
      lexis_dt_set__(
        lexis = lexis_stratum_subset,
        lexis_ts_col_nms = lexis_ts_col_nms
      )
      out <- lapply(seq_len(nrow(stratum_box_dt)), function(stratum_box_no) {
        # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
        #   + Loop over every box defined by all other time scales except the
        #     last one. E.g. if
        #     `breaks = list(ts_cal = c(2001, 2004, 2007), ts_fut = 0:5)`
        #     then we perform what follows below separately for the boxes
        #     `]2001, 2004]`, `]2004, 2007]`.
        #     * Run
        #       `optional_steps[["stratum_pre_split"]](stratum_eval_env = stratum_eval_env, eval_env = eval_env, call_env = call_env)`
        #       if that `optional_steps` element exists.
        # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
        if ("stratum_pre_split" %in% names(optional_steps)) {
          optional_steps[["stratum_pre_split"]](
            stratum_eval_env = stratum_eval_env,
            eval_env = eval_env,
            call_env = call_env
          )
        }
        # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
        #     * Using a subset `lexis` containing data only for the current
        #       stratum (e.g. `sex = 0`), drop data outside the current
        #       stratum box (e.g. `ts_cal` outside of
        #       `]2001, 2004]`). Also crop follow-up to the end of the box.
        #       This does not split the data yet.
        # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
        stratum_box_breaks <- lapply(names(breaks), function(ts_col_nm) {
          c(
            stratum_box_dt[[paste0(ts_col_nm, "_start")]][[stratum_box_no]],
            stratum_box_dt[[paste0(ts_col_nm, "_stop")]][[stratum_box_no]]
          )
        })
        names(stratum_box_breaks) <- names(breaks)
        lexis_stratum_subset <- lexis_drop(
          lexis = lexis_stratum_subset,
          breaks = stratum_box_breaks
        )
        lexis_stratum_subset <- lexis_crop(
          lexis = lexis_stratum_subset,
          breaks = stratum_box_breaks,
          inplace = TRUE
        )
        stratum_box_breaks[[split_ts_col_nm]] <- breaks[[split_ts_col_nm]]
        # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
        #     * If `!is.null(breaks_collapse_args)`, run
        #       `popEpi::lexis_breaks_collapse_1d` on the last element of
        #       `breaks`. Arguments `lexis` and `breaks` are set
        #       automatically. This makes it possible to "collapse" the breaks
        #       for each stratum and stratum box combination separately.
        # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
        #' @param breaks_collapse_args `[NULL, list]` (default `NULL`)
        #'
        #' Optional, if you supply this argument then
        #' `[lexis_breaks_collapse_1d]` will be called for each stratum defined
        #' via `aggre_by` separately.
        #' 
        #' - `NULL`: `[lexis_breaks_collapse_1d]` is not called.
        #' - `list`: E.g. `list(mandatory_breaks = 0:5)`.
        if (!is.null(breaks_collapse_args)) {
          breaks_collapse_args <- as.list(breaks_collapse_args)
          breaks_collapse_args[["lexis"]] <- lexis_stratum_subset
          breaks_collapse_args[["breaks"]] <- stratum_box_breaks[
            split_ts_col_nm
          ]
          stratum_box_breaks[[split_ts_col_nm]] <- call_with_arg_list__(
            popEpi::lexis_breaks_collapse_1d,
            breaks_collapse_args
          )
        }

        # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
        #     * Run `[splitMulti]` for the second time, this time splitting the
        #       data by the last time scale in `breaks`. Remember that we are
        #       inside the box defined by the other time scales already.
        # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
        lexis_stratum_subset_split <- surv_split__(
          lexis = lexis_stratum_subset,
          breaks = stratum_box_breaks[split_ts_col_nm],
          merge = TRUE
        )
        # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
        #     * Run
        #       `optional_steps[["stratum_post_split"]](stratum_eval_env = stratum_eval_env, eval_env = eval_env, call_env = call_env)`
        #       if that `optional_steps` element exists.
        # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
        if ("stratum_post_split" %in% names(optional_steps)) {
          optional_steps[["stratum_post_split"]](
            stratum_eval_env = stratum_eval_env,
            eval_env = eval_env,
            call_env = call_env
          )
        }
        # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
        #     * Run
        #       `[lexis_merge]` with `merge_dt`, `merge_dt_by`, and
        #       `merged_optional_args`, if `merge_dt` has been supplied.
        # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
        if (!is.null(merge_dt)) {
          lexis_merge_arg_list[["lexis"]] <- lexis_stratum_subset_split
          call_with_arg_list__(lexis_merge, lexis_merge_arg_list)
        }
        # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
        #     * Run
        #       `optional_steps[["stratum_post_merge"]](stratum_eval_env = stratum_eval_env, eval_env = eval_env, call_env = call_env)`
        #       if that `optional_steps` element exists.
        # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
        if ("stratum_post_merge" %in% names(optional_steps)) {
          optional_steps[["stratum_post_merge"]](
            stratum_eval_env = stratum_eval_env,
            eval_env = eval_env,
            call_env = call_env
          )
        }
        # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
        #      * Evaluate `aggre_exprs` as follows:
        # @codedoc_insert_comment_block popEpi:::lexis_aggregate_one_stratum__
        # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
        out <- lexis_aggregate_one_stratum__(
          lexis_stratum_subset_split = lexis_stratum_subset_split,
          box_dt = lexis_box_dt__(stratum_box_breaks),
          aggre_exprs = aggre_exprs,
          split_lexis_column_exprs = split_lexis_column_exprs,
          eval_env = eval_env,
          call_env = call_env,
          stratum_eval_env = stratum_eval_env
        )
        # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
        #     * Run
        #       `optional_steps[["stratum_post_aggregation"]](stratum_eval_env = stratum_eval_env, eval_env = eval_env, call_env = call_env)`
        #       if that `optional_steps` element exists.
        # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
        if ("stratum_post_aggregation" %in% names(optional_steps)) {
          optional_steps[["stratum_post_aggregation"]](
            stratum_eval_env = stratum_eval_env,
            eval_env = eval_env,
            call_env = call_env
          )
        }
        out[]
      })
      data.table::rbindlist(out)
    },
    .SDcols = names(lexis_dt),
    keyby = eval(names(aggre_by))
  ])
  if (is.null(aggre_by)) {
    out_expr["keyby"] <- NULL
  }
  if (length(aggre_exprs) == 0) {
    out_expr[".SDcols"] <- NULL
    out_expr[["j"]] <- quote(as.list(box_dt))
  }
  if (!all(subset)) {
    out_expr[["i"]] <- quote(subset)
  }
  out <- eval(out_expr)
  if (data.table::is.data.table(aggre_by) && nrow(aggre_by) > 0) {
    out <- data.table::rbindlist(lapply(seq_len(nrow(aggre_by)), function(i) {
      aggre_by_i <- aggre_by[i, ]
      out_i <- out[
        i = aggre_by_i,
        on = names(aggre_by_i),
        #' @importFrom data.table .SD
        j = .SD,
        nomatch = 0L
      ]
      if (nrow(out_i) == 0) {
        out_i <- cbind(aggre_by_i, box_dt)
      }
      return(out_i)
    }), use.names = TRUE, fill = TRUE)
  }
  local({
    # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
    # - Replace `NA` values in value columns starting with `t_` or with `n_`
    #   with zeroes. E.g. `t_at_risk` will then be zero in intervals with no
    #   subjects in follow-up. Even a custom expression such as
    #   `t_at_risk_squared` will be handled in this manner.
    # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
    zero_col_nms <- names(aggre_exprs)[grepl("(^t_)|(^n_)", names(aggre_exprs))]
    for (vcn in names(aggre_exprs)) {
      if (vcn %in% zero_col_nms) {
        data.table::set(
          x = out,
          i = if (vcn %in% names(out)) which(is.na(out[[vcn]])) else NULL,
          j = vcn,
          value = 0L
        )
      } else if (!vcn %in% names(out)) {
        data.table::set(
          x = out,
          j = vcn,
          value = NA
        )
      }
    }
  })
  local({
    # we have to reset the id columns here because the aggregation above is
    # performed one time scale stratum box at a time, e.g. one period at a time,
    # and that leads to e.g. ts_cal_id being the same for every period.
    lapply(names(breaks), function(ts_col_nm) {
      out[
        #' @importFrom data.table := .GRP
        j = (paste0(ts_col_nm, "_id")) := .GRP,
        by = eval(paste0(ts_col_nm, "_", c("start", "stop")))
      ]
      NULL
    })
    out[
      #' @importFrom data.table := .GRP
      j = "box_id" := .GRP,
      by = eval(paste0(names(breaks), "_id"))
    ]
  })

  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
  # - Set proper `data.table`
  #   attributes on the resulting big table and sort it by the stratifying
  #   variables.
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
  # note that this clears Epi attributes
  out <- as.list(out)
  attributes(out) <- attributes(out)["names"]
  data.table::setDT(out)
  data.table::setkeyv(
    x = out,
    cols = c(
      names(aggre_by),
      paste0(rep(names(breaks), each = 3), c("_start", "_stop", "_id"))
    )
  )
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
  # - Store as metadata a list with names `stratum_col_nms`, `ts_col_nms`, and
  #   `value_col_nms` into the attribute named
  #   `lexis_split_merge_aggregate_by_stratum_meta`, where
  #   `stratum_col_nms = names(aggre_by)`, `ts_col_nms = names(breaks)`, and
  #   `value_col_nms` are the names of the columns resulting from `aggre_exprs`.
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
  data.table::setattr(
    out,
    "lexis_split_merge_aggregate_by_stratum_meta",
    list(
      stratum_col_nms = names(aggre_by),
      ts_col_nms = names(breaks),
      value_col_nms = setdiff(names(out), c(names(aggre_by), names(box_dt)))
    )
  )
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
  # - Run
  #   `optional_steps[["post_aggregation"]](eval_env = eval_env, call_env = call_env)`
  #   if that `optional_steps` element exists.
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
  if ("post_aggregation" %in% names(optional_steps)) {
    optional_steps[["post_aggregation"]](
      eval_env = eval_env,
      call_env = call_env
    )
  }
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
  # - Return a `data.table` with stratum columns as specified via
  #   `aggre_by`, time scale columns specified via `breaks` and value columns
  #   as specified via `aggre_exprs`.
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum
  # @codedoc_comment_block return(popEpi::lexis_split_merge_aggregate_by_stratum)
  # Returns a `data.table` with stratum columns as specified via
  # `aggre_by`, time scale columns specified via `breaks` and value columns
  # as specified via `aggre_exprs`.
  # @codedoc_comment_block return(popEpi::lexis_split_merge_aggregate_by_stratum)
  return(out[])
}
