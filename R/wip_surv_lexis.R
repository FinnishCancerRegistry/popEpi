#' @title Survival Time Statistics
#' @description
#' Functions used for estimation of various survival time statistics.
#' E.g. relative survival.
#' @name surv_functions
NULL

surv_aggre_util_expressions__ <- list(
  n_in_follow_up_at_interval_start = quote(
    sum(in_follow_up_at_interval_start)
  ),
  n_entered_late_during_interval = quote(
    sum(entered_late_during_interval)
  ),
  n_left_early_during_interval = quote(
    sum(left_early_during_interval)
  ),
  n_at_risk_effective = quote(
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
  total_subject_time = quote(
    sum(lex.dur * iw)
  ),
  n_events_pp = quote(
    sum((lex.Xst != lex.Cst) * pp * iw)
  ),
  n_events_pp_double_weighted = quote(
    sum((lex.Xst != lex.Cst) * pp * pp * iw)
  ),
  n_events_expected_pp = quote(
    sum(lex.dur * haz * pp * iw)
  ),
  total_subject_time_pp = quote(
    sum(lex.dur * pp * iw)
  ),
  "n_events_[x, y]" = quote(
    sum((lex.Cst == x & lex.Xst == y) * iw)
  )
)
surv_aggre_expressions__ <- list(
  "lifetable_observed_survival" = quote(
    list(
      n_in_follow_up_at_interval_start = n_in_follow_up_at_interval_start,
      n_entered_late_during_interval = n_entered_late_during_interval,
      n_left_early_during_interval = n_left_early_during_interval,
      n_at_risk_effective = n_at_risk_effective,
      n_events = n_events
    )
  ),
  "hazard_observed_survival" = quote(
    list(
      total_subject_time = total_subject_time,
      n_events = n_events
    )
  ),
  "hazard_relative_survival_ederer_ii" = quote(
    list(
      total_subject_time = total_subject_time,
      n_events = n_events,
      n_events_expected_ederer_ii = sum(lex.dur * haz * iw)
    )
  ),
  "hazard_net_survival_pohar_perme" = quote(
    list(
      n_events_pp = n_events_pp,
      n_events_pp_double_weighted = n_events_pp_double_weighted,
      n_events_expected_pp = n_events_expected_pp,
      total_subject_time_pp = total_subject_time_pp
    )
  ),
  "hazard_observed_absolute_risk_[x, y]" = quote(
    list(
      total_subject_time = total_subject_time,
      n_events = n_events,
      "n_events_[x, y]" = `n_events_[x, y]`
    )
  ),
  "lifetable_observed_absolute_risk_[x, y]" = quote(
    list(
      n_in_follow_up_at_interval_start = n_in_follow_up_at_interval_start,
      n_entered_late_during_interval = n_entered_late_during_interval,
      n_left_early_during_interval = n_left_early_during_interval,
      n_at_risk_effective = n_at_risk_effective,
      n_events = n_events,
      "n_events_[x, y]" = `n_events_[x, y]`
    )
  )
)
surv_aggre_expressions__ <- lapply(surv_aggre_expressions__, function(expr) {
  # initially same as e.g.
  # expr = quote(list(n_events = n_events))
  expr <- substitute(
    substitute(expr, surv_aggre_util_expressions__),
    list(expr = expr)
  )
  # now same as e.g.
  # quote(substitute(list(n_events = n_events), surv_aggre_util_expressions__))
  expr <- eval(expr)
  # now same as e.g.
  # quote(list(n_events = sum(lex.Cst != lex.Xst * iw)))
  return(expr)
})
surv_aggre_expression__ <- function(
  estimator_dt,
  individual_weight_col_nm = NULL
) {
  out <- unlist(
    lapply(
      seq_len(nrow(estimator_dt)),
      function(i) {
        expr <- surv_aggre_expressions__[[
          estimator_dt[["standard_estimator_name"]][i]
        ]]
        out <- as.list(expr)[-1]
        names(out) <- sub(
          "[x, y]",
          sprintf(
            "[%s, %s]",
            as.character(estimator_dt[["state_from"]][i]),
            as.character(estimator_dt[["state_to"]][i])
          ),
          names(out),
          fixed = TRUE
        )
        out <- lapply(out, function(expr) {
          expr_lines <- deparse(expr)
          expr_lines <- sub(
            "(?<=\\W)x(?=\\W)",
            sprintf(" %s ", as.character(estimator_dt[["state_from"]][i])),
            expr_lines,
            perl = TRUE
          )
          expr_lines <- sub(
            "(?<=\\W)y(?=\\W)",
            sprintf(" %s ", as.character(estimator_dt[["state_to"]][i])),
            expr_lines,
            perl = TRUE
          )
          parse(text = paste0(expr_lines, collapse = "\n"))[[1]]
        })
        out
      }
    ),
    recursive = FALSE, use.names = TRUE
  )
  out <- out[!duplicated(names(out))]
  if (!is.null(individual_weight_col_nm)) {
    iw_replacement <- paste0(" * ", individual_weight_col_nm)
  } else {
    iw_replacement <- ""
  }
  out <- lapply(out, function(expr) {
    expr_string <- deparse1(expr)
    expr_string <- gsub(" *[*] *iw", iw_replacement, expr_string)
    expr <- parse(text = expr_string)[[1]]
    expr
  })
  out <- as.call(c(quote(list), out))
  return(out)
}

#' @eval codedoc::pkg_doc_fun(
#'   "popEpi::surv_lexis",
#'   "surv_functions"
#' )
surv_lexis <- function(
  dt,
  breaks,
  merge_dt_by = NULL,
  merge_dt = NULL,
  merge_dt_harmonisers = NULL,
  aggre_by = NULL,
  aggre_ts_col_nms = NULL,
  aggre_expr = NULL,
  subset = NULL,
  estimators = "hazard_observed_survival",
  conf_methods = "log",
  conf_lvls = 0.95,
  weights = NULL
) {
  # @codedoc_comment_block popEpi::surv_lexis
  # Compute survival estimates on a `Lexis` dataset (`[Epi::Lexis]`).
  #
  # Performs the following steps:
  # @codedoc_comment_block popEpi::surv_lexis
  subset <- handle_arg_subset()
  aggre_by <- handle_arg_by(by = aggre_by, dataset = dt)
  # @codedoc_comment_block surv_arg_weights
  # - `popEpi::surv_lexis`: `[data.table, character, NULL]` (default `NULL`)
  # @codedoc_comment_block surv_arg_weights
  do_direct_adjusting <- data.table::is.data.table(weights)
  if (do_direct_adjusting) {
    aggre_by <- handle_arg_by(
      by = list(
        aggre_by,
        local({
          da_stratum_col_nms <- setdiff(names(weights), "weight")
          nondup <- !duplicated(dt, by = da_stratum_col_nms)
          da_stratum_dt <- dt[
            i = nondup,
            #' @importFrom data.table .SD
            j = .SD,
            .SDcols = da_stratum_col_nms
          ]
          data.table::setkeyv(da_stratum_dt, names(da_stratum_dt))
          da_stratum_dt[]
        })
      ),
      dataset = dt
    )
  }
  # @codedoc_comment_block popEpi::surv_lexis
  # - Call `surv_split_merge_aggregate_by_stratum`. If `aggre_expr` is `NULL`,
  #   it is replaced with an internally specified expressions based on
  #   argument `estimators`. The resulting table of aggregated data is
  #   stratified by both `aggre_by` and by any stratifying columns found in
  #   `weights` if a `data.table` was supplied as that argument. E.g.
  #   with `aggre_by = "sex"` and
  #   `weights = data.table::data.table(ag = 1:3, weight = c(100, 150, 200))`,
  #   the statistics table is stratified by both `sex` and `ag`.
  #   With `aggre_by = "sex"` and `weights = "individual_weight"` the table is
  #   stratified by sex and contains individually weighted statistics.
  # @codedoc_comment_block popEpi::surv_lexis
  estimator_dt <- handle_arg_estimators(estimators)
  if (is.null(aggre_expr)) {
    aggre_expr <- surv_aggre_expression__(
      estimator_dt = estimator_dt,
      individual_weight_col_nm = if (is.character(weights)) weights else NULL
    )
  }
  sdt <- surv_split_merge_aggregate_by_stratum(
    dt = dt,
    breaks = breaks,
    merge_dt_by = merge_dt_by,
    merge_dt = merge_dt,
    merge_dt_harmonisers = merge_dt_harmonisers,
    aggre_by = aggre_by,
    aggre_ts_col_nms = aggre_ts_col_nms,
    aggre_expr = aggre_expr,
    subset = subset
  )
  aggre_meta <- attr(sdt, "surv_split_merge_aggregate_by_stratum_meta")
  estimation_stratum_col_nms <- aggre_meta[["stratum_col_nms"]]
  aggre_ts_col_nms <- aggre_meta[["ts_col_nms"]]
  if (length(aggre_ts_col_nms) > 1) {
    # e.g. if aggregating by ts_cal to get a period analysis time series.
    estimation_stratum_col_nms <- c(
      estimation_stratum_col_nms,
      paste0(aggre_ts_col_nms[seq_len(length(aggre_ts_col_nms) - 1L)], "_id")
    )
  }
  # @codedoc_comment_block popEpi::surv_lexis
  # - Call `surv_estimate` to produce estimates by the strata specified above.
  #   If `weights` was `NULL` or `character` we are done.
  # @codedoc_comment_block popEpi::surv_lexis
  sdt <- surv_estimate(
    dt = sdt,
    ts_fut_col_nm = aggre_ts_col_nms[length(aggre_ts_col_nms)],
    stratum_col_nms = estimation_stratum_col_nms,
    estimators = structure(
      estimator_dt[["expression_set"]],
      names = estimator_dt[["user_estimator_name"]]
    ),
    conf_methods = conf_methods
  )
  # @codedoc_comment_block return(popEpi::surv_lexis)
  # A `data.table` of summary statistics and survival estimates with their
  # standard errors and confidence intervals is returned.
  # @codedoc_comment_block return(popEpi::surv_lexis)
  return(sdt[])
}
