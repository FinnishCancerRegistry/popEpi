#' @title Survival Time Statistics
#' @description
#' Functions used for estimation of various survival time statistics.
#' E.g. relative survival.
#' @name surv_functions
NULL

surv_aggre_expression_list__ <- list(
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
    sum((lex.Cst == x & lex.Xst == y) * iw)
  )
)
surv_aggre_expression_table__ <- function() {
  dt <- data.table::data.table(
    "Name" = surv_estimate_expression_table_clean__(
      names(surv_aggre_expression_list__)
    ),
    "Expression" = vapply(
      surv_aggre_expression_list__,
      surv_estimate_expression_table_clean__,
      character(1L)
    )
  )
  return(dt)
}
surv_aggre_expression__ <- function(
  estimator_dt,
  individual_weight_col_nm = NULL
) {
  if (!is.null(individual_weight_col_nm)) {
    iw_replacement <- paste0(" * ", individual_weight_col_nm)
  } else {
    iw_replacement <- ""
  }
  aggre_expr_set <- unlist(lapply(seq_len(nrow(estimator_dt)), function(i) {
    # @codedoc_comment_block popEpi:::surv_aggre_expression__
    #   + Detect variables used in the estimation expressions with `[all.vars]`.
    # @codedoc_comment_block popEpi:::surv_aggre_expression__
    est_expr_set <- estimator_dt[["expression_set"]][[i]]
    var_nm_set <- unlist(lapply(est_expr_set, all.vars))
    var_nm_set <- unique(var_nm_set)
    var_nm_set <- var_nm_set[
      var_nm_set %in% names(surv_aggre_expression_list__) |
        grepl("_\\[.+, *.+\\]$", var_nm_set)
    ]
    # @codedoc_comment_block popEpi:::surv_aggre_expression__
    #   + Detect the general name of a transition-specific variable such as
    #     `n_events_[0, 1]` as `n_events_[x, y]`. This general name is used
    #     to fetch the general aggregation expression
    #     (e.g. `"n_events_[x, y]" = quote(sum(lex.Cst == x & lex.Xst == y))`)
    #     and this definition is then made specific to the requested transition,
    #     e.g.  `n_events_[0, 1] = quote(sum(lex.Cst == x & lex.Xst == y))`.
    #     Of course aggregation expressions without a specific transition such
    #     as `t_at_risk` are unaffected by this. The table of general
    #     aggregation expressions known to `popEpi` is shown below.
    # @codedoc_comment_block popEpi:::surv_aggre_expression__
    var_nm_set <- sub(
      "[x, y]",
      sprintf(
        "[%s, %s]",
        as.character(estimator_dt[["state_from"]][i]),
        as.character(estimator_dt[["state_to"]][i])
      ),
      var_nm_set,
      fixed = TRUE
    )
    standard_var_nm_set <- sub(
      "\\[.+, .+\\]$",
      "[x, y]",
      var_nm_set
    )
    aggre_expr_set <- surv_aggre_expression_list__[standard_var_nm_set]
    names(aggre_expr_set) <- var_nm_set
    aggre_expr_string_set <- vapply(aggre_expr_set, deparse1, character(1L))
    aggre_expr_string_set <- sub(
      "(?<=\\W)x(?=\\W)",
      sprintf(" %s ", as.character(estimator_dt[["state_from"]][i])),
      aggre_expr_string_set,
      perl = TRUE
    )
    aggre_expr_string_set <- sub(
      "(?<=\\W)y(?=\\W)",
      sprintf(" %s ", as.character(estimator_dt[["state_to"]][i])),
      aggre_expr_string_set,
      perl = TRUE
    )
    # @codedoc_comment_block popEpi:::surv_aggre_expression__
    #   + The general variable definitions usually have ` * iw` in them for
    #     enabling individual weighting. But if this is not requested then
    #     those ` * iw` parts are removed from the aggregation expressions.
    # @codedoc_comment_block popEpi:::surv_aggre_expression__
    aggre_expr_string_set <- gsub(
      " *[*] *iw",
      iw_replacement,
      aggre_expr_string_set
    )
    lapply(aggre_expr_string_set, function(s) {
      parse(text = s)[[1]]
    })
  }), recursive = FALSE, use.names = TRUE)
  # @codedoc_comment_block popEpi:::surv_aggre_expression__
  #   + After going through every requested estimator we have a set of
  #     quoted expressions such as
  #     `list(t_at_risk = quote(sum(lex.dur)), n_events = quote(sum(lex.Cst != lex.Xst)))`.
  #     This is turned into a quoted list of expressions with `call` and we are
  #     done, e.g.
  #     `quote(list(t_at_risk = sum(lex.dur), n_events = sum(lex.Cst != lex.Xst)))`.
  #   + Table of general aggregation expressions known to `popEpi`:
  #
  # ${paste0(knitr::kable(popEpi:::surv_aggre_expression_table__()), collapse = "\n")}
  # @codedoc_comment_block popEpi:::surv_aggre_expression__
  aggre_expr_set[duplicated(names(aggre_expr_set))] <- NULL
  aggre_expr_set <- aggre_expr_set[order(names(aggre_expr_set))]
  out <- as.call(c(quote(list), aggre_expr_set))
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
  estimators = "S_pch",
  conf_methods = "log",
  conf_lvls = 0.95,
  weights = NULL
) {
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
  # Compute survival estimates on a `Lexis` dataset (`[Epi::Lexis]`).
  #
  # Performs the following steps:
  #
  # - If `aggre_expr` is `NULL`,
  #   `estimators` is analysed and `aggre_expr` will be created by `surv_lexis`
  #   as follows:
  # @codedoc_insert_comment_block popEpi:::surv_aggre_expression__
  # @codedoc_comment_block popEpi::surv_lexis
  estimator_dt <- handle_arg_estimators(estimators)
  if (is.null(aggre_expr)) {
    aggre_expr <- surv_aggre_expression__(
      estimator_dt = estimator_dt,
      individual_weight_col_nm = if (is.character(weights)) weights else NULL
    )
  }
  # @codedoc_comment_block popEpi::surv_lexis
  # - Call `surv_split_merge_aggregate_by_stratum`.
  #   The resulting table of aggregated data is
  #   stratified by both `aggre_by` and by any stratifying columns found in
  #   `weights` if a `data.table` was supplied as that argument. E.g.
  #   with `aggre_by = "sex"` and
  #   `weights = data.table::data.table(ag = 1:3, weight = c(100, 150, 200))`,
  #   the statistics table is stratified by both `sex` and `ag`.
  #   With `aggre_by = "sex"` and `weights = "individual_weight"` the table is
  #   stratified by sex and contains individually weighted statistics.
  # @codedoc_comment_block popEpi::surv_lexis
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
  # - Call `surv_estimate` and return its result.
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
  # Returns a `data.table` as produced by `surv_estimate`.
  # @codedoc_comment_block return(popEpi::surv_lexis)
  return(sdt[])
}
