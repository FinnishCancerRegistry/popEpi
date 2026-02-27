#' @title Survival Time Statistics
#' @description
#' Functions used for estimation of various survival time statistics.
#' E.g. relative survival.
#' @name surv_functions
NULL

surv_lexis_aggre_exprs__ <- function(
  estimator_dt
) {
  var_nm_set <- unique(unlist(lapply(seq_len(nrow(estimator_dt)), function(i) {
    # @codedoc_comment_block popEpi:::surv_lexis_aggre_exprs__
    #   + Detect variables used in the estimation expressions with `[all.vars]`.
    # @codedoc_comment_block popEpi:::surv_lexis_aggre_exprs__
    est_expr_set <- estimator_dt[["expression_set"]][[i]]
    var_nm_set <- unlist(lapply(est_expr_set, all.vars))
    return(var_nm_set)
  })))
  # @codedoc_comment_block popEpi:::surv_lexis_aggre_exprs__
  #   + Retain only those known to `popEpi` --- see
  #    `?lexis_split_merge_aggregate_by_stratum`. This results in a character
  #    string vector that the argument `aggre_exprs` of
  #    `lexis_split_merge_aggregate_by_stratum` accepts.
  # @codedoc_comment_block popEpi:::surv_lexis_aggre_exprs__
  standard_var_nm_set <- sub(
    "\\[.+, .+\\]",
    "[x, y]",
    var_nm_set
  )
  var_nm_set <- var_nm_set[standard_var_nm_set %in% names(SURV_AGGRE_EXPRS__)]
  names(var_nm_set) <- var_nm_set
  var_nm_set <- as.list(var_nm_set)
  return(var_nm_set)
}

#' @eval codedoc::pkg_doc_fun(
#'   "popEpi::surv_lexis",
#'   "surv_functions"
#' )
surv_lexis <- function(
  lexis,
  breaks,
  merge_dt_by = NULL,
  merge_dt = NULL,
  merge_dt_harmonisers = NULL,
  aggre_by = NULL,
  aggre_ts_col_nms = NULL,
  aggre_exprs = NULL,
  subset = NULL,
  estimators = "S_pch",
  conf_methods = "log",
  conf_lvls = 0.95,
  weights = NULL
) {
  #' @template param_lexis
  assert_is_arg_lexis(lexis, dt = FALSE)
  subset <- handle_arg_subset(dataset_nm = "lexis")
  aggre_by <- handle_arg_by(by = aggre_by, dataset = lexis)
  #' @param weights `[NULL, data.table, character]` (default `NULL`)
  #'
  #' Weights for adjusting estimates.
  #'
  #' - `NULL`: No adjusting is performed.
  #' - `data.table`: Passed to `[surv_estimate]`.
  #' - `character`: Passed to `[lexis_split_merge_aggregate_by_stratum]`.
  weight_dt <- weight_col_nm <- NULL
  do_direct_adjusting <- data.table::is.data.table(weights)
  if (do_direct_adjusting) {
    weight_dt <- weights
    aggre_by <- handle_arg_by(
      by = list(
        aggre_by,
        local({
          da_stratum_col_nms <- setdiff(names(weights), "weight")
          nondup <- !duplicated(lexis, by = da_stratum_col_nms)
          da_stratum_dt <- lexis[
            i = nondup,
            #' @importFrom data.table .SD
            j = .SD,
            .SDcols = da_stratum_col_nms
          ]
          data.table::setkeyv(da_stratum_dt, names(da_stratum_dt))
          da_stratum_dt[]
        })
      ),
      dataset = lexis
    )
  } else {
    weight_col_nm <- weights
  }
  # @codedoc_comment_block popEpi::surv_lexis
  # Compute survival estimates on a `Lexis` dataset
  # (`[Epi::Lexis]` / `[Lexis_dt]`).
  #
  # Performs the following steps:
  #
  # - `estimators` is analysed and the following will be appended to
  #   `aggre_exprs`by `surv_lexis`:
  # @codedoc_insert_comment_block popEpi:::surv_lexis_aggre_exprs__
  # - This results in `aggre_exprs` with both anything that the user defined
  #   and also what was added by `surv_lexis`. However, we drop duplicates
  #   in `aggre_exprs` based on both `duplicated(names(aggre_exprs))` and
  #   `duplicated(aggre_exprs)`. E.g. if you supply
  #   `aggre_exprs = list(n_events = quote(sum(lex.Xst != 0)))`
  #   and `n_events` is also added by `surv_lexis` then only the one you
  #   supplied is retained.
  # @codedoc_comment_block popEpi::surv_lexis
  estimator_dt <- handle_arg_estimators(estimators)
  aggre_exprs <- c(
    aggre_exprs,
    surv_lexis_aggre_exprs__(estimator_dt = estimator_dt)
  )
  aggre_exprs <- aggre_exprs[
    !duplicated(aggre_exprs) & !duplicated(names(aggre_exprs))
  ]
  # @codedoc_comment_block popEpi::surv_lexis
  # - Call `lexis_split_merge_aggregate_by_stratum`.
  #   The resulting table of aggregated data is
  #   stratified by both `aggre_by` and by any stratifying columns found in
  #   `weight_dt if a `data.table` was supplied as that argument. E.g.
  #   with `aggre_by = "sex"` and
  #   `weight_dt = data.table::data.table(ag = 1:3, weight = c(100, 150, 200))`,
  #   the statistics table is stratified by both `sex` and `ag`.
  #   With `aggre_by = "sex"` and `weights = "individual_weight"` the table is
  #   stratified by sex and contains individually weighted statistics.
  # @codedoc_comment_block popEpi::surv_lexis
  #' @param breaks
  #' Passed to `[lexis_split_merge_aggregate_by_stratum]`.
  #' @param merge_dt
  #' Passed to `[lexis_split_merge_aggregate_by_stratum]`.
  #' @param merge_dt_by
  #' Passed to `[lexis_split_merge_aggregate_by_stratum]`.
  #' @param merge_dt_harmonisers
  #' Passed to `[lexis_split_merge_aggregate_by_stratum]`.
  #' @param aggre_by
  #' Passed to `[lexis_split_merge_aggregate_by_stratum]`.
  #' @param aggre_ts_col_nms
  #' Passed to `[lexis_split_merge_aggregate_by_stratum]`.
  #' @param aggre_exprs
  #' Passed to `[lexis_split_merge_aggregate_by_stratum]`.
  #' @param subset
  #' Passed to `[lexis_split_merge_aggregate_by_stratum]`.
  sdt <- lexis_split_merge_aggregate_by_stratum(
    lexis = lexis,
    breaks = breaks,
    merge_dt_by = merge_dt_by,
    merge_dt = merge_dt,
    merge_dt_harmonisers = merge_dt_harmonisers,
    aggre_by = aggre_by,
    aggre_ts_col_nms = aggre_ts_col_nms,
    aggre_exprs = aggre_exprs,
    weight_col_nm = weight_col_nm,
    subset = subset
  )
  aggre_meta <- attr(sdt, "surv_split_merge_aggregate_by_stratum_meta")
  estimation_stratum_col_nms <- setdiff(
    aggre_meta[["stratum_col_nms"]],
    names(weights)
  )
  aggre_ts_col_nms <- aggre_meta[["ts_col_nms"]]
  if (length(aggre_ts_col_nms) > 1) {
    # e.g. if aggregating by ts_cal to get a period analysis time series.
    estimation_stratum_col_nms <- c(
      estimation_stratum_col_nms,
      paste0(aggre_ts_col_nms[seq_len(length(aggre_ts_col_nms) - 1L)], "_id")
    )
  }
  # @codedoc_comment_block popEpi::surv_lexis
  # - Call `surv_estimate`.
  # @codedoc_comment_block popEpi::surv_lexis
  # surv_lexis_env? see `est` of `S_exp_e1_pch`.
  surv_lexis_env <- environment() # nolint
  surv_lexis_env[["ts_fut_col_nm"]] <- utils::tail(aggre_ts_col_nms, 1L)
  sdt <- surv_estimate(
    dt = sdt,
    ts_fut_col_nm = aggre_ts_col_nms[length(aggre_ts_col_nms)],
    stratum_col_nms = estimation_stratum_col_nms,
    estimators = structure(
      estimator_dt[["expression_set"]],
      names = estimator_dt[["user_estimator_name"]]
    ),
    conf_methods = conf_methods,
    weight_dt = weight_dt
  )

  # @codedoc_comment_block return(popEpi::surv_lexis)
  # Returns a `data.table` as produced by `surv_estimate`.
  # @codedoc_comment_block return(popEpi::surv_lexis)
  return(sdt[])
}
