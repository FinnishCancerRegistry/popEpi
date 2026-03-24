#' @title Survival Time Statistics
#' @description
#' Functions used for estimation of various survival time statistics.
#' E.g. relative survival.
#' @name surv_functions
NULL

#' @eval codedoc::pkg_doc_fun(
#'   "popEpi::lexis_breaks_collapse_1d",
#'   "surv_functions"
#' )
#' @examples
#'
#' # popEpi::lexis_breaks_collapse_1d
#' lexis <- Epi::Lexis(
#'   entry = list(ts_fut = c(0.0, 3.0)),
#'   duration = c(2.5, 10.0),
#'   entry.status = 0L,
#'   exit.status = c(0L, 1L)
#' )
#' bl <- list(ts_fut = seq(0, 5, 0.5))
#' agdt <- popEpi::lexis_split_merge_aggregate_by_stratum(
#'   lexis = lexis,
#'   breaks = bl,
#'   aggre_exprs = "t_at_risk"
#' )
#' # what to do about empty intervals? we have intervals with no subjects in them.
#' stopifnot(
#'   agdt[["t_at_risk"]][-6] > 0,
#'   agdt[["t_at_risk"]][6] == 0.0
#' )
#' # combine intervals until there are no empty ones left.
#' breaks_ts_fut <- popEpi::lexis_breaks_collapse_1d(
#'   lexis = lexis,
#'   breaks_1d = bl
#' )
#' # this has resulted in the combined interval ]2.5, 3.5].
#' stopifnot(!3.0 %in% breaks_ts_fut)
#'
#' # but sometimes we want to retain specific breaks in the output. for instance
#' # if the goal is to produce survival estimates for every year.
#' # we can make use of arg `mandatory_breaks` to achieve this.
#' breaks_ts_fut <- popEpi::lexis_breaks_collapse_1d(
#'   lexis = lexis,
#'   breaks_1d = bl,
#'   mandatory_breaks = 0:5
#' )
#' # this has resulted in the combined interval ]2.0, 3.0].
#' stopifnot(!2.5 %in% breaks_ts_fut)
#' agdt <- popEpi::lexis_split_merge_aggregate_by_stratum(
#'   lexis = lexis,
#'   breaks = list(ts_fut = breaks_ts_fut),
#'   aggre_exprs = "t_at_risk"
#' )
#' stopifnot(
#'   agdt[["t_at_risk"]] > 0.0
#' )
#'
lexis_breaks_collapse_1d <- function(
  lexis,
  breaks_1d,
  test_expr = NULL,
  mandatory_breaks = NULL
) {
  assert_is_arg_lexis(lexis, dt = FALSE)
  #' @param breaks_1d `[list]` (no default)
  #'
  #' List of breaks with one element. These define the initial intervals which
  #' are subject to being combined. E.g. `list(ts_fut = seq(0, 5, 1 / 12))`.
  assert_is_arg_breaks(breaks_1d, lexis = lexis)
  stopifnot(length(breaks_1d) == 1, breaks_1d[[1]][1] == 0)
  #' @param test_expr `[NULL, call]` (default `NULL`)
  #'
  #' This expression is evaluated for every interval. If the test does not pass,
  #' the interval must be aggregated with one of its neighbours.
  stopifnot(
    inherits(test_expr, c("call", "NULL"))
  )
  #' @param mandatory_breaks `[NULL, other]` (default `NULL`)
  #'
  #' These breaks cannot be aggregated over. E.g. with `mandatory_breaks = 1.0`
  #' this function will not attempt to aggregate `]0.9, 1.0]` and `]1.0, 1.1]`
  #' into `]0.9, 1.1]`, not even if it is the only way to produce intervals
  #' which all pass `test_expr`.
  stopifnot(
    is.null(mandatory_breaks) || is.vector(mandatory_breaks)
  )
  ts_col_nm <- names(breaks_1d)[1]
  if (!is.null(mandatory_breaks)) {
    mandatory_breaks <- methods::as(
      mandatory_breaks,
      storage.mode(lexis[[ts_col_nm]])
    )
  }
  if (is.null(test_expr)) {
    test_expr <- quote(sum(lex.dur) == 0)
  }
  ts_breaks <- breaks_1d[[1]]
  out <- rep(NA_integer_, length(ts_breaks))
  out[1L] <- 1L
  out_pos <- 1L
  break_lo_pos <- 1L
  break_hi_pos <- 2L
  call_env <- parent.frame(1L)
  while (TRUE) {
    if (break_hi_pos > length(ts_breaks)) {
      break
    }
    break_lo <- ts_breaks[break_lo_pos]
    break_hi <- ts_breaks[break_hi_pos]
    can_combine_next <- break_hi_pos + 1L <= length(ts_breaks) &&
      !break_hi %in% mandatory_breaks
    can_combine_previous <- break_lo_pos > 1 &&
      !break_lo %in% mandatory_breaks
    can_combine <- can_combine_next || can_combine_previous
    do_combine <- can_combine && local({
      work_dt <- surv_interval(
        lexis = lexis,
        break_lo = break_lo,
        break_hi = break_hi,
        ts_col_nm = ts_col_nm,
        merge = FALSE
      )
      eval(test_expr, envir = work_dt, enclos = call_env)
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
  out <- ts_breaks[union(out[!is.na(out)], length(ts_breaks))]
  return(out)
}

surv_lexis_aggre_exprs__ <- function(
  estimator_dt
) {
  var_nm_set <- unique(unlist(lapply(seq_len(nrow(estimator_dt)), function(i) {
    # @codedoc_comment_block popEpi::surv_lexis_aggre_exprs__
    #   + Detect variables used in the estimation expressions with `[all.vars]`.
    # @codedoc_comment_block popEpi::surv_lexis_aggre_exprs__
    est_expr_set <- estimator_dt[["expression_set"]][[i]]
    var_nm_set <- unlist(lapply(est_expr_set, all.vars))
    return(var_nm_set)
  })))
  # @codedoc_comment_block popEpi::surv_lexis_aggre_exprs__
  #   + Retain only those known to `popEpi` --- see
  #    `?lexis_split_merge_aggregate_by_stratum`. This results in a character
  #    string vector that the argument `aggre_exprs` of
  #    `lexis_split_merge_aggregate_by_stratum` accepts.
  # @codedoc_comment_block popEpi::surv_lexis_aggre_exprs__
  standard_var_nm_set <- sub(
    "\\[.+, .+\\]",
    "[x, y]",
    var_nm_set
  )
  lexis_aggre_expr_list__ <- get_internal_dataset("lexis_aggre_expr_list__")
  var_nm_set <- var_nm_set[
    standard_var_nm_set %in% names(lexis_aggre_expr_list__)
  ]
  names(var_nm_set) <- var_nm_set
  var_nm_set <- as.list(var_nm_set)
  return(var_nm_set)
}

# SURV_LEXIS_ADD_COL_EXPRS__ <- list(
#   S_exp_e1_ch_mean = quote(surv_lexis_S_exp_e1_ch_mean(
#     lexis = surv_lexis_eval_env[["lexis_dt"]],
#     breaks = surv_lexis_eval_env[["breaks"]],
#     merge_dt = surv_lexis_eval_env[["merge_dt"]],
#     merge_dt_by = surv_lexis_eval_env[["merge_dt_by"]],
#     aggre_by = surv_lexis_eval_env[["aggre_by"]],
#     weight_col_nm = surv_lexis_eval_env[["weight_col_nm"]]
#   ))
# )

#' @eval codedoc::pkg_doc_fun(
#'   "popEpi::surv_lexis",
#'   "surv_functions"
#' )
#' @examples
#'
#' # popEpi::surv_lexis
#' make_pm <- function() {
#'   pm <- data.table::copy(popEpi::popmort)
#'   data.table::setnames(
#'     pm,
#'     c("year", "agegroup", "haz"),
#'     c("ts_cal", "ts_age", "h_exp")
#'   )
#'   data.table::setkeyv(pm, c("sex", "ts_cal", "ts_age"))
#'   pm <- c(
#'     list(pm),
#'     lapply(101:110, function(age) {
#'       sub_pm <- pm[
#'         pm[["ts_age"]] == 100
#'       ]
#'       sub_pm[
#'         j = "ts_age" := age
#'       ]
#'       sub_pm[]
#'     })
#'   )
#'   pm <- data.table::rbindlist(pm)
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
#'       sire[["ex_date"]] >= as.Date("1999-01-01") &
#'       (get.yrs(sire[["ex_date"]]) - get.yrs(sire[["bi_date"]])) < 100
#'   ]
#'   set.seed(1337)
#'   sire[j = "my_stratum" := sample(2L, size = .N, replace = TRUE)]
#'   # you can also use popEpi::Lexis_dt
#'   sire <- Epi::Lexis(
#'     data = sire,
#'     entry = list(
#'       ts_cal = popEpi::get.yrs(dg_date),
#'       ts_age = popEpi::get.yrs(dg_date) - popEpi::get.yrs(bi_date),
#'       ts_fut = 0.0
#'     ),
#'     duration = popEpi::get.yrs(ex_date) - popEpi::get.yrs(dg_date),
#'     entry.status = 0L,
#'     exit.status = status
#'   )
#'   sire[["icss_ag"]] <- make_column_icss_ag(sire[["dg_age"]])
#'   sire[["individual_weight"]] <- popEpi::surv_individual_weights(
#'     df = sire,
#'     standard_weight_dt = wdt
#'   )
#'   return(sire[])
#' }
#'
#' pm <- make_pm()
#' wdt <- make_standard_weight_dt()
#' sire <- make_sire()
#'
#' # using breaks on the calendar time causes period analysis to be performed.
#' # this is done here for demonstration purposes but of course everything works
#' # also without ts_cal breaks.
#' bl <- list(
#'   ts_cal = c(1999, 2004),
#'   ts_fut = popEpi::lexis_breaks_collapse_1d(
#'     lexis = sire,
#'     breaks_1d = list(ts_fut = seq(0, 5, 1 / 12)),
#'     mandatory_breaks = 0:5
#'   )
#' )
#'
#' # observed survival
#' sdt <- popEpi::surv_lexis(
#'   lexis = sire,
#'   breaks = bl,
#'   aggre_by = "my_stratum",
#'   subset = NULL,
#'   estimators = "S_ch",
#'   conf_methods = "log",
#'   conf_lvls = 0.95
#' )
#' stopifnot(
#'   inherits(sdt, "data.table"),
#'   "my_stratum" %in% names(sdt),
#'   c("S_ch_est", "S_ch_se", "S_ch_lo", "S_ch_hi") %in% names(sdt)
#' )
#'
#' # observed survival with direct adjusting
#' sdt_da <- popEpi::surv_lexis(
#'   lexis = sire,
#'   breaks = bl,
#'   aggre_by = "my_stratum",
#'   estimators = "S_ch",
#'   weights = wdt
#' )
#' stopifnot(
#'   inherits(sdt_da, "data.table"),
#'   "my_stratum" %in% names(sdt_da),
#'   c("S_ch_est", "S_ch_se", "S_ch_lo", "S_ch_hi") %in% names(sdt_da)
#' )
#'
#' # observed survival with individual weighting
#' sdt_iw <- popEpi::surv_lexis(
#'   lexis = sire,
#'   breaks = bl,
#'   aggre_by = "my_stratum",
#'   estimators = "S_ch",
#'   weights = "individual_weight"
#' )
#' stopifnot(
#'   inherits(sdt_iw, "data.table"),
#'   "my_stratum" %in% names(sdt_iw),
#'   c("S_ch_est", "S_ch_se", "S_ch_lo", "S_ch_hi") %in% names(sdt_iw),
#'
#'   # direct adjusting and individual weighting produce similar results.
#'   # the larger the dataset, the smaller the difference between the two.
#'   max(abs(sdt_da[["S_ch_est"]] - sdt_iw[["S_ch_est"]])) < 0.02,
#'   max(abs(sdt_da[["S_ch_se"]] - sdt_iw[["S_ch_se"]])) < 0.001
#' )
#'
#' # observed survival with direct adjusting, multiple period estimates
#' # with the periods as strata in output
#' bl_multi_period <- bl
#' bl_multi_period[["ts_cal"]] <- c(bl[["ts_cal"]], max(bl[["ts_cal"]]) + 5)
#' sdt_da <- popEpi::surv_lexis(
#'   lexis = sire,
#'   breaks = bl_multi_period,
#'   aggre_by = "my_stratum",
#'   estimators = "S_ch",
#'   weights = wdt
#' )
#' stopifnot(
#'   inherits(sdt_da, "data.table"),
#'   c("ts_cal_start", "ts_fut_start") %in% names(sdt_da),
#'   c("S_ch_est", "S_ch_se", "S_ch_lo", "S_ch_hi") %in% names(sdt_da)
#' )
#'
#' # observed survival with individual adjusting, period estimates
#' sdt_iw <- popEpi::surv_lexis(
#'   lexis = sire,
#'   breaks = bl_multi_period,
#'   aggre_by = "my_stratum",
#'   estimators = "S_ch",
#'   weights = "individual_weight"
#' )
#' stopifnot(
#'   inherits(sdt_iw, "data.table"),
#'   c("ts_cal_start", "ts_fut_start") %in% names(sdt_iw),
#'   c("S_ch_est", "S_ch_se", "S_ch_lo", "S_ch_hi") %in% names(sdt_iw),
#'
#'   # direct adjusting and individual weighting produce similar results.
#'   # the larger the dataset, the smaller the difference between the two.
#'   max(abs(sdt_da[["S_ch_est"]] - sdt_iw[["S_ch_est"]])) < 0.03,
#'   max(abs(sdt_da[["S_ch_se"]] - sdt_iw[["S_ch_se"]])) < 0.006
#' )
#'
#' # a few common survival time function estimates
#' sdt <- popEpi::surv_lexis(
#'   lexis = sire,
#'   breaks = bl,
#'   merge_dt_by = c("sex", "ts_cal", "ts_age"),
#'   merge_dt = pm,
#'   aggre_by = "my_stratum",
#'   estimators = c(
#'     "S_ch",
#'     "RS_e2_ch",
#'     "NS_pp_ch"
#'   )
#' )
#' stopifnot(
#'   inherits(sdt, "data.table"),
#'   c("S_ch_est", "RS_e2_ch_est", "NS_pp_ch_est") %in% names(sdt)
#' )
#'
#' # your very own estimator (the example `se` is made up)
#' sdt <- popEpi::surv_lexis(
#'   lexis = sire,
#'   breaks = bl,
#'   merge_dt_by = c("sex", "ts_cal", "ts_age"),
#'   merge_dt = pm,
#'   aggre_by = "my_stratum",
#'   estimators = list(
#'     my_estimator = list(
#'       est = quote(n_events / n_events_exp_e2),
#'       se = quote(sqrt(n_events / (n_events_exp_e2 ^ 2)))
#'     )
#'   ),
#'   conf_method = "identity"
#' )
#' stopifnot(
#'   c("my_estimator_est", "my_estimator_se") %in% names(sdt)
#' )
#'
#' # your very own estimator with direct adjusting
#' sdt <- popEpi::surv_lexis(
#'   lexis = sire,
#'   breaks = bl,
#'   merge_dt_by = c("sex", "ts_cal", "ts_age"),
#'   merge_dt = pm,
#'   aggre_by = "my_stratum",
#'   estimators = list(
#'     my_estimator = list(
#'       est = quote(n_events / n_events_exp_e2),
#'       se = quote(sqrt(n_events / (n_events_exp_e2 ^ 2)))
#'     )
#'   ),
#'   conf_method = "identity",
#'   weights = wdt
#' )
#' stopifnot(
#'   c("my_estimator_est", "my_estimator_se") %in% names(sdt)
#' )
#'
#' # your very own estimator with individual weighting
#' sdt <- popEpi::surv_lexis(
#'   lexis = sire,
#'   breaks = bl,
#'   merge_dt_by = c("sex", "ts_cal", "ts_age"),
#'   merge_dt = pm,
#'   aggre_by = "my_stratum",
#'   estimators = list(
#'     my_estimator = list(
#'       est = quote(n_events / n_events_exp_e2),
#'       se = quote(sqrt(n_events / (n_events_exp_e2 ^ 2)))
#'     )
#'   ),
#'   conf_method = "identity",
#'   weights = "individual_weight"
#' )
#' stopifnot(
#'   c("my_estimator_est", "my_estimator_se") %in% names(sdt)
#' )
#'
#' # your very own method of confidence interval estimation
#' sdt <- popEpi::surv_lexis(
#'   lexis = sire,
#'   breaks = bl,
#'   estimators = c("S_ch", "S_lt"),
#'   conf_methods = list(list(
#'     g = quote(qnorm(theta)),
#'     g_inv = quote(pnorm(g))
#'   )),
#'   conf_lvls = 0.95,
#'   weights = "individual_weight"
#' )
#' stopifnot(
#'   c("S_ch_lo", "S_ch_hi") %in% names(sdt)
#' )
#'
#' # a bunch of estimators, cause-specific ones from state 0 to 1 or 2
#' # and non-cause-specific ones also
#' sdt <- popEpi::surv_lexis(
#'   lexis = sire,
#'   breaks = bl,
#'   merge_dt_by = c("sex", "ts_cal", "ts_age"),
#'   merge_dt = pm,
#'   aggre_by = NULL,
#'   subset = NULL,
#'   estimators = c(
#'     "h_ch_[0, 1]",
#'     "h_ch_[0, 2]",
#'     "H_ch_[0, 1]",
#'     "H_ch_[0, 2]",
#'     "F_ch_[0, 1]",
#'     "F_ch_[0, 2]",
#'     "S_ch_[0, 1]",
#'     "S_ch_[0, 2]",
#'     "S_ch_[0, 1:2]",
#'     "H_ch",
#'     "H_lt",
#'     "h_ch",
#'     "h_lt",
#'     "F_ch",
#'     "F_lt",
#'     "S_ch",
#'     "S_lt",
#'     "NS_pp_ch",
#'     "RF_e2_ch"
#'   )
#' )
#' stopifnot(
#'   # hazard is additive.
#'   all.equal(
#'     sdt[["h_ch_est"]],
#'     sdt[["h_ch_[0, 1]_est"]] + sdt[["h_ch_[0, 2]_est"]]
#'   ),
#'   # cumulative hazard is additive.
#'   all.equal(
#'     sdt[["H_ch_est"]],
#'     sdt[["H_ch_[0, 1]_est"]] + sdt[["H_ch_[0, 2]_est"]]
#'   ),
#'   # competing risks cdf is additive. in epidemiology the cdf is often called
#'   # the cumulative incidence function.
#'   all.equal(
#'     sdt[["F_ch_est"]],
#'     sdt[["F_ch_[0, 1]_est"]] + sdt[["F_ch_[0, 2]_est"]]
#'   ),
#'
#'   # competing risks survival is NOT additive!
#'   sdt[["S_ch_[0, 1]_est"]] + sdt[["S_ch_[0, 2]_est"]] > 1,
#'
#'   # you can define multiple states in the cause-specific transition.
#'   # this one covers all exit statuses and is therefore the same as overall
#'   # survival by definition.
#'   sdt[["S_ch_[0, 1:2]_est"]] == sdt[["S_ch_est"]]#,
#'
#'   # the different estimation methods produce about the same results.
#'   # abs(sdt[["S_ch_est"]] - sdt[["S_lt_est"]]) < 0.01,
#'   # abs(sdt[["H_ch_est"]] - sdt[["H_lt_est"]]) < 0.01,
#'
#'   # net survival is approximately the same as cause-specific survival in this
#'   # rather ideal dataset.
#'   # abs(sdt[["S_ch_[0, 1]_est"]] - sdt[["NS_pp_ch_est"]]) < 0.01,
#'   # the ederer 2 based "relative cdf" is approximately the same as
#'   # cause-specific cdf in this rather ideal dataset.
#'   # abs(sdt[["F_ch_[0, 1]_est"]] - sdt[["RF_e2_ch_est"]]) < 0.01
#' )
#'
#' # factor status
#' data.table::set(
#'   x = sire,
#'   j = c("lex.Cst", "lex.Xst"),
#'   value = list(
#'     factor(
#'       x = sire[["lex.Cst"]],
#'       levels = 0:2,
#'       labels = c("alive", "dead from cancer", "dead from other")
#'     ),
#'     factor(
#'       x = sire[["lex.Xst"]],
#'       levels = 0:2,
#'       labels = c("alive", "dead from cancer", "dead from other")
#'     )
#'   )
#' )
#' sdt <- popEpi::surv_lexis(
#'   lexis = sire,
#'   breaks = bl,
#'   merge_dt_by = c("sex", "ts_cal", "ts_age"),
#'   merge_dt = pm,
#'   aggre_by = NULL,
#'   subset = NULL,
#'   estimators = c(
#'     "F_ch_['alive', 'dead from cancer']",
#'     "F_ch_['alive', 'dead from other']"
#'   )
#' )
#' stopifnot(
#'   # produces the estimates as intended. maybe these are not too pretty but
#'   # you could use shorter factor labels such as c("a", "dc", "do").
#'   "F_ch_['alive', 'dead from cancer']_est" %in% names(sdt)
#' )
#'
#' # TRUE/FALSE status
#' data.table::set(
#'   x = sire,
#'   j = c("lex.Cst", "lex.Xst"),
#'   value = list(
#'     sire[["lex.Cst"]] != "alive",
#'     sire[["lex.Xst"]] != "alive"
#'   )
#' )
#' sdt <- popEpi::surv_lexis(
#'   lexis = sire,
#'   breaks = bl,
#'   merge_dt_by = c("sex", "ts_cal", "ts_age"),
#'   merge_dt = pm,
#'   aggre_by = NULL,
#'   subset = NULL,
#'   estimators = c(
#'     "F_ch",
#'     "F_ch_[F, T]"
#'   )
#' )
#' stopifnot(
#'   "F_ch_[F, T]_est" %in% names(sdt),
#'
#'   max(abs(sdt[["F_ch_[F, T]_est"]] - sdt[["F_ch_est"]])) < 1e-15
#' )
#'
#' # more arguments to popEpi::lexis_split_merge_aggregate_by_stratum
#' sdt <- popEpi::surv_lexis(
#'   lexis = sire,
#'   breaks = bl,
#'   split_merge_aggregate_optional_args = list(
#'     breaks_collapse_args = list(mandatory_breaks = 0:5)
#'   ),
#'   estimators = "S_ch"
#' )
#' stopifnot(
#'   1:5 %in% sdt[["ts_fut_stop"]]
#' )
surv_lexis <- function(
  lexis,
  breaks,
  merge_dt = NULL,
  merge_dt_by = NULL,
  aggre_by = NULL,
  subset = NULL,
  split_merge_aggregate_optional_args = NULL,
  estimators = "S_ch",
  conf_methods = "log",
  conf_lvls = 0.95,
  weights = NULL
) {
  #' @template param_lexis
  assert_is_arg_lexis(lexis, dt = FALSE)
  split_merge_aggregate_args <- list(
    lexis = lexis_to_lexis_dt__(lexis),
    breaks = breaks,
    merge_dt = merge_dt,
    merge_dt_by = merge_dt_by,
    aggre_by = handle_arg_by(by = aggre_by, dataset = lexis),
    subset = handle_arg_subset(dataset_nm = "lexis")
  )
  surv_estimate_args <- list(
    ts_fut_col_nm = utils::tail(names(breaks), 1),
    stratum_col_nms = names(split_merge_aggregate_args[["aggre_by"]]),
    conf_methods = conf_methods,
    conf_lvls = conf_lvls
  )
  #' @param weights `[NULL, data.table, character]` (default `NULL`)
  #'
  #' Weights for adjusting estimates.
  #'
  #' - `NULL`: No adjusting is performed.
  #' - `data.table`: Passed to `[surv_estimate]`.
  #' - `character`: Passed to `[lexis_split_merge_aggregate_by_stratum]`.
  if (data.table::is.data.table(weights)) {
    surv_estimate_args[["weight_dt"]] <- weights
    split_merge_aggregate_args[["aggre_by"]] <- handle_arg_by(
      by = list(
        split_merge_aggregate_args[["aggre_by"]],
        local({
          da_stratum_dt <- as.list(weights)[setdiff(names(weights), "weight")]
          data.table::setDT(da_stratum_dt)
          da_stratum_dt[]
        })
      ),
      dataset = lexis
    )
  } else {
    split_merge_aggregate_args[["weight_col_nm"]] <- weights
  }
  # @codedoc_comment_block popEpi::surv_lexis
  # Compute survival estimates on a `Lexis` dataset
  # (`[Epi::Lexis]` / `[Lexis_dt]`).
  #
  # Performs the following steps:
  #
  # - `estimators` is analysed and the following will be appended to
  #   `aggre_exprs` arg of `[lexis_split_merge_aggregate_by_stratum]` by
  #   `surv_lexis`:
  # @codedoc_insert_comment_block popEpi::surv_lexis_aggre_exprs__
  # - This results in `aggre_exprs` with both anything that the user defined
  #   and also what was added by `surv_lexis`. However, we drop duplicates
  #   in `aggre_exprs` based on both `duplicated(names(aggre_exprs))` and
  #   `duplicated(aggre_exprs)`. E.g. if you supply
  #   `aggre_exprs = list(n_events = quote(sum(lex.Xst != 0)))`
  #   and `n_events` is also added by `surv_lexis` then only the one you
  #   supplied is retained.
  # @codedoc_comment_block popEpi::surv_lexis
  estimator_dt <- handle_arg_estimators(estimators)
  split_merge_aggregate_args[["aggre_exprs"]] <- local({
    aggre_exprs <- c(
      split_merge_aggregate_args[["aggre_exprs"]],
      surv_lexis_aggre_exprs__(estimator_dt = estimator_dt)
    )
    keep <- !duplicated(aggre_exprs)
    if (!is.null(names(aggre_exprs))) {
      keep <- keep & !duplicated(names(aggre_exprs))
    }
    aggre_exprs <- aggre_exprs[keep]
    aggre_exprs
  })
  surv_estimate_args[["estimators"]] <- structure(
    estimator_dt[["expression_set"]],
    names = estimator_dt[["user_estimator_name"]]
  )
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
  #' @param aggre_by
  #' Passed to `[lexis_split_merge_aggregate_by_stratum]`.
  #' @param split_merge_aggregate_optional_args `[NULL, list]` (default `NULL`)
  #'
  #' Optional arguments to pass to `[lexis_split_merge_aggregate_by_stratum]`.
  #'
  #' - `NULL`: No additional arguments.
  #' - `list`: Pass these arguments. E.g.
  #'   `list(breaks_collapse_args = list(mandatory_breaks = 0:5))`.
  #' @param subset
  #' Passed to `[lexis_split_merge_aggregate_by_stratum]`.
  split_merge_aggregate_args <- c(
    split_merge_aggregate_args,
    split_merge_aggregate_optional_args
  )
  split_merge_aggregate_args <- split_merge_aggregate_args[
    !duplicated(names(split_merge_aggregate_args))
  ]
  surv_estimate_args[["dt"]] <- sdt <- call_with_arg_list__(
    popEpi::lexis_split_merge_aggregate_by_stratum,
    split_merge_aggregate_args
  )

  surv_estimate_args[["stratum_col_nms"]] <- local({
    scn <- surv_estimate_args[["stratum_col_nms"]]
    if (length(breaks) > 1) {
      # e.g. if aggregating by ts_cal to get a period analysis time series.
      stratum_ts_col_nms <- names(breaks)[-length(breaks)]
      scn <- c(
        surv_estimate_args[["stratum_col_nms"]],
        paste0(stratum_ts_col_nms, "_id")
      )
    }
    scn
  })
  # @codedoc_comment_block popEpi::surv_lexis
  # - Call `surv_estimate`.
  # @codedoc_comment_block popEpi::surv_lexis
  sdt <- call_with_arg_list__(
    popEpi::surv_estimate,
    surv_estimate_args
  )

  # @codedoc_comment_block return(popEpi::surv_lexis)
  # Returns a `data.table` as produced by `surv_estimate`.
  # @codedoc_comment_block return(popEpi::surv_lexis)
  return(sdt[])
}
