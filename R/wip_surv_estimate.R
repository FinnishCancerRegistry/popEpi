surv_pohar_perme_weight__ <- function(
  dt,
  ts_fut_breaks,
  ts_fut_col_nm,
  hazard_col_nm,
  method = c("subject subinterval", "survival interval")[1L]
) {
  assert_is_arg_dt(dt = dt, lexis = TRUE)
  lexis_ts_col_nms <- attr(dt, "time.scales")
  dt_key_col_nms <- data.table::key(dt)
  stopifnot(
    c("lex.id", "lex.dur") %in% names(dt),

    length(ts_fut_col_nm) == 1,
    ts_fut_col_nm %in% names(dt),
    ts_fut_col_nm %in% lexis_ts_col_nms,

    length(hazard_col_nm) == 1,
    hazard_col_nm %in% names(dt),
    # we actually only care about ts_fut_col_nm, but it is not always
    # the second key. note that the order of the data
    # is always the same regardless of the order of lexis_ts_col_nms
    # in the keys because the different time scales are simply the same
    # information with different first values (e.g. ts_fut starts from zero,
    # ts_age starts from age at diagnosis, etc). so we allow also ts_fut_col_nm
    # being e.g. the third key and the second being some other time scale.
    dt_key_col_nms[1] == "lex.id",
    ts_fut_col_nm %in% dt_key_col_nms,
    dt_key_col_nms[seq(2L, which(dt_key_col_nms == ts_fut_col_nm) - 1L)] %in%
      lexis_ts_col_nms,

    method %in% c("survival interval", "subject subinterval")
  )
  work_dt <- data.table::setDT(list(
    lex.id = dt[["lex.id"]],
    ts_fut = dt[[ts_fut_col_nm]],
    lex.dur = dt[["lex.dur"]],
    "expected_hazard" = dt[[hazard_col_nm]]
  ))
  data.table::setkeyv(work_dt, c("lex.id", "ts_fut"))
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
    stop("This may not be working correctly")
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
        x = dt[[ts_fut_col_nm]],
        breaks = ts_fut_breaks,
        right = FALSE,
        labels = FALSE
      )
    )
    data.table::setkeyv(
      work_dt,
      c("lex.id", "survival_interval_id")
    )
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
        #     and subject j the entry point to follow-up of that subject.
        #     This is typically the start of the first survival interval but
        #     late entries start after that.
        #     E.g. for one subject's rows with `t_start = c(0.10, 0.20)`
        #     the integration starts in both rows from
        #     from `t = 0.10`.
        # @codedoc_comment_block basicepistats:::surv_pohar_perme_weight__
        {
          rep(
            dt[[ts_fut_col_nm]][!duplicated(dt, by = "lex.id")],
            #' @importFrom data.table .N
            times = work_dt[j = list(n = .N), by = "lex.id"][["n"]]
          )
        },
        # @codedoc_comment_block basicepistats:::surv_pohar_perme_weight__
        #   + Take as the end point of integration for survival interval i
        #     and subject j the end of the survival interval i.
        #     E.g. `t = 1.0` for interval `[0.0, 1.0[`.
        # @codedoc_comment_block basicepistats:::surv_pohar_perme_weight__
        ts_fut_breaks[work_dt[["survival_interval_id"]] + 1L],
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
    join_dt <- data.table::setDT(list(
      lex.id = work_dt[["lex.id"]],
      survival_interval_id = work_dt[["survival_interval_id"]]
    ))
    data.table::setkeyv(join_dt, c("lex.id", "survival_interval_id"))
    work_dt <- work_dt[
      i = join_dt,
      on = c("lex.id", "survival_interval_id"),
      #' @importFrom data.table .SD
      j = .SD,
      .SDcols = "pp_weight"
    ]
  }
  return(work_dt[["pp_weight"]])
}

surv_estimate_expr_list__ <- list(
  # h
  hazard_observed = list(
    estimate = quote(hazard_observed),
    standard_error = quote(sqrt(hazard_observed))
  ),
  # h_exc_e2
  hazard_excess_ederer_ii = list(
    estimate = quote(hazard_excess_ederer_ii),
    standard_error = quote(sqrt(hazard_observed))
  ),
  # s_lt
  lifetable_observed_survival = list(
    estimate = quote(cumprod(1 -  n_events / n_at_risk_effective)),
    standard_error = quote(
      lifetable_observed_survival_estimate *
        sqrt(cumsum(n_events / (n_at_risk_effective * (n_at_risk_effective - n_events))))
    )
  ),
  # s_h
  hazard_observed_survival = list(
    estimate = quote(
      exp(-cumsum(survival_interval_width * hazard_observed))
    ),
    standard_error = quote(
      hazard_observed_survival_estimate *
        sqrt(cumsum((survival_interval_width ^ 2) * n_events / (total_subject_time ^ 2)))
    )
  ),
  # s_lt_exp_e2
  lifetable_expected_survival_ederer_ii = list(
    estimate = quote(
      cumprod(1 - n_events_expected_ederer_ii / n_at_risk_effective)
    ),
    standard_error = quote(
      rep(0.0, length(lifetable_expected_survival_estimate))
    )
  ),
  # s_h_exp_e2
  hazard_expected_survival_ederer_ii = list(
    estimate = quote(
      exp(-cumsum(survival_interval_width * hazard_expected_ederer_ii))
    ),
    standard_error = quote(
      rep(0.0, length(hazard_expected_survival_ederer_ii))
    )
  ),
  # rs_lt_e2
  lifetable_relative_survival_ederer_ii = list(
    estimate = quote(
      cumprod(1 - (n_events - n_events_expected_ederer_ii) / n_at_risk_effective)
    ),
    standard_error = quote(
      lifetable_observed_survival_standard_error / lifetable_expected_survival_ederer_ii_estimate
    )
  ),
  # rs_h_e2
  hazard_relative_survival_ederer_ii = list(
    estimate = quote(
      exp(-cumsum(survival_interval_width * hazard_excess_ederer_ii))
    ),
    standard_error = quote(
      hazard_observed_survival_standard_error / hazard_expected_survival_ederer_ii_estimate
    )
  ),
  # ns_lt_pp
  lifetable_net_survival_pohar_perme = list(
    estimate = quote(
      cumprod(1 - (n_events_pp - n_events_expected_pp) / n_in_follow_up_effective_pp)
    ),
    standard_error = quote(
      lifetable_net_survival_pohar_perme_estimate *
        sqrt(cumsum(n_events_pp_double_weighted / (n_at_risk_effective ^ 2)))
    )
  ),
  # ns_h_pp
  hazard_net_survival_pohar_perme = list(
    estimate = quote(
      exp(-cumsum(survival_interval_width * hazard_excess_pp))
    ),
    standard_error = quote(
      hazard_net_survival_pohar_perme_estimate *
        sqrt(cumsum((survival_interval_width ^ 2) * n_events_pp_double_weighted / (total_subject_time_pp ^ 2)))
    )
  ),
  # ar_lt
  lifetable_observed_absolute_risk <- list(
    estimate = quote(
      cumprod()
    ),
    standard_error = quote(
      NA_real_
    )
  ),
  # ar_h
  hazard_observed_absolute_risk <- list(
    estimate = quote(
      exp(-cumsum(hazard_observed))
    ),
    standard_error = quote(
      NA_real_
    )
  ),
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
  # s_lt_exp_e1
  lifetable_expected_survival_ederer_i = list(
    estimate = quote(
      lifetable_expected_survival_ederer_i_estimate
    ),
    standard_error = quote(
      0.0
    )
  ),
  # s_h_exp_e1
  hazard_expected_survival_ederer_i = list(
    estimate = quote(
      hazard_expected_survival_ederer_i_estimate
    ),
    standard_error = quote(
      0.0
    )
  ),
  # s_lt_def_e1
  lifetable_observed_survival_deficit_ederer_i = list(
    estimate = quote({
      obs_surv <- lifetable_observed_survival_estimate
      exp_surv <- lifetable_expected_survival_ederer_i_estimate
      exp_surv - obs_surv
    }),
    standard_error = quote(
      lifetable_observed_survival_standard_error
    )
  ),
  # s_h_def_e1
  hazard_observed_survival_deficit_ederer_i = list(
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
    # e.g. expr = quote(exp(-cumsum(survival_interval_width * hazard_observed)))
    expr <- utility_expr_list[[utility_expr_nm]]
    # e.g. expr_expr = quote(substitute(exp(-cumsum(survival_interval_width * hazard_observed)), utility_expr_list))
    expr_expr <- substitute(
      substitute(expr, utility_expr_list),
      list(expr = expr)
    )
    # e.g. expr = quote(exp(-cumsum(survival_interval_width * n_events / total_subject_time)))
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
surv_estimate_expression__ <- function(type) {
  stopifnot(
    type %in% names(surv_estimate_expr_list__)
  )
  out <- surv_estimate_expr_list__[type]
  return(out)
}
surv_estimate_expression_table__ <- function() {
  out <- lapply(names(surv_estimate_expr_list__), function(estimator_name) {
    cbind(
      estimator = estimator_name,
      as.data.frame(lapply(
        surv_estimate_expr_list__[[estimator_name]],
        deparse1
      ))
    )
  })
  out <- data.table::rbindlist(out)
  return(out[])
}

#' @eval codedoc::pkg_doc_fun(
#'   "popEpi::surv_estimate",
#'   "surv_functions"
#' )
#' @examples
#'
#' # popEpi::surv_estimate
#' dt <- data.table::data.table(
#'   ts_fut_start = seq(0, 5 - 1 / 12, 1 / 12),
#'   ts_fut_stop = seq(1 / 12, 5, 1 / 12),
#'   n_events = rpois(n = 60, lambda = 60:1),
#'   total_subject_time = rpois(n = 60, lambda = 300:240)
#' )
#' data.table::set(
#'   x = dt,
#'   j = "h",
#'   value = dt[["n_events"]] / dt[["total_subject_time"]]
#' )
#' popEpi::surv_estimate(
#'   dt = dt,
#'   ts_fut_col_nm = "ts_fut",
#'   estimators = c("hazard_observed", "hazard_observed_survival"),
#'   conf_methods = "log-log"
#' )
#' exp_col_nms <- sprintf(
#'   "%s_estimate", c("hazard_observed", "hazard_observed_survival")
#' )
#' stopifnot(
#'   exp_col_nms %in% names(dt)
#' )
#' popEpi::surv_estimate(
#'   dt = dt,
#'   ts_fut_col_nm = "ts_fut",
#'   estimators = list(
#'     "hazard_observed",
#'     my_surv = list(
#'       estimate = quote(
#'         exp(-cumsum((ts_fut_stop - ts_fut_start) * hazard_observed_estimate))
#'       ),
#'       standard_error = quote(rep(0.0, length(ts_fut_start)))
#'     )
#'   ),
#'   conf_methods = c("log", "none")
#' )
#' stopifnot(
#'   dt[["h"]] == dt[["hazard_observed_estimate"]]
#' )
#'
surv_estimate <- function(
  dt,
  ts_fut_col_nm,
  stratum_col_nms = NULL,
  value_col_nms = NULL,
  estimators = "hazard_observed_survival",
  weights = NULL,
  conf_methods = "log",
  conf_lvls = 0.95
) {
  # @codedoc_comment_block news("popEpi::surv_estimate", "2026-02-03", "0.5.0")
  # New function `surv_estimate` for estimating arbitrary survival time
  # functions using aggregated data.
  # @codedoc_comment_block news("popEpi::surv_estimate", "2026-02-03", "0.5.0")
  # @codedoc_comment_block popEpi::surv_estimate::dt
  # @param dt
  #
  # - `surv_estimate`: A `data.table`.
  # @codedoc_insert_comment_block surv_arg_dt
  # @codedoc_comment_block popEpi::surv_estimate::dt
  assert_is_arg_dt(dt, lexis = FALSE)
  stopifnot(
    #' @param ts_fut_col_nm `[character]` (no default)
    #'
    #' Name of time scale column over which survival estimates will be computed.
    #' E.g. `"ts_fut"`. `dt` must contain columns named
    #' `paste0(ts_fut_col_nm, "_", c("start", "stop"))`, e.g.
    #' `c("ts_fut_start", "ts_fut_stop")`.
    length(ts_fut_col_nm) == 1,
    paste0(ts_fut_col_nm, "_", c("start", "stop")) %in% names(dt),

    # @codedoc_comment_block popEpi::surv_estimate::conf_methods
    # @param conf_methods `[character, list]` (default `"log"`)
    #
    # Must be of length 1 or `length(estimators)`.
    # @codedoc_comment_block popEpi::surv_estimate::conf_methods
    inherits(conf_methods, c("list", "character")),
    length(conf_methods) %in% c(1L, length(estimators)),

    # @codedoc_comment_block popEpi::surv_estimate::conf_lvls
    # @param conf_lvls `[numeric]` (default `0.95`)
    #
    # Must be of length 1 or `length(estimators)`.
    # @codedoc_comment_block popEpi::surv_estimate::conf_lvls
    is.numeric(conf_lvls),
    length(conf_lvls) %in% c(1L, length(estimators)),

    inherits(weights, c("NULL", "data.table"))
  )
  call_env <- parent.frame(1L)

  # attribute can be NULL if `dt` is not the result of this function's call.
  # which we allow for generality.
  aggre_meta <- as.list(attr(dt, "surv_split_merge_aggregate_by_stratum_meta"))
  stopifnot(
    #' @param stratum_col_nms `[NULL, character]` (default `NULL`)
    #'
    #' Stratum column names in `dt`, if any.
    #'
    #' - `NULL`: If `dt` was the result of calling
    #'   `[surv_split_merge_aggregate_by_stratum]`, then `stratum_col_nms` is
    #'   taken from the attributes of `dt`. If not, this causes no
    #'   stratification of output.
    #' - `character`: `dt` is stratified by these columns. `character(0)` is
    #'   also allowed and causes no stratification of output.
    is.null(stratum_col_nms) || all(stratum_col_nms %in% names(dt))
  )
  if (is.null(stratum_col_nms)) {
    stratum_col_nms <- as.character(aggre_meta[["stratum_col_nms"]])
  }

  stopifnot(
    #' @param value_col_nms `[NULL, character]` (default `NULL`)
    #'
    #' Value column names in `dt`, if any.
    #'
    #' - `NULL`: If `dt` was the result of calling
    #'   `[surv_split_merge_aggregate_by_stratum]`, then `value_col_nms` is
    #'   taken from the attributes of `dt`. If not, having this `NULL` causes
    #'   no value columns from `dt` to be included in the output.
    #' - `character`: One or more names of columns in `dt` containing values
    #'   to be included in the output in addition to the estimate etc. columns.
    #'   E.g. `value_col_nms = c("n_events", "total_subject_time")`.
    is.null(value_col_nms) || all(value_col_nms %in% names(dt))
  )
  if (is.null(value_col_nms)) {
    value_col_nms <- as.character(aggre_meta[["value_col_nms"]])
    if (length(value_col_nms) == 0) {
      stop("Could not infer argument `value_col_nms`. Please supply it ",
           "yourself.")
    }
  }

  # @codedoc_comment_block popEpi::surv_estimate::weights
  # @param weights
  #
  # Specifies weights to adjust by. What is accepted depends on the function.
  # See **Details** to understand how the `weights` argument is used.
  #
  # - `popEpi::surv_estimate`: `[data.table, NULL]` (default `NULL`)
  # @codedoc_insert_comment_block surv_arg_weights
  # @codedoc_comment_block popEpi::surv_estimate::weights
  assert_is_arg_weights(weights = weights, dt = out)

  # @codedoc_comment_block popEpi::surv_estimate
  # Compute survival time function estimates. Performs the following steps:
  #
  # - Handles `estimators`. Elements of `estimators` that are character
  #   strings cause the corresponding pre-defined formulae to be used.
  #   E.g. `"observed_survival"`.
  #   Pre-defined estimators with their formulae:
  #
  # ${paste0(knitr::kable(popEpi:::surv_estimate_expression_table__()), collapse = "\n")}
  #
  # @codedoc_comment_block popEpi::surv_estimate
  expressions <- handle_arg_estimators(estimators)
  if (length(conf_methods) == 1) {
    conf_methods <- rep(conf_methods, length(expressions))
  }
  names(conf_methods) <- names(expressions)
  if (length(conf_lvls) == 1) {
    conf_lvls <- rep(conf_lvls, length(expressions))
  }
  names(conf_lvls) <- names(expressions)

  out <- data.table::setDT(as.list(dt))
  data.table::setkeyv(out, data.table::key(dt))

  estimate_stratum_col_nms <- stratum_col_nms
  do_direct_adjusting <- data.table::is.data.table(weights)
  if (do_direct_adjusting) {
    estimate_stratum_col_nms <- union(
      stratum_col_nms,
      setdiff(names(do_direct_adjusting), "weight")
    )
  }

  data.table::set(
    x = out,
    j = "survival_interval_width",
    value = dt[[paste0(ts_fut_col_nm, "_stop")]] -
      dt[[paste0(ts_fut_col_nm, "_start")]]
  )
  for (estimator_name in names(expressions)) {
    # @codedoc_comment_block popEpi::surv_estimate
    # - Armed with a list of expressions based on `estimates`, called
    #   `expressions`, for each `i`:
    #   + Evaluate each element of `expressions[[i]]` and add the result into
    #     `dt`. E.g. `hazard_observed_survival_estimate` and
    #     `hazard_observed_survival_standard_error`.
    # @codedoc_comment_block popEpi::surv_estimate
    for (element_name in names(expressions[[estimator_name]])) {
      # @codedoc_comment_block popEpi::surv_estimate::estimators
      # - `list`: Each element must be a list with named elements
      #   + `estimate`: Quoted ([quote]) R expression which when evaluated with
      #     `eval(expr, .SD, call_env)` produces the estimates. Here `expr`
      #     is the expression, `.SD` is the subset of `dt` for one stratum, and
      #     `call_env` is the environment from which this
      #     functions was called.
      #   + `standard_error`: Also a quoted R expression. This should produce
      #     the standard errors.
      # @codedoc_comment_block popEpi::surv_estimate::estimators
      add_col_nm <- sprintf("%s_%s", estimator_name, element_name)
      out[
        #' @importFrom data.table := .SD
        j = (add_col_nm) := eval(
          expressions[[estimator_name]][[element_name]],
          envir = .SD,
          enclos = call_env
        ),
        by = eval(estimate_stratum_col_nms)
      ]
    }
    # @codedoc_comment_block popEpi::surv_estimate
    #   + If `conf_methods[[i]]` is not `"none"`, compute confidence intervals
    #     by calling `[directadjusting::delta_method_confidence_intervals]`
    #     and add them into `dt`.
    # @codedoc_comment_block popEpi::surv_estimate
    if (!isTRUE(all.equal(
      conf_methods[[estimator_name]],
      "none",
      check.attributes = FALSE
    ))) {
      # @codedoc_comment_block popEpi::surv_estimate::conf_methods
      # Passed one at a time to
      # `[directadjusting::delta_method_confidence_intervals]`.
      # Except `"none"` causes no confidence intervals to be computed for that
      # estimator.
      # @codedoc_comment_block popEpi::surv_estimate::conf_methods
      # @codedoc_comment_block popEpi::surv_estimate::conf_lvls
      # Passed one at a time to
      # `[directadjusting::delta_method_confidence_intervals]`.
      # @codedoc_comment_block popEpi::surv_estimate::conf_lvls
      data.table::set(
        x = out,
        j = paste0(estimator_name, "_", c("lo", "hi")),
        value = directadjusting::delta_method_confidence_intervals(
          statistics = out[[paste0(estimator_name, "_estimate")]],
          variances = out[[paste0(estimator_name, "_standard_error")]] ^ 2,
          conf_lvl = conf_lvls[[estimator_name]],
          conf_method = conf_methods[[estimator_name]]
        )[
          #' @importFrom data.table .SD
          j = .SD,
          .SDcols = c("ci_lo", "ci_hi")
        ]
      )
    }
  }

  if (do_direct_adjusting) {
    sdt <- local({
      estimate_col_nms <- paste0(names(expressions), "_estimate")
      standard_error_col_nms <- paste0(names(expressions), "_standard_error")
      data.table::set(
        sdt,
        j = standard_error_col_nms,
        value = lapply(standard_error_col_nms, function(secn) {
          sdt[[secn]] ^ 2
        })
      )
      variance_col_nms <- sub(
        "_estimate$", "_variance", estimate_col_nms
      )
      data.table::setnames(sdt, standard_error_col_nms, variance_col_nms)
      da_stratum_col_nms <- c(stratum_col_nms,  "box_id")
      da_adjust_col_nms <- character(0)
      if (data.table::is.data.table(weights)) {
        da_adjust_col_nms <- setdiff(names(weights), "weight")
      }
      # @codedoc_comment_block popEpi::surv_estimate
      # - If `weights` was a `data.table`, we perform an additional direct
      #   adjusting step by calling
      #   `[directadjusting::directly_adjusted_estimates]`.
      # @codedoc_comment_block popEpi::surv_estimate
      sdta <- directadjusting::directly_adjusted_estimates(
        stats_dt = sdt,
        stratum_col_nms = da_stratum_col_nms,
        stat_col_nms = estimate_col_nms,
        var_col_nms = variance_col_nms,
        adjust_col_nms = da_adjust_col_nms,
        conf_methods = conf_methods,
        conf_lvls = conf_lvls,
        weights = weights
      )
      data.table::set(
        x = sdta,
        j = variance_col_nms,
        value = lapply(variance_col_nms, function(vcn) {
          sqrt(sdta[[vcn]])
        })
      )
      data.table::setnames(sdta, variance_col_nms, standard_error_col_nms)

      sum_col_nms <- aggre_meta[["value_col_nms"]]
      nonsum_col_nms <- c(
        intersect(aggre_meta[["stratum_col_nms"]], names(sdta)),
        setdiff(
          names(sdt),
          c(
            sum_col_nms, variance_col_nms, estimate_col_nms,
            aggre_meta[["stratum_col_nms"]]
          )
        )
      )
      # @codedoc_comment_block popEpi::surv_estimate
      # - If `weights` was a `data.table`, the summary statistics such as
      #   `n_events` are summed over the adjusting strata and will be included
      #   in the output. These are not weighted averages/sums but simple sums.
      # @codedoc_comment_block popEpi::surv_estimate
      sum_dt <- sdt[
        #' @importFrom data.table .SD
        j = lapply(.SD, sum),
        .SDcols = sum_col_nms,
        #' @importFrom data.table .EACHI
        keyby = eval(nonsum_col_nms)
      ]
      add_col_nms <- setdiff(
        names(sum_dt),
        names(sdta)
      )
      data.table::set(
        x = sdta,
        j = add_col_nms,
        value = sum_dt[
          #' @importFrom data.table .SD
          j = .SD,
          .SDcols = add_col_nms
        ]
      )
      ci_col_nms <- names(sdta)[grepl("(_lo$)|(_hi$)", names(sdta))]
      data.table::setcolorder(
        sdta,
        setdiff(
          names(sdta),
          c(estimate_col_nms, standard_error_col_nms, ci_col_nms)
        )
      )
      data.table::setkeyv(sdta, nonsum_col_nms)
      data.table::setnames(sdta, ci_col_nms, sub("estimate_", "", ci_col_nms))
      sdta
    })
  }

  # @codedoc_comment_block popEpi::surv_estimate
  # - Return `dt` invisibly.
  # @codedoc_comment_block popEpi::surv_estimate
  # @codedoc_comment_block return(popEpi::surv_estimate)
  # Returns `dt` invisibly after adding new columns depending on argument
  # `estimators`.
  # You don't need to keep the output of `popEpi::surv_estimate` because it
  # modifies `dt` in place.
  # @codedoc_comment_block return(popEpi::surv_estimate)
  return(out[])
}

surv_estimate_ederer_i <- function(
  dt,
  breaks,
  ts_col_nm,
  merge_dt,
  merge_dt_by
) {
  assert_is_arg_dt(dt, lexis = TRUE)
  stopifnot(
    identical(data.table::key(dt)[1], "lex.id"),
    ts_col_nm %in% data.table::key(dt)
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
    aggre_expr = quote(list(
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
