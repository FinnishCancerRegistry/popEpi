surv_pohar_perme_weight__ <- function(
  lexis,
  ts_fut_breaks,
  ts_fut_col_nm,
  hazard_col_nm,
  method = c("subject subinterval", "survival interval")[1L]
) {
  assert_is_arg_lexis(lexis, dt = FALSE)
  lexis_ts_col_nms <- Epi::timeScales(lexis)
  dt_key_col_nms <- data.table::key(lexis)
  stopifnot(
    c("lex.id", "lex.dur") %in% names(lexis),

    length(ts_fut_col_nm) == 1,
    ts_fut_col_nm %in% names(lexis),
    ts_fut_col_nm %in% lexis_ts_col_nms,

    length(hazard_col_nm) == 1,
    hazard_col_nm %in% names(lexis),
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
    lex.id = lexis[["lex.id"]],
    ts_fut = lexis[[ts_fut_col_nm]],
    lex.dur = lexis[["lex.dur"]],
    "expected_hazard" = lexis[[hazard_col_nm]]
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
        x = lexis[[ts_fut_col_nm]],
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
            lexis[[ts_fut_col_nm]][!duplicated(lexis, by = "lex.id")],
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

surv_estimate_expression__ <- function(type) {
  surv_estimate_expr_list__ <- get_internal_dataset("surv_estimate_expr_list__")
  if (!type %in% names(surv_estimate_expr_list__)) {
    stop("Unidentified estimator name: ", deparse1(type), ". Known ",
         "estimators: ", deparse1(names(surv_estimate_expr_list__)))
  }
  out <- surv_estimate_expr_list__[type]
  return(out)
}

surv_estimate_expression_table_clean__ <- function(x) {
  if (is.language(x)) {
    x <- deparse1(x, collapse = "; ")
  }
  x <- sprintf("\\verb{%s}", x)
  return(x)
}
surv_estimate_expression_table__ <- function() {
  dt <- get_internal_dataset("surv_estimate_expr_table__")
  out <- data.table::data.table(
    Name = surv_estimate_expression_table_clean__(dt[["name"]]),
    Explanation = dt[["info"]],
    Estimator = surv_estimate_expression_table_clean__(dt[["est"]]),
    "Standard Error" = surv_estimate_expression_table_clean__(dt[["se"]])
  )
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
#'   box_id = 1:60,
#'   ts_fut_start = seq(0, 5 - 1 / 12, 1 / 12),
#'   ts_fut_stop = seq(1 / 12, 5, 1 / 12),
#'   n_events = rpois(n = 60, lambda = 60:1),
#'   t_at_risk = rpois(n = 60, lambda = 300:240)
#' )
#' data.table::set(
#'   x = dt,
#'   j = "my_h_ch",
#'   value = dt[["n_events"]] / dt[["t_at_risk"]]
#' )
#' sdt <- popEpi::surv_estimate(
#'   dt = dt,
#'   ts_fut_col_nm = "ts_fut",
#'   value_col_nms = c("n_events", "t_at_risk"),
#'   estimators = c("h_ch", "S_ch"),
#'   conf_methods = "log-log"
#' )
#' stopifnot(
#'   c("h_ch_est", "S_ch_est") %in% names(sdt),
#'   dt[["my_h_ch"]] == sdt[["h_ch_est"]]
#' )
#' sdt <- popEpi::surv_estimate(
#'   dt = dt,
#'   ts_fut_col_nm = "ts_fut",
#'   value_col_nms = c("n_events", "t_at_risk"),
#'   estimators = list(
#'     "h_ch",
#'     my_surv = list(
#'       est = quote(
#'         exp(-cumsum((ts_fut_stop - ts_fut_start) * h_ch_est))
#'       ),
#'       se = quote(rep(0.0, length(ts_fut_start)))
#'     )
#'   ),
#'   conf_methods = c("log", "none")
#' )
#' stopifnot(
#'   c("h_ch_est", "my_surv_est") %in% names(sdt)
#' )
#' sdt <- popEpi::surv_estimate(
#'   dt = dt,
#'   ts_fut_col_nm = "ts_fut",
#'   value_col_nms = c("n_events", "t_at_risk"),
#'   estimators = list(
#'     "h_ch",
#'     "S_ch"
#'   ),
#'   conf_methods = list(
#'     "log",
#'     list(
#'       g = quote(stats::qnorm(p = theta)),
#'       g_inv = quote(stats::pnorm(q = g))
#'     )
#'   )
#' )
#' stopifnot(
#'   c("h_ch_est", "S_ch_est") %in% names(sdt)
#' )
#'
surv_estimate <- function(
  dt,
  ts_fut_col_nm,
  stratum_col_nms = NULL,
  value_col_nms = NULL,
  estimators = "S_ch",
  weight_dt = NULL,
  conf_methods = "log",
  conf_lvls = 0.95
) {
  # @codedoc_comment_block news("popEpi::surv_estimate", "2026-02-03", "0.5.0")
  # New function `surv_estimate` for estimating arbitrary survival time
  # functions using aggregated data.
  # @codedoc_comment_block news("popEpi::surv_estimate", "2026-02-03", "0.5.0")
  #' @param dt `[data.table]` (no default)
  #'
  #' Dataset containing aggregate statistics
  #' which can be used to compute survival function estimates. Must also have
  #' column `box_id`, a running number like the one produced by
  #' `[lexis_split_merge_aggregate_by_stratum]`.
  assert_is_arg_dt(dt, lexis = FALSE)
  stopifnot(
    "box_id" %in% names(dt),

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
    length(conf_lvls) %in% c(1L, length(estimators))
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
    #'   `[lexis_split_merge_aggregate_by_stratum]`, then `stratum_col_nms` is
    #'   taken from the attributes of `dt`. If not, this causes no
    #'   stratification of output.
    #' - `character`: `dt` is stratified by these columns. `character(0)` is
    #'   also allowed and causes no stratification of output.
    is.null(stratum_col_nms) || all(stratum_col_nms %in% names(dt))
  )
  if (is.null(stratum_col_nms)) {
    if ("stratum_col_nms" %in% names(aggre_meta)) {
      stratum_col_nms <- as.character(aggre_meta[["stratum_col_nms"]])
    }
  }

  stopifnot(
    #' @param value_col_nms `[NULL, character]` (default `NULL`)
    #'
    #' Value column names in `dt`, if any.
    #'
    #' - `NULL`: If `dt` was the result of calling
    #'   `[lexis_split_merge_aggregate_by_stratum]`, then `value_col_nms` is
    #'   taken from the attributes of `dt`. If not, having this `NULL` causes
    #'   no value columns from `dt` to be included in the output.
    #' - `character`: One or more names of columns in `dt` containing values
    #'   to be included in the output in addition to the estimate etc. columns.
    #'   E.g. `value_col_nms = c("n_events", "t_at_risk")`.
    is.null(value_col_nms) || all(value_col_nms %in% names(dt))
  )
  if ("value_col_nms" %in% names(aggre_meta)) {
    value_col_nms <- as.character(aggre_meta[["value_col_nms"]])
  } else if (is.null(value_col_nms)) {
    stop("Could not infer argument `value_col_nms`. Please supply it ",
         "yourself.")
  }

  # @codedoc_comment_block popEpi::surv_estimate::weight_dt
  # @param weight_dt `[NULL, data.table]` (default `NULL`)
  #
  # Weights for direct adjusting.
  # See **Details** to understand how the `weights` argument is used.
  #
  # - `NULL`: No direct adjusting is performed.
  # - `data.table`: These weights are used for adjusting.
  #
  # @codedoc_insert_comment_block popEpi:::assert_is_arg_weight_dt
  # @codedoc_comment_block popEpi::surv_estimate::weight_dt
  assert_is_arg_weight_dt(weight_dt = weight_dt, dt = dt)

  # @codedoc_comment_block popEpi::surv_estimate
  # Compute survival time function estimates. Performs the following steps:
  #
  # - Handles `estimators`. Elements of `estimators` that are character
  #   strings cause the corresponding pre-defined formulae to be used.
  #   E.g. `"S_ch"`.
  #   Pre-defined estimators with their formulae:
  #
  # ${paste0(knitr::kable(popEpi:::surv_estimate_expression_table__()), collapse = "\n")}
  #
  # @codedoc_comment_block popEpi::surv_estimate
  estimator_dt <- handle_arg_estimators(estimators)
  if (length(conf_methods) == 1) {
    conf_methods <- rep(conf_methods, nrow(estimator_dt))
  }
  names(conf_methods) <- estimator_dt[["user_estimator_name"]]
  if (length(conf_lvls) == 1) {
    conf_lvls <- rep(conf_lvls, nrow(estimator_dt))
  }
  names(conf_lvls) <- estimator_dt[["user_estimator_name"]]

  out <- data.table::setDT(as.list(dt))
  data.table::setkeyv(out, data.table::key(dt))

  estimate_stratum_col_nms <- stratum_col_nms
  do_direct_adjusting <- data.table::is.data.table(weight_dt)
  if (do_direct_adjusting) {
    estimate_stratum_col_nms <- union(
      stratum_col_nms,
      setdiff(names(weight_dt), "weight")
    )
  }
  # @codedoc_comment_block popEpi::surv_estimate
  # - Check columns `t_at_risk`, `n_at_risk_eff` for zeroes if they are in the
  #   `dt`. Intervals with e.g. `t_at_risk == 0` have no survival probability
  #   defined for them which causes `NA` values (i.e. zero divided by zero
  #   is not defined). Throw a warning if such intervals are found.
  # @codedoc_comment_block popEpi::surv_estimate
  lapply(intersect(names(dt), c("t_at_risk", "n_at_risk_eff")), function(nm) {
    is_na <- is.na(dt[[nm]])
    if (any(is_na)) {
      message("WARNING: intervals in `dt` where `dt$", nm, "` is zero:")
      print(dt[is_na, ])
      warning(
        "There were ", sum(is_na), " intervals in `dt` where `dt$", nm, "` ",
        "was zero. No survival probability can be estimated for such intervals."
      )
    }
  })

  # @codedoc_comment_block popEpi::surv_estimate
  # - Add column `delta_t` as the difference between the `_stop` and `_start`
  #   columns of the follow-up time scale, e.g. `ts_fut_stop - ts_fut_start`.
  # @codedoc_comment_block popEpi::surv_estimate
  data.table::set(
    x = out,
    j = "delta_t",
    value = dt[[paste0(ts_fut_col_nm, "_stop")]] -
      dt[[paste0(ts_fut_col_nm, "_start")]]
  )
  for (i in seq_len(nrow(estimator_dt))) {
    user_estimator_name <- estimator_dt[["user_estimator_name"]][i]
    # @codedoc_comment_block popEpi::surv_estimate
    # - Armed with a list of expressions based on `estimates`, called
    #   `expressions`, for each `i`:
    #   + Evaluate each element of `expressions[[i]]` and add the result into
    #     `dt`. E.g. `S_ch_est` and
    #     `S_ch_se`.
    # @codedoc_comment_block popEpi::surv_estimate
    for (element_name in names(estimator_dt[["expression_set"]][[i]])) {
      # @codedoc_comment_block popEpi::surv_estimate::estimators
      # - `list`: Each element must be a list with named elements
      #   + `est`: Quoted ([quote]) R expression which when evaluated with
      #     `eval(expr, .SD, call_env)` produces the estimates. Here `expr`
      #     is the expression, `.SD` is the subset of `dt` for one stratum, and
      #     `call_env` is the environment from which this
      #     functions was called.
      #   + `se`: Also a quoted R expression. This should produce
      #     the standard errors.
      # @codedoc_comment_block popEpi::surv_estimate::estimators
      add_col_nm <- sprintf("%s_%s", user_estimator_name, element_name)
      out[
        #' @importFrom data.table := .SD
        j = (add_col_nm) := eval(
          estimator_dt[["expression_set"]][[i]][[element_name]],
          envir = .SD,
          enclos = call_env
        ),
        by = eval(estimate_stratum_col_nms)
      ]
    }
  }

  out <- local({
    estimate_col_nms <- paste0(
      estimator_dt[["user_estimator_name"]], "_est"
    )
    standard_error_col_nms <- paste0(
      estimator_dt[["user_estimator_name"]], "_se"
    )
    data.table::set(
      out,
      j = standard_error_col_nms,
      value = lapply(standard_error_col_nms, function(secn) {
        out[[secn]] ^ 2
      })
    )
    variance_col_nms <- sub(
      "_est$", "_variance", estimate_col_nms
    )
    data.table::setnames(out, standard_error_col_nms, variance_col_nms)
    da_stratum_col_nms <- c(stratum_col_nms,  "box_id")
    da_adjust_col_nms <- character(0)
    if (data.table::is.data.table(weight_dt)) {
      da_adjust_col_nms <- setdiff(names(weight_dt), "weight")
    }
    #' @param conf_lvls
    #' Passed one at a time to
    #' `[directadjusting::directly_adjusted_estimates]`.
    #' @param conf_methods
    #' Passed one at a time to
    #' `[directadjusting::directly_adjusted_estimates]`.
    # @codedoc_comment_block popEpi::surv_estimate
    # - Call
    #   `[directadjusting::directly_adjusted_estimates]`.
    #   If no weights were given for direct adjusting then this step simply
    #   produces the confidence intervals.
    # @codedoc_comment_block popEpi::surv_estimate
    sdta <- directadjusting::directly_adjusted_estimates(
      stats_dt = out,
      stratum_col_nms = da_stratum_col_nms,
      stat_col_nms = estimate_col_nms,
      var_col_nms = variance_col_nms,
      adjust_col_nms = da_adjust_col_nms,
      conf_methods = conf_methods,
      conf_lvls = conf_lvls,
      weights = weight_dt
    )
    data.table::set(
      x = sdta,
      j = variance_col_nms,
      value = lapply(variance_col_nms, function(vcn) {
        sqrt(sdta[[vcn]])
      })
    )
    data.table::setnames(sdta, variance_col_nms, standard_error_col_nms)

    ci_col_nms <- names(out)[grepl("(_lo$)|(_hi$)", names(out))]
    nonsum_col_nms <- c(
      intersect(aggre_meta[["stratum_col_nms"]], names(sdta)),
      setdiff(
        names(out),
        c(
          value_col_nms, variance_col_nms, estimate_col_nms, ci_col_nms,
          aggre_meta[["stratum_col_nms"]]
        )
      )
    )
    # @codedoc_comment_block popEpi::surv_estimate
    # - If direct adjusting was performed, the summary statistics such as
    #   `n_events` are summed over the adjusting strata and will be included
    #   in the output. These are not weighted averages/sums but simple sums.
    # @codedoc_comment_block popEpi::surv_estimate
    if (length(value_col_nms) > 0) {
      sum_dt <- out[
        #' @importFrom data.table .SD
        j = lapply(.SD, sum),
        .SDcols = value_col_nms,
        #' @importFrom data.table .EACHI
        keyby = eval(nonsum_col_nms)
      ]
    } else {
      sum_dt <- out[
        i = !duplicated(out, by = nonsum_col_nms),
        #' @importFrom data.table .SD
        j = .SD,
        .SDcols = nonsum_col_nms
      ]
    }
    add_col_nms <- setdiff(
      names(sdta),
      names(sum_dt)
    )
    data.table::set(
      x = sum_dt,
      j = add_col_nms,
      value = as.list(sdta)[add_col_nms]
    )
    ci_col_nms <- names(sum_dt)[grepl("(_lo$)|(_hi$)", names(sum_dt))]
    data.table::setnames(
      sum_dt,
      ci_col_nms,
      sub("_est_", "_", ci_col_nms)
    )
    sum_dt[]
  })

  # @codedoc_comment_block popEpi::surv_estimate
  # - Return a `data.table` containing all columns in `dt` (unless direct
  #   adjusting was performed --- then the adjusting column(s) are not present)
  #   and additional columns of estimates, standard errors, and confidence
  #   intervals.
  # @codedoc_comment_block popEpi::surv_estimate
  # @codedoc_comment_block return(popEpi::surv_estimate)
  # Returns a `data.table` containing all columns in `dt` (unless direct
  # adjusting was performed --- then the adjusting column(s) are not present)
  # and additional columns of estimates, standard errors, and confidence
  # intervals.
  # @codedoc_comment_block return(popEpi::surv_estimate)
  return(out[])
}

surv_lexis_S_exp_e1_ch_mean <- function(
  lexis,
  breaks,
  aggre_ts_col_nms,
  merge_dt,
  merge_dt_by,
  aggre_by = NULL,
  weight_col_nm = NULL
) {
  assert_is_arg_lexis(lexis, dt = FALSE)
  assert_is_arg_weight_col_nm(weight_col_nm)
  # keep_col_nms <- unique(c(
  #   "lex.id",
  #   Epi::timeScales(lexis),
  #   "lex.dur", "lex.Cst", "lex.Xst",
  #   setdiff(names(merge_dt), "h_exp"),
  #   weight_col_nm,
  #   names(aggre_by)
  # ))
  # e1dt <- lexis_to_lexis_dt__(lexis, select = keep_col_nms)
  # data.table::set(
  #   x = e1dt,
  #   j = "keep",
  #   value = !duplicated(e1dt[["lex.id"]]) &
  #     # e.g. ts_fut = 0.5 is rejected if breaks[["ts_fut"]] = c(0, ...)
  #     # e.g. ts_cal = 2021.1 is rejected if breaks[["ts_cal"]] = c(..., 2021)
  #     rowSums(
  #       data.table::setDT(lapply(names(breaks), function(ts_col_nm) {
  #         data.table::between(
  #           e1dt[[ts_col_nm]],
  #           lower = min(breaks[[ts_col_nm]]) - 1e-10,
  #           upper = max(breaks[[ts_col_nm]]) - 1e-10,
  #           incbounds = TRUE
  #         )
  #       }))
  #     ) == length(breaks)
  # )
  # if (any(!e1dt[["keep"]])) {
  #   e1dt <- e1dt[e1dt[["keep"]], ]
  # }
  # data.table::set(e1dt, j = "keep", value = NULL)
  # ts_fut_col_nm <- utils::tail(aggre_ts_col_nms, 1L)
  # lexis_immortalise(lexis = e1dt, breaks = breaks[ts_fut_col_nm])
  # if (length(aggre_ts_col_nms) > 1) {
  #   stratum_ts_col_nms <- aggre_ts_col_nms[-length(aggre_ts_col_nms)]
  #   data.table::set(
  #     x = e1dt,
  #     j = stratum_ts_col_nms,
  #     value = lapply(stratum_ts_col_nms, function(ts_col_nm) {
  #       br <- breaks[[ts_col_nm]]
  #       idx <- cut(
  #         x = e1dt[[ts_col_nm]],
  #         breaks = br,
  #         right = FALSE,
  #         labels = FALSE
  #       )
  #       br[idx]
  #     })
  #   )
  #   aggre_by <- handle_arg_by(
  #     by = c(
  #       list(aggre_by),
  #       lapply(stratum_ts_col_nms, function(stratum_ts_col_nm) {
  #         br <- breaks[[stratum_ts_col_nm]]
  #         dt <- data.table::setDT(list(
  #           x = br[-length(br)]
  #         ))
  #         data.table::setnames(dt, "x", stratum_ts_col_nm)
  #         return(dt[])
  #       })
  #     ),
  #     dataset = e1dt
  #   )
  # }
  e1dt <- lexis_split_merge_aggregate_by_stratum(
    lexis = lexis,
    breaks = breaks,
    aggre_exprs = list(
      S_exp_e1_ch_mean = quote({
        if (!is.null(eval_env[["weight_col_nm"]])) {
          w <- .SD[[eval_env[["weight_col_nm"]]]]
          w <- w / sum(w[!duplicated(lex.id)])
          sum(S_exp_e1_ch_individual * w)
        } else {
          mean(S_exp_e1_ch_individual)
        }
      })
    ),
    aggre_by = aggre_by,
    aggre_ts_col_nms = aggre_ts_col_nms,
    merge_dt = merge_dt,
    merge_dt_by = merge_dt_by,
    weight_col_nm = weight_col_nm,
    optional_steps = list(
      stratum_post_merge = function(
        eval_env,
        call_env,
        stratum_eval_env
      ) {
        lexis_dt <- stratum_eval_env[["lexis_stratum_subset_split"]]
        aggre_ts_col_nms <- eval_env[["aggre_ts_col_nms"]]
        stratum_ts_col_nms <- aggre_ts_col_nms[-length(aggre_ts_col_nms)]
        if (length(stratum_ts_col_nms) > 0) {
          # problem: we want Ederer I curves to not be "period analysis" curves
          # where the same curve can contribute to multiple periods. instead
          # we want the whole curve to stay within the same period. or in other
          # words we want to turn the period analysis into a cohort analysis for
          # Ederer I.
          #
          # e.g. if breaks[["ts_fut"]] = seq(0, 5, 1 / 12), then certainly
          # `lexis_stratum_subset_split` has multiple calendar years for one
          # lex.id. if we want to aggregate by ts_cal, it can normally
          # happen that the same expected survival curve crosses over the
          # boundary of the periods, e.g. breaks[["ts_cal"]] = c(2003, 2008)
          # and one lex.id starts with ts_cal = 2007.13. but for the purpose
          # of computing the Ederer I curve per period we want the whole curve
          # to stay in the same period. we accomplish this by cheating --- we
          # set the (for instance) ts_cal value back to what the lex.id started
          # with for all observations for that lex.id in the split dataset.
          # e.g. then `ts_cal == 2007.13` for every observation for that lex.id.
          # then when the split data are aggregated by (e.g.) ts_cal we use the
          # whole Ederer I curve for that lex.id within the same period.
          data.table::set(
            x = lexis_dt,
            j = stratum_ts_col_nms,
            value = stratum_eval_env[["lexis_stratum_subset"]][
              i = lexis_dt,
              on = "lex.id",
              j = .SD,
              .SDcols = stratum_ts_col_nms
            ]
          )
          lexis_dt <- lexis_dt[]
          data.table::setkeyv(lexis_dt, c("lex.id", Epi::timeScales(lexis_dt)))
          stratum_eval_env[["lexis_stratum_subset_split"]] <- lexis_dt
        }
      }
    ),
    split_lexis_column_exprs = list(
      S_exp_e1_ch_individual = quote({
        e1dt <- data.table::setDT(list(
          lex.id = lex.id,
          ts_fut = ts_fut,
          box_id = box_id,
          lex.dur = lex.dur, # this is here equal to width of interval always
          h_exp = h_exp
        ))
        data.table::set(
          x = e1dt,
          j = "e1",
          value = e1dt[["h_exp"]] * e1dt[["lex.dur"]]
        )
        # e1dt$e1 is now the integrated hazard over the individual survival
        # interval,i.e. H(t_i|t_{i-1}), per subject & interval
        data.table::setkeyv(e1dt, c("lex.id", "box_id"))
        e1dt[
          #' @importFrom data.table := .SD
          j = "e1" := lapply(.SD, cumsum),
          .SDcols = "e1",
          by = "lex.id"
        ]
        # e1dt$e1 is now the cumulative hazard (i.e. H(t_i)) per subject
        data.table::set(
          x = e1dt,
          j = "e1",
          value = exp(-e1dt[["e1"]])
        )
        # e1dt$e1 is now the expected survival per subject
        e1dt[["e1"]]
      })
    )
  )
  return(e1dt[["S_exp_e1_ch_mean"]])
  # e1dt <- popEpi::splitMulti(
  #   data = e1dt,
  #   breaks = breaks[ts_fut_col_nm]
  # )
  # data.table::setkeyv(e1dt, c("lex.id", ts_fut_col_nm))
  # lexis_merge(
  #   lexis = e1dt,
  #   merge_dt = merge_dt,
  #   merge_dt_by = merge_dt_by
  # )
  # box_dt <- lexis_box_dt__(breaks = breaks[ts_fut_col_nm])
  # lexis_box_id__(lexis = e1dt, box_dt = box_dt)
  # data.table::setDT(e1dt)
  # data.table::set(
  #   x = box_dt,
  #   j = "delta_t",
  #   value = box_dt[[paste0(ts_fut_col_nm, "_stop")]] -
  #     box_dt[[paste0(ts_fut_col_nm, "_start")]]
  # )
  # data.table::set(
  #   x = e1dt,
  #   j = "e1",
  #   value = e1dt[["h_exp"]] * box_dt[["delta_t"]][e1dt[["box_id"]]]
  # )
  # # e1dt$e1 is now the integrated hazard over the individual survival interval,
  # # i.e. H(t_i|t_{i-1}), per subject
  # data.table::set(
  #   x = e1dt,
  #   j = setdiff(names(e1dt), c("lex.id", "box_id", "e1", "w")),
  #   value = NULL
  # )
  # data.table::setkeyv(e1dt, c("lex.id", "box_id"))
  # e1dt[
  #   #' @importFrom data.table := .SD
  #   j = "e1" := lapply(.SD, cumsum),
  #   .SDcols = "e1",
  #   by = "lex.id"
  # ]
  # # e1dt$e1 is now the cumulative hazard (i.e. H(t_i)) per subject
  # data.table::set(
  #   x = e1dt,
  #   j = "e1",
  #   value = exp(-e1dt[["e1"]])
  # )
  # # e1dt$e1 is now the expected survival per subject
  # data.table::set(
  #   x = e1dt,
  #   j = "e1",
  #   value = e1dt[["e1"]] * e1dt[["w"]]
  # )
  # e1dt <- e1dt[
  #   #' @importFrom data.table .SD
  #   j = lapply(.SD, sum),
  #   .SDcols = "e1",
  #   keyby = "box_id"
  # ]
  # # e1dt$e1 is now the overall expected survival
  # return(e1dt[["e1"]])
}
