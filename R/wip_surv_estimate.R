surv_pohar_perme_weight__ <- function(
  lexis,
  ts_fut_breaks,
  ts_fut_col_nm,
  hazard_col_nm,
  method = c("subject subinterval", "survival interval")[1L]
) {
  assert_is_arg_lexis(lexis, dt = FALSE)
  if (nrow(lexis) == 0) {
    return(double(0))
  }
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
    dt_key_col_nms[seq(which(dt_key_col_nms == ts_fut_col_nm))] %in%
      c("lex.id", lexis_ts_col_nms),

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

surv_estimate_expr__ <- function(type) {
  surv_estimate_expr_list__ <- get_internal_dataset("surv_estimate_expr_list__")
  if (!type %in% names(surv_estimate_expr_list__)) {
    stop("Unidentified estimator name: ", deparse1(type), ". Known ",
         "estimators: ", deparse1(names(surv_estimate_expr_list__)))
  }
  out <- surv_estimate_expr_list__[type]
  return(out)
}

doc_verb__ <- function(x) {
  if (is.language(x)) {
    x <- deparse1(x, collapse = "; ")
  }
  x <- sprintf("\\verb{%s}", x)
  return(x)
}
surv_estimate_expression_table__ <- function() {
  dt <- get_internal_dataset("surv_estimate_expr_table__")
  out <- data.table::data.table(
    Name = doc_verb__(dt[["name"]]),
    Explanation = dt[["info"]],
    Estimator = doc_verb__(dt[["est"]]),
    "Standard Error" = doc_verb__(dt[["se"]])
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
#' # one approach to brenner weighting --- effectively the same as assigning
#' # a weight for each individual using strata.
#' make_column_ag_icss <- function(age) {
#'   cut(
#'     age,
#'     breaks = c(0, 60, 70, 80, Inf),
#'     right = FALSE,
#'     labels = c("0-59", "60-69", "70-79", "80+")
#'   )
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
#'   sire[["ag_icss"]] <- make_column_ag_icss(sire[["dg_age"]])
#'   sire
#' }
#'
#' sire <- make_sire()
#'
#' make_weight_dt <- function() {
#'   wdt <- popEpi::ICSS[
#'     j = list(
#'       standard_weight = as.double(sum(.SD[["ICSS1"]]))
#'     ),
#'     keyby = list(
#'       ag_icss = make_column_ag_icss(popEpi::ICSS[["age"]])
#'     )
#'   ]
#'   wdt[
#'     j = "standard_weight" := wdt[["standard_weight"]] /
#'       sum(wdt[["standard_weight"]])
#'   ]
#'   owdt <- data.table::setDT(as.list(sire))[
#'     j = list(observed_weight = .N),
#'     keyby = "ag_icss"
#'   ]
#'   owdt[
#'     j = "observed_weight" := owdt[["observed_weight"]] /
#'       sum(owdt[["observed_weight"]])
#'   ]
#'   wdt[
#'     j = "observed_weight" := owdt[["observed_weight"]]
#'   ]
#'   wdt[
#'     j = "brenner_weight" := wdt[["standard_weight"]] / owdt[["observed_weight"]]
#'   ]
#'   wdt[i = is.na(wdt[["brenner_weight"]]), j = "brenner_weight" := 0.0]
#'   return(wdt[])
#' }
#'
#' wdt <- make_weight_dt()
#'
#' sdt <- popEpi::lexis_split_merge_aggregate_by_stratum(
#'   lexis = sire,
#'   breaks = list(ts_fut = seq(0, 5, 1 / 12)),
#'   aggre_exprs = c("t_at_risk", "n_events"),
#'   aggre_by = "ag_icss"
#' )
#'
#' sdt_bw <- popEpi::surv_estimate(
#'   dt = sdt,
#'   ts_fut_col_nm = "ts_fut",
#'   estimators = "S_ch",
#'   weight_dt = data.table::data.table(
#'     ag_icss = wdt[["ag_icss"]],
#'     brenner_weight = wdt[["brenner_weight"]]
#'   ),
#'   value_col_nms = c("n_events", "t_at_risk")
#' )
#' stopifnot("S_ch_est" %in% names(sdt_bw), !"ag_icss" %in% names(sdt_bw))
#'
#' # direct adjusting for comparison
#' sdt_da <- popEpi::surv_estimate(
#'   dt = sdt,
#'   ts_fut_col_nm = "ts_fut",
#'   estimators = "S_ch",
#'   weight_dt = data.table::data.table(
#'     ag_icss = wdt[["ag_icss"]],
#'     weight = wdt[["standard_weight"]]
#'   ),
#'   value_col_nms = c("n_events", "t_at_risk")
#' )
#' stopifnot("S_ch_est" %in% names(sdt_da), !"ag_icss" %in% names(sdt_da))
#'
#' stopifnot(
#'   max(abs(sdt_bw[["S_ch_est"]] - sdt_da[["S_ch_est"]])) < 0.01
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
    length(stratum_col_nms) == 0 || all(stratum_col_nms %in% names(dt)),

    !duplicated(
      x = dt,
      by = intersect(
        names(dt),
        c(stratum_col_nms, names(weight_dt), paste0(ts_fut_col_nm, "_start"))
      )
    )
  )

  stopifnot(
    #' @param value_col_nms `[NULL, character]` (default `NULL`)
    #'
    #' Value column names in `dt`, if any.
    #'
    #' - `NULL`: If `dt` was the result of calling
    #'   `[lexis_split_merge_aggregate_by_stratum]`, then `value_col_nms` is
    #'   taken from the attributes of `dt`. Else we take `double` and `integer`
    #'   columns whose names match regex `"(^t_)|(^n_)"` as the names of the
    #'   value columns.
    #' - `character`: One or more names of columns in `dt` containing values
    #'   to be included in the output in addition to the estimate etc. columns.
    #'   E.g. `value_col_nms = c("n_events", "t_at_risk")`.
    is.null(value_col_nms) || all(value_col_nms %in% names(dt))
  )
  # aggre_meta can be empty list()
  aggre_meta <- as.list(attr(dt, "lexis_split_merge_aggregate_by_stratum_meta"))
  if (is.null(value_col_nms)) {
    if ("value_col_nms" %in% names(aggre_meta)) {
      value_col_nms <- as.character(aggre_meta[["value_col_nms"]])
    } else {
      value_col_nms <- names(dt)[grepl("(^t_)|(^n_)", names(dt))]
      value_col_nms <- value_col_nms[vapply(
        as.list(dt)[value_col_nms], inherits, logical(1L),
        what = c("integer", "numeric")
      )]
      if (length(value_col_nms) == 0) {
        stop("Could not infer argument `value_col_nms`. Please supply it ",
             "yourself.")
      }
    }
  }

  # @codedoc_comment_block popEpi::surv_estimate::weight_dt
  # @param weight_dt `[NULL, data.table]` (default `NULL`)
  #
  # Weights for direct adjusting or Brenner weighting.
  # See **Details** to understand how the `weight_dt` argument is used.
  #
  # - `NULL`: No direct adjusting nor Brenner weighting is performed.
  # - `data.table`: Contains weighting strata and either column `"weight"` for
  #   direct adjusting or column `"brenner_weight"` for Brenner weighting.
  #
  # @codedoc_insert_comment_block popEpi:::assert_is_arg_weight_dt
  # @codedoc_comment_block popEpi::surv_estimate::weight_dt
  stopifnot(
    inherits(weight_dt, c("NULL", "data.table"))
  )
  if (data.table::is.data.table(weight_dt)) {
    stopifnot(
      "weight" %in% names(weight_dt) || "brenner_weight" %in% names(weight_dt),
      setdiff(names(weight_dt), c("weight", "brenner_weight")) %in% names(dt),
      !anyNA(weight_dt)
    )
  }

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
  do_direct_adjusting <- data.table::is.data.table(weight_dt) &&
    "weight" %in% names(weight_dt)
  if (do_direct_adjusting) {
    estimate_stratum_col_nms <- union(
      stratum_col_nms,
      setdiff(names(weight_dt), "weight")
    )
  }
  # @codedoc_comment_block popEpi::surv_estimate
  # - If `weight_dt` is a `data.table` with column `"brenner_weight"`, we
  #   merge the weights into `dt`, multiply all `value_col_nms` with the
  #   respective Brenner weight, and sum `value_col_nms` over the adjusting
  #   strata in `weight_dt`. E.g. if `dt` is stratified by `sex` and `ag` and
  #   `weight_dt` contains columns
  #   `ag` and `brenner_weight` then the resulting table will have
  #   weighted sums of `value_col_nms` by `sex` (and the intervals).
  #   This approach is the same as assigning weights for each individual before
  #   splitting and aggregating when the individual weights are based on
  #   strata only (and not maybe some continuous variable).
  # @codedoc_comment_block popEpi::surv_estimate
  do_brenner_weighting <- data.table::is.data.table(weight_dt) &&
    "brenner_weight" %in% names(weight_dt)
  if (do_brenner_weighting) {
    dt_join_assign(
      x = out,
      i = weight_dt,
      on = intersect(names(out), names(weight_dt)),
      x_col_nms = "__brenner_weight__",
      i_col_nms = "brenner_weight"
    )
    invisible(lapply(value_col_nms, function(value_col_nm) {
      data.table::set(
        x = out,
        j = value_col_nm,
        value = out[[value_col_nm]] * out[["__brenner_weight__"]]
      )
    }))
    data.table::set(
      x = out,
      j = "__brenner_weight__",
      value = NULL
    )
    out <- out[
      j = lapply(.SD, sum),
      .SDcols = value_col_nms,
      keyby = eval(setdiff(
        names(dt),
        c(value_col_nms, setdiff(names(weight_dt), stratum_col_nms))
      ))
    ]
    estimate_stratum_col_nms <- intersect(estimate_stratum_col_nms, names(out))
  }
  # @codedoc_comment_block popEpi::surv_estimate
  # - Check columns `t_at_risk`, `n_at_risk_eff` for zeroes/NAs if they are in
  #   `dt`. Intervals with e.g. `t_at_risk == 0` have no survival probability
  #   defined for them which causes `NA` values (i.e. zero divided by zero
  #   is not defined). Throw a warning if such intervals are found.
  # @codedoc_comment_block popEpi::surv_estimate
  surv_estimate_call <- match.call()
  lapply(intersect(names(dt), c("t_at_risk", "n_at_risk_eff")), function(nm) {
    is_bad <- dt[[nm]] %in% c(NA, 0L)
    if (any(is_bad)) {
      warning(simpleWarning(paste0(
        "There were ", sum(is_bad), " intervals in `dt` where `dt$", nm, "` ",
        "was zero/NA. No survival probability can be estimated for such ",
        "intervals."
      ), call = surv_estimate_call))
    }
  })

  # @codedoc_comment_block popEpi::surv_estimate
  # - Add column `delta_t` as the difference between the `_stop` and `_start`
  #   columns of the follow-up time scale, e.g. `ts_fut_stop - ts_fut_start`.
  # @codedoc_comment_block popEpi::surv_estimate
  data.table::set(
    x = out,
    j = "delta_t",
    value = out[[paste0(ts_fut_col_nm, "_stop")]] -
      out[[paste0(ts_fut_col_nm, "_start")]]
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
    da_stratum_col_nms <- intersect(
      names(out),
      c(stratum_col_nms, "box_id")
    )
    da_adjust_col_nms <- character(0)
    if (do_direct_adjusting) {
      da_adjust_col_nms <- setdiff(names(weight_dt), "weight")
      da_stratum_col_nms <- setdiff(da_stratum_col_nms, da_adjust_col_nms)
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

    # @codedoc_comment_block popEpi::surv_estimate
    # - If direct adjusting was performed, the summary statistics such as
    #   `n_events` are summed over the adjusting strata and will be included
    #   in the output. These are not weighted averages/sums but simple sums.
    # @codedoc_comment_block popEpi::surv_estimate
    nonsum_col_nms <- setdiff(
      names(out),
      c(
        value_col_nms, estimate_col_nms,
        standard_error_col_nms, variance_col_nms,
        names(weight_dt)
      )
    )
    nonsum_col_nms <- intersect(
      names(out),
      union(nonsum_col_nms, stratum_col_nms)
    )
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
        .SDcols = eval(nonsum_col_nms)
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
  # lexis_immortalise(lexis = e1dt, breaks = breaks[ts_fut_col_nm])
  # ts_col_nms <- names(breaks)
  # if (length(ts_col_nms) > 1) {
  #   stratum_ts_col_nms <- ts_col_nms[-length(ts_col_nms)]
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
        ts_col_nms <- names(eval_env[["breaks"]])
        stratum_ts_col_nms <- ts_col_nms[-length(ts_col_nms)]
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

#' @eval codedoc::pkg_doc_fun(
#'   "popEpi::surv_collapse_1d",
#'   "surv_functions"
#' )
#' @examples
#'
#' # popEpi::surv_collapse_1d
#' sdt <- data.table::data.table(
#'   box_id = 1:3,
#'   ts_fut_id = 1:3,
#'   ts_fut_start = 0:2,
#'   ts_fut_stop = 1:3,
#'   t_at_risk = c(1.0, 0.0, 0.5),
#'   n_events = 0L
#' )
#' sdt <- surv_collapse_1d(
#'   dt = sdt,
#'   ts_fut_col_nm = "ts_fut",
#'   value_col_nms = c("t_at_risk", "n_events"),
#'   test_expr = quote(t_at_risk > 0.0)
#' )
#' stopifnot(
#'   nrow(sdt) == 2,
#'   sdt[["ts_fut_start"]] == c(0.0, 1.0),
#'   sdt[["ts_fut_stop"]] == c(1.0, 3.0),
#'   sdt[["t_at_risk"]] == c(1.0, 0.5)
#' )
surv_collapse_1d <- function(
  dt,
  ts_fut_col_nm,
  stratum_col_nms = NULL,
  value_col_nms = NULL,
  test_expr = NULL,
  mandatory_breaks = NULL
) {
  if (is.null(value_col_nms)) {
    value_col_nms <- names(dt)[grepl("(^t_)|(^n_)", names(dt))]
    value_col_nms <- value_col_nms[vapply(
      as.list(dt)[value_col_nms], inherits, logical(1L),
      what = c("integer", "numeric")
    )]
    if (length(value_col_nms) == 0) {
      stop("Could not infer `value_col_nms`; please supply this argument.")
    }
  }
  stopifnot(
    data.table::is.data.table(dt),
    paste0(ts_fut_col_nm, c("_start", "_stop")) %in% names(dt),
    !duplicated(dt, by = c(stratum_col_nms, paste0(ts_fut_col_nm, "_start"))),
    value_col_nms %in% names(dt),
    is.null(test_expr) || is.language(test_expr)
  )
  if (is.null(test_expr)) {
    test_col_nms <- intersect(
      c(
        "t_at_risk",
        names(dt)[grepl("^t_", names(dt))],
        "n_at_risk_eff",
        names(dt)[grepl("^n_", names(dt))]
      ),
      names(dt)
    )
    if (length(test_col_nms) == 0) {
      stop(
        "Could not automatically determine value column to test whether ",
        "intervals should be combined. If you see this, add `t_at_risk` to ",
        "the columns created by `lexis_split_merge_aggregate_by_stratum`."
      )
    }
    test_expr <- substitute(
      sum(col),
      list(col = str2lang(test_col_nms[1]))
    )
  }
  if (!is.null(stratum_col_nms)) {
    return(dt[
      j = surv_collapse_1d(
        dt = .SD,
        ts_fut_col_nm = ts_fut_col_nm,
        stratum_col_nms = NULL,
        value_col_nms = value_col_nms,
        test_expr = test_expr,
        mandatory_breaks = mandatory_breaks
      ),
      keyby = eval(stratum_col_nms)
    ])
  }

  ts_start_col_nm <- paste0(ts_fut_col_nm, "_start")
  ts_stop_col_nm <- paste0(ts_fut_col_nm, "_stop")

  br_dt <- data.table::setDT(list(
    br = c(dt[[ts_start_col_nm]], dt[[ts_stop_col_nm]][nrow(dt)])
  ))
  data.table::set(
    x = br_dt,
    j = "in_mandatory_breaks",
    value =  br_dt[["br"]] %in% mandatory_breaks
  )
  collapsed_grp_ids <- rep(NA_integer_, nrow(dt))
  collapsed_grp_ids[1L] <- collapsed_grp_id <- 1L
  interval_id_lo <- 1L
  interval_id_hi <- 1L
  while (TRUE) {
    need_to_combine <- !surv_collapse_1d_eval_test_expr__(
      test_expr = test_expr,
      dt = dt,
      subset_idx = substitute(
        interval_id_lo:interval_id_hi,
        list(interval_id_lo = interval_id_lo, interval_id_hi = interval_id_hi)
      ),
      call_env = parent.frame(1L)
    )
    if (need_to_combine) {
      can_combine_with_next <- interval_id_hi < nrow(dt) &&
        !br_dt[["in_mandatory_breaks"]][interval_id_lo]
      can_combine_with_previous <- interval_id_lo > 1 &&
        !br_dt[["in_mandatory_breaks"]][interval_id_hi + 1L]
      can_combine <- can_combine_with_next || can_combine_with_previous
      # can combine and need to combine
      do_combine <- can_combine
    } else {
      do_combine <- FALSE
    }
    if (do_combine) {
      # lets collapse with next or previous and try again
      if (can_combine_with_next) {
        interval_id_lo <- interval_id_lo
        interval_id_hi <- interval_id_hi + 1L
      } else {
        interval_id_lo <- which(collapsed_grp_ids == collapsed_grp_id - 1L)[1L]
        interval_id_hi <- interval_id_hi
      }
    } else {
      # done with this (potentially collapsed) group
      collapsed_grp_ids[interval_id_lo:interval_id_hi] <- collapsed_grp_id
      collapsed_grp_id <- collapsed_grp_id + 1L
      interval_id_lo <- interval_id_hi <- interval_id_hi + 1L
    }
    if (interval_id_hi > nrow(dt)) {
      collapsed_grp_ids[is.na(collapsed_grp_ids)] <- collapsed_grp_id
      break
    }
  }
  collapsed_grp_ids <- cumsum(!duplicated(collapsed_grp_ids))
  ts_id_col_nm <- paste0(ts_fut_col_nm, "_id")
  dt <- data.table::copy(data.table::setDT(as.list(dt)))
  data.table::set(
    x = dt,
    j = ts_id_col_nm,
    value = collapsed_grp_ids
  )
  ts_id_col_nms <- setdiff(names(dt)[grepl("_id$", names(dt))], "box_id")
  data.table::set(
    x = dt,
    j = "box_id",
    value = cumsum(!duplicated(dt, by = ts_id_col_nms))
  )
  data.table::set(
    x = dt,
    j = c(ts_start_col_nm, ts_stop_col_nm),
    value = lapply(c(ts_start_col_nm, ts_stop_col_nm), function(col_nm) {
      data.table::copy(dt[[col_nm]])
    })
  )
  dt[
    j = c(ts_start_col_nm, ts_stop_col_nm) := list(
      .SD[[ts_start_col_nm]][1L],
      .SD[[ts_stop_col_nm]][.N]
    ),
    by = ts_id_col_nm
  ]
  dt <- dt[
    #' @importFrom data.table .SD
    j = lapply(.SD, sum, na.rm = TRUE),
    .SDcols = value_col_nms,
    keyby = setdiff(names(dt), value_col_nms)
  ]
  return(dt[])
}

#' @eval codedoc::pkg_doc_fun(
#'   "popEpi::surv_collapse_strata_list",
#'   "surv_functions"
#' )
#' @examples
#'
#' # popEpi::surv_collapse_strata_list
#' sdt <- data.table::CJ(
#'   ag = 1:3,
#'   box_id = 1:5
#' )
#' sdt[
#'   i = sdt[["ag"]] == 1,
#'   j = "t_at_risk" := 0.0
#' ]
#' sdt[
#'   i = sdt[["ag"]] == 2,
#'   j = "t_at_risk" := c(1, 1, 0, 2, 1)
#' ]
#' sdt[
#'   i = sdt[["ag"]] == 3,
#'   j = "t_at_risk" := c(1, 1, 1, 0, 1)
#' ]
#' sdt_collapse_data <- popEpi::surv_collapse_strata_list(
#'   dt = sdt,
#'   stratum_col_nms = "ag",
#'   collapse_stratum_col_nms = "ag"
#' )
#' stopifnot(
#'   "result" %in% names(sdt_collapse_data),
#'   inherits(sdt_collapse_data[["result"]][[1]], "list"),
#'   c("new", "old") %in% names(sdt_collapse_data[["result"]][[1]]),
#'   sdt_collapse_data[["result"]][[1]][["new"]][["stratum_id"]] == 1L
#' )
#' sdt[
#'   i = sdt[["ag"]] == 1,
#'   j = "t_at_risk" := 0.5
#' ]
#' sdt_collapse_data <- popEpi::surv_collapse_strata_list(
#'   dt = sdt,
#'   stratum_col_nms = "ag",
#'   collapse_stratum_col_nms = "ag"
#' )
#' stopifnot(
#'   "result" %in% names(sdt_collapse_data),
#'   inherits(sdt_collapse_data[["result"]][[1]], "list"),
#'   c("new", "old") %in% names(sdt_collapse_data[["result"]][[1]]),
#'   identical(
#'     sdt_collapse_data[["result"]][[1]][["new"]][["stratum_id"]],
#'     c(rep(1L, 5L), rep(2L, 10L))
#'   )
#' )
#' # popEpi::surv_collapse_strata_1d
#' sdt_collapsed <- popEpi::surv_collapse_strata_1d(
#'   dt = sdt,
#'   stratum_col_nms = "ag",
#'   collapse_stratum_col_nm = "ag"
#' )
#' stopifnot(
#'   nrow(sdt_collapsed) == 10,
#'   names(sdt) %in% names(sdt_collapsed),
#'   names(sdt_collapsed) %in% names(sdt),
#'   is.character(sdt_collapsed[["ag"]]),
#'   identical(unique(sdt_collapsed[["ag"]]), c("1", "2 & 3"))
#' )
#'
surv_collapse_strata_list <- function(
  dt,
  stratum_col_nms,
  collapse_stratum_col_nms,
  value_col_nms = NULL,
  test_expr = NULL
) {
  stopifnot(
    data.table::is.data.table(dt),
    stratum_col_nms %in% names(dt),
    collapse_stratum_col_nms %in% stratum_col_nms,
    "box_id" %in% names(dt),
    !duplicated(
      x = dt,
      by = c(stratum_col_nms, "box_id")
    ),
    #' @param collapse_stratum_col_nms `[character]` (no default)
    #'
    #' Names of stratum columns in `dt` where collapsing (combining) strata
    #' is allowed to achieve `test_expr` to pass in every stratum.
    is.character(collapse_stratum_col_nms),
    collapse_stratum_col_nms %in% names(dt),

    inherits(test_expr, c("call", "NULL"))
  )
  if (is.null(value_col_nms)) {
    value_col_nms <- names(dt)[grepl("(^t_)|(^n_)", names(dt))]
    value_col_nms <- value_col_nms[vapply(
      as.list(dt)[value_col_nms], inherits, logical(1L),
      what = c("integer", "numeric")
    )]
    if (length(value_col_nms) == 0) {
      stop("Could not infer `value_col_nms`; please supply this argument.")
    }
  }
  stopifnot(
    value_col_nms %in% names(dt),
    vapply(
      as.list(dt)[value_col_nms], inherits, logical(1L),
      what = c("integer", "numeric")
    )
  )
  if (is.null(test_expr)) {
    test_expr <- quote(min(t_at_risk) > 0)
  }
  noncollapse_stratum_col_nms <- setdiff(
    stratum_col_nms,
    collapse_stratum_col_nms
  )
  if (length(noncollapse_stratum_col_nms) > 0) {
    return(dt[
      j = popEpi::surv_collapse_strata_list(
        #' @importFrom data.table .SD
        dt = .SD,
        stratum_col_nms = collapse_stratum_col_nms,
        collapse_stratum_col_nms = collapse_stratum_col_nms,
        value_col_nms = value_col_nms,
        test_expr = test_expr
      ),
      keyby = eval(noncollapse_stratum_col_nms)
    ])
  }

  work_dt <- data.table::copy(data.table::setDT(as.list(dt)[
    c("box_id", value_col_nms)
  ]))
  data.table::set(
    x = work_dt,
    j = "stratum_id",
    value = cumsum(!duplicated(dt, by = collapse_stratum_col_nms))
  )
  out <- data.table::setDT(list(
    stratum_id = data.table::copy(work_dt[["stratum_id"]])
  ))
  data.table::setcolorder(work_dt, "stratum_id")
  data.table::setkeyv(work_dt, "stratum_id")
  tdt_expr <- substitute(
    work_dt[
      j = list(test_result = test_expr),
      keyby = "stratum_id"
    ],
    list(test_expr = test_expr)
  )
  tdt <- eval(tdt_expr)
  stratum_id <- 1L
  performed_aggregation <- !all(tdt[["test_result"]])
  while (
    nrow(tdt) > 1 &&
      stratum_id <= max(tdt[["stratum_id"]]) &&
      !all(tdt[["test_result"]])
  ) {
    if (!tdt[["test_result"]][stratum_id]) {
      if (stratum_id < max(tdt[["stratum_id"]])) {
        data.table::set(
          x = work_dt,
          i = which(work_dt[["stratum_id"]] == stratum_id + 1L),
          j = "stratum_id",
          value = stratum_id
        )
        data.table::set(
          x = out,
          i = which(out[["stratum_id"]] == stratum_id + 1L),
          j = "stratum_id",
          value = stratum_id
        )
      } else {
        data.table::set(
          x = work_dt,
          i = which(work_dt[["stratum_id"]] == stratum_id - 1L),
          j = "stratum_id",
          value = stratum_id
        )
        data.table::set(
          x = out,
          i = which(out[["stratum_id"]] == stratum_id - 1L),
          j = "stratum_id",
          value = stratum_id
        )
      }
      data.table::set(
        x = work_dt,
        j = "stratum_id",
        value = cumsum(!duplicated(work_dt[["stratum_id"]]))
      )
      data.table::set(
        x = out,
        j = "stratum_id",
        value = cumsum(!duplicated(out[["stratum_id"]]))
      )
      work_dt <- work_dt[
        j = lapply(.SD, sum),
        .SDcols = value_col_nms,
        keyby = c("stratum_id", "box_id")
      ]
      tdt <- eval(tdt_expr)
    } else {
      stratum_id <- stratum_id + 1L
    }
  }
  out <- data.table::data.table(
    result = list(list(
      old = data.table::setDT(as.list(dt)[collapse_stratum_col_nms])[],
      new = out[],
      performed_aggregation = performed_aggregation,
      value_col_nms = value_col_nms,
      collapse_stratum_col_nms = collapse_stratum_col_nms
    ))
  )
  return(out[])
}

#' @eval codedoc::pkg_doc_fun(
#'   "popEpi::surv_collapse_strata_1d",
#'   "surv_functions"
#' )
surv_collapse_strata_1d <- function(
  dt,
  stratum_col_nms,
  collapse_stratum_col_nm,
  value_col_nms = NULL,
  test_expr = quote(min(t_at_risk) > 0)
) {
  #' @param collapse_stratum_col_nm `[character]`
  #'
  #' As the `collapse_stratum_col_nms` argument of
  #' `popEpi::surv_collapse_strata_list` --- and passed to it, as that argument
  #' --- but must be of length one.
  stopifnot(
    length(collapse_stratum_col_nm) == 1,
    is.character(collapse_stratum_col_nm),
    collapse_stratum_col_nm %in% names(dt)
  )
  noncollapse_stratum_col_nms <- setdiff(
    stratum_col_nms,
    collapse_stratum_col_nm
  )
  if (length(noncollapse_stratum_col_nms) > 0) {
    dt <- data.table::setDT(as.list(dt))
    data.table::set(
      x = dt,
      j = collapse_stratum_col_nm,
      value = as.character(dt[[collapse_stratum_col_nm]])
    )
    out <- dt[
      j = surv_collapse_strata_1d(
        dt = .SD,
        stratum_col_nms = collapse_stratum_col_nm,
        collapse_stratum_col_nm = collapse_stratum_col_nm,
        value_col_nms = value_col_nms,
        test_expr = test_expr
      ),
      keyby = eval(noncollapse_stratum_col_nms)
    ]
    data.table::setcolorder(out, names(dt))
    data.table::setkeyv(out, data.table::key(dt))
    return(out[])
  }
  expr <- match.call()
  expr[[1L]] <- quote(surv_collapse_strata_list)
  names(expr)[names(expr) == "collapse_stratum_col_nm"] <-
    "collapse_stratum_col_nms"
  cl <- eval(expr, parent.frame(1L))[["result"]][[1L]]
  if (!cl[["performed_aggregation"]]) {
    return(dt[])
  }
  join_dt <- data.table::data.table(
    old = cl[["old"]][[collapse_stratum_col_nm]],
    new = cl[["new"]][["stratum_id"]]
  )
  join_dt <- unique(join_dt, by = names(join_dt))
  data.table::set(
    x = join_dt,
    j = "new",
    value = as.character(join_dt[["new"]])
  )
  join_dt[
    #' @importFrom data.table := .SD
    j = "new" := paste0(.SD[["old"]], collapse = " & "),
    .SDcols = "old",
    by = "new"
  ]
  out <- data.table::copy(
    data.table::setDT(as.list(dt)[collapse_stratum_col_nm])
  )
  data.table::set(
    x = out,
    j = setdiff(names(dt), names(out)),
    value = as.list(dt)[setdiff(names(dt), names(out))]
  )
  data.table::set(
    x = out,
    j = collapse_stratum_col_nm,
    value = as.character(factor(
      x = out[[collapse_stratum_col_nm]],
      levels = join_dt[["old"]],
      labels = join_dt[["new"]]
    ))
  )
  out <- out[
    #' @importFrom data.table .SD
    j = lapply(.SD, sum),
    .SDcols = cl[["value_col_nms"]],
    keyby = eval(setdiff(names(out), cl[["value_col_nms"]]))
  ]
  data.table::setcolorder(out, names(dt))
  data.table::setkeyv(out, data.table::key(dt))
  return(out[])
}
