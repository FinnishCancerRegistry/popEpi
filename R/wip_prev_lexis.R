#' @title Prevalence
#' @description
#' Function(s) to compute prevalence statistics.
#' @name prev_functions
NULL

#' @eval codedoc::pkg_doc_fun(
#'   "popEpi::prev_lexis",
#'   "prev_functions"
#' )
#' @examples
#' # this data.table::setDTthreads call is included here only to
#' # conform to the CRAN submission requirement to only use at most 2
#' # threads. you do not need to set this to use popEpi.
#' # however some long calculations may benefit from using more threads.
#' data.table::setDTthreads(2L)
#'
#' # popEpi::prev_lexis
#' make_sire <- function() {
#'   sire <- popEpi::sire
#'   sire <- sire[
#'     sire[["dg_date"]] < sire[["ex_date"]] &
#'       sire[["ex_date"]] >= as.Date("1999-01-01") &
#'       (get.yrs(sire[["ex_date"]]) - get.yrs(sire[["bi_date"]])) < 100
#'   ]
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
#'   return(sire[])
#' }
#' sire <- make_sire()
#'
#' # observed counts only
#' pdt <- popEpi::prev_lexis(
#'   lexis = sire,
#'   aggre_by = "sex",
#'   observation_time_points = list(ts_cal = 2011.99),
#'   stratum_breaks = list(
#'     ts_age = c(seq(0, 85, 5), Inf),
#'     ts_fut = c(0, 1, 5, 10, Inf)
#'   )
#' )
#' stopifnot(
#'   inherits(pdt, "data.table"),
#'   "sex" %in% names(pdt),
#'   "n_prev" %in% names(pdt)
#' )
#'
#' # + effective counts with survival table for computing expected counts
#' sdt <- popEpi::surv_lexis(
#'   lexis = sire,
#'   breaks = list(
#'     # it is best to have smaller intervals initially but they can be rather
#'     # wide after a while
#'     ts_fut = c(seq(0, 1, 1 / 12), seq(2, 50, 1))
#'   ),
#'   aggre_by = "sex",
#'   first_record_by = "lex.id",
#'   split_merge_aggregate_optional_args = list(
#'     # we allow for collapsing intervals to ensure none are totally empty
#'     breaks_collapse_args = list(
#'       test_expr = quote(sum(lex.dur))
#'     )
#'   )
#' )
#' # sdt goes ultimately to popEpi::lexis_merge, so we massage it a bit first
#' data.table::set(
#'   x = sdt,
#'   j = "ts_fut",
#'   value = data.table::fctr(paste0(
#'     "]",
#'     sdt[["ts_fut_start"]],
#'     ",",
#'     sdt[["ts_fut_stop"]],
#'     "]"
#'   ))
#' )
#' data.table::setnames(x = sdt, "S_ch", "S")
#' sdt <- data.table::setDT(as.list(sdt)[c(
#'   "sex",
#'   "ts_fut",
#'   "S"
#' )])
#' pdt <- popEpi::prev_lexis(
#'   lexis = sire,
#'   aggre_by = "sex",
#'   observation_time_points = list(ts_cal = 2011.99),
#'   stratum_breaks = list(
#'     ts_age = c(seq(0, 85, 5), Inf),
#'     ts_fut = c(0, 1, 5, 10, Inf)
#'   ),
#'   merge_dt = sdt
#' )
#' stopifnot(
#'   inherits(pdt, "data.table"),
#'   "sex" %in% names(pdt),
#'   c("n_prev", "n_prev_eff") %in% names(pdt),
#'   !is.na(pdt[["n_prev_eff"]])
#' )
#'
#' # with survival estimated on the fly
#' surv_lexis_arg_list <- list(
#'   breaks = list(
#'     # optional period analysis estimation approach
#'     ts_cal = c(2001.99, 2011.99),
#'     # it is best to have smaller intervals initially but they can be rather
#'     # wide after a while
#'     ts_fut = c(seq(0, 1, 1 / 12), seq(2, 50, 1))
#'   ),
#'   split_merge_aggregate_optional_args = list(
#'     # we allow for collapsing intervals to ensure none are totally empty
#'     breaks_collapse_args = list(
#'       test_expr = quote(sum(lex.dur))
#'     )
#'   ),
#'   estimators = "S_ch"
#' )
#' pdt <- popEpi::prev_lexis(
#'   lexis = sire,
#'   aggre_by = "sex",
#'   observation_time_points = list(ts_cal = 2011.99),
#'   stratum_breaks = list(
#'     ts_age = c(seq(0, 85, 5), Inf),
#'     ts_fut = c(0, 1, 5, 10, Inf)
#'   ),
#'   merge_dt = surv_lexis_arg_list
#' )
#' stopifnot(
#'   inherits(pdt, "data.table"),
#'   "sex" %in% names(pdt),
#'   c("n_prev", "n_prev_eff") %in% names(pdt)
#' )
#'
#' # + prevalence proportions
#' # first: this is the finnish population size on 2011-12-31
#' my_n_at_risk_dt <- data.table::data.table(
#'   sex = 1L,
#'   ts_age = data.table::fctr(paste0(
#'     "[",
#'     seq(0, 85, 5),
#'     ",",
#'     c(seq(5, 85, 5), Inf),
#'     "["
#'   )),
#'   ts_cal = 2011.99,
#'   n_at_risk = c(
#'     148096L,
#'     143394L,
#'     143320L,
#'     160535L,
#'     162625L,
#'     167897L,
#'     165536L,
#'     155863L,
#'     166939L,
#'     185618L,
#'     185742L,
#'     193930L,
#'     201272L,
#'     154642L,
#'     130644L,
#'     104590L,
#'     91239L,
#'     86851L
#'   )
#' )
#' pdt <- popEpi::prev_lexis(
#'   lexis = sire,
#'   aggre_by = "sex",
#'   observation_time_points = list(ts_cal = 2011.99),
#'   stratum_breaks = list(
#'     ts_age = c(seq(0, 85, 5), Inf),
#'     ts_fut = c(0, 1, 5, 10, Inf)
#'   ),
#'   merge_dt = surv_lexis_arg_list,
#'   n_at_risk_dt = my_n_at_risk_dt
#' )
#' stopifnot(
#'   inherits(pdt, "data.table"),
#'   "sex" %in% names(pdt),
#'   c("n_prev", "n_prev_eff") %in% names(pdt),
#'   paste0("p_prev", c("", "_se", "_lo", "_hi")) %in% names(pdt),
#'   paste0("p_prev_eff", c("", "_se", "_lo", "_hi")) %in% names(pdt)
#' )
#'
#' # same without the effective numbers
#' pdt <- popEpi::prev_lexis(
#'   lexis = sire,
#'   aggre_by = "sex",
#'   observation_time_points = list(ts_cal = 2011.99),
#'   stratum_breaks = list(
#'     ts_age = c(seq(0, 85, 5), Inf),
#'     ts_fut = c(0, 1, 5, 10, Inf)
#'   ),
#'   n_at_risk_dt = my_n_at_risk_dt
#' )
#' stopifnot(
#'   inherits(pdt, "data.table"),
#'   "sex" %in% names(pdt),
#'   "n_prev" %in% names(pdt),
#'   paste0("p_prev", c("", "_se", "_lo", "_hi")) %in% names(pdt),
#'   nrow(pdt) == 72
#' )
#'
#' # + direct adjusting
#' # first make a table of weights for direct adjusting.
#' # this is the finnish population on 2023-12-31.
#' wdt <- data.table::data.table(
#'   ts_age = data.table::fctr(paste0(
#'     "[",
#'     seq(0, 85, 5),
#'     ",",
#'     c(seq(5, 85, 5), Inf),
#'     "["
#'   )),
#'   weight_fi_2023 = c(
#'     115211L,
#'     136012L,
#'     155573L,
#'     153424L,
#'     148987L,
#'     165738L,
#'     182972L,
#'     178335L,
#'     177751L,
#'     169327L,
#'     155810L,
#'     180155L,
#'     182260L,
#'     179869L,
#'     178732L,
#'     161722L,
#'     99450L,
#'     108625L
#'   )
#' )
#' pdt <- popEpi::prev_lexis(
#'   lexis = sire,
#'   aggre_by = "sex",
#'   observation_time_points = list(ts_cal = 2011.99),
#'   stratum_breaks = list(
#'     ts_age = c(seq(0, 85, 5), Inf),
#'     ts_fut = c(0, 1, 5, 10, Inf)
#'   ),
#'   n_at_risk_dt = my_n_at_risk_dt,
#'   weight_dt = wdt
#' )
#' stopifnot(
#'   inherits(pdt, "data.table"),
#'   "sex" %in% names(pdt),
#'   "n_prev" %in% names(pdt),
#'   paste0("p_prev_fi_2023", c("", "_se", "_lo", "_hi")) %in% names(pdt),
#'   nrow(pdt) == 4
#' )
#'
#' # + effective numbers
#' pdt <- popEpi::prev_lexis(
#'   lexis = sire,
#'   aggre_by = "sex",
#'   observation_time_points = list(ts_cal = 2011.99),
#'   stratum_breaks = list(
#'     ts_age = c(seq(0, 85, 5), Inf),
#'     ts_fut = c(0, 1, 5, 10, Inf)
#'   ),
#'   merge_dt = surv_lexis_arg_list,
#'   n_at_risk_dt = my_n_at_risk_dt,
#'   weight_dt = wdt
#' )
#' stopifnot(
#'   inherits(pdt, "data.table"),
#'   "sex" %in% names(pdt),
#'   "n_prev" %in% names(pdt),
#'   paste0("p_prev_fi_2023", c("", "_se", "_lo", "_hi")) %in% names(pdt),
#'   paste0("p_prev_eff_fi_2023", c("", "_se", "_lo", "_hi")) %in% names(pdt),
#'   nrow(pdt) == 4
#' )
#'
#'
#' # + multiple weights
#' data.table::set(
#'   x = wdt,
#'   j = c("weight", "weight_eu_1976"),
#'   value = list(
#'     runif(18),
#'     c(
#'       # from Waterhouse et al. 1976
#'       8000L,
#'       rep(7000L, 10L),
#'       6000L,
#'       5000L,
#'       4000L,
#'       3000L,
#'       2000L,
#'       1000L,
#'       1000L
#'     )
#'   )
#' )
#' pdt <- popEpi::prev_lexis(
#'   lexis = sire,
#'   aggre_by = "sex",
#'   observation_time_points = list(ts_cal = 2011.99),
#'   stratum_breaks = list(
#'     ts_age = c(seq(0, 85, 5), Inf),
#'     ts_fut = c(0, 1, 5, 10, Inf)
#'   ),
#'   merge_dt = surv_lexis_arg_list,
#'   n_at_risk_dt = my_n_at_risk_dt,
#'   weight_dt = wdt
#' )
#' stopifnot(
#'   inherits(pdt, "data.table"),
#'   "sex" %in% names(pdt),
#'   "n_prev" %in% names(pdt),
#'   paste0("p_prev_fi_2023", c("", "_se", "_lo", "_hi")) %in% names(pdt),
#'   paste0("p_prev_eff_fi_2023", c("", "_se", "_lo", "_hi")) %in% names(pdt),
#'   paste0("p_prev_eu_1976", c("", "_se", "_lo", "_hi")) %in% names(pdt),
#'   paste0("p_prev_eff_eu_1976", c("", "_se", "_lo", "_hi")) %in% names(pdt),
#'   # based on `wdt$weight`
#'   paste0("p_prev_da", c("", "_se", "_lo", "_hi")) %in% names(pdt),
#'   paste0("p_prev_eff_da", c("", "_se", "_lo", "_hi")) %in% names(pdt),
#'   nrow(pdt) == 4
#' )
#'
prev_lexis <- function(
  lexis,
  observation_time_points,
  stratum_breaks,
  aggre_by = NULL,
  subset = NULL,
  first_record_by = NULL,
  merge_dt = NULL,
  merge_optional_args = NULL,
  n_at_risk_dt = NULL,
  weight_dt = NULL,
  conf_lvls = 0.95,
  conf_methods = "log"
) {
  #' @template param_lexis
  assert_is_arg_lexis(lexis, dt = FALSE)
  #' @param observation_time_points `[list]` (no default)
  #'
  #' Prevalence observation time points.
  #' The `list` can have only one element, but multiple time points can be
  #' supplied, e.g. `list(ts_cal = c(2009.999, 2010.999))`.
  #' The output of this function will have these observation time points as
  #' a column with the same name as the time scale.
  assert_is_arg_breaks(observation_time_points, lexis)
  stopifnot(length(observation_time_points) == 1)
  #' @param stratum_breaks `[list]` (no default)
  #'
  #' Breaks to split `lexis` by. These are passed to `[splitMulti]`. These
  #' breaks create new strata in the output using the time scales in
  #' `lexis`. The last time scale used here is assumed to be the follow-up time
  #' time scale.
  #' E.g. `list(ts_age = seq(0, 100, 10), ts_fut = c(0, 1, 5, Inf))`
  #' to stratify output by age interval and time since entry interval at a
  #' given observation point.
  assert_is_arg_breaks(stratum_breaks, lexis)
  stopifnot(
    stratum_breaks[[length(stratum_breaks)]][1] == 0
  )
  # @codedoc_comment_block popEpi::prev_lexis::subset
  # @codedoc_insert_comment_block popEpi:::handle_arg_subset
  # @codedoc_comment_block popEpi::prev_lexis::subset
  subset <- handle_arg_subset(dataset_nm = "lexis")
  #' @param aggre_by Passed to `[lexis_split_merge_aggregate_by_stratum]`.
  aggre_by <- handle_arg_by(by = aggre_by, dataset = lexis)
  if (data.table::is.data.table(aggre_by)) {
    subset <- subset &
      local({
        join_dt <- data.table::setDT(as.list(lexis)[intersect(
          names(lexis),
          names(aggre_by)
        )])
        wh <- join_dt[
          i = aggre_by,
          on = names(aggre_by),
          which = TRUE
        ]
        seq_len(nrow(join_dt)) %in% wh
      })
  }

  #' @param merge_dt `[NULL, data.table]` (default `NULL`)
  #'
  #' Table containing survival estimates applicable to `lexis`.
  #' These survival
  #' estimates are used in the "projection" of prevalence in those
  #' observations
  #' which have been lost to follow-up. See **Functions** for how this
  #' works and
  #' what you need to have.
  #'
  #' - `NULL`: No projection is performed.
  #' - `data.table`: Must contain stratum columns also found in `lexis` and
  #'   exactly one value column named `S`. This table is passed
  #'   to `[lexis_merge]` and should conform to its requirements. E.g.
  #'   `data.table(ts_fut = factor(c("[0, 1[", ...)), S = c(0.9, ...))`.
  #' - `list`: We produce a table of survival estimates on the fly and
  #'   these are arguments passed to `[surv_lexis]`. See **Functions**.
  stopifnot(
    inherits(merge_dt, c("list", "data.table", "NULL"))
  )
  merge_dt_by <- NULL
  if (data.table::is.data.table(merge_dt)) {
    stopifnot(
      "S" %in% names(merge_dt),
      !is.na(merge_dt[["S"]]),
      merge_dt[["S"]] >= 0,
      setdiff(names(merge_dt), "S") %in% names(lexis)
    )
    merge_dt_by <- setdiff(names(merge_dt), "S")
  } else if (inherits(merge_dt, "list")) {
    stopifnot(
      data.table::uniqueN(names(merge_dt)) == length(merge_dt),
      names(merge_dt) %in% names(formals(surv_lexis))
    )
  }

  #' @param n_at_risk_dt `[NULL, data.table]` (default `NULL`)
  #'
  #' - `NULL`: No population size data is merged into the output table.
  #' - `data.table`: Merge data from this table. See **Functions** and
  #'   **Examples**.
  #'   + It can but does not need to contain as a stratifying column
  #'     the time scale used in argument `observation_time_points`. If it is
  #'     included, then it must be included containing the same exact values,
  #'     e.g. with
  #'     `observation_time_points = list(ts_cal = 2023.9999)` you must have in
  #'     `n_at_risk_dt` rows with `ts_cal = 2023.9999`.
  #'   + It can but does not have to contain
  #'     the time scales used in argument `stratum_breaks` as `factor` columns
  #'     where the levels must contain the lower and upper bound of each
  #'     interval,
  #'     e.g. the levels of `ts_age` might be
  #'     `c("[0,5[", "[5,10[", ..., "[80,85[", "[85,Inf[")`.
  #'     The brackets must be included but their directions are ignored.
  #'     The simple way to form a `factor` with such levels is `[cut]`.
  #'     The lower and upper bounds must match the breaks used in argument
  #'     `stratum_breaks`, so for the above example you should have
  #'     `stratum_breaks = list(ts_cal = c(0, 5, 10, ..., 80, 85, Inf), ...)`.
  stopifnot(
    inherits(n_at_risk_dt, c("NULL", "data.frame"))
  )
  if (is.data.frame(n_at_risk_dt)) {
    stopifnot(
      "n_at_risk" %in% names(n_at_risk_dt),
      setdiff(names(n_at_risk_dt), "n_at_risk") %in% names(lexis),
      names(observation_time_points) %in% names(n_at_risk_dt)
    )
  }

  #' @param weight_dt `[NULL, data.table]` (default `NULL`)
  #'
  #' - `NULL`: No direct adjusting is performed.
  #' - `data.table`: Passed to `[directadjusting::directly_adjusted_estimates]`
  #'   after some harmonisation. See **Functions** and **Examples**.
  assert_is_arg_weight_dt(
    weight_dt,
    dt = lexis,
    allowed = c("NULL", "data.table")
  )

  # @codedoc_comment_block popEpi::prev_lexis
  # `popEpi::prev_lexis` can be used to compute numbers of (potentially
  # effective numbers of) subjects remaining in follow-up at arbitrary time
  # points. It performs the following steps:
  #
  # - For each observation time point in `observation_time_points[[1]]`:
  #   + Calls `[lexis_split_merge_aggregate_by_stratum]` with the
  #     `lexis`, `subset`, `aggre_by` supplied to
  #     `popEpi::prev_lexis` and with
  #     `aggre_exprs = list(n_prev = quote(.N))`, and `breaks = stratum_breaks`.
  #     This produces a table stratified by
  #     `aggre_by` and all time scales used in `stratum_breaks`.
  #     The only value column at this point is `n_prev`, the number
  #     of subjects in follow-up at the given observation time point.
  # @codedoc_comment_block popEpi::prev_lexis
  obs_ts_col_nm <- names(observation_time_points)
  agdt <- lapply(seq_along(observation_time_points[[1]]), function(i) {
    obs_tp_i <- observation_time_points[[1L]][i]
    agdt_i <- local({
      lexis_dt_obs_tp <- lexis_to_lexis_dt__(lexis)
      lexis_delay_entry(
        lexis = lexis_dt_obs_tp,
        ts_col_new_entry = obs_tp_i,
        ts_col_nm = obs_ts_col_nm
      )
      could_delay_entry <- !is.na(lexis_dt_obs_tp[["lex.dur"]])
      # this prevents splitting. we only want the first interval per lex.id.
      data.table::set(
        x = lexis_dt_obs_tp,
        i = which(could_delay_entry),
        j = "lex.dur",
        value = if (storage.mode(lexis[["lex.dur"]]) == "integer") {
          methods::as(1L, storage.mode(lexis_dt_obs_tp[["lex.dur"]]))
        } else {
          methods::as(1e-6, storage.mode(lexis_dt_obs_tp[["lex.dur"]]))
        }
      )
      agdt_i <- lexis_split_merge_aggregate_by_stratum(
        lexis = lexis_dt_obs_tp,
        subset = subset & could_delay_entry,
        breaks = stratum_breaks,
        #' @param first_record_by
        #' Passed to `[lexis_split_merge_aggregate_by_stratum]`.
        first_record_by = first_record_by,
        aggre_exprs = list(n_prev = quote(.N)),
        aggre_by = aggre_by
      )
      na_idx <- which(is.na(agdt_i[["n_prev"]]))
      if (length(na_idx) > 0) {
        data.table::set(
          x = agdt_i,
          i = na_idx,
          j = "n_prev",
          value = 0L
        )
      }
      data.table::set(
        x = agdt_i,
        j = obs_ts_col_nm,
        value = obs_tp_i
      )
      data.table::setcolorder(
        x = agdt_i,
        neworder = union(obs_ts_col_nm, names(agdt_i))
      )
      agdt_i[]
    })
    if (!is.null(merge_dt)) {
      data.table::set(agdt_i, j = "n_prev_eff", value = NA_real_)
      # @codedoc_comment_block popEpi::prev_lexis
      #   + If `!is.null(merge_dt)`,
      #     collect subjects in `lexis` who were censored before the
      #     current observation time point. This is defined as those in
      #     `lexis` who have `lexis[["lex.Cst"]] == lexis[["lex.Xst"]]` and
      #     `lexis[[obs_ts_col_nm]] + lexis[["lex.dur"]] < obs_tp`, where
      #     `obs_ts_col_nm = names(observation_time_points)` and
      #     `obs_tp` is the current observation time point. These collected
      #     subjects are the ones we need to extrapolate to the current
      #     observation time point.
      # @codedoc_comment_block popEpi::prev_lexis
      lexis_dt_extrapolate <- local({
        was_censored <- lexis[["lex.Cst"]] == lexis[["lex.Xst"]]
        before_obs_tp <-
          (lexis[[obs_ts_col_nm]] + lexis[["lex.dur"]]) < obs_tp_i
        lexis_dt_extrapolate <- lexis_to_lexis_dt__(
          lexis,
          subset = subset & was_censored & before_obs_tp,
          select = intersect(
            names(lexis),
            c(
              "lex.id",
              Epi::timeScales(lexis),
              "lex.dur",
              "lex.Cst",
              "lex.Xst",
              names(aggre_by),
              merge_dt_by
            )
          )
        )
        lexis_dt_extrapolate
      })
      # @codedoc_comment_block popEpi::prev_lexis
      #   + If there are no observations that need to be extrapolated
      #     then we end the extrapolation step early and simply use
      #     `n_prev_eff = n_prev`. Otherwise proceed as described below.
      # @codedoc_comment_block popEpi::prev_lexis
      if (nrow(lexis_dt_extrapolate) == 0) {
        data.table::set(
          x = agdt_i,
          j = "n_prev_eff",
          value = agdt_i[["n_prev"]]
        )
      } else {
        merge_arg_list <- as.list(merge_optional_args)
        merge_arg_list[["merge_dt"]] <- merge_dt
        merge_arg_list[["merge_dt_by"]] <- merge_dt_by
        if (inherits(merge_dt, "list")) {
          # @codedoc_comment_block popEpi::prev_lexis
          #   + If `inherits(merge_dt, "list")`, collect arguments from
          #     `merge_dt` to call `[surv_lexis]`.
          #   + However, arguments `lexis`, `subset`, and `estimators = "S_ch"`
          #     are assigned
          #     internally to the respective arguments supplied to `prev_lexis`
          #     and cannot be overridden.
          #   + Note that `lexis` is the input argument `lexis` and not only
          #     those whose
          #     follow-up we need to "extrapolate". So estimation of survival
          #     is based on all subjects (in the `subset`).
          # @codedoc_comment_block popEpi::prev_lexis
          merge_arg_list[["merge_dt"]] <- local({
            ts_fut_col_nm <- names(stratum_breaks)[length(stratum_breaks)]
            subset <- subset & lexis[[obs_ts_col_nm]] < obs_tp_i
            surv_lexis_arg_list <- c(
              list(
                lexis = lexis,
                subset = subset,
                estimators = "S_ch"
              ),
              merge_dt,
              list(
                # @codedoc_comment_block popEpi::prev_lexis
                #   + We set as overrideable defaults
                #     * `aggre_by` as-is,
                # @codedoc_comment_block popEpi::prev_lexis
                aggre_by = aggre_by,
                # @codedoc_comment_block popEpi::prev_lexis
                #     * `breaks` as monthly intervals up to 5 years of follow-up
                #       and thereafter as annual intervals. Note that this
                #       default only works correctly if your time scales are
                #       specified in fractional years so that e.g.
                #       `lex.dur = 5.1` means 5.1 years of follow-up.
                #       The last interval ends where the longest
                #       "extrapolation" ends, e.g. for observation time point
                #       2021.99 and the earliest extrapolatable observation
                #       starting at `ts_cal = 1995.53` the last interval
                #       is `]26, 27]`. From `ts_cal = 2020.13` the last interval
                #       is `]1.83333, 1.91667]`.
                # @codedoc_comment_block popEpi::prev_lexis
                breaks = local({
                  ts_fut_max <- max(
                    obs_tp_i,
                    lexis_dt_extrapolate[[obs_ts_col_nm]]
                  )
                  bl <- list(
                    ts_fut = c(seq(0, 5, 1 / 12), 6:200)
                  )
                  diff <- bl[["ts_fut"]] - ts_fut_max
                  diff <- data.table::fifelse(diff < 0, Inf, diff)
                  bl[["ts_fut"]] <- bl[["ts_fut"]][
                    seq(1L, which.min(diff))
                  ]
                  names(bl) <- ts_fut_col_nm
                  bl
                }),
                # @codedoc_comment_block popEpi::prev_lexis
                #     * `split_merge_aggregate_optional_args` contains element
                #       `breaks_collapse_args` with value
                #       `list(test_expr = quote(sum(lex.dur)))`.
                #       Therefore by default we re-aggregate survival intervals
                #       to avoid empty ones.
                # @codedoc_comment_block popEpi::prev_lexis
                split_merge_aggregate_optional_args = list(
                  breaks_collapse_args = list(
                    test_expr = quote(sum(lex.dur))
                  )
                ),
                # @codedoc_comment_block popEpi::prev_lexis
                #     * `surv_estimate_optional_args` as
                #       `list(empty_interval_action = "surv_collapse_ts_1d")`.
                # @codedoc_comment_block popEpi::prev_lexis
                surv_estimate_optional_args = list(
                  empty_interval_action = "surv_collapse_ts_1d"
                )
              )
            )
            surv_lexis_arg_list <- surv_lexis_arg_list[
              !duplicated(names(surv_lexis_arg_list))
            ]
            sdt <- call_with_arg_list__(
              surv_lexis,
              surv_lexis_arg_list
            )
            ts_fut_col_nm <- utils::tail(
              names(surv_lexis_arg_list[["breaks"]]),
              1
            )
            # @codedoc_comment_block popEpi::prev_lexis
            #   + If `inherits(merge_dt, "list")`, we also interpolate survival
            #     estimates in the table we have just produced so that there
            #     are always 1000 intervals starting from zero and ending where
            #     the last survival estimate is available. This is peformed to
            #     ensure that the survival estimates we merge will be available
            #     at a sufficient "resolution". For instance, even if the
            #     survival estimates are for one-year intervals due to sparsity,
            #     we get a reasonable survival estimate for a subject who was
            #     censored at 1.01 --- from somewhere close to 1.01 instead from
            #     2.0.
            # @codedoc_comment_block popEpi::prev_lexis
            sdt_ts_col_nms <- intersect(
              names(surv_lexis_arg_list[["breaks"]]),
              Epi::timeScales(lexis)
            )
            sdt_stratum_ts_col_nms <- setdiff(sdt_ts_col_nms, ts_fut_col_nm)
            sdt_stratum_col_nms <- intersect(
              c(
                names(aggre_by),
                paste0(
                  rep(sdt_stratum_ts_col_nms, each = 2L),
                  rep(
                    c("_start", "_stop"),
                    times = length(sdt_stratum_ts_col_nms)
                  )
                )
              ),
              names(sdt)
            )
            sdt <- sdt[
              j = {
                ts_fut_stop_col_nm <- paste0(ts_fut_col_nm, "_stop")
                n_interpolate <- 1001L
                ts_fut_interpolation_breaks <- seq(
                  0.0,
                  #' @importFrom data.table .SD
                  max(.SD[[ts_fut_stop_col_nm]]),
                  length.out = n_interpolate
                )
                interpolated_estimates <- surv_interpolate(
                  estimates = .SD[["S_ch"]],
                  ts_fut_stops = .SD[[ts_fut_stop_col_nm]],
                  ts_fut_stop_value = ts_fut_interpolation_breaks,
                  estimate_start_value = 1.0,
                  method = "linear"
                )
                out <- list(
                  ts_fut_start = ts_fut_interpolation_breaks[
                    -n_interpolate
                  ],
                  ts_fut_stop = ts_fut_interpolation_breaks[-1],
                  est = interpolated_estimates[-1]
                )
                names(out) <- c(
                  paste0(ts_fut_col_nm, "_", c("start", "stop")),
                  "S_ch"
                )
                out
              },
              keyby = eval(sdt_stratum_col_nms)
            ]
            lapply(sdt_ts_col_nms, function(sdt_ts_col_nm) {
              if (
                sdt_ts_col_nm != ts_fut_col_nm &&
                  data.table::uniqueN(sdt[[paste0(
                    sdt_ts_col_nm,
                    "_start"
                  )]]) ==
                    1
              ) {
                # @codedoc_comment_block popEpi::prev_lexis
                #   + If you passed `breaks` via `merge_dt` with multiple time
                #     scales (e.g. for period analysis), the time scales other
                #     than the survival time one are removed from the table of
                #     survival estimates if the time scale only defined one
                #     interval. For instance if you used
                #     `ts_cal = c(2020, 2022.9999)` then the table of survival
                #     estimates contains only one period and we discard the
                #     `ts_cal_start` and `ts_cal_stop` columns. This is to
                #     enable computing e.g. period analysis survival and then
                #     allowing those results to be applied to observations
                #     outside of the period. Otherwise e.g. period analysis
                #     survival estimates for `ts_cal = c(2020, 2022.9999)`
                #     would not be merged to an extrapolateable observation at
                #     `ts_cal = 2019.5`. But if multiple intervals are defined
                #     then those are kept, e.g.
                #     `ts_cal = c(2020, 2023, 2026)` leads to the period
                #     estimates `2020-2022` and `2023-2025`. If you do period
                #     analysis here, you must make sure to only use one period
                #     or the ensure that the estimated multiple periods cover
                #     all the extrapolateable observations.
                # @codedoc_comment_block popEpi::prev_lexis
                data.table::set(
                  x = sdt,
                  j = paste0(sdt_ts_col_nm, c("_start", "_stop")),
                  value = NULL
                )
              } else {
                data.table::set(
                  x = sdt,
                  j = sdt_ts_col_nm,
                  value = data.table::fctr(paste0(
                    "]",
                    round(sdt[[paste0(sdt_ts_col_nm, "_start")]], 11L),
                    ", ",
                    round(sdt[[paste0(sdt_ts_col_nm, "_stop")]], 11L),
                    "]"
                  ))
                )
              }
              NULL
            })
            sdt_ts_col_nms <- intersect(sdt_ts_col_nms, names(sdt))
            data.table::setnames(
              x = sdt,
              old = intersect(c("S_ch", "S_lt"), names(sdt))[1],
              new = "S"
            )
            sdt <- data.table::setDT(as.list(sdt)[
              intersect(c(names(aggre_by), sdt_ts_col_nms, "S"), names(sdt))
            ])
            sdt[]
          })
          merge_arg_list[["merge_dt_by"]] <- setdiff(
            names(merge_arg_list[["merge_dt"]]),
            "S"
          )
        }
        local({
          #' @param merge_optional_args `[NULL, list]` (default `NULL`)
          #'
          #' Each element passed to `[lexis_merge]`.
          #' E.g. `list(merge_dt_harmonisers = my_harmonisers)`.
          # @codedoc_comment_block popEpi::prev_lexis
          #   + Merge (for the first time) `merge_dt` with the collected
          #     subjects at the original exit time of each
          #     subject. This yields the survival probability for each
          #     subject at exit, in math we name this `S(t_e)`.
          #     E.g. subject was censored at `t_e = 2.1` means
          #     `S(t_e) = S(2.1)`.
          # @codedoc_comment_block popEpi::prev_lexis
          merge_arg_list[["lexis"]] <- lexis_dt_extrapolate
          merge_arg_list[["lex_dur_multiplier"]] <- 1L
          call_with_arg_list__(lexis_merge, merge_arg_list)
          data.table::setnames(
            x = lexis_dt_extrapolate,
            old = "S",
            new = "S_at_original_exit__"
          )

          # so that lexis_delay_entry will work
          lexis_immortalise(
            lexis = lexis_dt_extrapolate,
            breaks = NULL
          )
          # so that we can merge to the correct location
          lexis_delay_entry(
            lexis = lexis_dt_extrapolate,
            ts_col_new_entry = obs_tp_i,
            ts_col_nm = obs_ts_col_nm
          )
          # because lexis_immortalise produces e.g. Inf into lex.dur
          data.table::set(
            x = lexis_dt_extrapolate,
            i = seq_len(nrow(lexis_dt_extrapolate)),
            j = "lex.dur",
            value = switch(
              storage.mode(lexis_dt_extrapolate[["lex.dur"]]),
              integer = 1L,
              1e-6
            )
          )
          # @codedoc_comment_block popEpi::prev_lexis
          #   + Merge `merge_dt` for the second time, this time at the
          #     current prevalence observation time point such as at
          #     `ts_cal = 2009.999`.
          #     In math this is `S(t_p)` where `t_p` is the prevalence
          #     observation time point. For instance for one subject `t_p`
          #     might be `t_p = 5.7` because they were censored 5.7 years
          #     before the prevalence observation time point. Then we use
          #     `S(t_p) = S(5.7)`.
          # @codedoc_comment_block popEpi::prev_lexis
          merge_arg_list[["lex_dur_multiplier"]] <- 0L
          call_with_arg_list__(lexis_merge, merge_arg_list)
          data.table::setnames(
            x = lexis_dt_extrapolate,
            old = "S",
            new = "S_at_obs_tp_i__"
          )
          # @codedoc_comment_block popEpi::prev_lexis
          #   + With both `S(t_e)` and `S(t_p)` available, our
          #     "extrapolated" or
          #     "effective" for one subject for being in follow-up is
          #     between zero and
          #     one for each subject and defined simply as the conditional
          #     survival up to `t_p` starting from `t_e`,
          #     `S(t_p|t_e) = S(t_p) / S(t_e)`. E.g.
          #     `S(t_p) / S(t_e) = S(5.7) / S(2.1) = 0.8 / 0.9 ~ 0.8888889`.
          # @codedoc_comment_block popEpi::prev_lexis
          data.table::set(
            x = lexis_dt_extrapolate,
            j = "n_prev_extrapolated",
            value = exp(
              log(lexis_dt_extrapolate[["S_at_obs_tp_i__"]]) -
                log(lexis_dt_extrapolate[["S_at_original_exit__"]])
            )
          )
          # @codedoc_comment_block popEpi::prev_lexis
          #   + Call `[lexis_split_merge_aggregate_by_stratum]` for the second
          #     time, this time with the subjects collected for extrapolation,
          #     and sum the number of extrapolated subjects in follow-up into a
          #     table with the identical stratification as the one created
          #     before.
          # @codedoc_comment_block popEpi::prev_lexis
          # - here we currently have e.g. ts_cal = 2023.99, ts_age = 129.49,
          #   ts_fut = 40.37. now we just have to tabulate by the amount of time
          #   from the diagnosis, i.e. by ts_fut (and by age attained).
          agdt_add <- lexis_split_merge_aggregate_by_stratum(
            lexis = lexis_dt_extrapolate,
            breaks = stratum_breaks,
            aggre_exprs = list(
              n_prev_extrapolated = quote(sum(n_prev_extrapolated))
            ),
            aggre_by = aggre_by,
            first_record_by = first_record_by
          )
          na_idx <- which(is.na(agdt_add[["n_prev_extrapolated"]]))
          if (length(na_idx) > 0) {
            data.table::set(
              x = agdt_add,
              i = na_idx,
              j = "n_prev_extrapolated",
              value = 0.0
            )
          }
          # @codedoc_comment_block popEpi::prev_lexis
          #   + Add column `n_prev_eff` as the sum of the number of extrapolated
          #     subjects and the original `n_prev` into the first table we
          #     created.
          # @codedoc_comment_block popEpi::prev_lexis
          data.table::set(
            x = agdt_i,
            j = "n_prev_eff",
            value = agdt_i[["n_prev"]] + agdt_add[["n_prev_extrapolated"]]
          )
        })
      }
    }
    ts_fut_col_nm <- names(stratum_breaks)[length(stratum_breaks)]
    ts_fut_start_col_nm <- paste0(ts_fut_col_nm, "_start")
    data.table::set(
      x = agdt_i,
      j = ts_fut_start_col_nm,
      value = rep(
        methods::as(0L, storage.mode(agdt_i[[ts_fut_start_col_nm]])),
        nrow(agdt_i)
      )
    )
    value_col_nms <- intersect(names(agdt_i), c("n_prev", "n_prev_eff"))
    agdt_i[
      j = (value_col_nms) := lapply(.SD, cumsum),
      .SDcols = value_col_nms,
      by = intersect(
        names(agdt_i),
        c(
          names(aggre_by),
          paste0(setdiff(names(stratum_breaks), ts_fut_col_nm), "_id")
        )
      )
    ]
    return(agdt_i[])
  })
  # @codedoc_comment_block popEpi::prev_lexis
  # - Collect the observation time point-specific results into one big table.
  # @codedoc_comment_block popEpi::prev_lexis
  agdt <- data.table::rbindlist(agdt)
  data.table::setkeyv(
    x = agdt,
    cols = setdiff(names(agdt), c("n_prev", "n_prev_eff"))
  )

  # @codedoc_comment_block popEpi::prev_lexis
  # - If supplied, merge data from `n_at_risk_dt` into the big table. To
  #   accomplish this, any stratifying time scale columns defined via
  #   `stratum_breaks` and found also in `n_at_risk_dt` (the rather usual one
  #   being age, e.g. `ts_age`) is assumed to be a `factor` column with levels
  #   that specify the lower and upper bound of each interval in brackets, e.g.
  #   `[0, 5[`, though the brackets are ignored. The lower and upper bounds are
  #   made use of when merging the data from `n_at_risk_dt` into the big table,
  #   as the latter contains the lower and upper bounds as separate columns.
  #   E.g. column `ts_age` in `n_at_risk_dt` with levels
  #   `c("[0,5[", "[5,10[", ..., "[80,85[", "[85,Inf[")` is turned into the two
  #   columns `ts_age_start = c(0, 5, 10, ..., 80, 85)` and
  #   `ts_age_stop = c(5, 10, 15, ..., 85, Inf)`. The big output table contains
  #   the very same columns if one has supplied something like
  #   `stratum_breaks = list(ts_age = c(0, 5, 10, ..., 80, 85, Inf), ...)`.
  #   Then the merge is straightforward and the big table gains the additional
  #   column `n_at_risk`.
  # @codedoc_comment_block popEpi::prev_lexis
  if (!is.null(n_at_risk_dt)) {
    n_at_risk_dt <- dt_independent_frame_dependent_contents(n_at_risk_dt)
    lapply(
      intersect(names(stratum_breaks), names(n_at_risk_dt)),
      function(ts_col_nm) {
        interval_to_bounds_dt <- attr(
          infer_cut_args__(n_at_risk_dt[[ts_col_nm]]),
          "infer_cut_args_meta"
        )[
          j = .SD,
          .SDcols = c("level", "lo", "hi")
        ]
        data.table::setnames(interval_to_bounds_dt, "level", ts_col_nm)
        dt_join_assign(
          x = n_at_risk_dt,
          i = interval_to_bounds_dt,
          on = ts_col_nm,
          x_col_nms = paste0(ts_col_nm, c("_start", "_stop")),
          i_col_nms = c("lo", "hi")
        )
      }
    )
    dt_join_assign(
      x = agdt,
      i = n_at_risk_dt,
      on = setdiff(intersect(names(n_at_risk_dt), names(agdt)), "n_at_risk"),
      x_col_nms = "n_at_risk"
    )
    value_meta_dt <- data.table::rbindlist(lapply(
      intersect(names(agdt), c("n_prev", "n_prev_eff")),
      function(n_col_nm) {
        p_col_nm <- sub("^n_", "p_", n_col_nm)
        data.table::set(
          x = agdt,
          j = p_col_nm,
          value = agdt[[n_col_nm]] / agdt[["n_at_risk"]]
        )
        p_var_col_nm <- paste0(p_col_nm, "_var")
        data.table::set(
          x = agdt,
          j = p_var_col_nm,
          value = agdt[[p_col_nm]] *
            (1 - agdt[[p_col_nm]]) /
            agdt[["n_at_risk"]]
        )
        return(data.table::data.table(
          n_col_nm = n_col_nm,
          p_col_nm = p_col_nm,
          p_var_col_nm = p_var_col_nm
        ))
      }
    ))
    agdt <- local({
      # @codedoc_comment_block popEpi::prev_lexis
      # - If `weight_dt` was supplied, we harmonise it with the current big
      #   output table. Similarly to `n_at_risk_dt`, it is possible to include
      #   time scales from `stratum_breaks` in `weight_dt` as a factor with
      #   levels which are bracketed intervals, e.g.
      #    `c("[0,5[", "[5,10[", ..., "[80,85[", "[85,Inf[")`, following the
      #   format produced by `[cut]`. The lower and upper bounds are extracted
      #   and matched with the correspoding columns in the current big output
      #   table, e.g. with `ts_age_start` and `ts_age_stop`.
      # @codedoc_comment_block popEpi::prev_lexis
      adjust_col_nms <- if_else__(
        is.null(weight_dt),
        NULL,
        names(weight_dt)[!grepl("^weight", names(weight_dt))]
      )
      weight_dt <- if_else__(
        is.null(weight_dt),
        NULL,
        dt_independent_frame_dependent_contents(weight_dt)
      )
      adjust_ts_col_nms <- intersect(names(stratum_breaks), adjust_col_nms)
      lapply(
        adjust_ts_col_nms,
        function(ts_col_nm) {
          interval_to_bounds_dt <- attr(
            infer_cut_args__(weight_dt[[ts_col_nm]]),
            "infer_cut_args_meta"
          )[
            #' @importFrom data.table .SD
            j = .SD,
            .SDcols = c("level", "lo", "hi")
          ]
          data.table::setnames(
            x = interval_to_bounds_dt,
            c("level", "lo", "hi"),
            paste0(ts_col_nm, c("", "_start", "_stop"))
          )
          data.table::set(
            x = interval_to_bounds_dt,
            j = "id",
            value = seq_len(nrow(interval_to_bounds_dt))
          )
          dt_join_assign(
            x = interval_to_bounds_dt,
            i = lexis_box_dt__(breaks = stratum_breaks[ts_col_nm]),
            on = paste0(ts_col_nm, c("_start", "_stop")),
            x_col_nms = paste0(ts_col_nm, "_id")
          )
          dt_join_assign(
            x = weight_dt,
            i = interval_to_bounds_dt,
            on = ts_col_nm,
            x_col_nms = paste0(ts_col_nm, "_id")
          )
          data.table::set(x = weight_dt, j = ts_col_nm, value = NULL)
        }
      )
      local({
        # we must reset box_id because it is currently based also on
        # adjust_ts_col_nms. in the output we want it to be based only on the
        # output ts strata.
        output_stratum_box_dt <- lexis_box_dt__(
          breaks = stratum_breaks[!names(stratum_breaks) %in% adjust_ts_col_nms]
        )
        dt_join_assign(
          x = agdt,
          i = output_stratum_box_dt,
          on = setdiff(
            names(output_stratum_box_dt)[grepl(
              "_id$",
              names(output_stratum_box_dt)
            )],
            "box_id"
          ),
          x_col_nms = "box_id"
        )
      })
      stratum_col_nms <- setdiff(
        names(agdt),
        c(unlist(value_meta_dt), "n_at_risk")
      )
      adjust_col_nms <- if_else__(
        is.null(weight_dt),
        NULL,
        intersect(stratum_col_nms, names(weight_dt))
      )
      adjust_ts_col_nms <- intersect(
        sub("_id$", "", adjust_col_nms),
        Epi::timeScales(lexis)
      )
      stratum_col_nms <- if_else__(
        length(adjust_ts_col_nms) > 0,
        setdiff(
          stratum_col_nms,
          paste0(adjust_ts_col_nms, "_", c("start", "stop"))
        ),
        stratum_col_nms
      )
      output_stratum_col_nms <- setdiff(
        stratum_col_nms,
        adjust_col_nms
      )
      # @codedoc_comment_block popEpi::prev_lexis
      # - `[directadjusting::directly_adjusted_estimates]` is called to produce
      #   adjusted estimates of prevalence proportions if `weight_dt` was
      #   supplied or non-adjusted ones of not. Both `p_prev` and `p_prev_eff`
      #   are passed to `[directadjusting::directly_adjusted_estimates]`, if
      #   the latter was estimated.
      # @codedoc_comment_block popEpi::prev_lexis
      agdt_adj <- directadjusting::directly_adjusted_estimates(
        stats_dt = agdt,
        stat_col_nms = value_meta_dt[["p_col_nm"]],
        var_col_nms = value_meta_dt[["p_var_col_nm"]],
        stratum_col_nms = output_stratum_col_nms,
        adjust_col_nms = adjust_col_nms,
        #' @param conf_lvls Passed as-is to
        #' `[directadjusting::directly_adjusted_estimates]`
        conf_lvls = conf_lvls,
        #' @param conf_methods Passed as-is to
        #' `[directadjusting::directly_adjusted_estimates]`
        conf_methods = conf_methods,
        weights = weight_dt
      )
      lapply(
        attr(agdt_adj, "directly_adjusted_estimates_meta")[["meta_dt"]][[
          "var_col_nm_w"
        ]],
        function(var_col_nm) {
          data.table::set(
            x = agdt_adj,
            j = var_col_nm,
            value = sqrt(agdt_adj[[var_col_nm]])
          )
          data.table::setnames(
            x = agdt_adj,
            old = var_col_nm,
            new = sub("_var$", "_se", var_col_nm)
          )
        }
      )

      # @codedoc_comment_block popEpi::prev_lexis
      # - Even if adjusting is performed, the output table will contain
      #   "raw" sums of `n_at_risk`, `n_prev`, and `n_prev_eff` if it was
      #   computed.
      # @codedoc_comment_block popEpi::prev_lexis
      agdt <- agdt[
        #' @importFrom data.table .SD
        j = lapply(.SD, sum),
        .SDcols = c("n_at_risk", value_meta_dt[["n_col_nm"]]),
        keyby = output_stratum_col_nms
      ]
      dt_join_assign(
        x = agdt,
        i = agdt_adj,
        on = output_stratum_col_nms,
        x_col_nms = setdiff(names(agdt_adj), output_stratum_col_nms)
      )
      agdt[]
    })
  }

  # @codedoc_comment_block return(popEpi::prev_lexis)
  # Returns a `data.table` with
  # - Stratifying columns defined via `aggre_by` (if any),
  # - Stratifying time scale columns defined via `stratum_breaks` (if any),
  # - The observation time scale column defined via `observation_time_points`,
  # - `n_prev`, the number of subjects in follow-up, and
  # - `n_prev_eff`, the above plus the "extrapolated" number in follow-up, if
  #   this was requested.
  # - `n_at_risk`, the merged values from `n_at_risk_dt` if it was supplied.
  # - `p_prev`, `n_prev / n_at_risk`, the proportion of prevalent records out of
  #   `n_at_risk`, if `n_at_risk_dt` was supplied.
  # - `p_prev_eff`, `n_prev_eff / n_at_risk`, the proportion of the effective
  #   number of prevalent records out of `n_at_risk`, if `n_at_risk_dt` was
  #   supplied and `n_prev_eff` was computed.
  # - For `p_prev` and `p_prev_eff` the additional columns which indicate
  #   standard error and the lower and upper confidence bounds, identified with
  #   the suffix `_se`, `_lo`, and `_hi`, respectively.
  # @codedoc_comment_block return(popEpi::prev_lexis)
  return(agdt[])
}
