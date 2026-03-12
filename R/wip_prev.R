#' @title Prevalence
#' @description
#' Function(s) to compute prevalence statistics.
#' @name prev_functions
NULL

#' @eval codedoc::pkg_doc_fun(
#'   "popEpi::prev_lexis",
#'   "prev_functions"
#' )
prev_lexis <- function(
  lexis,
  observation_time_points,
  stratum_breaks,
  aggre_by = NULL,
  subset = NULL,
  merge_dt_by = NULL,
  merge_dt = NULL,
  merge_optional_args = NULL
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
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum::subset
  # @codedoc_insert_comment_block popEpi:::handle_arg_subset
  # @codedoc_comment_block popEpi::lexis_split_merge_aggregate_by_stratum::subset
  subset <- handle_arg_subset(dataset_nm = "lexis")
  #' @param aggre_by Passed to `[lexis_split_merge_aggregate_by_stratum]`.
  aggre_by <- handle_arg_by(by = aggre_by, dataset = lexis)

  #' @param merge_dt `[NULL, data.table]` (default `NULL`)
  #'
  #' Table containing survival estimates applicable to `lexis`.
  #' These survival
  #' estimates are used in the "projection" of prevalence in those
  #' observations
  #' which have been lost to follow-up. See **Details** for how this
  #' works and
  #' what you need to have.
  #'
  #' - `NULL`: No projection is performed.
  #' - `data.table`: Must contain stratum columns also found in `lexis`
  #'   and
  #'   exactly one value column named `S`. This is table is  passed
  #'   to `[lexis_merge]` and should conform to its requirements. E.g.
  #'   `data.table(ts_fut = factor(c("[0, 1[", ...)), S = c(0.9, ...))`.
  if (!is.null(merge_dt)) {
    stopifnot(
      "S" %in% names(merge_dt),
      !is.na(merge_dt[["S"]]),
      merge_dt[["S"]] > 0,
      !duplicated(merge_dt, by = merge_dt_by),
      merge_dt_by %in% names(lexis)
    )
  }
  # @codedoc_comment_block popEpi::prev_lexis
  # `popEpi::prev_lexis` can be used to compute numbers of (potentially
  # effective numbers of) subjects remaining in follow-up at arbitrary time
  # points. It performs the following steps:
  #
  # - For each observation time point in `observation_time_points[[1]]`:
  #   + Calls `[lexis_split_merge_aggregate_by_stratum]` with the
  #     `lexis`, `subset`, `aggre_by` supplied to
  #     `popEpi::prev_lexis` and with
  #     `aggre_exprs = list(n_prev = quote(.N))`, `breaks = stratum_breaks` and
  #     `aggre_ts_col_nms = names(stratum_breaks)`.
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
          methods::as(1L, class(lexis_dt_obs_tp[["lex.dur"]])[1])
        } else {
          methods::as(1e-6, class(lexis_dt_obs_tp[["lex.dur"]])[1])
        }
      )
      agdt_i <- popEpi::lexis_split_merge_aggregate_by_stratum(
        lexis = lexis_dt_obs_tp,
        subset = subset & could_delay_entry,
        breaks = stratum_breaks,
        aggre_exprs = list(n_prev = quote(.N)),
        aggre_by = aggre_by,
        aggre_ts_col_nms = names(stratum_breaks)
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
      local({
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
            (lexis[[obs_ts_col_nm]] + lexis[["lex.dur"]]) <
            obs_tp_i
          lexis_dt_extrapolate <- lexis_to_lexis_dt__(
            lexis,
            subset = subset & was_censored & before_obs_tp,
            select = intersect(names(lexis), c(
              "lex.id", Epi::timeScales(lexis), "lex.dur", "lex.Cst", "lex.Xst",
              names(aggre_by),
              merge_dt_by
            ))
          )
          lexis_dt_extrapolate
        })
        #' @param merge_dt_by Passed to `[lexis_merge]`.
        #' @param merge_optional_args `[NULL, list]` (default `NULL`)
        #'
        #' Each element passed to `[lexis_merge]`.
        #' E.g. `list(merge_dt_harmonisers = my_harmonisers)`.
        # @codedoc_comment_block popEpi::prev_lexis
        #   + Merge (for the first time) `merge_dt` with the collected
        #     subjects at the original exit time of each
        #     subject. This yields the survival probability for each subject
        #     at exit, in math `S(t_e)`
        #     (starting from zero --- delayed entry is not supported).
        # @codedoc_comment_block popEpi::prev_lexis
        merge_arg_list <- as.list(merge_optional_args)
        merge_arg_list[
          c("lexis", "merge_dt", "merge_dt_by", "lex_dur_multiplier")
        ] <-
          list(
            lexis_dt_extrapolate,
            merge_dt,
            merge_dt_by,
            1L
          )
        do.call(popEpi::lexis_merge, merge_arg_list, quote = TRUE)
        data.table::setnames(
          x = lexis_dt_extrapolate,
          old = "S",
          new = "S_at_original_exit__"
        )

        lexis_immortalise(
          lexis = lexis_dt_extrapolate,
          breaks = NULL
        )
        lexis_delay_entry(
          lexis = lexis_dt_extrapolate,
          ts_col_new_entry = obs_tp_i,
          ts_col_nm = obs_ts_col_nm
        )
        # @codedoc_comment_block popEpi::prev_lexis
        #   + Merge `merge_dt` for the second time, this time at the
        #     current prevalence observation time point such as at
        #     `ts_cal = 2009.999`.
        #     In math this is `S(t_p)` where `t_p` is the prevalence
        #     observation time point.
        # @codedoc_comment_block popEpi::prev_lexis
        merge_arg_list <- as.list(merge_optional_args)
        merge_arg_list[
          c("lexis", "merge_dt", "merge_dt_by", "lex_dur_multiplier")
        ] <-
          list(
            lexis_dt_extrapolate,
            merge_dt,
            merge_dt_by,
            0L
          )
        do.call(popEpi::lexis_merge, merge_arg_list, quote = TRUE)
        data.table::setnames(
          x = lexis_dt_extrapolate,
          old = "S",
          new = "S_at_obs_tp_i__"
        )
        # @codedoc_comment_block popEpi::prev_lexis
        #   + With both `S(t_e)` and `S(t_p)` available, our "extrapolated" or
        #     "effective" number of being in follow-up is between zero and
        #     one for each subject and defined simply as the conditional
        #     survival up to `t_p` starting from `t_e`,
        #     `S(t_p|t_e) = S(t_p) / S(t_e)`. E.g.
        #     `S(t_p) / S(t_e) = 0.8 / 0.9 ~ 0.8888889`.
        # @codedoc_comment_block popEpi::prev_lexis
        data.table::set(
          x = lexis_dt_extrapolate,
          j = "n_prev_extrapolated",
          value = lexis_dt_extrapolate[["S_at_obs_tp_i__"]] /
            lexis_dt_extrapolate[["S_at_original_exit__"]]
        )
        # @codedoc_comment_block popEpi::prev_lexis
        #   + Call `[lexis_split_merge_aggregate_by_stratum]` for the second
        #     time, this time with the subjects collected for extrapolation,
        #     and sum the number of extrapolated subjects in follow-up into a
        #     table with the identical stratification as the one created before.
        # @codedoc_comment_block popEpi::prev_lexis
        agdt_add <- popEpi::lexis_split_merge_aggregate_by_stratum(
          lexis = lexis_dt_extrapolate,
          breaks = stratum_breaks,
          aggre_exprs = list(
            n_prev_extrapolated = quote(sum(n_prev_extrapolated))
          ),
          aggre_by = aggre_by,
          aggre_ts_col_nms = names(stratum_breaks)
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
          value = agdt_i[["n_prev"]] + agdt_add[["n_prev_eff"]]
        )
      })
    }
    ts_fut_col_nm <- names(stratum_breaks)[length(stratum_breaks)]
    ts_fut_start_col_nm <- paste0(ts_fut_col_nm, "_start")
    data.table::set(
      x = agdt_i,
      j = ts_fut_start_col_nm,
      value = rep(
        methods::as(0L, class(agdt_i[[ts_fut_start_col_nm]])[1]),
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
  # - Collect the observation time point-specific results into one big table and
  #   return it.
  # @codedoc_comment_block popEpi::prev_lexis
  agdt <- data.table::rbindlist(agdt)
  data.table::setkeyv(
    x = agdt,
    cols = setdiff(names(agdt), c("n_prev", "n_prev_eff"))
  )

  # @codedoc_comment_block return(popEpi::prev_lexis)
  # Returns a `data.table` with
  # - Stratifying columns defined via `aggre_by` (if any),
  # - Stratifying time scale columns defined via `stratum_breaks` (if any),
  # - The observation time scale column defined via `observation_time_points`,
  # - `n_prev`, the number of subjects in follow-up, and
  # - `n_prev_eff`, the above plus the "extrapolated" number in follow-up, if
  #   this was requested.
  # @codedoc_comment_block return(popEpi::prev_lexis)
  return(agdt[])
}
