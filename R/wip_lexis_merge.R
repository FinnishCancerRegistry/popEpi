lexis_merge_guess_breaks__ <- function(x) {
  # @codedoc_comment_block lexis_merge_guess_breaks__
  #   + If `merge_dt[[col_nm]]` contains numbers we define `cut` breaks as the
  #     unique values of
  #     `merge_dt[[col_nm]]` and as the ceiling
  #     `max(merge_dt[[col_nm]]) + last_diff`. Here `last_diff` is the
  #     difference between the highest and second highest values. E.g.
  #     `c(1950, 1960, 1970:2020, 2021)` for `merge_dt[[col_nm]]`
  #     containing unique values `c(1950, 1960, 1970:2020)`.
  # @codedoc_comment_block lexis_merge_guess_breaks__
  last_diff <- diff(utils::tail(x, 2L))
  cut_breaks <- c(
    x,
    x[length(x)] + last_diff
  )
  return(cut_breaks)
}

lexis_merge_default_harmoniser__ <- function(
  col_nm,
  cut_breaks,
  lex_dur_multiplier = 0.5
) {
  # @codedoc_comment_block lexis_merge_default_harmoniser__
  #    + With the `cut` breaks defined, the automatically created harmoniser
  #      becomes a `cut` call with arguments
  #      * `x = col + lex.dur * lex_dur_multiplier`, where `COL` is the current
  #        column and `lex_dur_multiplier` is by default `0.5`,
  #      * `breaks` as specified above,
  #      * `right = FALSE`, and
  #      * `labels = FALSE`.
  #    + This results in indices to the breaks, and the harmoniser returns
  #      break values at those indices. E.g. the cut results in
  #      `3` in `breaks = c(1950, 1960, 1970:2020, 2021)` and output is
  #      `1970` for every value of `COL + lex.dur / 2` in the interval
  #      `]1970, 1971]`.
  # @codedoc_comment_block lexis_merge_default_harmoniser__
  substitute(
    {
      breaks <- cut_breaks # nolint
      idx <- cut( # nolint
        x = col + lex.dur * lex_dur_multiplier,
        breaks = breaks,
        right = FALSE,
        labels = FALSE
      )
      breaks[idx]
    },
    list(
      col = parse(text = col_nm)[[1]],
      cut_breaks = cut_breaks,
      lex_dur_multiplier = lex_dur_multiplier
    )
  )
}

#' @title Merge Data into `Lexis` Object
#' @description
#' Function(s) to merge data into `Lexis` objects intelligently when merging
#' is (partially) based on time scales.
#' @name lexis_merge
#' @family Lexis_functions
NULL

#' @eval codedoc::pkg_doc_fun(
#'   "popEpi::lexis_merge",
#'   "lexis_merge"
#' )
#' @examples
#' # popEpi::lexis_merge
#' lexis <- Epi::Lexis(
#'   entry = list(ts_fut = 0.0, ts_cal = 2010.3, ts_age = 56.8),
#'   exit = list(ts_cal = 2024.9999),
#'   entry.status = 0L,
#'   exit.status = 0L
#' )
#' lexis$sex <- 0L
#' lexis <- popEpi::splitMulti(
#'   data = lexis,
#'   breaks = list(ts_fut = seq(0, 3, 1 / 12))
#' )
#' my_merge_dt <- data.table::CJ(sex = 0:1, ts_age = 0:100, ts_cal = 2000:2025)
#' data.table::set(
#'   x = my_merge_dt,
#'   j = "merge_value",
#'   value = runif(nrow(my_merge_dt))
#' )
#' popEpi::lexis_merge(
#'   dt = lexis,
#'   merge_dt = my_merge_dt,
#'   merge_dt_by = c("sex", "ts_age", "ts_cal")
#' )
#' stopifnot(
#'   "merge_value" %in% names(lexis),
#'   !is.na(lexis[["merge_value"]])
#' )
#' data.table::set(
#'   x = lexis,
#'   j = "merge_value",
#'   value = NULL
#' )
#' popEpi::lexis_merge(
#'   dt = lexis,
#'   merge_dt = my_merge_dt,
#'   merge_dt_by = c("sex", "ts_age", "ts_cal"),
#'   merge_dt_harmonisers = list(
#'     ts_cal = quote(as.integer(ts_cal)),
#'     ts_age = quote(as.integer(ts_age))
#'   )
#' )
#' stopifnot(
#'   "merge_value" %in% names(lexis),
#'   !is.na(lexis[["merge_value"]])
#' )
#'
lexis_merge <- function(
  dt,
  merge_dt,
  merge_dt_by,
  merge_dt_harmonisers = NULL,
  optional_steps = NULL
) {
  # @codedoc_comment_block popEpi::lexis_merge
  # `popEpi::lexis_merge` can be used to merge additional information into
  # `Lexis` data, allowing the use of the `Lexis` time scales in the
  # merge. The typical use-case is to split `Lexis` data and then merge
  # population (expected) hazards to the subject-intervals.
  # `popEpi::lexis_merge` performs the following steps:
  #
  # @codedoc_comment_block popEpi::lexis_merge
  call_env <- parent.frame(1L)
  eval_env <- environment()
  #' @param optional_steps `[NULL, list]` (default `NULL`)
  #'
  #' Optional steps to perform during the function's run.
  #'
  #' - `NULL`: No additional steps are performed.
  #' - `list`: Each element is named and a function. See **Details**
  #'   For what each functions you can make use of what their arguments should
  #'   be.
  # @codedoc_comment_block popEpi::lexis_merge
  # - Run
  #   `optional_steps[["on_entry"]](call_env = call_env, eval_env = eval_env)`
  #   if that `optional_steps` element exists. `call_env` is the environment
  #   where this function was called and `eval_env` is the temporary environment
  #   in which the commands that this function consists of are evaluated.
  # @codedoc_comment_block popEpi::lexis_merge
  if ("on_entry" %in% names(optional_steps)) {
    optional_steps[["on_entry"]](
      call_env = call_env,
      eval_env = eval_env
    )
  }
  # @codedoc_comment_block popEpi::lexis_merge
  # - Run
  #   `on.exit(optional_steps[["on_exit"]](call_env = call_env, eval_env = eval_env))`
  #   if that `optional_steps` element exists.
  # @codedoc_comment_block popEpi::lexis_merge
  if ("on_exit" %in% names(optional_steps)) {
    on.exit(optional_steps[["on_exit"]](
      call_env = call_env,
      eval_env = eval_env
    ))
  }
  #' @param dt `[Lexis]` (no default)
  #'
  #' A `Lexis` dataset (`[Epi::Lexis]` / `[Lexis_dt]`).
  # @codedoc_comment_block surv_arg_dt
  assert_is_arg_dt(dt, lexis = TRUE)
  #' @param merge_dt `[data.table, NULL]` (no default or default `NULL`)
  #'
  #' A `data.table` to merge with `dt`, possibly after splitting.
  #'
  #' - `NULL`: Allowed only if this is the function's default for this argument.
  #'   When both `merge_dt` and `merge_dt_by` are `NULL`, no merge is performed.
  #' - `data.table`: Merge this table. Typically this contains the
  #'   expected hazards for the estimation of relative or net survival.
  #' @param merge_dt_by `[character, NULL]` (no default or default `NULL`)
  #'
  #' Names of columns in both `merge_dt` and `dt` by which `merge_dt` will be
  #' merged with ` dt`.
  #'
  #' - `NULL`: Allowed only if this is the function's default for this argument.
  #'   When both `merge_dt` and `merge_dt_by` are `NULL`, no merge is performed.
  #' - `character`: Use these columns.
  assert_is_arg_merge_dt_and_merge_dt_by(
    merge_dt,
    merge_dt_by,
    dt,
    mandatory = TRUE
  )
  call_env <- parent.frame(1L)
  lexis_ts_col_nms <- attr(dt, "time.scales")
  merge_ts_col_nms <- intersect(lexis_ts_col_nms, merge_dt_by)
  #' @param merge_dt_harmonisers `[NULL, list]` (default `NULL`)
  #'
  #' Optional list of quoted expressions which, when evaluated, harmonise
  #' data in `dt` (possibly after splitting) to look the same as the data in
  #' `merge_dt`. For example, if `merge_dt` contains expected hazards by
  #' calendar year `ts_cal` and 1-year age group `ts_age`, and if `dt` has been
  #' split into monthly survival intervals, then we must somehow find the
  #' correct row in `merge_dt` for each row in `dt`. E.g. `ts_cal = 2010.5323`
  #' and `ts_age = 76.4435` need to be harmonised into values found in
  #' `merge_dt` such as `ts_cal = 2010` and `ts_age = 76`.
  #'
  #' - `NULL`: This function comes up with reasonable harmonisers if possible.
  #'   See **Details**.
  #' - `list`: Each element must a quoted expression (`[quote]`) and each
  #'   element must have a name corresponding to a column name in both `dt`
  #'   and `merge_dt`. See **Examples**.
  if (is.null(merge_dt_harmonisers)) {
    # @codedoc_comment_block popEpi::lexis_merge
    # - If `is.null(merge_dt_harmonisers)`, attempt to
    #   automatically determine the harmonisers making use of `cut` by looking
    #   at the unique
    #   values of the time scale to merge by in `merge_dt` (e.g. calendar year
    #   in `ts_cal`):
    # @codedoc_comment_block popEpi::lexis_merge
    merge_dt_harmonisers <- lapply(merge_ts_col_nms, function(col_nm) {
      cut_breaks <- sort(unique(merge_dt[[col_nm]]))
      if (is.integer(cut_breaks) || is.double(cut_breaks)) {
        # @codedoc_comment_block popEpi::lexis_merge
        # @codedoc_insert_comment_block lexis_merge_guess_breaks__
        # @codedoc_comment_block popEpi::lexis_merge
        cut_breaks <- lexis_merge_guess_breaks__(cut_breaks)
      } else {
        # @codedoc_comment_block popEpi::lexis_merge
        #   + If `merge_dt[[col_nm]]` does not contain numbers, an error is
        #     raised because we don't know how to automatically form a
        #     harmoniser.
        # @codedoc_comment_block popEpi::lexis_merge
        stop(
          "Cannot automatically determine `merge_dt_harmonisers$", col_nm, "`;",
          "Please supply argument `merge_dt_harmonisers` yourself."
        )
      }
      surv_merge_default_harmoniser_arg_list <- list(
        col_nm = col_nm,
        cut_breaks = cut_breaks,
        lex_dur_multiplier = 0.5
      )
      # @codedoc_comment_block popEpi::lexis_merge
      # - Run
      #   `optional_steps[["pre_default_harmoniser_creation"]](call_env = call_env, eval_env = eval_env, lapply_eval_env = lapply_eval_env)`
      #   if that `optional_steps` element exists.
      #   Here `lapply_eval_env` is similar to `eval_env` but it is the
      #   evaluation environment of the anonymous function passed to `lapply`
      #   which attempts to handle each harmoniser.
      # @codedoc_comment_block popEpi::lexis_merge
      lapply_eval_env <- environment()
      if ("pre_default_harmoniser_creation" %in% names(optional_steps)) {
        optional_steps[["pre_default_harmoniser_creation"]](
          call_env = call_env,
          eval_env = eval_env,
          lapply_env = lapply_eval_env
        )
      }
      # @codedoc_comment_block popEpi::lexis_merge
      # @codedoc_insert_comment_block lexis_merge_default_harmoniser__
      # @codedoc_comment_block popEpi::lexis_merge
      do.call(
        lexis_merge_default_harmoniser__,
        surv_merge_default_harmoniser_arg_list,
        quote = TRUE
      )
    })
    names(merge_dt_harmonisers) <- merge_ts_col_nms
  }
  # @codedoc_comment_block popEpi::lexis_merge
  # - Run
  #   `optional_steps[["post_merge_dt_harmonisers"]](call_env = call_env, eval_env = eval_env)`
  #   if that `optional_steps` element exists.
  # @codedoc_comment_block popEpi::lexis_merge
  if ("post_merge_dt_harmonisers" %in% names(optional_steps)) {
    optional_steps[["post_merge_dt_harmonisers"]](
      call_env = call_env,
      eval_env = eval_env
    )
  }
  # @codedoc_comment_block popEpi::lexis_merge
  # - Armed with either user-defined or automatically created
  #   `merge_dt_harmonisers`, they are each evaluated to create a temporary
  #   `data.table` with harmonised data from `dt`. This is performed via
  #   `eval` with `envir = dt` and `enclos = call_env` where `call_env` is the
  #   environment where `popEpi::lexis_merge` was called. Of course if a column
  #   has no harmoniser at this point then it is used as-is. For instance there
  #   is no need to harmonise stratifying columns because they are not changed
  #   by splitting the `Lexis` data.
  # @codedoc_comment_block popEpi::lexis_merge
  join_dt <- data.table::setDT(lapply(merge_dt_by, function(col_nm) {
    if (col_nm %in% names(merge_dt_harmonisers)) {
      eval(
        merge_dt_harmonisers[[col_nm]],
        envir = dt,
        enclos = call_env
      )
    } else {
      dt[[col_nm]]
    }
  }))
  data.table::setnames(join_dt, merge_dt_by)
  merge_value_col_nms <- setdiff(names(merge_dt), merge_dt_by)
  # @codedoc_comment_block popEpi::lexis_merge
  # - Run
  #   `optional_steps[["post_harmonisation"]](call_env = call_env, eval_env = eval_env)`
  #   if that `optional_steps` element exists.
  # @codedoc_comment_block popEpi::lexis_merge
  if ("post_harmonisation" %in% names(optional_steps)) {
    optional_steps[["post_harmonisation"]](
      call_env = call_env,
      eval_env = eval_env
    )
  }
  # @codedoc_comment_block popEpi::lexis_merge
  # - Then we perform the actual merge between `merge_dt` and the harmonised
  #   data. This merges in data from `merge_dt` into every row of `dt` in-place.
  #   So `dt` is modified and no additional copy is taken for the sake of
  #   efficiency.
  # @codedoc_comment_block popEpi::lexis_merge
  data.table::set(
    x = dt,
    j = merge_value_col_nms,
    value = merge_dt[
      i = join_dt,
      on = names(join_dt),
      #' @importFrom data.table .SD
      j = .SD,
      .SDcols = merge_value_col_nms
    ]
  )
  # @codedoc_comment_block popEpi::lexis_merge
  # - Run
  #   `optional_steps[["post_merge"]](call_env = call_env, eval_env = eval_env)`
  #   if that `optional_steps` element exists.
  # @codedoc_comment_block popEpi::lexis_merge
  if ("post_merge" %in% names(optional_steps)) {
    optional_steps[["post_merge"]](
      call_env = call_env,
      eval_env = eval_env
    )
  }
  # @codedoc_comment_block popEpi::lexis_merge
  # - Each merged-in column from `merge_dt` (all columns not in `merge_dt_by`)
  #   are inspected for missing values. If there are any, an error is raised.
  #   This usually occurs if `merge_dt` does not contain data for all data in
  #   `dt`. For instance it only covers years 1950-2020 but `dt` contains also
  #   data for 2021. This error helps you to spot those problems early instead
  #   of producing nonsense results downstream.
  # @codedoc_comment_block popEpi::lexis_merge
  for (merge_value_col_nm in merge_value_col_nms) {
    is_missing <- is.na(dt[[merge_value_col_nm]])
    if (any(is_missing)) {
      print(data.table::setDT(dt[is_missing, ]))
      stop("Merging `merge_dt` into split (subset of) `dt` produced NA values ",
           "in column `dt$", merge_value_col_nm, "`. ",
           "See the table printed above.")
    }
  }
  # @codedoc_comment_block popEpi::lexis_merge
  # - Returns `dt` invisibly after adding columns from
  #   `merge_dt` into `dt` in-place, without taking a copy.
  # @codedoc_comment_block popEpi::lexis_merge
  # @codedoc_comment_block return(popEpi::lexis_merge)
  # Returns `dt` invisibly after adding columns from
  # `merge_dt` into `dt` in-place, without taking a copy.
  # @codedoc_comment_block return(popEpi::lexis_merge)
  return(invisible(dt[]))
}

lexis_merge_survival <- function(
  dt,
  survival_dt,
  survival_dt_by,
  survival_dt_harmonisers = NULL,
  ts_fut_col_nm
) {
  stopifnot(
    is.data.frame(survival_dt),
    "survival" %in% names(survival_dt),
    ts_fut_col_nm %in% names(survival_dt),
    ts_fut_col_nm %in% survival_dt_by
  )
  survival_dt <- data.table::setDT(as.list(survival_dt))
  data.table::set(
    x = survival_dt,
    j = c(
      "cumulative_hazard",
      "survival_interval_start",
      "delta_t"
    ),
    value = list(
      -log(survival_dt[["survival"]]),
      survival_dt[[ts_fut_col_nm]],
      local({
        breaks <- lexis_merge_guess_breaks__(
          sort(unique(survival_dt[[ts_fut_col_nm]]))
        )
        breaks[match(survival_dt[[ts_fut_col_nm]], breaks) + 1L] -
          survival_dt[[ts_fut_col_nm]]
      })
    )
  )
  survival_dt[
    #' @importFrom data.table := .SD
    j = "hazard" := diff(c(0.0, .SD[["cumulative_hazard"]])),
    by = setdiff(survival_dt_by, ts_fut_col_nm)
  ]
  lexis_merge(
    dt = dt,
    merge_dt = survival_dt,
    merge_dt_by = survival_dt_by,
    merge_dt_harmonisers = survival_dt_harmonisers,
    optional_steps = list(
      pre_default_harmoniser_creation = function(
        eval_env,
        call_env,
        lapply_eval_env
      ) {
        lapply_eval_env[["surv_merge_default_harmoniser_arg_list"]][[
          "lex_dur_multiplier"
        ]] <- 1.0
        NULL
      }
    )
  )
  value_col_nms <- setdiff(names(survival_dt), survival_dt_by)
  work_dt <- data.table::setDT(as.list(dt)[value_col_nms])
  data.table::set(x = dt, j = value_col_nms, value = NULL)
  work_dt[
    j = "delta" := (
      work_dt[["survival_interval_start"]] +
        work_dt[["delta_t"]]
    ) - (dt[[ts_fut_col_nm]] + dt[["lex.dur"]])
  ]
  work_dt[
    j = "cumulative_hazard" := work_dt[["cumulative_hazard"]] -
      work_dt[["delta"]] * work_dt[["hazard"]]
  ]
  data.table::set(
    x = dt,
    j = "survival",
    value = exp(-work_dt[["cumulative_hazard"]])
  )
  return(invisible(dt[]))
}
