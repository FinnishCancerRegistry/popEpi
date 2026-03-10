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

lexis_merge_make_harmoniser__ <- function(
  col_nm,
  merge_dt,
  optional_steps,
  call_env,
  eval_env,
  lex_dur_multiplier
) {
  col <- merge_dt[[col_nm]]
  if (is.factor(col)) {
    # @codedoc_comment_block popEpi:::lexis_merge_make_harmoniser__
    #   + If `merge_dt[[col_nm]]` is a factor column,
    # @codedoc_insert_comment_block popEpi:::infer_cut_args__
    # @codedoc_comment_block popEpi:::lexis_merge_make_harmoniser__
    cut_arg_list <- infer_cut_args__(col)
    if (is.null(cut_arg_list)) {
      stop(
        "merge_dt$", col_nm, " was of class factor, ",
        "but could infer how it can be created using on split lexis ",
        "data. Either ensure that merge_dt$", col_nm, " has levels such ",
        "as `\"[2000,2001[\"`, `\"[60, 61[\"` etc or supply argument ",
        "`merge_dt_harmonisers`."
      )
    }
    cut_arg_list[["labels"]] <- attr(cut_arg_list, "infer_cut_args_meta")[[
      "level"
    ]]
  } else if (is.integer(col) || is.double(col)) {
    lower_bounds <- sort(unique(merge_dt[[col_nm]]))
    # @codedoc_comment_block popEpi:::lexis_merge_make_harmoniser__
    # @codedoc_insert_comment_block lexis_merge_guess_breaks__
    # @codedoc_comment_block popEpi:::lexis_merge_make_harmoniser__
    cut_arg_list <- list(
      breaks = lexis_merge_guess_breaks__(lower_bounds),
      right = FALSE,
      labels = lower_bounds
    )
  } else {
    # @codedoc_comment_block popEpi:::lexis_merge_make_harmoniser__
    #   + If `merge_dt[[col_nm]]` is not of class integer, numeric, or
    #     factor, an error is
    #     raised because we don't know how to automatically form a
    #     harmoniser.
    # @codedoc_comment_block popEpi:::lexis_merge_make_harmoniser__
    stop(
      "Column merge_dt$", col_nm, " was not of class ",
      "integer, numeric, or factor, so we ",
      "could not infer a harmoniser for it. ",
      "Please supply one yourself via `merge_dt_harmonisers`."
    )
  }
  # @codedoc_comment_block popEpi:::lexis_merge_make_harmoniser__
  #   + Run
  #     `optional_steps[["pre_default_harmoniser_creation"]](call_env = call_env, eval_env = eval_env, lapply_eval_env = lapply_eval_env)`
  #     if that `optional_steps` element exists.
  #     Here `make_harmoniser_eval_env` is similar to `eval_env` but it is the
  #     evaluation environment of the function passed to `lapply`
  #     which attempts to make each default harmoniser.
  # @codedoc_comment_block popEpi:::lexis_merge_make_harmoniser__
  is_num <- inherits(cut_arg_list[["labels"]], c("numeric", "integer"))
  substitute_arg_list <- list(
    breaks = cut_arg_list[["breaks"]],
    right = cut_arg_list[["right"]],
    labels = if (is_num) FALSE else cut_arg_list[["labels"]],
    # @codedoc_comment_block popEpi:::lexis_merge_make_harmoniser__::lex_dur_multiplier
    # `lex_dur_multiplier` is used in the following expression when a default
    # harmoniser is being made: `col + lex.dur * lex_dur_multiplier`.
    # Here `col` is the column in question. When merging population expected
    # hazards into split `lexis` data it is reasonable to use
    # `lex_dur_multiplier = 0.5`, e.g. with `ts_cal = 2001.9`, `ts_fut = 0.0`,
    # and `lex_dur = 0.5`
    # we arrive to the middle of the interval (or end of follow-up for that
    # subject) at `ts_cal = 2002.15` and `ts_fut = 0.25`. Here it is more
    # reasonable to merge data from 2002 rather than 2001 because the majority
    # of follow-up occurs in 2002 for that record.
    #
    # When `lex_dur_multiplier == 0` we simplify the expression to just `col`
    # instead of using `col + lex.dur * 0`. This handles correctly edge cases
    # such as `lex.dur = Inf`.
    #
    # As an aside, of course if you split also by calendar time (and age and
    # whatever you have in your `merge_dt`) then this makes no difference
    # because every split `lexis` record is strictly within each interval of
    # your `merge_dt`. But in practice this does not improve much and can be
    # computationally costly.
    # @codedoc_comment_block popEpi:::lexis_merge_make_harmoniser__::lex_dur_multiplier
    cut_x = if (lex_dur_multiplier != 0L) substitute(
      col + lex.dur * lex_dur_multiplier,
      list(
        col = parse(text = col_nm)[[1]],
        lex_dur_multiplier = lex_dur_multiplier
      )
    ) else parse(text = col_nm)[[1]],
    return_expr = if (is_num) quote(cut_breaks[out]) else quote(out), # nolint
    lex_dur_multiplier = lex_dur_multiplier
  )
  make_harmoniser_eval_env <- environment()
  if ("pre_default_harmoniser_creation" %in% names(optional_steps)) {
    optional_steps[["pre_default_harmoniser_creation"]](
      call_env = call_env,
      eval_env = eval_env,
      make_harmoniser_eval_env = make_harmoniser_eval_env
    )
  }

  # @codedoc_comment_block popEpi:::lexis_merge_make_harmoniser__
  #    + With the `cut` breaks defined, the automatically created harmoniser
  #      becomes a `cut` call with arguments inferred above and with
  #      * `x = col + lex.dur * lex_dur_multiplier`, where `col` is the current
  #        column.
  #      * `dig.lab = 10`.
  #    + Depending on what we are harmonising with, the harmoniser will return
  #      either a `factor` with labels based on the
  #      `cut` call, e.g. `ts_cal = 2021.524` turns into `[2021, 2022[`
  #      with both 2021 and 2022 in `breaks`, or the identified interval's
  #      lower bound such as `2021`.
  # @codedoc_comment_block popEpi:::lexis_merge_make_harmoniser__
  out <- substitute(
    {
      cut_breaks <- breaks # nolint
      cut_labels <- labels # nolint
      out <- cut( # nolint
        x = cut_x, # nolint
        breaks = cut_breaks,
        right = right, # nolint
        labels = cut_labels,
        dig.lab = 10
      )
      return_expr
    },
    substitute_arg_list
  )
  # @codedoc_comment_block popEpi:::lexis_merge_make_harmoniser__
  #   + Run
  #     `optional_steps[["post_default_harmoniser_creation"]](call_env = call_env, eval_env = eval_env, lapply_eval_env = lapply_eval_env)`
  #     if that `optional_steps` element exists.
  # @codedoc_comment_block popEpi:::lexis_merge_make_harmoniser__
  if ("post_default_harmoniser_creation" %in% names(optional_steps)) {
    optional_steps[["post_default_harmoniser_creation"]](
      call_env = call_env,
      eval_env = eval_env,
      make_harmoniser_eval_env = make_harmoniser_eval_env
    )
  }
  return(out)
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
#' # lexis is also a data.table and is modified in-place. no need to keep
#' # output of popEpi::lexis_merge call.
#' popEpi::lexis_merge(
#'   lexis = lexis,
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
#'   lexis = lexis,
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
#' # special factor columns in merge_dt
#' data.table::set(
#'   x = lexis,
#'   j = "merge_value",
#'   value = NULL
#' )
#' data.table::set(
#'   x = my_merge_dt,
#'   j = c("ts_age", "ts_cal"),
#'   value = list(
#'     cut(my_merge_dt[["ts_age"]], breaks = 0:101, right = FALSE),
#'     cut(my_merge_dt[["ts_cal"]], breaks = 2000:2026, right = FALSE,
#'         dig.lab = 4)
#'   )
#' )
#' popEpi::lexis_merge(
#'   lexis = lexis,
#'   merge_dt = my_merge_dt,
#'   merge_dt_by = c("sex", "ts_age", "ts_cal")
#' )
#' stopifnot(
#'   "merge_value" %in% names(lexis),
#'   !is.na(lexis[["merge_value"]])
#' )
#'
lexis_merge <- function(
  lexis,
  merge_dt,
  merge_dt_by,
  merge_dt_harmonisers = NULL,
  subset = NULL,
  optional_steps = NULL,
  lex_dur_multiplier = NULL
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
  #' @param subset `[NULL, logical, integer]` (default `NULL`)
  #'
  #' Merge data only into specific rows in `lexis`.
  #'
  #' - `NULL`: All rows.
  #' - `logical`: Only rows where this is `TRUE`. Must of length `nrow(lexis)`.
  #' - `integer`: Only rows identified by these indices. Every index must be in
  #'   `1:nrow(lexis)`.
  need_to_subset <- !identical(substitute(subset), NULL)
  if (need_to_subset) {
    subset <- handle_arg_subset(dataset_nm = "lexis", output_type = "logical")
    need_to_subset <- !all(subset)
  }

  #' @template param_lexis
  assert_is_arg_lexis(lexis, dt = FALSE)
  #' @param merge_dt `[data.table]` (no default)
  #'
  #' A `data.table` to merge with `lexis`. Usually `lexis` has been split
  #' with e.g. `[splitMulti]`.
  #' @param merge_dt_by `[character]` (no default)
  #'
  #' Names of columns in both `merge_dt` and `lexis` by which `merge_dt` will be
  #' merged with ` lexis`.
  assert_is_arg_merge_dt_and_merge_dt_by(
    merge_dt,
    merge_dt_by,
    lexis,
    mandatory = TRUE
  )

  # @codedoc_comment_block popEpi::lexis_merge::lex_dur_multiplier
  # @param lex_dur_multiplier `[NULL, integer, numeric]` (default `NULL`)
  #
  # - `NULL`: Use `0.5`.
  # - `integer` / `numeric`: Use this is multiplier.
  #
  # @codedoc_insert_comment_block popEpi:::lexis_merge_make_harmoniser__::lex_dur_multiplier
  # @codedoc_comment_block popEpi::lexis_merge::lex_dur_multiplier
  if (is.null(lex_dur_multiplier)) {
    lex_dur_multiplier <- 0.5
  } else {
    stopifnot(
      length(lex_dur_multiplier) == 1,
      !is.na(lex_dur_multiplier),
      inherits(lex_dur_multiplier, c("integer", "numeric", "double"))
    )
  }

  call_env <- parent.frame(1L)
  lexis_ts_col_nms <- attr(lexis, "time.scales")
  merge_ts_col_nms <- intersect(lexis_ts_col_nms, merge_dt_by)
  #' @param merge_dt_harmonisers `[NULL, list]` (default `NULL`)
  #'
  #' Optional list of quoted expressions which, when evaluated, harmonise
  #' data in `lexis` (possibly after splitting) to look the same as the data in
  #' `merge_dt`. For example, if `merge_dt` contains expected hazards by
  #' calendar year `ts_cal` and 1-year age group `ts_age`, and if `lexis` has
  #' been
  #' split into monthly survival intervals, then we must somehow find the
  #' correct row in `merge_dt` for each row in `lexis`. E.g.
  #' `ts_cal = 2010.5323`
  #' and `ts_age = 76.4435` need to be harmonised into values found in
  #' `merge_dt` such as `ts_cal = 2010` and `ts_age = 76`.
  #'
  #' - `NULL`: This function comes up with reasonable harmonisers if possible.
  #'   See **Details**.
  #' - `list`: Each element must a quoted expression (`[quote]`) and each
  #'   element must have a name corresponding to a column name in both `lexis`
  #'   and `merge_dt`. See **Examples**.
  if (is.null(merge_dt_harmonisers)) {
    # @codedoc_comment_block popEpi::lexis_merge
    # - If `is.null(merge_dt_harmonisers)`, attempt to
    #   automatically determine the harmonisers making use of `cut` for each
    #   time scale column to merge by as follows:
    # @codedoc_insert_comment_block popEpi:::lexis_merge_make_harmoniser__
    # @codedoc_comment_block popEpi::lexis_merge
    merge_dt_harmonisers <- lapply(
      merge_ts_col_nms,
      lexis_merge_make_harmoniser__,
      merge_dt = merge_dt,
      optional_steps = optional_steps,
      call_env = call_env,
      eval_env = eval_env,
      lex_dur_multiplier = lex_dur_multiplier
    )
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
  #   `data.table` with harmonised data from `lexis`. This is performed via
  #   `eval` with `envir = lexis` and `enclos = harmoniser_eval_env` where
  #   `harmoniser_eval_env` is a temporary environment which contains every
  #   argument passed to `lexis_merge` as well as `eval_env` and `call_env`
  #   which you can read about in the detailed explanation of `optional_steps`.
  #   `lexis` is a subset if `!is.null(subset)`.
  #   However, if a column
  #   has no harmoniser at this point then it is used as-is. For instance there
  #   is no need to harmonise stratifying columns such as area because they are
  #   not changed by splitting the `Lexis` data.
  # @codedoc_comment_block popEpi::lexis_merge
  harmoniser_eval_env <- new.env(parent = call_env)
  lapply(names(formals(lexis_merge)), function(arg_nm) {
    harmoniser_eval_env[[arg_nm]] <- eval_env[[arg_nm]]
  })
  harmoniser_eval_env[["eval_env"]] <- eval_env
  harmoniser_eval_env[["call_env"]] <- call_env
  join_dt <- data.table::setDT(lapply(merge_dt_by, function(col_nm) {
    if (col_nm %in% names(merge_dt_harmonisers)) {
      expr <- merge_dt_harmonisers[[col_nm]]
      lexis_dt <- data.table::setDT(as.list(lexis)[
        intersect(names(lexis), all.vars(expr))
      ])
      if (!all(subset)) {
        lexis_dt <- lexis_dt[(subset), ]
      }
      out <- eval(
        expr,
        envir = lexis_dt,
        enclos = harmoniser_eval_env
      )
    } else {
      out <- lexis[[col_nm]]
      if (need_to_subset) {
        out <- out[subset]
      }
    }
    out
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
  #   data. The value columns in `merge_dt` are added into `lexis` either
  #   using `data.table::set` for a `data.table` or a simple
  #   `lexis[, merge_value_col_nms] <- join_result` assignment.
  #   Note that `data.table::set` modifies the `lexis` object in-place.
  # @codedoc_comment_block popEpi::lexis_merge
  join_result <- merge_dt[
    i = join_dt,
    on = names(join_dt),
    #' @importFrom data.table .SD
    j = .SD,
    .SDcols = merge_value_col_nms
  ]
  if (data.table::is.data.table(lexis)) {
    data.table::set(
      x = lexis,
      i = if (need_to_subset) which(subset) else NULL,
      j = merge_value_col_nms,
      value = join_result
    )
  } else {
    lexis[subset, merge_value_col_nms] <- join_result
  }
  rm(list = "join_result")
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
  #   `lexis`. For instance it only covers years 1950-2020 but `lexis` contains
  #   also data for 2021. This error helps you to spot those problems early
  #   isntead of producing nonsense results downstream.
  # @codedoc_comment_block popEpi::lexis_merge
  for (merge_value_col_nm in merge_value_col_nms) {
    is_missing <- is.na(lexis[[merge_value_col_nm]])
    if (need_to_subset) {
      is_missing[!subset] <- FALSE
    }
    if (any(is_missing)) {
      print(data.table::setDT(lexis[is_missing, ]))
      print(data.table::setDT(join_dt[is_missing, ]))
      stop("Merging `merge_dt` into split (subset of) `lexis` produced NA ",
           "values in column `lexis$", merge_value_col_nm, "`. ",
           "See the `lexis` and its harmonised form printed above.")
    }
  }
  # @codedoc_comment_block popEpi::lexis_merge
  # - Returns `lexis` after adding value columns from
  #   `merge_dt` into `lexis`.
  # @codedoc_comment_block popEpi::lexis_merge
  # @codedoc_comment_block return(popEpi::lexis_merge)
  # Returns `lexis` after adding value columns from
  # `merge_dt` into `lexis`.
  # @codedoc_comment_block return(popEpi::lexis_merge)
  return(lexis[])
}
