
assert_is_arg_merge <- function(
  merge,
  dt
) {
  stopifnot(
    isTRUE(merge) || isFALSE(merge) || all(merge %in% names(dt))
  )
}

handle_arg_merge <- function(
  merge,
  dt
) {
  assert_is_arg_merge(merge, dt)
  if (isTRUE(merge)) {
    merge <- setdiff(
      names(dt),
      c("lex.id", "lex.dur", "lex.Cst", "lex.Xst", attr(dt, "time.scales"))
    )
  } else if (isFALSE(merge)) {
    merge <- character(0L)
  }
  return(merge)
}

assert_is_arg_dt <- function(dt, lexis = FALSE) {
  stopifnot(
    data.table::is.data.table(dt)
  )
  if (lexis) {
    stopifnot(inherits(dt, "Lexis"))
  }
}

assert_is_arg_weights <- function(
  weights,
  dt = NULL,
  allowed = c("NULL", "character", "data.table")
) {
  # @codedoc_comment_block popEpi:::assert_is_arg_weights
  # A table of weights must fulfill these requirements:
  # - Is a `data.table`.
  # - Has at least one stratifying column. No duplicate strata are permitted,
  #   e.g. the same age group twice in a table stratified by age group only.
  # - Has column `"weight"`. All values must be >= 0. No missing values are
  #   allowed.
  # @codedoc_comment_block popEpi:::assert_is_arg_weights
  stopifnot(
    inherits(weights, allowed)
  )
  if (data.table::is.data.table(weights)) {
    stopifnot(
      ncol(weights) >= 2,
      "weight" %in% names(weights),
      !duplicated(weights, by = setdiff(names(weights), "weight")),

      !is.na(weights[["weight"]]),
      weights[["weight"]] >= 0.0
    )
    if (!is.null(dt)) {
      stopifnot(
        setdiff(names(weights), "weight") %in% names(dt)
      )
    }
  } else if (is.character(weights)) {
    stopifnot(
      weights %in% names(dt),
      !is.na(dt[[weights]]),
      dt[[weights]] >= 0
    )
  }
}

assert_is_arg_merge_dt_and_merge_dt_by <- function(
  merge_dt,
  merge_dt_by,
  dt,
  mandatory = TRUE
) {
  if (mandatory) {
    stopifnot(
      inherits(merge_dt, "data.table"),
      inherits(merge_dt_by, "character")
    )
  } else {
    stopifnot(
      inherits(merge_dt, c("NULL", "data.table")),
      inherits(merge_dt_by, c("NULL", "character"))
    )
  }
  if (inherits(merge_dt_by, "character")) {
    stopifnot(
      merge_dt_by %in% names(dt),
      merge_dt_by %in% names(merge_dt)
    )
  }
}

assert_is_arg_breaks <- function(breaks, dt) {
  stopifnot(
    inherits(breaks, "list"),
    names(breaks) %in% attr(dt, "time.scales")
  )
}

assert_is_arg_aggre_expr <- function(aggre_expr) {
  stopifnot(
    inherits(aggre_expr, c("{", "call", "name"))
  )
}

assert_is_arg_box_dt <- function(
  box_dt,
  ts_col_nms = NULL
) {
  stopifnot(
    inherits(box_dt, "data.table"),
    "box_id" %in% names(box_dt)
  )
  if (!is.null(ts_col_nms)) {
    for (ts_col_nm in ts_col_nms) {
      eval(substitute(stopifnot(
        paste0(ts_col_nm, "_id") %in% names(box_dt),
        paste0(ts_col_nm, "_start") %in% names(box_dt),
        paste0(ts_col_nm, "_stop") %in% names(box_dt)
      ), list(ts_col_nm = ts_col_nm)))
    }
  }
}

box_dt_col_nms__ <- function(ts_col_nms) {
  c(
    "box_id",
    paste0(ts_col_nms, "_id")
  )
}

handle_arg_estimators <- function(estimators) {
  stopifnot(
    # @codedoc_comment_block popEpi::surv_estimate::estimators
    # @param estimators `[character, list]`
    # (default `"hazard_observed_survival"`)
    #
    # One or more names of estimators whose estimates will be computed into
    # `dt`.
    #
    # - `character`: Causes formulae defined internally into `popEpi` to be
    #   used to compute the estimates and their standard errors. For available
    #   options see **Details**.
    # @codedoc_comment_block popEpi::surv_estimate::estimators
    is.character(estimators) || inherits(estimators, "list"),
    vapply(estimators, inherits, logical(1L), what = c("character", "list"))
  )
  if (inherits(estimators, "list")) {
    for (i in seq_along(estimators)) {
      if (!is.character(estimators[[i]])) {
        eval(substitute(stopifnot(
          c("estimate", "standard_error") %in% names(estimators[[i]]),
          is.language(estimators[[i]][["estimate"]]),
          is.language(estimators[[i]][["standard_error"]])
        ), list(i = i)))
      }
    }
  }
  expressions <- lapply(seq_along(estimators), function(i) {
    if (is.character(estimators[[i]])) {
      out <- surv_estimate_expression__(estimators[[i]])
    } else {
      out <- estimators[i]
    }
    out
  })
  expressions <- unlist(expressions, recursive = FALSE)

  return(expressions)
}