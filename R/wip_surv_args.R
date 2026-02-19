
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
    stopifnot(
      inherits(dt, "Lexis"),
      inherits(dt[["lex.Cst"]], c("logical", "integer", "factor")),
      inherits(dt[["lex.Xst"]], c("logical", "integer", "factor")),
      identical(class(dt[["lex.Cst"]]), class(dt[["lex.Xst"]])),
      !is.na(dt[["lex.Cst"]]),
      !is.na(dt[["lex.Xst"]]),
      ifelse(
        is.factor(dt[["lex.Cst"]]),
        identical(
          levels(dt[["lex.Cst"]]),
          levels(dt[["lex.Xst"]])
        ),
        TRUE
      ),
      ifelse(
        is.integer(dt[["lex.Cst"]]),
        all(dt[["lex.Cst"]] >= 0 & dt[["lex.Xst"]] >= 0),
        TRUE
      )
    )
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
  # @codedoc_comment_block popEpi:::assert_is_arg_breaks
  # @param breaks `[list]` (no default)
  #
  # List of breaks to split `Lexis` data by. Passed to `[splitMulti]`.
  # E.g. `list(ts_fut = seq(0, 5, 1 / 12), ts_cal = c(2001, 2006))`.
  # @codedoc_comment_block popEpi:::assert_is_arg_breaks
  stopifnot(
    inherits(breaks, "list"),
    names(breaks) %in% attr(dt, "time.scales")
  )
}

assert_is_arg_aggre_exprs <- function(aggre_exprs) {
  stopifnot(
    inherits(aggre_exprs, "list"),
    data.table::uniqueN(names(aggre_exprs)) == length(aggre_exprs),
    nchar(names(aggre_exprs)) > 0
  )
  for (i in seq_along(aggre_exprs)) {
    eval(substitute(stopifnot(
      is.character(aggre_exprs[[i]]) || is.language(aggre_exprs[[i]])
    ), list(i = i)))
  }
}
handle_arg_aggre_exprs <- function(aggre_exprs) {
  assert_is_arg_aggre_exprs(aggre_exprs)
  stop("TODO")
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

handle_arg_estimators <- function(estimators, dt) {
  stopifnot(
    # @codedoc_comment_block popEpi::surv_estimate::estimators
    # @param estimators `[character, list]`
    # (default `"S_pch"`)
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
          c("est", "se") %in% names(estimators[[i]]),
          is.language(estimators[[i]][["est"]]),
          is.language(estimators[[i]][["se"]])
        ), list(i = i)))
      }
    }
  }
  estimator_dt <- data.table::setDF(data.table::setDT(list(
    estimator = as.list(estimators),
    user_estimator_name = vapply(seq_along(estimators), function(i) {
      if (is.character(estimators[[i]])) {
        estimators[[i]]
      } else {
        names(estimators)[i]
      }
    }, character(1L))
  )))
  estimator_dt[["state_from"]] <- unlist(lapply(
    seq_len(nrow(estimator_dt)),
    function(i) {
      if (!is.character(estimator_dt[["estimator"]][[i]])) {
        return(NA)
      }
      state_from <- regex_extract_first__(
        "(?<=[[]).+(?=[,])",
        estimator_dt[["user_estimator_name"]][i],
        perl = TRUE
      )
      if (!is.na(state_from)) {
        state_from <- eval(parse(text = state_from))
      }
      if (is.numeric(state_from)) {
        state_from <- as.integer(state_from)
      }
      return(state_from)
    }
  ))
  estimator_dt[["state_to"]] <- unlist(lapply(
    seq_len(nrow(estimator_dt)),
    function(i) {
      if (!is.character(estimator_dt[["estimator"]][[i]])) {
        return(NA)
      }
      state_to <- regex_extract_first__(
        "(?<=[,]).+(?=[]]$)",
        estimator_dt[["user_estimator_name"]][i],
        perl = TRUE
      )
      if (!is.na(state_to)) {
        state_to <- eval(parse(text = state_to))
      }
      if (is.numeric(state_to)) {
        state_to <- as.integer(state_to)
      }
      return(state_to)
    }
  ))
  estimator_dt[["general_estimator_name"]] <- data.table::fifelse(
    is.na(estimator_dt[["state_from"]]),
    estimator_dt[["user_estimator_name"]],
    paste0(
      sub(
        "[[].+[]]$",
        "",
        estimator_dt[["user_estimator_name"]]
      ),
      "[x, y]"
    )
  )
  if (is.numeric(estimator_dt[["state_to"]])) {
    estimator_dt[["state_from"]] <- as.integer(estimator_dt[["state_from"]])
    estimator_dt[["state_from"]][is.na(estimator_dt[["state_from"]])] <- 0L
    estimator_dt[["state_to"]] <- as.integer(estimator_dt[["state_to"]])
  } else if (is.logical(estimator_dt[["state_to"]])) {
    estimator_dt[["state_from"]] <- FALSE
    estimator_dt[["state_to"]] <- TRUE
  }
  estimator_dt[["expression_set"]] <- unlist(lapply(
    seq_len(nrow(estimator_dt)),
    function(i) {
      if (is.character(estimator_dt[["estimator"]][[i]])) {
        out <- surv_estimate_expression__(
          estimator_dt[["general_estimator_name"]][[i]]
        )
      } else {
        out <- estimator_dt[["estimator"]][i]
      }
      out
    }
  ), recursive = FALSE)
  estimator_dt[["expression_set"]] <- data.table::fifelse(
    is.na(estimator_dt[["state_from"]]),
    estimator_dt[["expression_set"]],
    lapply(seq_len(nrow(estimator_dt)), function(i) {
      expr_set <- estimator_dt[["expression_set"]][[i]]
      lapply(expr_set, function(expr) {
        expr_lines <- deparse(expr)
        expr_lines <- gsub(
          "[x, y]",
          sprintf(
            "[%s, %s]",
            deparse1(estimator_dt[["state_from"]][i]),
            deparse1(estimator_dt[["state_to"]][i])
          ),
          expr_lines,
          fixed = TRUE
        )
        parse(text = paste0(expr_lines, collapse = "\n"))[[1]]
      })
    })
  )
  data.table::setDT(estimator_dt)
  return(estimator_dt[])
}
