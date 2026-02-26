
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

assert_is_arg_weight_col_nm <- function(
  weight_col_nm,
  dt = NULL
) {
  stopifnot(
    is.null(weight_col_nm) || is.character(weight_col_nm)
  )
  if (is.character(weight_col_nm)) {
    stopifnot(
      length(weight_col_nm) == 1
    )
    if (!is.null(dt)) {
      stopifnot(
        weight_col_nm %in% names(dt),
        !is.na(dt[[weight_col_nm]]),
        dt[[weight_col_nm]] >= 0
      )
    }
  }
}

assert_is_arg_weights <- function(
  weights,
  dt = NULL,
  allowed = c("NULL", "character", "data.table")
) {
  stopifnot(
    inherits(weights, allowed)
  )
  if (data.table::is.data.table(weights)) {
    assert_is_arg_weight_dt(
      weight_dt = weights,
      dt = dt,
      allowed = "data.table"
    )
  } else if (is.character(weights)) {
    stopifnot(
      weights %in% names(dt),
      !is.na(dt[[weights]]),
      dt[[weights]] >= 0
    )
  }
}

assert_is_arg_weight_dt <- function(
  weight_dt,
  dt = NULL,
  allowed = c("NULL", "data.table")
) {
  # @codedoc_comment_block popEpi:::assert_is_arg_weight_dt
  # A table of weights must fulfill these requirements:
  # - Is a `data.table`.
  # - Has at least one stratifying column. No duplicate strata are permitted,
  #   e.g. the same age group twice in a table stratified by age group only.
  # - Has column `"weight"`. All values must be >= 0. No missing values are
  #   allowed.
  # @codedoc_comment_block popEpi:::assert_is_arg_weight_dt
  eval(substitute(stopifnot(
    inherits(weight_dt, allowed)
  ), list(allowed = allowed)))
  if (data.table::is.data.table(weight_dt)) {
    stopifnot(
      ncol(weight_dt) >= 2,
      "weight" %in% names(weight_dt),
      !duplicated(weight_dt, by = setdiff(names(weight_dt), "weight")),

      !is.na(weight_dt[["weight"]]),
      weight_dt[["weight"]] >= 0.0
    )
    if (!is.null(dt)) {
      stopifnot(
        setdiff(names(weight_dt), "weight") %in% names(dt)
      )
    }
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
    inherits(aggre_exprs, c("list", "character"))
  )
  for (i in seq_along(aggre_exprs)) {
    eval(substitute(stopifnot(
      is.character(aggre_exprs[[i]]) || is.language(aggre_exprs[[i]])
    ), list(i = i)))
    if (is.character(aggre_exprs[[i]])) {
      if (
        !aggre_exprs[[i]] %in% names(SURV_AGGRE_EXPRS__) &&
          !grepl("\\[.+, *.+\\]", aggre_exprs[[i]])
      ) {
        stop(
          "`aggre_exprs[[", i, "]] = ", deparse1(aggre_exprs[[i]]),
          "` was not identified as the ",
          "name of an aggregation expression. Known names: ",
          deparse1(names(SURV_AGGRE_EXPRS__))
        )
      }
    } else {
      eval(substitute(stopifnot(
        !is.null(names(aggre_exprs)),
        names(aggre_exprs)[i] != ""
      ), list(i = i)))
    }
  }
}
handle_arg_aggre_exprs <- function(
  aggre_exprs,
  weight_col_nm = NULL
) {
  assert_is_arg_aggre_exprs(aggre_exprs)
  assert_is_arg_weight_col_nm(weight_col_nm)
  aggre_exprs <- as.list(aggre_exprs)
  wh_is_string <- which(vapply(aggre_exprs, is.character, logical(1L)))
  names(aggre_exprs)[wh_is_string] <- unlist(aggre_exprs[wh_is_string])
  if (is.null(weight_col_nm)) {
    iw_replacement <- ""
  } else {
    iw_replacement <- sprintf("* %s", weight_col_nm)
  }
  aggre_exprs <- lapply(aggre_exprs, function(aggre_expr) {
    if (is.character(aggre_expr)) {
      user_var_nm <- aggre_expr
      state_from <- regex_extract_first__(
        "(?<=[[]).+(?=[,])",
        user_var_nm,
        perl = TRUE
      )
      if (!is.na(state_from)) {
        state_from <- eval(parse(text = state_from))
        if (is.numeric(state_from)) {
          state_from <- as.integer(state_from)
        }
      }
      state_to <- regex_extract_first__(
        "(?<=[,]).+(?=[]]$)",
        user_var_nm,
        perl = TRUE
      )
      if (!is.na(state_to)) {
        state_to <- eval(parse(text = state_to))
        if (is.numeric(state_to)) {
          state_to <- as.integer(state_to)
        }
      }
      # @codedoc_comment_block popEpi:::handle_arg_aggre_exprs
      #   + An `aggre_exprs` element that is string such as `"n_events"`
      #     is replaced with the appropriate expression retrieved from a table
      #     of pre-defined expressions (see below).
      #   + If a string element is transition-specific, e.g. `n_events_[0, 1]`,
      #     we first turn it into its general form (e.g. `n_events_[x, y]`).
      #     Additionally, any uses of `x` and `y` in the fetched expression
      #     is replaced with the correct states, e.g.
      #     `sum(lex.Cst == x & lex.Xst == y)` is turned into
      #     `sum(lex.Cst == 0 & lex.Xst == 1)`.
      # @codedoc_comment_block popEpi:::handle_arg_aggre_exprs
      general_var_nm <- sub(
        "\\[.+, .+\\]$",
        "[x, y]",
        user_var_nm
      )
      if (!general_var_nm %in% names(SURV_AGGRE_EXPRS__)) {
        stop(
          deparse1(user_var_nm), " not recognised as the name of one of the ",
          "pre-defined aggregation expressions."
        )
      }
      general_var_nm <- general_var_nm
      aggre_expr <- SURV_AGGRE_EXPRS__[[general_var_nm]]
      aggre_expr_string <- deparse1(aggre_expr)
      aggre_expr_string <- sub(
        "(?<=\\W)x(?=\\W)",
        sprintf(" %s ", deparse1(state_from)),
        aggre_expr_string,
        perl = TRUE
      )
      aggre_expr_string <- sub(
        "(?<=\\W)y(?=\\W)",
        sprintf(" %s ", deparse1(state_to)),
        aggre_expr_string,
        perl = TRUE
      )
    } else {
      # @codedoc_comment_block popEpi:::handle_arg_aggre_exprs
      #   + An `aggre_exprs` element that is an expression already
      #     (e.g. `my_n_events = quote(sum(lex.Cst == 0 & lex.Xst != 0))`)
      #     does not receive the same treatment regarding the states and when
      #     you write your own expressions you must specify them yourself.
      #     Therefore e.g.
      #     `my_n_events = quote(sum(lex.Cst == x & lex.Xst != y))` will not
      #     work.
      # @codedoc_comment_block popEpi:::handle_arg_aggre_exprs
      aggre_expr_string <- deparse1(aggre_expr)
    }
    # @codedoc_comment_block popEpi:::handle_arg_aggre_exprs
    #   + Regardless of the type of the `aggre_exprs` element, we remove every
    #     use of ` * iw` in the expression if individual weights are not used.
    # @codedoc_comment_block popEpi:::handle_arg_aggre_exprs
    aggre_expr_string <- gsub(
      " *[*] *iw",
      iw_replacement,
      aggre_expr_string
    )
    parse(text = aggre_expr_string)[[1]]
  })
  # @codedoc_comment_block popEpi:::handle_arg_aggre_exprs
  #   + Table of general aggregation expressions known to `popEpi`:
  #
  # ${paste0(knitr::kable(popEpi:::surv_aggre_exprs_table__()), collapse = "\n")}
  # @codedoc_comment_block popEpi:::handle_arg_aggre_exprs
  return(aggre_exprs)
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
  estimator_dt[["transition_string"]] <- regex_extract_first__(
    "\\[.+, *.+\\]",
    estimator_dt[["user_estimator_name"]],
    perl = TRUE
  )
  estimator_dt[["state_from"]] <- gsub(
    "(\\[)|(\\])|(,.+)",
    "",
    estimator_dt[["transition_string"]],
    perl = TRUE
  )
  estimator_dt[["state_to"]] <- gsub(
    "(\\])|(^.*, *)",
    "",
    estimator_dt[["transition_string"]],
    perl = TRUE
  )
  estimator_dt[["general_estimator_name"]] <- data.table::fifelse(
    is.na(estimator_dt[["state_from"]]),
    estimator_dt[["user_estimator_name"]],
    sprintf(
      "%s[x, y]",
      sub(
        "\\[.+, *.+\\]$",
        "",
        estimator_dt[["user_estimator_name"]]
      )
    )
  )
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
          estimator_dt[["transition_string"]][i],
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

assert_is_arg_split_lexis_column_exprs <- function(split_lexis_column_exprs) {
  stopifnot(
    inherits(split_lexis_column_exprs, c("list", "NULL"))
  )
  if (inherits(split_lexis_column_exprs, "list")) {
    for (i in seq_along(split_lexis_column_exprs)) {
      if (!is.language(split_lexis_column_exprs[[i]])) {
        stop("`split_lexis_column_exprs[[", i, "]]` was not an R expression ",
             "but instead had class(es) ",
             deparse1(class(split_lexis_column_exprs[[i]])))
      }
    }
  }
}
