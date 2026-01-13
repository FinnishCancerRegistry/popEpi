
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
    merge <- setdiff(names(dt), c(names(work_dt), names(ts_dt)))
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
  dt
) {
  stopifnot(
    inherits(weights, c("NULL", "data.table", "character"))
  )
  if (is.character(weights)) {
    stopifnot(
      weights %in% names(dt)
    )
  } else if (inherits(weights, "data.table")) {
    assert_is_weight_dt(weights, dt)
  }
}

assert_is_weight_dt <- function(
  weight_dt,
  dt = NULL
) {
  stopifnot(
    inherits(weight_dt, "data.table"),
    "weight" %in% names(weight_dt),
    weight_dt[["weight"]] >= 0.0
  )
  if (!is.null(dt)) {
    stopifnot(
      setdiff(names(weight_dt), "weight") %in% names(dt)
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
