dt_join_assign <- function(
  x,
  i,
  on,
  x_col_nms,
  i_col_nms = x_col_nms
) {
  stopifnot(
    inherits(x, "data.table"),
    inherits(i, "data.table"),
    is.character(on),
    is.character(x_col_nms),
    is.character(i_col_nms),
    i_col_nms %in% names(i)
  )
  expr <- substitute(
    x[
      i = i,
      on = on,
      j = (j_lhs) := j_rhs
    ],
    list(
      on = on,
      j_lhs = x_col_nms,
      j_rhs = parse(
        text = paste0(
          "list(",
          paste0("i.", i_col_nms, collapse = ", "),
          ")"
        )
      )[[1]]
    )
  )
  eval(expr)
  return(invisible(x[]))
}

dt_independent_frame_dependent_contents <- function(
  dt,
  select = NULL
) {
  # this funtion creates a new data.table without copying any of the underlying
  # data. so for instance adding a new column into the output of this function
  # does not influence the original data.table, but modifying (a subset of)
  # a column does modify the data in the original object as well.
  stopifnot(
    is.data.frame(dt),
    inherits(select, c("NULL", "character"))
  )
  if (is.null(select)) {
    select <- names(dt)
  }
  dt <- data.table::setDT(as.list(dt)[select])
  return(dt[])
}
