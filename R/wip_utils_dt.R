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
    is.character(i_col_nms), i_col_nms %in% names(i)
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
      j_rhs = parse(text = paste0(
        "list(",
        paste0("i.", i_col_nms, collapse = ", "),
        ")"
      ))[[1]]
    )
  )
  eval(expr)
  return(invisible(x[]))
}