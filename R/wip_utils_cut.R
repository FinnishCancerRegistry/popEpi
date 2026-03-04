infer_cut_args__ <- function(x) {
  if (!inherits(x, c("factor", "character"))) {
    return(NULL)
  }
  # @codedoc_comment_block popEpi:::infer_cut_args__
  #     look at the unique values / `levels()` of the column. If they all look
  #     like `[x, y[`, `]x, y]`, or `x - y`, extract breaks from them and
  #     infer whether `right = FALSE` or `TRUE`.
  # @codedoc_comment_block popEpi:::infer_cut_args__
  out <- data.table::data.table(
    level = if (is.factor(x)) levels(x) else sort(unique(x))
  )
  clean_levels <- sub(
    "(?<=[0-9])-(?=[0-9])", " - ",
    out[["level"]],
    perl = TRUE
  )
  re_number <- "-?(?:[0-9]+[.]?[0-9]*)(?:[eE][+-]?[0-9]+)?"
  re_left_number <- paste0("(?<left_number>", re_number, ")")
  re_right_number <- paste0("(?<right_number>", re_number, ")")
  re_left_inc <- "(?<left_inc>[\\[\\](] *)?"
  re_right_inc <- "(?<right_inc> *[\\[\\])])?"
  re_sep <- "(?<sep> *[,-] *)"
  re_total <- paste0(
    "^",
    re_left_inc,
    re_left_number,
    re_sep,
    re_right_number,
    re_right_inc,
    "$"
  )
  is_valid <- local({
    matches <- regexpr(re_total, clean_levels, perl = TRUE)
    starts <- attr(matches, "capture.start")
    stops <- starts + attr(matches, "capture.length") - 1L
    substrings <- vapply(
      colnames(starts),
      function(grp_nm) {
        substr(clean_levels, starts[, grp_nm], stops[, grp_nm])
      },
      character(nrow(starts))
    )
    data.table::set(
      x = out,
      j = colnames(substrings),
      value = lapply(colnames(substrings), function(col_nm) {
        substrings[, col_nm]
      })
    )
    substrings[, "left_number"] != ""
  })
  if (!all(is_valid)) {
    return(NULL)
  }
  data.table::set(
    x = out,
    j = c("is_left_inclusive", "is_right_inclusive"),
    value = list(
      !grepl("^ *[\\]()]", out[["left_inc"]]),
      !grepl("[\\[()] *$", out[["right_inc"]])
    )
  )
  if (
    data.table::uniqueN(out[["is_left_inclusive"]]) > 1 ||
      data.table::uniqueN(out[["is_right_inclusive"]]) > 1
  ) {
    return(NULL)
  }
  is_valid <- out[["is_left_inclusive"]] | out[["is_right_inclusive"]]
  if (!all(is_valid)) {
    return(NULL)
  }
  data.table::set(
    x = out,
    j = c("lo", "hi"),
    value = lapply(c("left_number", "right_number"), function(col_nm) {
      col <- vapply(
        out[[col_nm]],
        function(col_elem) {
          eval(parse(text = col_elem))
        },
        numeric(1L)
      )
      if (all(col %% 1 == 0)) {
        col <- as.integer(col)
      }
      col
    })
  )
  data.table::setkeyv(out, c("lo", "hi"))
  is_integer_range <- is.integer(out[["lo"]]) && is.integer(out[["hi"]]) &
    out[["is_left_inclusive"]] & out[["is_right_inclusive"]] &
    grepl("-", out[["sep"]])
  if (all(is_integer_range)) {
    cut_arg_list <- list(
      breaks = c(out[["lo"]][1], out[["hi"]] + 1L),
      right = FALSE
    )
  } else {
    if (all(out[["is_right_inclusive"]])) {
      cut_arg_list <- list(
        breaks = c(out[["lo"]][1], out[["hi"]]),
        right = TRUE
      )
    } else {
      cut_arg_list <- list(
        breaks = c(out[["lo"]], out[["hi"]][nrow(out)]),
        right = FALSE
      )
    }
  }
  return(list(
    meta_dt = out[],
    cut_arg_list =  cut_arg_list
  ))
}
