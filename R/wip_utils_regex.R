regex_extract_first__ <- function(pattern, text, ...) {
  arg_list <- list(...)
  arg_list[["pattern"]] <- pattern
  arg_list[["text"]] <- text
  m <- do.call(regexpr, arg_list)
  out <- rep(NA_character_, length(arg_list[["text"]]))
  has_match <- !m %in% c(-1L, NA_integer_)
  out[has_match] <- regmatches(x = arg_list[["text"]], m = m)
  out
}

regex_extract_groups__ <- function(pattern, text, ...) {
  matches <- regexpr(pattern, text, perl = TRUE)
  starts <- attr(matches, "capture.start")
  stops <- starts + attr(matches, "capture.length") - 1L
  substrings <- vapply(
    colnames(starts),
    function(grp_nm) {
      substr(text, starts[, grp_nm], stops[, grp_nm])
    },
    character(nrow(starts))
  )
  if (nrow(starts) == 1) {
    substrings_matrix <- matrix(substrings, nrow = 1L)
    colnames(substrings_matrix) <- names(substrings)
    substrings <- substrings_matrix
  }
  return(substrings)
}