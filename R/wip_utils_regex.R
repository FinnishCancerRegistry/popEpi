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
