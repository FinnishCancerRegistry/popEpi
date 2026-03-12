.onLoad <- function(...) {
  opt <- getOption("popEpi.datatable")
  if (is.null(opt)) {
    options("popEpi.datatable" = TRUE)
  }
  # rm_fun_nms <- c("ltable", "expr.by.cj")
  # packageStartupMessage(
  #   "popEpi: ",
  #   "The following functions have been marked for removal in popEpi 1.0.0 ",
  #   "to be released in June 2027: ",
  #   paste0(rm_fun_nms, collapse = ", "),
  #   ". Some of them have replacements --- see the help page of the function ",
  #   "in question for whether they have a replacement."
  # )
  invisible(NULL)
}
