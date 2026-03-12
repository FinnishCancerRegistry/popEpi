get_internal_dataset <- function(nm) {
  pkg_env <- environment(get_internal_dataset)
  if (!nm %in% ls(pkg_env)) {
    stop("Internal error: no such dataset: ", nm)
  }
  out <- pkg_env[[nm]]
  if (is.function(out)) {
    stop("Internal error: dataset named ", nm, " was a function")
  }
  return(out)
}
