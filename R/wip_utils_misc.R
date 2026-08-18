if_else__ <- function(
  test,
  yes,
  no
) {
  # lazy man's if (test) yes else no
  stopifnot(
    is.logical(test),
    length(test) == 1,
    !is.na(test)
  )
  if (test) {
    return(yes)
  } else {
    return(no)
  }
}
