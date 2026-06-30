if (requireNamespace("testthat")) {
  library("testthat")
  library("popEpi")
  library("data.table")

  # this data.table::setDTthreads call is included here only to
  # conform to the CRAN requirement at submission to only use at most 2
  # threads. you do not need to set this to use popEpi.
  # however some long calculations may benefit from using more threads.
  data.table::setDTthreads(2L)

  testthat::test_check("popEpi")
}
