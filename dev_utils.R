




test_as_cran <- function(...)  {
  ## runs only the tests that CRAN will run
  old <- Sys.getenv("NOT_CRAN")
  on.exit(Sys.setenv("NOT_CRAN" = old))
  Sys.setenv("NOT_CRAN" = "false")
  devtools::test(...)
}





test_all <- function(...)  {
  ## runs all possible tests; only with options("popEpi.datatable" = TRUE)
  old <- Sys.getenv("NOT_CRAN")
  on.exit(Sys.setenv("NOT_CRAN" = old))
  Sys.setenv("NOT_CRAN" = "true")
  devtools::test(...)
}





test_as_travis <- function(...) {
  ## runs all tests that Travis runs; 
  ## only with options("popEpi.datatable" = TRUE);
  oldCR <- Sys.getenv("NOT_CRAN")
  oldTR <- Sys.getenv("TRAVIS")
  on.exit({
    Sys.setenv("NOT_CRAN" = oldCR)
    Sys.setenv("TRAVIS" = oldTR)
    })
  Sys.setenv("NOT_CRAN" = "true")
  Sys.setenv("TRAVIS" = "true")
  devtools::test(...)
}





test_all_datatable <- function(...) {
  ## runs all possible tests with both TRUE/FALSE for 
  ## options("popEpi.datatable")
  old <- Sys.getenv("NOT_CRAN")
  on.exit(Sys.setenv("NOT_CRAN" = old))
  
  Sys.setenv("NOT_CRAN" = "true")
  options("popEpi.datatable" = TRUE)
  cat("Starting checking with popEpi.datatable = TRUE \n")
  devtools::test(...)
  
  cat("Finished checking with popEpi.datatable = TRUE;",
      "starting with popEpi.datatable = FALSE \n")
  options("popEpi.datatable" = FALSE)
  devtools::test(...)
  
  cat("Finished checking with popEpi.datatable = FALSE \n")
  
}





check_bare <- function(...) {
  ## runs R CMD CHECK without running any tests
  devtools::check(..., args = c("--no-tests", "--timings"))
}





check_on_rhub <- function(platforms = NULL, show_status = FALSE, ...) {
  requireNamespace("rhub") ## 1.0.1 on github only
  if (is.null(platforms)) {
    platforms <- rhub::platforms()$name
  }
  rhub::check(platform = platforms, show_status = show_status, ...)
}










