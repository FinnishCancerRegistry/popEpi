

check_all <- function() {
  ## runs R CMD CHECK with all possible tests; only with options("popEpi.datatable" = TRUE)
  old <- Sys.getenv("NOT_CRAN")
  on.exit(Sys.setenv("NOT_CRAN" = old))
  Sys.setenv("NOT_CRAN" = "true")
  devtools::check(".")
}

check_some <- function() {
  ## runs R CMD CHECK with only the tests that CRAN will run; only with options("popEpi.datatable" = TRUE)
  old <- Sys.getenv("NOT_CRAN")
  on.exit(Sys.setenv("NOT_CRAN" = old))
  Sys.setenv("NOT_CRAN" = "false")
  devtools::check(".")
  
}

test_some <- function()  {
  ## runs only the tests that CRAN will run
  old <- Sys.getenv("NOT_CRAN")
  on.exit(Sys.setenv("NOT_CRAN" = old))
  Sys.setenv("NOT_CRAN" = "false")
  devtools::test(".")
}

test_all <- function()  {
  ## runs all possible tests; only with options("popEpi.datatable" = TRUE)
  old <- Sys.getenv("NOT_CRAN")
  on.exit(Sys.setenv("NOT_CRAN" = old))
  Sys.setenv("NOT_CRAN" = "true")
  devtools::test(".")
}

test_full <- function() {
  ## runs all possible tests with both TRUE/FALSE for options("popEpi.datatable")
  old <- Sys.getenv("NOT_CRAN")
  on.exit(Sys.setenv("NOT_CRAN" = old))
  
  Sys.setenv("NOT_CRAN" = "true")
  options("popEpi.datatable" = TRUE)
  cat("Starting checking with popEpi.datatable = TRUE \n")
  devtools::test(".")
  
  cat("Finished checking with popEpi.datatable = TRUE; starting with popEpi.datatable = FALSE \n")
  options("popEpi.datatable" = FALSE)
  devtools::test(".")
  
  cat("Finished checking with popEpi.datatable = FALSE \n")
  
}

check_none <- function() {
  ## runs R CMD CHECK without running any tests
  devtools::check(".", args = "--test-dir=tests/emptyDir/")
}