

check_full <- function() {
  ## uns R CMD CHECK with all possible tests
  old <- Sys.getenv("NOT_CRAN")
  Sys.setenv("NOT_CRAN" = "true")
  devtools::check(".")
  Sys.setenv("NOT_CRAN" = old)
}

check_some <- function() {
  ## runs R CMD CHECK with only the tests that CRAN will run
  old <- Sys.getenv("NOT_CRAN")
  Sys.setenv("NOT_CRAN" = "false")
  devtools::check(".")
  Sys.setenv("NOT_CRAN" = old)
  
}

test_some <- function()  {
  ## runs only the tests that CRAN will run
  old <- Sys.getenv("NOT_CRAN")
  Sys.setenv("NOT_CRAN" = "false")
  devtools::test(".")
  Sys.setenv("NOT_CRAN" = old)
}

test_all <- function()  {
  ## runs all possible tests
  old <- Sys.getenv("NOT_CRAN")
  Sys.setenv("NOT_CRAN" = "true")
  devtools::test(".")
  Sys.setenv("NOT_CRAN" = old)
}

check_none <- function() {
  ## runs R CMD CHECK without running any tests
  devtools::check(".", args = "--test-dir=tests/emptyDir/")
}