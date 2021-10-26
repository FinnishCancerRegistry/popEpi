




run_as_rstudio_job <- function(expr) {
  tf <- tempfile(fileext = ".R")
  lines <- deparse(substitute(expr))
  lines <- c(
    "source(\"dev_utils.R\")",
    lines
  )
  
  writeLines(lines, con = tf)
  rstudioapi::jobRunScript(path = tf, workingDir = getwd())
}

run_fun_as_rstudio_job <- function(fun_nm) {
  stopifnot(
    is.character(fun_nm)
  )
  fun <- match.fun(fun_nm)
  
  tf <- tempfile(fileext = ".R")
  lines <- c(
    "source(\"dev_utils.R\")",
    paste0(fun_nm, "()")
  )
  writeLines(lines, con = tf)
  rstudioapi::jobRunScript(path = tf, workingDir = getwd(), name = fun_nm)
}

run_r_cmd_check_all_unit_tests <- function() {
  ## runs R CMD CHECK with all possible tests
  old <- Sys.getenv("NOT_CRAN")
  on.exit(Sys.setenv("NOT_CRAN" = old))
  Sys.setenv("NOT_CRAN" = "true")
  devtools::check(".")
}

run_r_cmd_check_cran_unit_tests <- function() {
  ## runs R CMD CHECK with only the tests that CRAN will run
  old <- Sys.getenv("NOT_CRAN")
  on.exit(Sys.setenv("NOT_CRAN" = old))
  Sys.setenv("NOT_CRAN" = "false")
  devtools::check(".")
}

run_cran_unit_tests <- function(filter = NULL)  {
  ## runs only the tests that CRAN will run
  old <- Sys.getenv("NOT_CRAN")
  on.exit(Sys.setenv("NOT_CRAN" = old))
  Sys.setenv("NOT_CRAN" = "false")
  stopifnot(
    testthat:::on_cran() == TRUE
  )
  devtools::test(".", filter = filter)
}

run_all_unit_tests <- function(filter = NULL)  {
  ## runs all possible tests
  old <- Sys.getenv("NOT_CRAN")
  on.exit(Sys.setenv("NOT_CRAN" = old))
  Sys.setenv("NOT_CRAN" = "true")
  stopifnot(
    testthat:::on_cran() == FALSE
  )
  devtools::test(".", filter = filter)
}

run_examples <- function() {
  devtools::run_examples(run_donttest = TRUE, run_dontrun = TRUE)
}

run_r_cmd_check_no_unit_tests_no_examples_no_vignettes <- function() {
  ## runs R CMD CHECK without running any tests
  devtools::check(
    ".", 
    args = c("--no-tests", "--no-examples", "--no-vignettes", "--no-build-vignettes"),
    vignettes = FALSE
  )
}

run_r_cmd_check_no_unit_tests_no_examples <- function() {
  ## runs R CMD CHECK without running any tests
  devtools::check(
    ".", 
    args = c("--no-tests", "--no-examples")
  )
}

run_r_cmd_check_no_unit_tests <- function() {
  ## runs R CMD CHECK without running any tests
  devtools::check(
    ".", 
    args = c("--no-tests")
  )
}

run_all_unit_tests_popEpi_datatable <- function(...) {
  ## runs all possible tests with both TRUE/FALSE for 
  ## options("popEpi.datatable")
  old <- Sys.getenv("NOT_CRAN")
  on.exit(Sys.setenv("NOT_CRAN" = old))
  
  Sys.setenv("NOT_CRAN" = "true")
  options("popEpi.datatable" = TRUE)
  message("* run_all_unit_tests_popEpi_datatable: ",
          "Starting checking with popEpi.datatable = TRUE")
  devtools::test(...)
  
  message("* run_all_unit_tests_popEpi_datatable: ",
          "Finished checking with popEpi.datatable = TRUE;",
          "starting with popEpi.datatable = FALSE")
  options("popEpi.datatable" = FALSE)
  devtools::test(...)
  
  message("* run_all_unit_tests_popEpi_datatable: ",
          "Finished checking with popEpi.datatable = FALSE")
}

run_r_cmd_check_on_rhub <- function(
  platforms = NULL, 
  show_status = FALSE, 
  ...
) {
  requireNamespace("rhub") ## 1.0.1 on github only
  if (is.null(platforms)) {
    platforms <- rhub::platforms()$name
  }
  rhub::check(platform = platforms, show_status = show_status, ...)
}

run_r_cmd_check_on_winbuilder <- function(...) {
  requireNamespace("devtools")
  devtools::check_win_release(...)
  devtools::check_win_devel(...)
  devtools::check_win_oldrelease(...)
}








