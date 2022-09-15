
default_check_args <- function() {
  c("--as-cran", "--timings")
}
no_suggests_check_env_vars <- function() {
  c("_R_CHECK_DEPENDS_ONLY_" = "true")
}

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

without_all_unit_tests <- function(expr) {
  message("running with popEpi_run_all_unit_tests = false")
  old_all <- Sys.getenv("popEpi_run_all_unit_tests")
  on.exit({
    Sys.setenv("popEpi_run_all_unit_tests" = old_all)
  })
  Sys.setenv("popEpi_run_all_unit_tests" = "false")
  return(expr)
}

with_all_unit_tests <- function(expr) {
  message("running with popEpi_run_all_unit_tests = true")
  old_all <- Sys.getenv("popEpi_run_all_unit_tests")
  on.exit({
    Sys.setenv("popEpi_run_all_unit_tests" = old_all)
  })
  Sys.setenv("popEpi_run_all_unit_tests" = "true")
  return(expr)
}

with_cran_unit_tests <- function(expr) {
  without_all_unit_tests(expr)
}

with_ci_unit_tests <- function(expr) {
  without_all_unit_tests(expr)
}

run_r_cmd_check <- function(no_suggests = FALSE) {
  env <- character(0)
  if (no_suggests) {
    env <- no_suggests_check_env_vars()
  }
  rcmdcheck::rcmdcheck(".", args = default_check_args(), env = env)
}

run_r_cmd_check_all_unit_tests <- function(no_suggests = FALSE) {
  with_all_unit_tests(
    run_r_cmd_check(no_suggests)
  )  
}

run_r_cmd_check_cran_unit_tests <- function(no_suggests = FALSE) {
  with_cran_unit_tests(
    run_r_cmd_check(no_suggests)
  )
}

run_r_cmd_check_ci_unit_tests <- function(no_suggests = FALSE) {
  with_ci_unit_tests(
    run_r_cmd_check(no_suggests)
  )
}

run_cran_unit_tests <- function(...)  {
  with_cran_unit_tests(devtools::test(pkg = ".", ...))
}
run_ci_unit_tests <- function(...)  {
  with_ci_unit_tests(devtools::test(pkg = ".", ...))
}

run_all_unit_tests <- function(...)  {
  ## runs all possible tests
  with_all_unit_tests(devtools::test(pkg = ".", ...))
}

run_examples <- function() {
  devtools::run_examples(run_donttest = TRUE, run_dontrun = TRUE)
}

check_triple <- function() {
  run_fun_as_rstudio_job("run_examples")
  run_fun_as_rstudio_job("run_r_cmd_check_no_unit_tests_no_examples_no_vignettes")
  run_fun_as_rstudio_job("run_cran_unit_tests")
  invisible(NULL)
}

run_r_cmd_check_no_unit_tests_no_examples_no_vignettes <- function(
  no_suggests = FALSE
) {
  ## runs R CMD CHECK without running any tests
  env <- character(0L)
  if (no_suggests) {
    no_suggests <- no_suggests_check_env_vars()
  }
  rcmdcheck::rcmdcheck(
    path = ".", 
    args = union(
      c("--no-tests", "--no-examples", 
        "--no-vignettes", "--no-build-vignettes"),
      default_check_args()
    ),
    env = env
  )
}

run_r_cmd_check_no_unit_tests_no_examples <- function(
  no_suggests = FALSE
) {
  ## runs R CMD CHECK without running any tests
  env <- character(0L)
  if (no_suggests) {
    no_suggests <- no_suggests_check_env_vars()
  }
  rcmdcheck::rcmdcheck(
    ".", 
    args = union(default_check_args(), c("--no-tests", "--no-examples")),
    env = env
  )
}

run_r_cmd_check_no_unit_tests <- function(
  no_suggests = FALSE
) {
  ## runs R CMD CHECK without running any tests
  env <- character(0L)
  if (no_suggests) {
    no_suggests <- no_suggests_check_env_vars()
  }
  rcmdcheck::rcmdcheck(
    ".", 
    args = union(default_check_args(), "--no-tests"),
    env = env
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

run_r_cmd_check_on_winbuilder <- function(
  r.versions = c("release", "devel", "oldrelease"), 
  ...
) {
  requireNamespace("devtools")
  if ("release" %in% r.versions) {
    devtools::check_win_release(...)
  }
  if ("devel" %in% r.versions) {
    devtools::check_win_devel(...)
  }
  if ("oldrelease" %in% r.versions) {
    devtools::check_win_oldrelease(...)
  }
  return(invisible(NULL))
}

run_revdep_check <- function(
  timeout = as.difftime(12, units = "hours")
  num_workers = 4L, 
  bioc = FALSE, 
  ...
) {
  requireNamespace("revdepcheck")
  # devtools::install_github("r-lib/devdepcheck")
  revdepcheck::revdep_check(bioc = bioc, num_workers = num_workers, ...)
}







