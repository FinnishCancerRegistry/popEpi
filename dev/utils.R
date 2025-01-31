stopifnot(
  c("devtools", "rcmdcheck", "revdepcheck") %in%
    rownames(utils::installed.packages())
)

default_check_args <- function() {
  c("--as-cran", "--timings")
}
depends_only_check_env_vars <- function() {
  c("_R_CHECK_DEPENDS_ONLY_" = "true")
}
handle_arg_depends_only <- function(depends_only) {
  stopifnot(identical(depends_only, TRUE) || identical(depends_only, FALSE))
  env <- character(0)
  if (depends_only) {
    env <- depends_only_check_env_vars()
    message("running with env var ", names(env), " = ", env)
  }
  return(env)
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
  match.fun(fun_nm)

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

run_r_cmd_check <- function(depends_only = FALSE) {
  env <- handle_arg_depends_only(depends_only)
  rcmdcheck::rcmdcheck(".", args = default_check_args(), env = env)
}

run_r_cmd_check_all_unit_tests <- function(depends_only = FALSE) {
  with_all_unit_tests(
    run_r_cmd_check(depends_only)
  )
}

run_r_cmd_check_cran_unit_tests <- function(depends_only = FALSE) {
  with_cran_unit_tests(
    run_r_cmd_check(depends_only)
  )
}

run_r_cmd_check_ci_unit_tests <- function(depends_only = FALSE) {
  with_ci_unit_tests(
    run_r_cmd_check(depends_only)
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
  depends_only = FALSE
) {
  ## runs R CMD CHECK without running any tests
  env <- handle_arg_depends_only(depends_only)
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
  depends_only = FALSE
) {
  ## runs R CMD CHECK without running any tests
  env <- handle_arg_depends_only(depends_only)
  rcmdcheck::rcmdcheck(
    ".",
    args = union(default_check_args(), c("--no-tests", "--no-examples")),
    env = env
  )
}

run_r_cmd_check_no_unit_tests <- function(
  depends_only = FALSE
) {
  ## runs R CMD CHECK without running any tests
  env <- handle_arg_depends_only(depends_only)
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
  ...
) {
  requireNamespace("rhub")
  if (is.null(platforms)) {
    platforms <- c("linux", "windows", "macos")
  }
  rhub::rhub_check(
    platforms = platforms,
    ...
  )
  message("running rhub checks on github. check the 'actions' tab on the ",
          "repo's github page. note that github actions may have outdated ",
          "versions of other R packages and that may cause issues in edge ",
          "cases.")
  return(invisible(NULL))
}

run_revdepcheck <- function() {
  requireNamespace("revdepcheck")
  revdepcheck::revdep_check(
    num_workers = 4L,
    timeout = 1e6L
  )
  md_file_paths <- dir("revdep", pattern = "[.]md$", full.names = TRUE)
  invisible(lapply(md_file_paths, function(md_file_path) {
    message("contents of ", md_file_path, " ----------------------------------")
    message(suppressWarnings(readLines(md_file_path)))
  }))
  return(invisible(NULL))
}

run_r_cmd_check_on_winbuilder <- function(
  r.versions = c("release", "devel", "oldrelease"),
  ...
) {
  requireNamespace("devtools")
  targz_path <- sprintf(
    "./dev/popEpi_%s.tar.gz",
    read.dcf(file = "DESCRIPTION", fields = "Version")
  )
  devtools::build(
    path = targz_path,
    binary = FALSE
  )
  message("There is a new .tar.gz file at ", targz_path, ".")
  message("Press enter after uploading it manually to ",
          "https://win-builder.r-project.org/ ",
          "(all three of R-oldrelease, R-release, R-devel)")
  readline(": ")
  return(invisible(NULL))
}

ask_yn <- function(
  q,
  yes_msg = NULL,
  no_msg = NULL
) {
  message(q)
  a <- ""
  while (!a %in% c("y", "n")) {
    a <- tolower(readline(": "))
  }
  answered_yes <- identical(a, "y")
  if (answered_yes && !is.null(yes_msg)) {
    message(yes_msg)
  } else if (!answered_yes && !is.null(no_msg)) {
    message(no_msg)
  }
  return(answered_yes)
}

git_exe_cmd <- function(args, system2.arg.list = NULL) {
  stdout_file_path <- tempfile()
  stderr_file_path <- tempfile()
  override_arg_list <- list(command = "git", args = args,
                            stdout = stdout_file_path,
                            stderr = stderr_file_path)
  arg_list <- as.list(system2.arg.list)
  arg_list[names(override_arg_list)] <- override_arg_list
  status <- do.call(system2, arg_list, quote = TRUE)
  stdout <- tryCatch(readLines(stdout_file_path), error = function(e) e)
  stderr <- tryCatch(readLines(stderr_file_path), error = function(e) e)
  return(list(status = status, stdout = stdout, stderr = stderr))
}

git_commit_if_changes_made <- function(
  expr,
  message
) {
  stopifnot(
    is.character(message),
    !is.na(message),
    length(message) == 1
  )
  s1 <- git_exe_cmd("status")
  stopifnot(
    any(grepl("nothing to commit, working tree clean", s1[["stdout"]]))
  )
  out <- expr # lazy eval triggered
  s2 <- git_exe_cmd("status")
  if (!identical(s1, s2)) {
    git_exe_cmd(c("commit", paste0(message, collapse = " ")))
  }
  return(out)
}
