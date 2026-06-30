ask_yn <- function(...) {
  message(..., " [y/n]")
  a <- "x"
  while (!a %in% c("y", "n")) {
    a <- tolower(readline(": "))
  }
  return(a == "y")
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
  if (!identical(status, 0L)) {
    stop("Got nonzero status ", status, "; stdout: ",
         paste0(stdout, collapse = " "),
         "; stderr: ", paste0(stderr, collapse = " "))
  }
  return(list(status = status, stdout = stdout, stderr = stderr))
}

git_commit_if_changes_made <- function(
  expr,
  message
) {
  stopifnot(
    is.character(message),
    !is.na(message)
  )
  message <- paste0(message, collapse = "\n")
  s1 <- git_exe_cmd("status")
  stopifnot(
    any(grepl("nothing to commit, working tree clean", s1[["stdout"]]))
  )
  out <- expr # lazy eval triggered
  s2 <- git_exe_cmd("status")
  if (!identical(s1, s2)) {
    git_exe_cmd(c("add", "-A"))
    git_exe_cmd(c("commit", "-m", paste0("\"", message, "\"")))
  }
  return(out)
}
