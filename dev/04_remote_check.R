## winbuilder ------------------------------------------------------------------
local({
  # devtools::build(binary = FALSE, path = "dev/")
  # tar_gz_file_path <- dir("dev", pattern = "[.]tar[.]gz$", full.names = TRUE)
  out <- tryCatch(
    {
      devtools::check_win_devel(webform = TRUE)
      devtools::check_win_release(webform = TRUE)
      devtools::check_win_oldrelease(webform = TRUE)
      TRUE
    },
    error = function(e) e
  )
  if (inherits(out, "error")) {
    unlink(dir("dev", pattern = "[.]tar[.]gz$", full.names = TRUE))
    message(
      "Automatic upload failed. ",
      "Upload the created source file to this address on every R version it ",
      "allows: https://win-builder.r-project.org/. ",
      "Press enter when you have done that."
    )
    readline(": ")
  } else {
    message(
      "Uploaded package to WinBuilder. You will receive the results to the ",
      "e-mail of the maintainer."
    )
  }
})

## rhub ------------------------------------------------------------------------
# if this does not work, see rhub::rhub_doctor.
# I found that only the "classic" PAT works.
# If you still struggle, you can trigger this manually on the actions page
# of the github repo.
rhub::rhub_check(platforms = c("linux", "windows", "macos"))
message(
  "The above triggered checks on Github's servers. See the actions page ",
  "of the repository to see the results. ",
  "Press enter to confirm that you understand."
)
readline(": ")

## cran-comments.md ------------------------------------------------------------
message(
  "After winbuilder has e-mailed its passing checks and rhub checks ",
  "have passed, update cran-comments.md. Press enter when you have done ",
  "that."
)
readline(": ")
system2("git", c("add", "cran-comments.md"))
system2("git", c("commit", "--m", "\"docs: update cran-comments.md\""))
