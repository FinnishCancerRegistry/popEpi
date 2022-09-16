# steps to do after CRAN has accepted the new release.

lines <- readLines("CRAN-SUBMISSION")

cran_version <- sub("Version: ", "", lines[1])
git_ref <- sub("SHA: ", "", lines[3])

cmd <- paste0("git tag ", cran_version, " ", git_ref)
message("run this cmd in system command line:")
message(cmd)
