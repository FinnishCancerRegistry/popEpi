# steps to do after CRAN has accepted the new release.

lines <- readLines("CRAN-SUBMISSION")

cran_version <- sub("Version: ", "", lines[1])
git_ref <- sub("SHA: ", "", lines[3])

system2("git", c("tag", cran_version, git_ref))
system2("git", c("push", "--tags"))
