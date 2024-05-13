# steps to do after CRAN has accepted the new release.

lines <- readLines("CRAN-SUBMISSION")

cran_version <- sub("Version: ", "", lines[1])
git_ref <- sub("SHA: ", "", lines[3])
system2("git", c("checkout", git_ref))
source("https://gist.githubusercontent.com/WetRobot/a741c9d2a22fbfb52784369be8354f15/raw/r_pkg_release.R")
system2("git", c("checkout", "master"))
