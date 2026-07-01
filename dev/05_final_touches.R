# all of these must be OK before CRAN release.
library("devtools")
du <- new.env()
source("dev/utils.R", local = du)

## spelling --------------------------------------------------------------------
devtools::spell_check()

## README / NEWS ---------------------------------------------------------------
if (du$ask_yn("Is README.Rmd up-to-date?")) {
  du$git_commit_if_changes_made(
    rmarkdown::render("README.Rmd"),
    message = "docs: render README.Rmd"
  )
}
if (!du$ask_yn("Do you have a news item in NEWS.md for the release?")) {
  stop("fix NEWS.md")
}
if (!du$ask_yn("Have you updated cran-comments.md?")) {
  stop("fix cran-comments.md")
}
if (!du$ask_yn("Have you bumped the package version?")) {
  stop("Bump package version (with e.g. desc::desc_bump_version or manually)")
}
