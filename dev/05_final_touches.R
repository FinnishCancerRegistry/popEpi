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
if (!du$ask_yn("Have you bumped the version number?")) {
  stop("bump it manually in DESCRIPTION")
}
if (!du$ask_yn("Do you have a news item in NEWS.md for the release?")) {
  stop("fix NEWS.md by adding a codedoc comment block near where you made ",
       "changes in the R scripts.")
}
if (!du$ask_yn("Have updated cran-comments.md?")) {
  stop("fix cran-comments.md")
}
