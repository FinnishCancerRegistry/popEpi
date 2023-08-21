# all of these must be OK before CRAN release.
library("devtools")

## spelling --------------------------------------------------------------------
devtools::spell_check()

## manual checks ---------------------------------------------------------------
# have you updated NEWS, NEWS.Rmd -> NEWS.md?

# have you changed the version number?

# is the author list up-to-date?

# is cran-comments.md up-to-date?
