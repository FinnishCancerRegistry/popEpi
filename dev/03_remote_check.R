## funs ------------------------------------------------------------------------
du <- new.env()
source("dev/utils.R", local = du)

## winbuilder ------------------------------------------------------------------
targz_path <- sprintf(
  "./dev/popEpi_%s.tar.gz",
  read.dcf(file = "DESCRIPTION", fields = "Version")
)
devtools::build(
  path = targz_path,
  binary = FALSE
)
# if this fails, you can always upload manually:
# https://win-builder.r-project.org/
du$run_r_cmd_check_on_winbuilder(
  c("release", "devel", "oldrelease"),
  targz_path = targz_path
)

## rhub ------------------------------------------------------------------------
du$run_r_cmd_check_on_rhub()

## check that other CRAN packages are not broken -------------------------------
message("go to the repo's github page, tab 'actions', run action ",
        "'Reverse dependency check'")

# if everything looks OK, remember to add to cran-comments.md.
