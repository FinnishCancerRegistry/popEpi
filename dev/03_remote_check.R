# these checks on remote services must pass.

## funs ------------------------------------------------------------------------
du <- new.env()
source("dev/utils.R", local = du)

## build -----------------------------------------------------------------------
targz_path <- sprintf(
    "./dev/popEpi_%s.tar.gz",
    read.dcf(file = "DESCRIPTION", fields = "Version")
  )
devtools::build(
  path = targz_path,
  binary = FALSE
)

## winbuilder ------------------------------------------------------------------
# if this fails, you can always upload manually:
# https://win-builder.r-project.org/
du$run_r_cmd_check_on_winbuilder(
  c("release", "devel", "oldrelease"),
  targz_path = targz_path
)
# insert each URL to raw (NOT html) result. they take some time ---
# but less than 2 hours usually.
wb_release <- du$read_winbuilder_results(readline("R-release url: "))
wb_devel <- du$read_winbuilder_results(readline("R-devel url: "))
wb_oldrel <- du$read_winbuilder_results(readline("R-oldred url: "))
cat(wb_release, sep = "\n")
cat(wb_devel, sep = "\n")
cat(wb_oldrel, sep = "\n")
# if everything looks OK, remember to add to cran-comments.md.

## rhub ------------------------------------------------------------------------
du$run_r_cmd_check_on_rhub()
# insert each URL to raw (NOT html) result. each check takes time ---
# check your email in 2-3 hours.
rh_release <- du$read_rhub_results(readline("R-release url: "))
rh_patched <- du$read_rhub_results(readline("R-patched url: "))
rh_devel <- du$read_rhub_results(readline("R-devel url: "))
cat(rh_release, sep = "\n")
cat(rh_patched, sep = "\n")
cat(rh_devel, sep = "\n")
# if everything looks OK, remember to add to cran-comments.md.
