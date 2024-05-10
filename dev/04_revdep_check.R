# make sure that pkgs depending on popEpi weren't broken.

## funs ------------------------------------------------------------------------
du <- new.env()
source("dev/utils.R", local = du)

## revdep ----------------------------------------------------------------------
revchk <- du$run_revdep_check()

# once everything checks out OK:
if (du$ask_yn("did revdep check pass?")) {
  revdepcheck::revdep_reset()
} else {
  stop("fix problems, re-run this script.")
}
