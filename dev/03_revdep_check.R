# make sure that pkgs depending on popEpi weren't broken.

## funs ------------------------------------------------------------------------
du <- new.env()
source("dev/utils.R", local = du)

## revdep ----------------------------------------------------------------------
revchk <- du$revdep_check()

# once everything checks out OK:
revdepcheck::revdep_reset()
