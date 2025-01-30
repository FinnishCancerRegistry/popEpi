## funs ------------------------------------------------------------------------
du <- new.env()
source("dev/utils.R", local = du)

## winbuilder ------------------------------------------------------------------
du$run_r_cmd_check_on_winbuilder()

## rhub ------------------------------------------------------------------------
du$run_r_cmd_check_on_rhub()

## check that other CRAN packages are not broken -------------------------------
message("go to the repo's github page, tab 'actions', run action ",
        "'Reverse dependency check'")

# if everything looks OK, remember to add to cran-comments.md.
