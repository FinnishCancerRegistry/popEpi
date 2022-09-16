
# keep fixing until both R CMD checks pass.

# at the time of writing this before releasing 0.3.10,
# there is a NOTE about the submission itself which can be ignored.
# another NOTE is the inability of checking whether URLs work.
# you can try them manually but they virtually certainly are working.
# if the PDF manual fails to build, that is almost certainly a problem
# of the system you are using and not this R package.

## funs ------------------------------------------------------------------------
du <- new.env()
source("dev/utils.R", local = du)

## check part 1 ----------------------------------------------------------------
chk_1 <- du$run_r_cmd_check_cran_unit_tests(depends_only = FALSE)
print(chk_1)

## check part 2 ----------------------------------------------------------------
chk_2 <- du$run_r_cmd_check_cran_unit_tests(depends_only = TRUE)
print(chk_2)

## extra -----------------------------------------------------------------------
# you should run this once but it takes a very long time.
# many additional tests are run. run in a separate session in the background
# and proceed with other things.
chk_3 <- du$run_r_cmd_check_all_unit_tests()
