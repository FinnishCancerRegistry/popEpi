
# keep fixing until both R CMD checks pass.

## funs ------------------------------------------------------------------------
du <- new.env()
source("dev/utils.R", local = du)

## check part 1 ----------------------------------------------------------------
chk_1 <- du$run_r_cmd_check_cran_unit_tests(no_suggests = FALSE)
print(chk_1)

## check part 2 ----------------------------------------------------------------
chk_2 <- du$run_r_cmd_check_cran_unit_tests(no_suggests = TRUE)
print(chk_2)

## extra -----------------------------------------------------------------------
# you should run this once but it takes a very long time.
# many additional tests are run. run in a separate session in the background
# and proceed with other things.
chk_3 <- du$run_r_cmd_check_all_unit_tests()
