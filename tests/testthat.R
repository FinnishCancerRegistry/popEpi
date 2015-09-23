library(testthat)
library(popEpi)
options("popEpi.datatable" = TRUE)
cat("Starting checking with popEpi.datatable = TRUE \n")
test_check("popEpi")

cat("Finished checking with popEpi.datatable = TRUE; starting with popEpi.datatable = FALSE \n")
options("popEpi.datatable" = FALSE)
test_check("popEpi")

cat("Finished checking with popEpi.datatable = FALSE \n")