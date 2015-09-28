library(testthat)
library(popEpi)

if (Sys.getenv("NOT_CRAN") == "true") {
  
  options("popEpi.datatable" = TRUE)
  cat("Starting checking with popEpi.datatable = TRUE \n")
  test_check("popEpi")
  
  cat("Finished checking with popEpi.datatable = TRUE; starting with popEpi.datatable = FALSE \n")
  options("popEpi.datatable" = FALSE)
  test_check("popEpi")
  
  cat("Finished checking with popEpi.datatable = FALSE \n")
} else {
  test_check("popEpi")
}
