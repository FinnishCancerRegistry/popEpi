

check_all <- function() {
  ## runs R CMD CHECK with all possible tests; only with options("popEpi.datatable" = TRUE)
  old <- Sys.getenv("NOT_CRAN")
  on.exit(Sys.setenv("NOT_CRAN" = old))
  Sys.setenv("NOT_CRAN" = "true")
  devtools::check(".")
}

check_some <- function() {
  ## runs R CMD CHECK with only the tests that CRAN will run; only with options("popEpi.datatable" = TRUE)
  old <- Sys.getenv("NOT_CRAN")
  on.exit(Sys.setenv("NOT_CRAN" = old))
  Sys.setenv("NOT_CRAN" = "false")
  devtools::check(".")
  
}

test_some <- function(filter = NULL)  {
  ## runs only the tests that CRAN will run
  old <- Sys.getenv("NOT_CRAN")
  on.exit(Sys.setenv("NOT_CRAN" = old))
  Sys.setenv("NOT_CRAN" = "false")
  devtools::test(".", filter = filter)
}

test_all <- function(filter = NULL)  {
  ## runs all possible tests; only with options("popEpi.datatable" = TRUE)
  old <- Sys.getenv("NOT_CRAN")
  on.exit(Sys.setenv("NOT_CRAN" = old))
  Sys.setenv("NOT_CRAN" = "true")
  devtools::test(".", filter = filter)
}

test_full <- function(filter = NULL) {
  ## runs all possible tests with both TRUE/FALSE for options("popEpi.datatable")
  old <- Sys.getenv("NOT_CRAN")
  on.exit(Sys.setenv("NOT_CRAN" = old))
  
  Sys.setenv("NOT_CRAN" = "true")
  options("popEpi.datatable" = TRUE)
  cat("Starting checking with popEpi.datatable = TRUE \n")
  devtools::test(".", filter = filter)
  
  cat("Finished checking with popEpi.datatable = TRUE; starting with popEpi.datatable = FALSE \n")
  options("popEpi.datatable" = FALSE)
  devtools::test(".", filter = filter)
  
  cat("Finished checking with popEpi.datatable = FALSE \n")
  
}

check_none <- function() {
  ## runs R CMD CHECK without running any tests
  devtools::check(".", args = "--test-dir=tests/emptyDir/")
}

resaveAllDatas <- function(folder = "data/", ext = ".rdata") {
  ## INTENDED USE: when package data.table is updated
  ## and datas need to be resaved using the new data.table version.
  n <- nchar(ext)
  fn <- dir(folder)
  fn <- fn[substr(fn, nchar(fn) - n+1, nchar(fn)) == ext]
  
  te <- new.env()
  for (k in fn) {
    path <- paste0(folder, if(substr(folder, nchar(folder), nchar(folder)) == "/") NULL else "/", k)
    cat("loading data file: '", path, "'... \n", sep = "")
    dtn <- load(file = path, envir = te)
    te[[dtn]] <- data.table(te[[dtn]])
    cat("saving data file: '", path, "'... \n", sep = "")
    save(list = dtn, envir = te, file = path)
    
  }
  
  invisible()
  
}
