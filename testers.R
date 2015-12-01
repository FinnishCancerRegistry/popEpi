

versionStep <- function(newVersion = NULL) {
  # License: CC0 (just be nice and point others to where you got this)
  # Author: Robert M Flight <rflight79@gmail.com>, github.com/rmflight
  #
  # This is a pre-commit hook that checks that there are files to be committed, and if there are, increments the package version
  # in the DESCRIPTION file. 
  #
  # To install it, simply copy this into the ".git/hooks/pre-commit" file of your git repo, change /path/2/Rscript, and make it
  # executable. Note that /path/2/Rscript is the same as your /path/2/R/bin/R, or may be in /usr/bin/Rscript depending on your
  # installation. This has been tested on both Linux and Windows installations.
  #
  # In instances where you do NOT want the version incremented, add the environment variable doIncrement=FALSE to your git call.
  # eg "doIncrement=FALSE git commit -m "commit message"". 
  # This is useful when you change the major version number for example.
  
  
  doIncrement <- TRUE # default
  
  # get the environment variable and modify if necessary
  tmpEnv <- as.logical(Sys.getenv("doIncrement"))
  if (!is.na(tmpEnv)){
    doIncrement <- tmpEnv
  }
  
#   # check that there are files that will be committed, don't want to increment version if there won't be a commit
#   fileDiff <- system("git diff HEAD --name-only", intern=TRUE)
  
  # if ((length(fileDiff) > 0) && doIncrement){
  if (doIncrement){
    # requireNamespace("git2r")
    
    currDir <- getwd() # this should be the top level directory of the git repo
    # repo <- repository(currDir)
    currDCF <- read.dcf("DESCRIPTION")
    currVersion <- currDCF[1,"Version"]
    splitVersion <- strsplit(currVersion, ".", fixed=TRUE)[[1]]
    nVer <- length(splitVersion)
    currEndVersion <- as.integer(splitVersion[nVer])
    newEndVersion <- as.character(currEndVersion + 1)
    splitVersion[nVer] <- newEndVersion
    if (is.null(newVersion)) newVersion <- paste(splitVersion, collapse=".")
    currDCF[1,"Version"] <- newVersion
    currDCF[1, "Date"] <- strftime(as.POSIXlt(Sys.Date()), "%Y-%m-%d")
    write.dcf(currDCF, "DESCRIPTION")
    # git2r::add(repo, "DESCRIPTION")
    # git2r::tag(repo, newVersion, message = paste0("Package version ", newVersion))
    
    cat("Incremented package version to", newVersion, "\n")
  }
}

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

test_all <- function(filter = NULL, examples = FALSE)  {
  ## runs all possible tests; only with options("popEpi.datatable" = TRUE)
  old <- Sys.getenv("NOT_CRAN")
  on.exit(Sys.setenv("NOT_CRAN" = old))
  Sys.setenv("NOT_CRAN" = "true")
  devtools::test(".", filter = filter)
  if (examples) devtools::run_examples(".", run = TRUE, fresh = TRUE)
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
