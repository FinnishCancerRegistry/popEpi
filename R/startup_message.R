




.onAttach <- function(...) {
  
  if (interactive()) {
    msg <- paste0("Using popEpi. See ?popEpi for info and ",
                  "news(package = \"popEpi\") for news.")
    packageStartupMessage(msg)
  }
  
  using_r_devel <- grepl(pattern = "devel", x = base::R.version$status)
  if (using_r_devel) {
    ## memory leak problem in data.table 1.11.2 in R-devel (3.6.0 atm)
    requireNamespace("data.table")
    data.table::setDTthreads(threads = 1L)
  }
  invisible(NULL)
}





.onLoad <- function(...) {
  opt <- getOption("popEpi.datatable")
  if (!is.null(opt) && is.logical(opt) && !isTRUE(opt)) {
    warning("Option 'popEpi.datatable' was set to TRUE when loading popEpi.", 
            call. = FALSE)
  }
  options("popEpi.datatable" = TRUE)
  invisible(NULL)
}








