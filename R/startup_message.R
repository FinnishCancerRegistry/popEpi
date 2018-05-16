




.onAttach <- function(...) {
  
  if (interactive()) {
    msg <- paste0("Using popEpi. See news(package='popEpi') for changes. \n",
                  "  popEpi's appropriate data outputs are in data.table ",
                  "(enhanced data.frame) format \n",
                  "  by default; see ?popEpi for changing this. \n", 
                  "  *** IMPORTANT FIXES \n",
                  "      - there was an error in survtab's adjusted outputs ",
                  "in versions <= 0.4.1 \n", 
                  "        leading to inflated confidence intervals; see ",
                  "news(package='popEpi')\n",
                  "      - splitMulti/splitLexisDT pre-0.4.4 sometimes ",
                  "produced duplicated transitions\n",
                  "        when splitting along multiple ",
                  "time scales; see news(package='popEpi')")
    packageStartupMessage(msg)
  }
  
  using_r_devel <- grepl(pattern = "devel", x = R.version$status)
  if (using_r_devel) {
    ## memory leak problem in data.table 1.11.2 in R-devel (3.6.0 atm)
    requireNamespace("data.table")
    data.table::setDTthreads(threads = 1L)
  }
}





.onLoad <- function(...) {
  opt <- getOption("popEpi.datatable")
  if (!is.null(opt) && is.logical(opt) && !isTRUE(opt)) {
    warning("Option 'popEpi.datatable' was set to TRUE when loading popEpi.", 
            call. = FALSE)
  }
  options("popEpi.datatable" = TRUE)
  
}








