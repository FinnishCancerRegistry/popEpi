
.onAttach <- function(...) {
  if (interactive()) {
    msg <- paste0("Using popEpi. See news(package='popEpi') for changes. \n",
                  "popEpi's appropriate data outputs are in data.table ",
                  "(enhanced data.frame) format by default; \n",
                  "see ?popEpi for changing this. \n", 
                  "*** IMPORTANT: there was an error in survtab's adjusted outputs ",
                  "in versions <= 0.4.1 \n", 
                  "*** leading to inflated confidence intervals; see ",
                  "news(package='popEpi')")
    packageStartupMessage(msg)
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



