#' @title popEpi: Functions for large-scale epidemiological analysis
#' @description
#' \pkg{popEpi} is built for the needs of registry-based (large-scale)
#' epidemiological analysis. This is in most part enabled by the
#' efficient \pkg{data.table} package for handling and aggregating large data sets.
#'
#' \pkg{popEpi} currently supplies some utility functions such as `[splitMulti]`
#' and `[get.yrs]` for preparing large data sets for epidemiological analysis.
#' Included are also a a few functions that can be used in
#' epidemiological analysis such as `[sir]` for estimating
#' standardized incidence/mortality ratios (SIRs/SMRs) and `[survtab]` for
#' estimating observed and relative/net survival as well as cumulative incidence
#' functions (CIFs). In particular, `survtab` implements the Ederer II
#' (Ederer and Heise (1959)) and
#' Pohar Perme estimators (Pohar Perme, Stare, and Esteve (2012)
#' \doi{10.1111/j.1541-0420.2011.01640.x}) and allows for easy
#' age-standardisation.
#'
#' Since there are many benefits to using `data.tables`, \pkg{popEpi} returns
#' outputs by default in the `data.table` format where appropriate.
#' Since `data.table`
#' objects are usually modified by reference, this may have surprising side
#' effects for users uninitiated in using `data.table`. To ensure
#' that appropriate outputs are in the `data.frame` format, set
#' `options("popEpi.datatable" = FALSE)`. However, `data.table`
#' usage is recommended due to better performance and testing coverage.
#' `data.table` is used
#' by most functions internally in both cases.
#'
#' @keywords internal
"_PACKAGE"
