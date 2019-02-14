

#' @title Convert date objects to fractional years
#' @author Joonas Miettinen
#' @description Using Date objects, calculates given 
#' dates as fractional years.
#' @param x a \code{Date} object, or anything that \code{link{as.Date}}
#' accepts
#' @param year.length character string, either \code{'actual'} or 
#' \code{'approx'}; can be abbreviated; see Details
#' @param ... additional arguments passed on to \code{\link{as.Date}};
#' typically \code{format} when \code{x} is a character string variable,
#' and \code{origin} when \code{x} is numeric
#' @import data.table
#' @export
#' @details
#' 
#' \code{x} should preferably be a \code{date}, \code{Date} or \code{IDate} 
#' object, although it can also be a character string variable 
#' which is coerced internally to \code{Date} format 
#' using \code{\link{as.Date.character}}.
#' 
#' When \code{ year.length = 'actual' }, fractional years are calculated as 
#' \code{ year + (day_in_year-1)/365 } for non-leap-years
#' and as \code{ year + (day_in_year-1)/366 } for leap years. 
#' If \code{ year.length = 'approx' }, fractional years are always
#' calculated as in \code{ year + (day_in_year-1)/365.242199 }. 
#' 
#' There is a slight difference, then, between the two methods
#' when calculating durations between fractional years. For
#' meticulous accuracy one might instead want to calculate durations using
#' dates (days) and convert the results to fractional years.
#'  
#' Note that dates are effectively converted to fractional years at 
#' \code{ 00:00:01 } o'clock:
#' 
#' 
#' \code{ get.yrs("2000-01-01") = 2000 }, and
#' \code{ get.yrs("2000-01-02") = 2000 + 1/365.242199 }. 
#' 
#' 
#' @seealso
#' \code{\link[Epi]{cal.yr}}, \code{\link{as.Date.yrs}}, \code{\link{as.Date}}
#' 
#' @examples
#' 
#' data("sire")
#' sire$dg_yrs <- get.yrs(sire$dg_date)
#' summary(sire$dg_yrs)
#' 
#' ## see: ?as.Date.yrs
#' dg_date2 <- as.Date(sire$dg_yrs)
#' summary(as.numeric(dg_date2 - as.Date(sire$dg_date)))
#' 
#' ## Epi's cal.yr versus get.yrs
#' d <- as.Date("2000-01-01")
#' Epi::cal.yr(d) ## 1999.999
#' get.yrs(d) ## 2000
#' 
#' ## "..." passed on to as.Date, so character / numeric also accepted as input
#' ## (and whatever else as.Date accepts)
#' get.yrs("2000-06-01")
#' get.yrs("20000601", format = "%Y%m%d")
#' get.yrs("1/6/00", format = "%d/%m/%y")
#' 
#' get.yrs(100, origin = "1970-01-01")
#' 
#' 
get.yrs <- function(x, year.length = "approx", ...) {
  as.yrs(x, year.length = year.length, ...)
}


as.yrs <- function(x, year.length, ...) {
  UseMethod("as.yrs")
}

as.yrs.Date <- function(x, year.length = "approx", ...) {
  year.length <- match.arg(year.length, c("actual", "approx"))
  
  yl <- 365.242199
  y <- year(x)
  if (year.length == "actual") {
    yl <- ifelse(is_leap_year(y), 366L, 365L)
  }
  d <- yday(x)
  
  yrs <- y + (d - 1L)/yl
  setattr(yrs, "year.length", year.length)
  setattr(yrs, "class", c("yrs", "numeric"))
  yrs
}

as.yrs.default <- function(x, year.length = "approx", ...) {
  
  x <- as.Date(x, ...)
  as.yrs(x, year.length = year.length)
  
}



#' @title Coerce Fractional Year Values to Date Values
#' @author Joonas Miettinen
#' @param x an \code{yrs} object created by \code{get.yrs}
#' @param ... unused, included for compatibility with other \code{as.Date}
#' methods
#' @description Coerces an \code{yrs} object to a \code{Date} object.
#' Some loss of information comes if \code{year.length = "approx"} 
#' was set when using \code{\link{get.yrs}}, so the transformation back
#' to \code{Date} will not be perfect there. With \code{year.length = "actual"}
#' the original values are perfectly retrieved.
#' @examples 
#' data("sire", package = "popEpi")
#' 
#' ## approximate year lengths: here 20 % have an extra day added
#' sire$dg_yrs <- get.yrs(sire$dg_date)
#' summary(sire$dg_yrs)
#' dg_date2 <- as.Date(sire$dg_yrs)
#' summary(as.numeric(dg_date2 - as.Date(sire$dg_date)))
#' 
#' ## using actual year lengths
#' sire$dg_yrs <- get.yrs(sire$dg_date, year.length = "actual")
#' summary(sire$dg_yrs)
#' dg_date2 <- as.Date(sire$dg_yrs)
#' summary(as.numeric(dg_date2 - as.Date(sire$dg_date)))
#' @seealso \code{\link{get.yrs}}
#' @export
as.Date.yrs <- function(x, ...) {
  
  yl <- attr(x, "year.length")
  if (is.null(yl)) {
    warning("x did not contain meta information about year length used ",
            "when forming the yrs object. Assuming 'approx'.")
    yl <- "approx"
  }
  
  y <- as.integer(x)
  
  mu <- 365.242199
  if (yl == "actual") {
    mu <- ifelse(is_leap_year(y), rep(365L, length(x)), rep(364L, length(x)))
  }
  x <- x + 1L/mu
  yd <- as.integer((x-y)*mu)
  d <- as.Date(paste0(y, "-01-01")) + yd
  d
}


