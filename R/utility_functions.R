

#' @title Cast \code{data.table}/\code{data.frame} from long format to wide format
#' @author Matti Rantanen, Joonas Miettinen
#'
#' @description 
#' Convenience function for using \code{\link[data.table]{dcast.data.table}}
#' and \code{\link[reshape2]{dcast}};
#' inputs are character strings (names of variables) instead of a formula.
#' 
#' @param data a \code{data.table} or \code{data.frame}
#' @param columns a character string vector; the (unique combinations of the) 
#' levels of these variable will be different rows
#' @param rows a character string vector; the (unique combinations of the) 
#' levels of these variable will be different columns
#' @param values a character string; the variable which will be represented
#' on rows and columns as specified by \code{columns} and \code{rows}
#' @import data.table
#' @import stats
#' @export cast_simple
#' @details This function is just a small interface for \code{dcast} / 
#' \code{dcast.data.table} and less flexible than the originals.
#' 
#' Note that all \code{data.table} objects are also \code{data.frame} 
#' objects, but that each have their own \code{dcast} method.
#' \code{\link[data.table]{dcast.data.table}} is faster.
#' 
#' If any values in \code{value.vars} need to be 
#' aggregated, they are aggregated using \code{sum}.
#' See \code{?dcast}.
#' 
#' @examples 
#' \dontrun{
#' ## e.g. silly counts from a long-format table to a wide format
#' test <- copy(sire)
#' test$dg_y <- year(test$dg_date)
#' test$ex_y <- year(test$ex_date)
#' tab <- ltable(test, c("dg_y","ex_y"))
#' cast_simple(tab, columns='dg_y', rows="ex_y", values="obs")
#' }


cast_simple <- function(data=NULL, columns=NULL, rows=NULL, values=NULL) {
  if (!is.data.frame(data)) stop("data needs be a data.frame or data.table")
  
  if (is.null(columns)) stop("columns cannot be NULL")
  
  all_names_present(data, c(columns, rows, values))
  
  rowsNULL <- FALSE
  if (is.null(rows)) rowsNULL <- TRUE
  if (rowsNULL) rows <- "1"
  form <- paste0(paste0(rows, collapse = " + "), " ~ ", paste0(columns, collapse = " + "))
  
  ## note: dcast probably usually finds the methods for data.frame / data.table,
  ## but this method is more certain
  if (is.data.table(data)) {
    d <- dcast.data.table(data, formula = form, value.var=values, drop=FALSE, fun.aggregate=sum)[]
  } else {
    d <- dcast(data, formula = form, value.var = values, drop = FALSE, fun.aggregate = sum)[]
  }
  if (rowsNULL) set(d, j = names(d)[1L], value = NULL)
  d
}


#' @title Convert NA's to zero in data.table
#' @author Joonas Miettinen
#' @description Given a \code{data.table DT}, replaces any \code{NA} values
#' in the variables given in \code{vars} in \code{DT}. Takes a copy of the 
#' original data and returns the modified copy.
#' @import data.table
#' @param DT \code{data.table} object
#' @param vars a character string vector of variables names in \code{DT};
#' if \code{NULL}, uses all variable names in \code{DT}
#' @export na2zero
#' @details Given a \code{data.table} object, converts \code{NA} values
#' to numeric (double) zeroes for all variables named in \code{vars} or
#' all variables if \code{vars = NULL}.
na2zero = function(DT, vars = NULL) { 
  if (!is.data.table(DT)) stop("DT must be a data.table")
  DT <- copy(DT)
  
  navars <- vars
  if (is.null(navars)) navars <- names(DT)
  all_names_present(DT, navars)
  for (k in navars) {
    DT[is.na(get(k)), (k) := 0]
  }
  
  return(DT[])
}


#' @title Convert factor variable to numeric 
#' @description Convert factor variable with numbers as levels into a numeric variable
#' @param x a factor variable with numbers as levels
#' @export fac2num
#' @details
#' For example, a factor with levels \code{c("5","7")} is converted into 
#' a numeric variable with values \code{c(5,7)}.
#' @seealso
#' \code{\link{robust_values}}
#' @source 
#' \href{http://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information}{Stackoverflow thread}
#' @examples
#' ## this is often not intended
#' as.numeric(factor(c(5,7))) ## result: c(1,2)
#' ## but this
#' fac2num(factor(c(5,7))) ## result: c(5,7)
#' 
#' ## however
#' as.numeric(factor(c("5","7","a"))) ## 1:3
#' 
#' fac2num(factor(c("5","7","a"))) ## result: c(5,7,NA) with warning
#' 
#' 
fac2num <- function(x) {
  as.numeric(levels(x))[x]
}



#' @title Convert date objects to fractional years
#' @author Joonas Miettinen
#' @description Using Date objects, calculates given 
#' dates as fractional years.
#' @param dates a vector or column of Date objects or right kind of character strings, see Details
#' @param format a character string; if \code{dates} is a character vector, 
#' specifies the format; see \code{\link{as.Date}}
#' @param year.length character string, either \code{'actual'} or 
#' \code{'approx'}; can be abbreviated; see Details
#' @import data.table
#' @export get.yrs
#' @details
#' 
#' \code{dates} should preferably be a \code{date}, \code{Date} or \code{IDate} 
#' object, 
#' although they can also be character strings in a format
#' specified by \code{format} (passed to \code{\link{as.Date}}).
#' 
#' When \code{ year.length = 'actual' }, fractional years are calculated as 
#' \code{ year + day_in_year/365 } for non-leap-years
#' and as \code{ year + day_in_year/366 } for leap years. 
#' If \code{ year.length = 'approx' }, fractional years are always
#' calculated as in \code{ year + day_in_year/365.242199 }. 
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
#' \code{\link[Epi]{cal.yr}}
#' 
#' @examples
#' 
#' test <- copy(sire)
#' test$dg_yrs <- get.yrs(test$dg_date)
#' summary(test$dg_yrs)
#' 
#' ## Epi's cal.yr versus get.yrs
#' Epi::cal.yr("2000-01-01") ## 1999.999
#' get.yrs("2000-01-01") ## 2000
#' 
get.yrs <- function(dates, format = "%Y-%m-%d", year.length = "approx") {
  match.arg(year.length, c("actual", "approx"))
  y <- yrs <- NULL ## to instate as global variable to appease R CMD CHECK
  
  orle <- length(dates)
  nale <- sum(is.na(dates))
  
  if (is.null(orle)) stop("dates vector is NULL; did you supply the right data?")
  
  if (orle == 0) stop("length of dates vector is zero; did you supply the right data?")
  
  dat <- data.table(dates=dates)
  if (is.character(dates)) {
    dat[, dates := as.IDate(dates, format = format)]
  } else if (inherits(dat$dates, "date")) {
    dat[, dates := as.IDate(dates)]
  }
  
  
  nale2 <- dat[, sum(is.na(dates))]
  
  
  if (year.length  == "actual") {
    ## fractional years using actual year lengths
    dat[, y    := year(dates)]
    ## calculate distance between first day of each year and given dates values
    dat[            , yrs := as.numeric(y + (yday(dates)-1L)/365L)]
    dat[is_leap_year(y), yrs := as.numeric(y + (yday(dates)-1L)/366L)]
    
  } else {
    ## fractional years using hypothetical year length    
    dat[, yrs := year(dates) + (yday(dates)-1L)/365.242199]
  }
  nale3 <- dat[, sum(is.na(yrs))]
  if (nale3 == orle) warning(paste0("ALL dates values were coerced to NA by get.yrs"))
  if (nale3 >  nale) warning(paste0(nale3-nale, "values were coerced to NA by get.yrs"))
  
  setattr(dat$yrs, "year.length", year.length)
  setattr(dat$yrs, "class", c("yrs", "numeric"))
  
  return(dat$yrs)
}



#' @title Detect leap years
#' @author Joonas Miettinen
#' @description Given a vector or column of year values (numeric or integer), \code{\link{is_leap_year}} returns a vector of equal length
#' of logical indicators, i.e. a vector where corresponding leap years have value TRUE, and FALSE otherwise.
#' 
#' @param years a vector or column of year values (numeric or integer)
#' @examples
#' ## can be used to assign new columns easily, e.g. a dummy indicator column
#' df <- data.frame(yrs=c(1900,1904,2005,1995))
#' df$lyd <- as.integer(is_leap_year(df$yrs))
#' 
#' ## mostly it is useful as a condition or to indicate which rows have leap years
#' which(is_leap_year(df$yrs)) # 2
#' df[is_leap_year(df$yrs),] # 2nd row
#' 
#' @export is_leap_year
#' 
is_leap_year <- function(years) {
  if (!is.numeric(years)) {
    stop("years must be a numeric vector, preferably integer for speed. Use e.g. as.integer().")
  }
  
  years <- try2int(years)
  if (!is.integer(years)) stop("years could not be coerced to integer; don't use fractional years such as 2000.1234 but integers such as 2000")
  
  # divisible by four
  isLeap <- years %% 4L == 0L
  # not divisible by 100
  isLeap <- isLeap & years %% 100L != 0L
  # unless divisible by 400 also
  isLeap <- isLeap | years %% 400L == 0L
  isLeap
  
}
#' @title Test if object is a \code{Date} object
#' @description Tests if an object is a \code{Date} object and returns
#' a logical vector of length 1. \code{IDate} objects are also 
#' \code{Date} objects, but \code{date} objects from package \pkg{date}
#' are not. 
#' @author Joonas Miettinen
#' @param obj object to test on
#' @export is.Date
#' @seealso
#' \code{\link{get.yrs}}, \code{\link{is_leap_year}}, \code{\link{as.Date}}
#' @examples
#' ## the base "capital Date" format
#' da <- as.Date("2000-01-01")
#' is.Date(da) ## TRUE
#' date::is.date(da) ## FALSE
#' 
#' ## IDate format from data.table
#' da <- as.IDate("2000-01-01")
#' is.Date(da) ## TRUE
#' date::is.date(da) ## FALSE
#' 
#' ## from package "date"
#' da <- date::as.date("1jan2000")
#' is.Date(da) ## FALSE
#' date::is.date(da) ## TRUE
#'  
is.Date <- function(obj) {
  
  if (any(c("IDate","Date") %in% class(obj))) {
    return(TRUE)
  }
  
  return(FALSE)
}


#' @title Convert values to numeric robustly
#' @author Joonas Miettinen
#' 
#' @param num.values values to convert to numeric
#' @param force logical; if \code{TRUE}, returns a vector of values where values that cannot be interpreted as numeric are
#' set to \code{NA}; if \code{FALSE}, returns the original vector and gives a warning if any value cannot be interpreted as
#' numeric.
#' @param messages logical; if \code{TRUE}, returns a message of what was done with the \code{num.values}
#' @description Brute force solution for ensuring a variable is numeric by 
#' coercing a variable of any type first to factor and then to numeric
#' @export robust_values
#' @import data.table
#' @note
#' Returns \code{NULL} if given \code{num.values} is \code{NULL}. 
#' @examples
#' ## this works
#' values <- c("1", "3", "5")
#' values <- robust_values(values)
#' 
#' ## this works
#' values <- c("1", "3", "5", NA)
#' values <- robust_values(values)
#' 
#' ## this returns originals
#' values <- c("1", "3", "5", "a")
#' values <- robust_values(values)
#' 
#' ## this forces "a" to NA and works otherwise
#' values <- c("1", "3", "5", "a")
#' values <- robust_values(values, force=TRUE)
#' 

robust_values <- function(num.values, force = FALSE, messages = TRUE) {
  a <- NULL
  if (is.null(num.values)) {
    return(NULL)
  }
  dt <- data.table(num.values)
  nas <- dt[is.na(num.values), .N]
  
  suppressWarnings(
    dt[,a := fac2num(factor(num.values))]
  )
  dt[, a := try2int(a)]
  nas2 <- dt[is.na(a), .N]
  
  if (!force & nas2 > nas) {
    if (messages) warning("since force = FALSE and NAs were created, returning original values")
    return(dt$num.values)
  }
  if (force) {
    if (nas2 > nas) {
      if (messages) warning("some NAs were created")
    }
    return(dt$a)
  }
  
  
  return(dt$a)
  
  
}

#' @title Check if all names are present in given data
#' @author Joonas Miettinen
#' @param data dataset where the variable names should be found
#' @param var.names a character vector of variable names, e.g.
#' \code{c("var1", "var2")}
#' @param stops logical, stop returns exception
#' @param msg Custom message to return instead of default message.
#' Special: include %%VARS%% in message and the missing variable names
#' will be inserted there (quoted, separated by comma, e.g. 
#' \code{'var1'}, \code{'var2'} --- no leading or tracing white space). 
#' @description given a character vector, checks if all names are present in \code{names(data)}.
#' Throws error if \code{stops=TRUE}, else returns \code{FALSE} if some variable name is not present.
#' @seealso
#' \code{\link{robust_values}}
#' @export all_names_present

all_names_present <- function(data, var.names, stops = TRUE, msg = NULL) {
  
  # if (!is.character(var.names)) stop("Argument 'var.names' must be character string vector. E.g. var.names = 'var1'")
  badNames <- setdiff(var.names, names(data))
  if (length(badNames) == 0L) return(TRUE)
  
  badNames <- paste0("'", badNames, "'", collapse = ", ")
  
  if (is.null(msg)) msg <- paste0("Cannot proceed - following given variable name(s) not present in dataset '",
                                  deparse(substitute(data)), "': ", badNames)
  if (!is.character(msg) || length(msg) > 1L) stop("Argument 'msg' must be a character string vector of length one.") else
    msg <- gsub(pattern = "%%VARS%%", replacement = badNames, x = msg)
  if (!is.logical(stops) || length(stops) > 1L) stop("Argument 'stops' must be either TRUE or FALSE.")
  
  if (stops) stop(msg)
  
  return(FALSE)
}


#' @title Return lower_bound value from char string (20,30]
#' @author Matti Rantanen
#' @description selects lowest values of each factor after cut() based
#' on that the value starts from index 2 and end in comma ",".
#' @param cut is a character vector of elements "(20,60]"
#' @export lower_bound

lower_bound <- function(cut) {
  cut <- as.character(cut)
  ind <- gregexpr(pattern=',',cut)
  ind <- as.numeric(ind) - 1
  t.sub <- as.numeric(substr(cut,2, ind))
  return(t.sub)
}


#' @title Change output values from cut(..., labels = NULL) output
#' @author Matti Rantanen
#' @param t is a character vector of elements, e.g. "(20,60]"
#' @param factor logical; TRUE returns informative character string, FALSE numeric (left value)
#' @description Selects lowest values of each factor after cut() based
#' on the assumption that the value starts from index 2 and end in comma ",".
#' @details type = 'factor': "[50,52)" -> "50-51" OR "[50,51)" -> "50"
#' 
#' type = 'numeric': lowest bound in numeric.
#' 
#' @export cut_bound
#' @examples
#' cut_bound("[1900, 1910)") ## "1900-1909"

cut_bound <- function(t, factor=TRUE) {
  if (!factor) {
    t <- as.character(t)
    ind <- gregexpr(pattern=',',t)
    ind <- as.numeric(ind) - 1
    t <- as.numeric(substr(t,2, ind))
    return(t)
  }
  if (factor) {
    t <- as.character(t)
    t <- gsub(',', '-' , substr(t, 2, nchar(t) - 1) )
    ind <-as.numeric( gregexpr(pattern='-',t) )
    if (any(as.numeric( substr(t,1,ind-1) ) +1 == as.numeric( substr(t,ind+1,nchar(t))) ) ) {
      t <- substr(t,1,ind-1)
      return(t)
    }
    t
    a <- substr(t, ind+1, nchar(t))
    t <- sub(a, as.character(as.numeric(a)-1), t)
    return(t)
  }
}




#' @title Set the class of an object (convencience function for
#'  \code{setattr(obj, "class", CLASS)}); can add instead of replace
#' @description Sets the class of an object in place to \code{cl}
#' by replacing or adding
#' @param obj and object for which to set class
#' @param cl class to set
#' @param add if \code{TRUE}, adds \code{cl} to the 
#' classes of the \code{obj}; otherwise replaces the class information
#' @param add.place \code{"first"} or \code{"last"}; adds \code{cl}
#' to the front or to the back of the \code{obj}'s class vector
#' @author Joonas Miettinen
setclass <- function(obj, cl, add=FALSE, add.place="first") {
  match.arg(add.place, c("first","last"))
  cl <- as.character(cl)
  
  if (add) {
    old_classes <- attr(obj, "class")
    
    if (add.place=="first") {
      setattr(obj, "class", c(cl, old_classes))
    } else {
      setattr(obj, "class", c(old_classes, cl))
    }
  } else {
    setattr(obj, "class", cl)
  }
}




#' @title Attempt coercion to integer
#' @author James Arnold
#' @description Attempts to convert a numeric object to integer, 
#' but won't if loss of information is imminent (if values after decimal
#' are not zero for even one value in \code{obj})
#' @param obj a numeric vector
#' @param tol tolerance; if each numeric value in \code{obj} deviate from
#' the corresponding integers at most the value of \code{tol}, they are considered
#' to be integers; e.g. by default \code{1 + .Machine$double.eps} is considered
#' to be an integer but \code{1 + .Machine$double.eps^0.49} is not.
#' @export try2int
#' @source \href{http://stackoverflow.com/questions/3476782/how-to-check-if-the-number-is-integer}{Stackoverflow thread}
try2int <- function(obj, tol = .Machine$double.eps^0.5) {
  if (!is.numeric(obj)) stop("obj needs to be integer or double (numeric)")
  if (is.integer(obj)) return(obj)
  
  test <- FALSE
  
  bad <- if (length(na.omit(obj)) == 0) TRUE else 
    min(obj, na.rm = TRUE) == -Inf || max(obj, na.rm = TRUE) == Inf
  if (bad) {
    return(obj)
  } else {
    test <- max(abs(obj) %% 1, na.rm = TRUE) < tol
  }
  
  if (is.na(test) || is.null(test)) test <- FALSE
  
  if (test) return(as.integer(obj))
  
  return(obj)
  
}


#' @title Shift a variable to create lag or lead values
#' @author Joonas Miettinen
#' @description 
#' \strong{DEPRECATED}: 
#' Intended to do what \code{\link[data.table]{shift}} from
#' \pkg{data.table} does better since \pkg{data.table} 1.9.6. 
#' Shifts the values of a variable forwards or 
#' backwards to create lag or lead values. Takes a copy of the whole data
#' and returns a new copy with the shifted variable.
#' @export shift.var
#' @param data a \code{data.frame} or \code{data.table}
#' @param id.vars a character string vector of variable names; \code{id.vars} are used to identify unique subjects,
#' for which shifting is done separately; e.g. with a panel data where \code{region} refers to different regions that
#' all have their own time series, using \code{id.vars = "region"} shifts the time series for each region separately
#' @param shift.var a character string vector of length one; specifies the variable according to which \code{value.vars}
#' are shifted; e.g. \code{id.vars = "year"} means shifting forward or backward in years (given one has a var name \code{"year"})
#' @param value.vars a character string vector; specifies the names of variables whose values that are shifted
#' @param shift.value an integer; specifies the direction and extent of shifting; e.g. \code{shift.value = -1L} shifts
#' one row backwards (a lag of one row) and \code{shift.value = 2L} creates a two-row lead
shift.var <- function(data, id.vars = NULL, shift.var = NULL, value.vars=NULL, shift.value=-1L) {
  .Deprecated(new = "shift", msg = "popEpi's shift.var is deprecated in 0.3.0 and will be removed in the next release; please use e.g. data.table's shift() function")
  
  merge_var <- makeTempVarName(data, pre = "merge_var")
  if (is.null(shift.var)||is.null(value.vars)) stop("shift.var and value.vars cannot be NULL")
  all_names_present(data, c(id.vars, shift.var, value.vars))
  if (shift.value == 0L) return(data)
  
  if (is.data.table(data)) old_key <- key(data)
  
  data <- data.table(data)
  setkeyv(data, c(id.vars, shift.var))
  data[, (merge_var) := as.integer(as.factor(get(shift.var)))]
  
  if (any(duplicated(data, by=c(id.vars,shift.var,value.vars)))) {
    stop("some levels of shift.var are duplicated in data, so shifting is not possible")
  }
  
  lagdata <- data[,c(id.vars, merge_var, value.vars), with=FALSE]
  lagdata[, (merge_var) := get(merge_var) - shift.value]
  
  if (shift.value<=0) {vn <- "lag"}
  if (shift.value >0) {vn <- "lead"}
  
  setnames(lagdata, value.vars, paste0(vn, abs(shift.value),"_", value.vars))
  
  setkeyv(data, c(id.vars, merge_var))
  setkeyv(lagdata, c(id.vars, merge_var))
  data <- lagdata[data]
#   data <- merge(data, lagdata, all.x=TRUE, all.y=FALSE, by = c(id.vars, merge_var))

  data[, (merge_var) := NULL]
  setkeyv(data, old_key)
  return(data[])
}



#' @title Get rate and exact Poisson confidence intervals
#' @author epitools
#' @description Computes confidence intervals for Poisson rates
#' @param x observed
#' @param pt expected
#' @param conf.level alpha level
#' 
#' @export poisson.ci
#' 
#' 
#' @examples
#' 
#' poisson.ci(x = 4, pt = 5, conf.level = 0.95)
#'
poisson.ci <- function(x, pt = 1, conf.level = 0.95) {
  xc <- cbind(x, conf.level, pt)
  pt2 <- xc[, 3]
  results <- matrix(NA, nrow(xc), 6)
  f1 <- function(x, ans, alpha = alp) {
    ppois(x, ans) - alpha/2
  }
  f2 <- function(x, ans, alpha = alp) 1 - ppois(x, ans) + dpois(x, ans) - alpha/2
  for (i in 1:nrow(xc)) {
    alp <- 1 - xc[i, 2]
    interval <- c(0, xc[i, 1] * 5 + 4)
    uci <- uniroot(f1, interval = interval, x = xc[i, 1])$root/pt2[i]
    if (xc[i, 1] == 0) {
      lci <- 0
    }
    else {
      lci <- uniroot(f2, interval = interval, x = xc[i,1])$root/pt2[i]
    }
    results[i, ] <- c(xc[i, 1], pt2[i], xc[i, 1]/pt2[i], lci, uci, xc[i, 2])
  }
  coln <- c("x", "pt", "rate", "lower", "upper", "conf.level")
  colnames(results) <- coln
  data.frame(results)
}


#' @title Delete \code{data.table} columns if there
#' @author Joonas Miettinen
#' @description Deletes columns in a \code{data.table} conveniently.
#' May only delete columns that are found silently. Somestimes useful in e.g.
#' \code{on.exit} expressions.
#' @param DT a \code{data.table}
#' @param delete a character vector of column names to be deleted
#' @param keep a character vector of column names to keep; 
#' the rest will be removed; \code{keep} overrides \code{delete}
#' @param colorder logical; if \code{TRUE}, also does \code{setcolorder} using
#' \code{keep}
#' @param soft logical; if \code{TRUE}, does not cause an error if any variable
#' name in \code{keep} or \code{delete} is missing; \code{soft = FALSE} useful 
#' for programming sometimes
#' 
#' 
#' @export setcolsnull
setcolsnull <- function(DT=NULL, delete=NULL, keep=NULL, colorder=FALSE, soft=TRUE) {
  if (!is.data.table(DT)) stop("not a data.table")
  if (!soft) {
    all_names_present(DT, keep)
    all_names_present(DT, delete)
  }
  del_cols <- NULL
  del_cols <- intersect(delete, names(DT))
  if (!is.null(keep)) {
    del_cols <- setdiff(names(DT), keep)
  }
  if (length(del_cols) > 0) {
    set(DT, j = (del_cols), value = NULL)
  }
  if (colorder) {
    setcolorder(DT, intersect(keep, names(DT)))
  }
  return(invisible())
}






#' @title Coerce a \code{ratetable} Object to Class \code{data.frame}
#' @description
#' \code{ratatable} objects used in e.g. \pkg{survival} and \pkg{relsurv}
#' can be conveniently coerced to a long-format \code{data.frame}.
#' However, the names and levels of variables in the result
#' may not match names and levels of variables in your data.
#' @author Joonas Miettinen
#' @param x a \code{ratetable}
#' @param ... unused but added for compatibility with \code{as.data.frame}
#' @examples
#' library(relsurv)
#' data(slopop)
#' df <- as.data.frame(slopop)
#' head(df)
#' @seealso 
#' \code{\link[survival]{ratetable}}, 
#' \code{\link{as.data.table.ratetable}}
#'
#' @export
as.data.frame.ratetable <- function(x, ...) {
  dimids <- attr(x, "dimid")
  x <- as.data.frame.table(as.table(as.array(x)))
  names(x) <- c(dimids, "haz")
  x[]
}


#' @title Coerce a \code{ratetable} Object to Class \code{data.table}
#' @author Joonas Miettinen
#' 
#' @description
#' \code{ratatable} objects used in e.g. \pkg{survival} and \pkg{relsurv}
#' can be conveniently coerced to a long-format \code{data.frame}.
#' However, the names and levels of variables in the result
#' may not match names and levels of variables in your data.
#' @param x a \code{ratetable}
#' @param ... other arguments passed on to \code{as.data.table}

#' @seealso 
#' \code{\link[survival]{ratetable}}, 
#' \code{\link{as.data.frame.ratetable}}
#'
#' @examples
#' library(relsurv)
#' data(slopop)
#' dt <- as.data.table(slopop)
#' dt
#' @export
as.data.table.ratetable <- function(x, ...) {
  dimids <- attr(x, "dimid")
  x <- as.data.table(as.table(as.array(x)), ...)
  x[, names(x) := lapply(.SD, robust_values, messages = FALSE, force = FALSE)]
  setnames(x, c(dimids, "haz"))
  x[]
}


#' @title \strong{Experimental}: Coerce a long-format \code{data.frame} to a \code{ratetable} object
#' @author Joonas Miettinen
#' @description Coerces a long-format \code{data.frame} of population hazards
#' to an array, and in turn to a \code{\link[survival]{ratetable}},
#' which can be used in e.g. \pkg{survival}'s expected survival computations
#' and \pkg{relsurv}'s relative survival computations.
#' @param DF a \code{data.frame}
#' @param value.var name of values variable in quotes
#' @param by.vars names vector of variables by which to create (array) dimensions
#' @seealso 
#' \code{\link[survival]{ratetable}}, 
#' \code{\link{as.data.table.ratetable}}, 
#' \code{\link{as.data.frame.ratetable}}
#'
longDF2ratetable <- function(DF, value.var = "haz", by.vars = setdiff(names(DF), value.var)) {
  univals <- lapply(DF[, by.vars], unique)
  names(univals) <- NULL
  dimvec <- sapply(DF[,by.vars], function(x) {length(unique(x))},
                   simplify=TRUE)
  ar <- array(DF[, value.var], dim = dimvec)
  dimnames(ar) <- univals
  attr(ar, "class") <- "ratetable"
  attr(ar, "dimid") <- colnames(DF)
  ar
}


#' @import stats
makeTempVarName <- function(data=NULL, names=NULL, pre=NULL, post=NULL) {
  DN <- NULL
  DN <- c(DN, names(data))
  DN <- c(DN, names)
  DN <- unique(DN)
  
  if (is.null(DN)) stop("no data nor names defined")
  
  ra <- "V123456789"
  tvn <- paste0(pre, ra, post)
  for (k in tvn) {
    
    if (k %in% DN) {
      revo <- 1L
      while (tvn %in% DN) {
        if (revo >= 1000) {
          stop("wow, did not find a random unused variable name even after 1000 tries. Specify 'pre' and or 'post'?")
        }
        ra <- paste0("V", as.integer(1e9*runif(1)))
        tvn <- paste0(pre, ra, post)
        revo <- revo + 1L
      }
    }
    DN <- c(DN, k)
  }
  
  return(tvn)
}


setDFpe <- function(x) {
  ## intended to only be used to set data.table to data.frame in place
  ## when option("popEpi.datatable") == FALSE
  if (!is.data.table(x)) stop("only accepts data.table as input")
  
  cl <- class(x)
  wh <- which(cl == "data.table")
  cl = c(cl[1:(wh-1)], cl[(wh+1):length(cl)])
  setattr(x, "class", cl)
  
  setattr(x, "sorted", NULL)
  setattr(x, ".internal.selfref", NULL)
}



evalLogicalSubset <- function(data, substiset, n = 2, enclos = parent.frame(n)) {
  ## NOTE: subset MUST be substitute()'d before using this function!
  ## we allow substiset to be a logical condition only
  ## ALWAYS returns a logical vector of length nrow(data)
  
  substiset <- eval(substiset, envir = data, enclos = enclos)
  if (!is.null(substiset)) {
    if (!is.logical(substiset)) stop("Expression to subset by must be a logical condition, e.g. var1 == 0, var1 %in% 1:2, var1 > 0, etc.")
    substiset <- substiset & !is.na(substiset)
    if (sum(substiset) == 0) stop("zero rows in data after subset")
  } else {
    substiset <- rep(TRUE, nrow(data))
  }
  substiset
}


subsetGently <- function(dt, subset=NULL, select=NULL) {
  ## source:
  ## http://stackoverflow.com/questions/10790204/how-to-delete-a-row-by-reference-in-r-data-table/10791729#10791729
  ## intended for sparing memory, may be slower due to evaluating subset
  ## multiple times
  
  ## - dt must be a data.table or a data.frame
  ## - subset must be already evaluated into a logical vector using e.g.
  ##   substitute & evalLogicalSubset
  ## - retains attributes
  
  if (!is.data.frame(dt)) stop("dt must be a data.table/data.frame")
  if (!is.logical(subset)) stop("subset must be logical condition, e.g. var1 > 0")
  
  if (is.null(select)) {
    select <- names(dt)
  } else {
    all_names_present(dt, select)
  }
  
  sdt <- data.table(dt[[select[1]]][subset])
  setnames(sdt, 1, select[1])
  if (length(select) > 1) {
    alloc.col(sdt, n = length(select) + 100L)
    for (k in select[-1]) {
      set(sdt, j = k, value = dt[[k]][subset])
    }
  }
  sdt
  
}


setDT2DF <- function(x) {
  if (!is.data.table(x)) stop("only accepts data.table as input")
  
  cl <- class(x)
  cl <- setdiff(cl, "data.table")
  setattr(x, "class", cl)  
  setattr(x, "sorted", NULL)
  setattr(x, ".internal.selfref", NULL)
  invisible(x)
}

setDF2DT <- function(x) {
  if (!is.data.frame(x) || is.data.table(x)) stop("only accepts data.frame as input")
  
  cl <- class(x)
  whDF <- which(cl == "data.frame")
  cl <- c(cl[1:(whDF-1)], "data.table", "data.frame", cl[whDF:length(cl)])
  
  setattr(x, "class", cl)
  alloc.col(x)
  
  invisible(x)
}




p.round <- function(p, dec=3) {
  th <- eval( parse(text=paste0('1E-', dec ) ))
  if( is.null(p)) return( '= NA') 
  if( is.na(p))   return( '= NA') 
  if( p < th ){
    p <- paste0('< ', th  )
  } else {
    p <- paste0('= ', round(p, dec) )
  }
  p 
}



evalPopArg <- function(data, arg, n = 1L, DT = TRUE, enclos = NULL, recursive = TRUE) {
  ## arg: an unevaluated AND substitute()'d argument within a function, which may be
  ## * an expression
  ## * a list of expressions
  ## * a character vector of variable names (in a given data set)
  ## n: steps upstream as in parent.frame(n); 0L refers to calling environment
  ## of evalPopArg, 1L to calling environment of e.g. sir which uses evalPopArg, etc.
  ## hence n = 1L should be almost always the right way to go.
  ## ALTERNATIVELY supply an environment by hand via enclos.
  ## enclos will override n.
  ## recursive: if TRUE, evals arg as many times as it is of type language.
  ## output:
  ## * vector as a result of an expression
  ## * list as a result of a list
  ## * character vector of names
  ## OR with DT = TRUE, a data.table based on aforementioned results.
  ## intention: output to be used in by argument of data.table.
  ## a data.table output is directly usable in by.
  ## if column names cannot be easily found, BV1, BV2, ... are imputed
  ## for missing names (unrobustly: such names may already exist, resulting in duplicates)
  
  if (!is.null(enclos) && !is.environment(enclos)) stop("enclos must be NULL or an environment")
  if (!is.environment(enclos)) enclos <- parent.frame(n + 1L)
  
  e <- eval(arg, envir = data, enclos = enclos)
  if (is.language(e) && !inherits(e, "formula")) {
    if (!recursive) stop("arg is of type language after evaluating, and recursive = FALSE")
    
    tick <- 1L
    while (is.language(e) && !inherits(e, "formula") && tick < 100L) {
      arg <- e
      e <- eval(arg, envir = data, enclos = enclos)
      tick <- tick + 1L
    }
    if (tick == 100L) stop("arg was of type language even after 100 evaluations. Something went wrong here...")
    
    
    
  } 
  argType <- "NULL"
  if (is.list(e)) argType <- "list" else 
    if (is.character(e)) argType <- "character" else 
      if (is.vector(e) || is.factor(e)) argType <- "expression" else 
        if (inherits(e, "formula")) argType <- "formula"
  
  if (argType == "NULL") return(NULL)
  
  av <- all.vars(arg)
  if (argType == "character") av <- e
  
  ## byNames: names of columns resulting from aggre argument, by which
  ## pyrs and such are aggregated. same functionality
  ## as in results seen in e.g.DT[, .N, by = list(factor(x), y, z = w)] ## factor, y, z
  ## note: first object in ags with list or expression aggre is "list"
  byNames <- NULL
  
  if (is.character(e)) byNames <- e
  else if (argType == "list" && substr(paste0(deparse(arg)), 1, 5) == "list(") byNames <- sapply(arg[-1], function(x) all.names(x)[1]) 
  else if (argType == "expression") byNames <- all.names(arg)[1]
  
  badNames <- c("$", ":")
  
  byNames[byNames %in% badNames] <- paste0("BV", 1:length(byNames))[byNames %in% badNames]
  
  
  if (argType == "formula") {
    
    e <- RHS2DT(formula = e, data = data, enclos = enclos)
      
  } else if (is.character(e)) {
    all_names_present(data, e)
    if (DT) {
      ## note: e contains variable names in character strings,
      ## ergo fully named list & DT created
      l <- lapply(e, function(x) data[[x]])
      setattr(l, "names", e)
      setDT(l)
      e <- l; rm(l)
    }
  } else if (is.list(e)) {
    ## note: fully unnamed list has NULL names()
    ## partially named list has some "" names
    ne <- names(e)
    
    if (DT && any(sapply(e, is.null))) stop("at least one object in list arg is NULL; cannot form data.table with such list")
    
    if (is.null(ne)) ne <- rep("", length(e))
    
    
    wh_bad <- which(ne == "")
    if (length(wh_bad) > 0) {
      if (is.null(byNames)) {
        byNames <- paste0("BV", 1:length(e))
      }
      
      ne[wh_bad] <- byNames[wh_bad]
      setattr(e, "names", ne)
    }
    
    if (DT) {
      ## NOTE: used to be setDT, but length of different elements
      ## in list may differ, which as.data.table handles correctly
      e <- as.data.table(e)
    }
  } else if ((is.vector(e) || is.factor(e))) {
    ## is e.g. a numeric vector or a factor
    if (DT) {
      e <- data.table(V1 = e)
      setnames(e, 1, byNames)
    }
  }
  
  ## NOTE: e may be of type language at this point if arg was double-quoted
  ## and recursive = FALSE
  
  if (DT) {
    if (any(duplicated(names(e))))  warning("Some column names are duplicated in data.table output by evalPopArg; make sure you are not using the same variable names in the same e.g. list of expressions or vector of variable names. If that is not the case, complain to the package maintainer")
    setDT(e)
    setattr(e, "all.vars", av)
    setattr(e, "quoted.arg", arg)
    setattr(e, "arg.type", argType)
  }
  e
}


popArgType <- function(arg, data = NULL, n = 1L, enclos = NULL, recursive = TRUE) {
  ## input: a substitute()'d expression / argument
  ## NOTE: recursive useful when arg might be quoted twice and want the eventual
  ## result; need to supply data for it though
  ## output: type of thingie that was substitute()'d
  ##  * list (of expressions)
  ##  * character string vector
  ##  * an expression (includes symbol)
  av <- all.vars(arg, unique = TRUE) ## all variables
  av <- setdiff(av, c("$", "T", "F"))
  an <- all.names(arg, unique = TRUE) ## all variables and functions
  af <- setdiff(an, av) ## all functions used
  
  a <- deparse(arg)
  a <- paste0(a, collapse = "") ## lists may somehow produce length > 1 here
  if (substr(a, 1, 5) == "list(") return("list")
  if (a == "NULL") return("NULL")
  ## detection of character arguments is not easy and should not be considered
  ## fool proof since user may pass e.g. a vector of character strings as a 
  ## symbol, which can only really be interpreted as an expression
  if (sum(grep('\\"', a)) && length(setdiff(af, "c")) == 0) return("character")
  
  if (is.data.frame(data)) {
    if (is.symbol(arg) && a %in% names(data)) return("expression")
    if (length(av) == 1L && av %in% names(data)) return("expression")
    e <- eval(arg, envir = data[1:min(nrow(data), 20L), ], 
              enclos = if (is.environment(enclos)) enclos else parent.frame(n + 1L))
    if (inherits(e, "formula")) return("formula")
    if (is.null(e)) return("NULL")
    if (is.list(e)) return("list")
    if (is.character(e) && all(e %in% names(data))) return("character")
    if (is.vector(e) || is.factor(e)) return("expression")
    
    if (recursive && is.language(e)) return(popArgType(e, data = data, n = n + 1L, enclos = enclos))
  }
  
  "expression"
  
}

cutLow <- function(x, breaks, tol =  .Machine$double.eps^0.5) {
  ## a cut function that returns the lower bounds of the cut intervals (as numeric) as levels
  
  breaks <- sort(breaks)
  x <- cut(x + tol, right = FALSE, breaks = breaks, labels = FALSE)
  x <- breaks[-length(breaks)][x]
  x
}




cutLowMerge <- function(x, y, by.x = by, by.y = by, by = NULL, all.x = all, all.y = all, all = FALSE, mid.scales = c("per", "age"), old.nums = TRUE) {
  ## INTENTION: merges y to x by by.x & by.y after cutLow()'ing appropriate
  ## variables in x so that y's values match with x's values
  ## requirements;
  ## * numeric variables in y correspond to lower limits of some intervals OR
  ##   are group variables (e.g. sex = c(0,1))
  ## inputs: two datas as in merge, preferably both data.table, and other args
  ## to merge()
  ## output: a data.table where y has been merged to x after cutLow()
  ## example: merging popmort to a split Lexis object, where popmort's variables
  ## correspond to at least some Lexis time scales
  ## old.nums: return old numeric variable values used in cutLow()'ing?
  ## mid.scales: use mid-point of interval when merging by these Lexis time scales
  ## computed by adding + 0.5*lex.dur, which must exist
  
  if ((is.null(by.x) && !is.null(by.y)) || (!is.null(by.x) && is.null(by.y))) stop("one but not both of by.x / by.y is NULL")
  if (!is.null(by)) by.x <- by.y <- by 
  
  if (length(by.x) != length(by.y)) stop("lengths differ for by.y & by.x")
  all_names_present(x, by.x)
  all_names_present(y, by.y)
  names(by.x) <- by.y
  names(by.y) <- by.x
  
  if (length(mid.scales)>0) all_names_present(x, c("lex.dur", mid.scales))
  
  whScale <- by.x %in% mid.scales
  xScales <- by.x[whScale]
  yScales <- by.y[whScale]
  
  if (length(yScales) > 0) {
    
    oldVals <- copy(with(x, mget(xScales)))
    on.exit(set(x, j = xScales, value = oldVals))
    setattr(oldVals, "names", yScales)
    
    for (yVar in yScales) {
      xVar <- xScales[yVar]
      xBr <- sort(unique(y[[yVar]]))
      xBr <- unique(c(xBr, Inf))
      set(x, j = xVar, value = cutLow(x[[xVar]] + x$lex.dur*0.5, breaks = xBr))
    }
    
  }
  xKey <- key(x)
  if (length(xKey) == 0 && is.data.table(x)) {
    xKey <- makeTempVarName(x, pre = "sort_")
    on.exit(setcolsnull(x, delete = xKey, soft = TRUE), add = TRUE)
    on.exit(setcolsnull(z, delete = xKey, soft = TRUE), add = TRUE)
    x[, (xKey) := 1:.N]
  }
  
  xClass <- class(x)
  on.exit(setattr(x, "class", xClass), add = TRUE)
  if (is.data.table(x)) setattr(x, "class", c("data.table", "data.frame"))
  z <- merge(x, y, by.x = by.x, by.y = by.y, all.x = all.x, all.y = all.y, all = all, sort = FALSE)
  
  setDT(z)
  setcolorder(z, c(names(x), setdiff(names(z), names(x))))
  if (old.nums) set(z, j = xScales, value = oldVals)
  if (length(xKey) > 0) setkeyv(z, xKey)
  z[]
  
}


getOrigin <- function(x) {
  ## input: Date, IDate, or date variable
  ## output: the origin date in Date format,
  ## the origin date being the date where the underlying index is zero.
  if (inherits(x, "Date") || inherits(x, "IDate")) {
    as.Date("1970-01-01")
  } else if (inherits(x, "date")) {
    as.Date("1960-01-01")
  } else if (inherits(x, "dates")) {
    as.Date(paste0(attr(x, "origin"), collapse = "-"), format = "%d-%m-%Y")
  } else {
    stop("class '", class(x), "' not supported; usage of Date recommended - see ?as.Date")
  }
  
}


setcols <- function(x, j, value) {
  ## intention: add new columns to DT via modifying in place, and to DF
  ## via DF$var <- value; both conserve memory (don't take copy of whole data)
  
  if (!is.data.frame(x)) stop("x must be a data.frame")
  if (!is.list(value)) stop("value must be a list of values (columns to add)")
  if (missing(j)) j <- names(value)
  
  if (!is.data.table(x)) {
    x[j] <- value
  } else {
    set(x, j = j, value = value)
  }
  x
}



`%.:%` <- function(x, y) {
  ## INTENTION: hacking formula calls using `:`
  ## which is apparently normally evaluated in C... (in model.matrix.default)
  ## USAGE: e.g. c(1,2) %.:% c(3,4) = c(3, 8)
  ## (instead of getting warning)
  if (length(x) > 1L && length(y) > 1L && is.numeric(x) && is.numeric(y)) {
    return(x*y)
  }
  as.factor(x):as.factor(y)
  
}



RHS2list <- function(formula) {
  ## INTENTION: turns the right-hand side of a formula
  ## into a list of substituted expressions;
  ## each element in list is an expressions separated
  ## by a '+' in the formula. needs to be eval()'d,
  ## preferably using the appropriate data set.
  if (!inherits(formula, "formula")) stop("not a formula")
  
  ## no response
  formula <- formula[c(1, length(formula))]
  
  te <- terms(formula)
  tl <- attr(te, "term.labels")
  ## to avoid e.g. c(1,2):c(3,4) NOT evaluating as c(3, 8)
  l <- lapply(tl, function(x) gsub(pattern = ":", x = x, replacement = "%.:%"))
  
  ## only want to substitute variables mentioned in formula (and not e.g.
  ## functions that are by coincidence the same name)
  ne <- new.env()
  oe <- attr(te, ".Environment")
  for(obj in intersect(all.vars(formula), ls(oe))) {
    assign(envir = ne, x = obj, value = eval(parse(text = paste0("oe$", obj))))
  }
  
  
  l <- lapply(l, function(x) eval(parse(text = paste0("substitute(", x, ", env = ne)"))))
  names(l) <- tl
  l
}

RHS2DT <- function(formula, data = data.frame(), enclos = parent.frame(1L)) {
  l <- RHS2list(formula)
  l <- lapply(l, function(x) eval(expr = x, envir = data, enclos = enclos))
  l <- data.table(l)
  l
}

Surv2DT <- function(Surv) {
  sa <- attributes(Surv)
  dt <- copy(Surv)
  setattr(dt, "class", "array")
  dt <- data.table(dt)
  
  type <- attr(Surv, "type")
  statNA <- sum(is.na(dt$status))
  if (statNA) 
    stop("Some status indicators (", statNA  ," values in total) were NA as a result of using Surv(). Usual suspects: original status variable has NA values, or you have numeric status variable with more than two levels and you did not assign e.g. type = 'mstate' (e.g. Surv(time = c(1,1,1), event = c(0,1,2), type = 'mstate') works).")
  
  
  setattr(dt, "type", type)
  testClass <- sa$inputAttributes$time2$class
  if (!is.null(testClass) && testClass == "factor") dt[, status := factor(status, labels = sa$inputAttributes$time2$levels)]
  testClass <- sa$inputAttributes$event$class
  if (!is.null(testClass) && testClass == "factor") dt[, status := factor(status, labels = sa$inputAttributes$event$levels)]
  
  dt[]
}

promptYN <- function(q) {
  
  rl <- readline(prompt = paste0(q, " (Y/N) ::: "))
  y <- c("y", "Y")
  n <- c( "n", "N")
  if (!rl %in% c(y,n)) {
    cat("Answer must be one of the following (without ticks):", paste0("'",c(y, n),"'", collapse = ", "))
    promptYN(q = q)
  }
  
  if (rl %in% y) TRUE else FALSE
  
}

#' @title Adjust Estimates by Categorical Variables
#' @description This function is only intended to be used within a formula
#' when supplied to e.g. \code{\link{survtaq_ag}} and should not be
#' used elsewhere. 
#' @return Returns a list of promises of the variables supplied which can be
#' evaluated.
#' @examples 
#' 
#' y ~ x + adjust(z)
#' @export
adjust <- function(...) {
  
  call <- sys.call(1L)
  call <- as.list(call)[1L]
  
  if (deparse(call) %in% c("adjust", "list(adjust)")) stop("Function adjust() only intended to be used within the formulas of certain functions of package popEpi. See e.g. ?survtab_ag for usage.")
  
  mc <- as.list(match.call())[-1L]
  mc
}

evalPopFormula <- function(formula, data = data.frame(), enclos = parent.frame(2L), subset = NULL, Surv.response = TRUE) {
  
  ## INTENTION: given a formula object, returns a DT where each column
  ## is an evaluated expression from formula (separated by  + )
  
  fe <- environment(formula)
  
  ## subset if needed ----------------------------------------------------------
  subset <- evalLogicalSubset(data, substitute(subset), enclos = enclos)
  if (!all(subset)) {
    data <- data[subset, ]
    setcolsnull(data, keep = all.vars(formula))
  }
  
  
  ## formula -------------------------------------------------------------------
  if (!inherits(formula, "formula")) stop("formula is not of class 'formula'; supply it as e.g. y ~ x")
  if (length(formula) < 3L) stop("formula appears to be one-sided, which is not supported; supply it as e.g. y ~ x")
  
  ## response
  y <- eval(formula[[2L]], envir = data, enclos = fe)
  if (inherits(y, "Surv") && !Surv.response) stop("Response is a result of using Surv(), which is not allowed in this context.")
  if (Surv.response) {
    if (!inherits(y, "Surv")) stop("the response of the formula must be a Surv object; see ?Surv (in package survival)")
    y <- Surv2DT(y)
    setcolsnull(y, keep = c("time", "start", "status"), colorder = TRUE)
    if (!any(c("time", "start") %in% names(y))) stop("Surv must have a 'time' argument")
    setnames(y, names(y), c("time", "status")[1:ncol(y)])
  } else {
    y <- data.table(y)
    setnames(y, deparse(formula[[2L]]))
  }
  
  
  ## RHS
  l <- RHS2list(formula)
  
  ## adjusting variables? ------------------------------------------------------
  adj <- names(l)[substr(names(l), 1, 6) == "adjust"]
  
  if (length(adj)) {
    adj <- l[adj]
    l <- l[setdiff(names(l), names(adj))]
    
    ## if e.g. used multiple adjust() calls in formula
    ## (adj may be a list of multiple adjust() expressions)
    
    adj <- lapply(adj, eval)
    adj <- unlist(adj, recursive = FALSE)
    names(adj) <- sapply(adj, function(x) deparse(x))
    # names(adj) <- paste0("adjust(", names(adj), ")")
    
    l <- c(l, adj)
  }
  
  l <- lapply(l, eval, envir = data, enclos = enclos)
  l <- as.data.table(l)
  
  # setnames(y, names(y), makeTempVarName(names = c(names(data), names(l)), pre = names(y)))
  l <- if (length(l) > 0L) cbind(y, l) else y
  
  setattr(l, "adjust.names", names(adj))
  setattr(l, "print.names", setdiff(names(l), c(names(adj), names(y))))
  setattr(l, "Surv.names", names(y))
  setattr(l, "formula", formula)
  
  l
}

oneWhitespace <- function(x) {
  if (!is.character(x)) stop("x not a character")
  x <- paste0(x, collapse = " ")
  while(sum(grep(pattern = "  ", x = x))) {
    x <- gsub(pattern = "  ", replacement = " ", x = x)
  }
  x
}



evalRecursive <- function(arg, env, enc, max.n = 100L) {
  ## INTENTION: digs out actual evaluatable value and expression
  
  if (missing(env)) env <- environment()
  if (missing(enc)) enc <- parent.frame(1L)
  
  if (is.data.frame(env)) {
    na <- names(env)
    env <- env[1:(min(10L, nrow(env))), ]
    
    env <- data.frame(env)
    setattr(env, "names", na)
    
  }
  argSub <- arg
  
  tick <- 1L
  while (is.language(arg) && !inherits(arg, "formula") && tick < max.n) {
    
    argSub <- arg
    arg <- eval(argSub, envir = env, enclos = enc)
    
    tick <- tick + 1L
  }
  
  if (tick == max.n) stop("evaluated expression ", max.n, " times and still could not find underlying expression")
  if (!is.language(argSub)) argSub <- substitute(arg)
  list(arg = arg, argSub = argSub, all.vars = all.vars(argSub))
}


usePopFormula <- function(form = NULL, adjust = NULL, data = data.frame(), enclos, Surv.response = TRUE) {
  ## INTENTION: evaluates form and combines with adjust appropriately
  # formSub <- substitute(form)
  al <- evalRecursive(arg = form, env = data, enc = enclos)
  
  if (!inherits(al$arg, "formula")) stop("'form' is not a formula object")
  
  dt <- evalPopFormula(formula = al$arg, data = data, enclos = enclos, Surv.response = Surv.response)
  adNames <- attr(dt, "adjust.names")
  prNames <- attr(dt, "print.names")
  suNames <- attr(dt, "Surv.names")
  
  
  adjust <- evalPopArg(data, substitute(adjust), DT = TRUE, recursive = TRUE, enclos = enclos)
  if (!is.null(adjust) && ncol(adjust) > 0L && length(adNames) > 0L) stop("Cannot both use argument 'adjust' AND use an adjust() term within the formula argument. Please only use one.")
  if (is.null(adjust) && length(adNames) > 0L) adjust <- dt[, .SD, .SDcols = c(adNames)]
  
  print <- dt[]
  
  list(y = dt[, .SD, .SDcols = c(suNames)], 
       print = dt[, .SD, .SDcols = c(prNames)], 
       adjust = adjust, formula = al$arg)
}



