

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
  if (is.null(data) || nrow(data) == 0L) stop("data NULL or has no rows")
  
  if (is.null(columns)) stop("columns cannot be NULL")
  
  msg <- paste0("Missing 'columns' variables: %%VARS%%")
  all_names_present(data, columns, msg = msg)
  msg <- paste0("Missing 'rows' variables: %%VARS%%")
  all_names_present(data, rows, msg = msg)
  msg <- paste0("Missing 'values' variables: %%VARS%%")
  all_names_present(data, values, msg = msg)
  
  ## allow rows = NULL 
  rowsNULL <- FALSE
  if (is.null(rows)) rowsNULL <- TRUE
  if (rowsNULL) rows <- "1"
  
  ## sometimes rows names appear to be like expressions, e.g. 'factor(V1)'
  ## (and this function only uses string-column-names, so that's fine.)
  actualRows <- rows
  if (length(rows) > 1L || rows != "1") {
    rows <- makeTempVarName(names = c(names(data), columns), 
                            pre = paste0("RN", 1:length(rows)))
    on.exit(setnames(data, rows, actualRows), add = TRUE)
    setnames(data, actualRows, rows)
  }
  ## same for cols
  actualCols <- columns
  columns <- makeTempVarName(names = c(names(data), rows), 
                             pre = paste0("CN", 1:length(columns)))
  on.exit(setnames(data, columns, actualCols), add = TRUE)
  setnames(data, actualCols, columns)
  
  form <- paste0(paste0(rows, collapse = " + "), " ~ ", 
                 paste0(columns, collapse = " + "))
  form <- as.formula(form)
  
  ## note: dcast probably usually finds the methods for data.frame / data.table,
  ## but this method is more certain
  if (is.data.table(data)) {
    d <- dcast.data.table(data, formula = form, value.var=values, 
                          drop=FALSE, fun.aggregate=sum)[]
  } else {
    d <- dcast(data, formula = form, value.var = values, 
               drop = FALSE, fun.aggregate = sum)[]
  }
  if (rowsNULL) set(d, j = names(d)[1L], value = NULL)
  wh_rows <- which(rows %in% names(d))
  if (sum(wh_rows, na.rm = TRUE)) setnames(d, rows[wh_rows], actualRows[wh_rows])
  
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
#' library("data.table")
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
#' Special: include \code{\%\%VARS\%\%} in message string and the missing 
#' variable names will be inserted there (quoted, separated by comma, e.g. 
#' \code{'var1'}, \code{'var2'} --- no leading or tracing white space). 
#' @description Given a character vector, checks if all names are present in \code{names(data)}.
#' Throws error if \code{stops=TRUE}, else returns \code{FALSE} if some variable name is not present.
#' @seealso
#' \code{\link{robust_values}}
#' @export all_names_present

all_names_present <- function(data, var.names, stops = TRUE, msg = NULL) {
  
  if (!is.null(var.names) && !is.character(var.names)) {
    stop("Argument 'var.names' must be NULL or a character vector of ",
         "variable names.")
  }
  if (length(var.names) && any(is.na(var.names))) {
    stop("There are ", sum(is.na(var.names)), " missing values in argument ",
         "'var.names'. Please only supply non-NA values.")
  }
  
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
#' May only delete columns that are found silently. Sometimes useful in e.g.
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
    all_names_present(DT, keep, msg = "Expected")
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
#' if (requireNamespace("relsurv", quietly = TRUE)) {
#'   data(slopop, package = "relsurv")
#'   df <- as.data.frame(slopop)
#'   head(df)
#' }

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
#' if (requireNamespace("relsurv", quietly = TRUE)) {
#'   library("data.table")
#'   data(slopop, package = "relsurv")
#'   dt <- as.data.table(slopop)
#'   dt
#' }

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

temp_var_names <- function(n = 1L, avoid = NULL, length = 10L) {
  ## INTENTION: make temporary variable names that don't exist in
  ## char vector "avoid", e.g. avoid = names(data).
  if (n < 1L || !is.integer(n)) {
    stop("n must an integer > 0")
  }
  if (length < 1L || !is.integer(length)) {
    stop("length must an integer > 0")
  }
  if (!is.null(avoid)) avoid <- as.character(avoid)
  
  pool <- c(0:9, letters, LETTERS)
  
  formTemp <- function(int) {
    v <- sample(x = pool, size = length, replace = TRUE)
    paste0(v, collapse = "")
  }
  
  l <- lapply(1:n, formTemp)
  dupll <- duplicated(l) | l %in% avoid
  tick <- 1L
  while (any(dupll) && tick <= 100L) {
    l[dupll] <- lapply(1:sum(dupll), formTemp)
    dupll <- duplicated(l) | l %in% avoid
    tick <- tick + 1L
  }
  if (tick >= 100L) {
    stop("ran randomization 100 times and could not create unique temporary",
         " names. Perhaps increase length?")
  }
  unlist(l)
}

#' @import stats
makeTempVarName <- function(data=NULL, names=NULL, 
                            pre=NULL, post=NULL, length = 10L) {
  DN <- NULL
  DN <- c(DN, names(data))
  DN <- c(DN, names)
  DN <- unique(DN)
  
  if (length(pre) != length(post) && length(post) > 0L && length(pre) > 0L) {
    stop("Lengths of arguments 'pre' and 'post' differ (", length(pre), " vs. ",
         length(post), "). (Tried to create temporary variables, so this is ",
         "most likely an internal error and the pkg maintainer should be ",
         "complained to.)")
  }
  useN <- max(length(pre), length(post), 1L)
  useL <- length
  tv <- temp_var_names(avoid = DN, n = useN, length = useL)
  tv <- paste0(pre, tv, post)
  tv
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


subsetDTorDF <- function(data, subset=NULL, select=NULL) {
  ## INTENTION: subsetting either a data.table or a data.frame
  ## and returning only selected variables for lazy people.
  if (!is.data.frame(data)) stop("data must be a data.table/data.frame")
  if (!is.logical(subset) && !is.null(subset)) stop("subset must be a logical vector or NULL")
  
  if (is.null(select)) {
    select <- names(data)
  } else {
    all_names_present(data, select)
  }
  
  e <- "data["
  if (!is.null(subset) && !all(subset)) e <- paste0(e, "subset") 
  if (!is.null(select) && (length(select) < names(data) || any(select != names(data)))) {
    e <- paste0(e, ", eval(select)")
    if (is.data.table(data)) e <- paste0(e, ", with = FALSE")
  }
  e <- paste0(e, "]")
  
  e <- parse(text = e)
  
  eval(e)
  
}

subsetRolling <- function(data, subset = NULL, select = NULL) {
  ## INTENTION: subsets a data.table column by column and by deleting columns
  ## in the old data.table.
  if (!is.data.table(data)) stop("data must be a data.table")
  if (!is.logical(subset)) stop("subset must be a logical vector")
  
  if (is.null(select)) {
    select <- names(data)
  } else {
    all_names_present(data, select)
  }
  
  if (length(select) == 0L) stop("select is of length zero, which would delete all columns in data")
  
  setcolsnull(data, keep = select)
  
  dt <- data[subset, select[1L], with = FALSE]
  
  setcolsnull(data, delete = select[1L])
  select <- select[-1L]
  
  for (v in select) {
    set(dt, j = v, value = data[[v]][subset])
    set(data, j = v, value = NULL)
  }
  
  rm(list = deparse(substitute(data)), envir = parent.frame(1L))
  
  dt
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


cutLow <- function(x, breaks, tol =  .Machine$double.eps^0.5) {
  ## a cut function that returns the lower bounds of the cut intervals (as numeric) as levels
  
  breaks <- sort(breaks)
  x <- cut(x + tol, right = FALSE, breaks = breaks, labels = FALSE)
  x <- breaks[-length(breaks)][x]
  x
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
  
  if (!is.data.table(x)) {
    stop("x must be a data.table")
  }
  
  if ((is.null(by.x) && !is.null(by.y)) || (!is.null(by.x) && is.null(by.y))) {
    stop("one but not both of by.x / by.y is NULL")
  }
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
  
  ## ensure x retains order (no copy taken of it)
  xKey <- key(x)
  if (length(xKey) == 0) {
    xKey <- makeTempVarName(x, pre = "sort_")
    on.exit(if ("x" %in% ls()) setcolsnull(x, delete = xKey, soft = TRUE), add = TRUE)
    on.exit(if ("z" %in% ls()) setcolsnull(z, delete = xKey, soft = TRUE), add = TRUE)
    x[, (xKey) := 1:.N]
  }
  
  if (any(duplicated(y, by = by.y))) {
    stop("y is duplicated by the inferred/supplied by.y variables (",
         paste0("'", by.y, "'", collapse = ", "), "). ",
         "First ensure this is not so before proceeding.")
  }
  
  ## avoid e.g. using merge.Lexis when x inherits Lexis
  xClass <- class(x)
  on.exit({
    setattr(x, "class", xClass)
    }, add = TRUE)
  setattr(x, "class", c("data.table", "data.frame"))
  
  ## return old numeric values of variables that were cutLow()'d
  ## by keeping them 
  if (old.nums && length(xScales)) {
    tmpXScales <- makeTempVarName(names = c(names(x), names(y)), pre = xScales)
    set(x, j = tmpXScales, value = oldVals)
    on.exit({
      xOrder <- setdiff(names(x), tmpXScales)
      setcolsnull(x, delete = xScales, soft = TRUE)
      setnames(x, tmpXScales, xScales)
      setcolorder(x, xOrder)
      
    }, add = TRUE)
  }
  
  ## merge
  z <- merge(x, y, by.x = by.x, by.y = by.y, 
             all.x = all.x, all.y = all.y, all = all, 
             sort = FALSE)
  
  setDT(z)
  if (old.nums && length(xScales)) {
    ## avoid warning due to coercing double to integer
    set(z, j = xScales, value = NULL)
    setnames(z, tmpXScales, xScales)
  }
  
  zOrder <- intersect(names(x), names(z))
  zOrder <- c(zOrder, setdiff(names(z), names(x)))
  setcolorder(z, zOrder)
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



oneWhitespace <- function(x) {
  if (!is.character(x)) stop("x not a character")
  x <- paste0(x, collapse = " ")
  while(sum(grep(pattern = "  ", x = x))) {
    x <- gsub(pattern = "  ", replacement = " ", x = x)
  }
  x
}


aliased_cols <- function(data, cols) {
  
  if (missing(cols)) cols <- names(data)
  all_names_present(data, cols)
  
  if (length(cols) < 2L) return(invisible())
  
  x <- with(data, mget(cols))
  x <- lapply(x, duplicated)
  
  sub_cols <- cols
  tl <- list()
  ## loop: each step reduce vector of names by one
  ## to avoid testing the same variables twice (in both directions)
  tick <- 0L
  aliased <- FALSE
  while (!aliased && length(sub_cols) > 1L && tick <= length(cols)) {
    
    currVar <- sub_cols[1L]
    sub_cols <- setdiff(sub_cols, currVar)
    tl[[currVar]] <- unlist(lapply(x[sub_cols], function(j) identical(x[[currVar]], j)))
    aliased <- sum(tl[[currVar]])
    
    tick <- tick + 1L
  }
  
  if (tick == length(cols)) warning("while loop went over the number of columns argument cols")
  
  ## result: list of logical vectors indicating if a column is aliased
  ## with other columns
  tl[vapply(tl, function(j) sum(j) == 0L, logical(1))] <- NULL
  
  if (length(tl) == 0L) return(invisible())
  
  ## take first vector for reporting
  var <- names(tl)[1L]
  aliases <- names(tl[[1L]])[tl[[1]]]
  aliases <- paste0("'", aliases, "'", collapse = ", ")
  stop("Variable '", var, "' is aliased with following variable(s): ", aliases, ".")
  
  invisible()
}







return_DT <- function() {
  
  x <- getOption("popEpi.datatable")
  if (!is.null(x) && !is.logical(x)) {
    stop("the option 'popEpi.datatable' must be either NULL or a logical ",
         "value (TRUE / FALSE).")
  }
  if (is.null(x) || isTRUE(x)) {
    return(TRUE)
  }
  return(FALSE)
  
}




#' @title Create a Lexis Object with Follow-up Time, Period, and Age
#' Time Scales
#' @description 
#' This is a simple wrapper around \code{\link[Epi]{Lexis}} for creating
#' a \code{Lexis} object with the time scales \code{fot}, \code{per},
#' and \code{age}.
#' @param data a \code{data.frame}; mandatory
#' @param birth the time of birth; A character string naming the variable in 
#' data or an expression to evaluate - see 
#' \link[=flexible_argument]{Flexible input}
#' @param entry the time at entry to follow-up; supplied the 
#' same way as \code{birth}
#' @param exit the time at exit from follow-up; supplied the 
#' same way as \code{birth}
#' @param entry.status passed on to \code{\link[Epi]{Lexis}} if not \code{NULL};
#' supplied the same way as \code{birth}
#' @param exit.status passed on to \code{\link[Epi]{Lexis}} if not \code{NULL};
#' supplied the same way as \code{birth}
#' @param subset a logical condition to subset by before passing data
#' and arguments to \code{\link[Epi]{Lexis}}
#' @param ... additional optional arguments passed on to 
#' \code{\link[Epi]{Lexis}}
#' @return 
#' A \code{Lexis} object with the usual columns that \code{Lexis} objects
#' have, with time scale columns \code{fot}, \code{per}, and \code{age}.
#' They are calculated as
#' 
#' \code{fot = entry - entry} (to ensure correct format, e.g. difftime)
#' 
#' \code{per = entry}
#' 
#' and 
#' 
#' \code{age = entry - birth}
#' 
#' @examples 
#' 
#' data("sire", package = "popEpi")
#' 
#' lex <- Lexis_fpa(sire, 
#'                  birth = "bi_date", 
#'                  entry = dg_date, 
#'                  exit = ex_date + 1L,
#'                  exit.status = "status")
#' 
#' ## some special cases
#' myVar <- "bi_date"
#' l <- list(myVar = "bi_date")
#' sire$l <- sire$myVar <- 1
#' 
#' ## conflict: myVar taken from data when "bi_date" was intended
#' lex <- Lexis_fpa(sire, 
#'                  birth = myVar, 
#'                  entry = dg_date, 
#'                  exit = ex_date + 1L,
#'                  exit.status = "status")
#' 
#' ## no conflict with names in data
#' lex <- Lexis_fpa(sire, 
#'                  birth = l$myVar, 
#'                  entry = dg_date, 
#'                  exit = ex_date + 1L,
#'                  exit.status = "status")
#' @export
Lexis_fpa <- function(data, 
                      birth = NULL, 
                      entry = NULL, 
                      exit = NULL, 
                      entry.status = NULL, 
                      exit.status = NULL, 
                      subset = NULL,
                      ...) {
  if (!requireNamespace("Epi", quietly = TRUE)) {
    stop("Install package Epi before using this function.")
  }
  TF <- environment()
  PF <- parent.frame(1L)
  
  checkVars <- c("fot", "per", "age", 
                 paste0("lex.", c("dur", "Xst", "Cst", "id")))
  checkVars <- intersect(names(data), checkVars)
  if (length(checkVars)) {
    stop("Following variable name(s) reserved but exist in data: ",
         paste0(checkVars, collapse = ", "))
  }
  
  
  sb <- substitute(subset)
  subset <- evalLogicalSubset(data, sb, enclos = PF)
  if (all(subset)) subset <- NULL
  x <- subsetDTorDF(data = data, subset = subset)
  setDT(x)
  
  an <- c("birth", "entry", "exit", "entry.status", "exit.status")
  
  l <- vector("list", length(an))
  names(l) <- an
  for (stri in an) {
    e <- paste0("substitute(", stri, ", env = TF)")
    e <- parse(text = e)[[1]]
    e <- eval(e, envir = TF) ## e.g. result of substitute(birth)
    e <- evalPopArg(data = x, arg = e, enclos = PF)[[1]]
    l[[stri]] <- e
  }
  
  l[sapply(l, is.null)] <- NULL
  
  missVars <- setdiff(c("birth", "entry", "exit"), names(l))
  if (length(missVars)) {
    stop("Following mandatory arguments were NULL: ",
         paste0(missVars, collapse = ", "))
  }
  
  fot <- l$entry - l$entry
  per <- l$entry
  age <- l$entry - l$birth
  per_exit <- l$exit
  
  en <- list(fot = fot, per = per, age = age)
  ex <- list(per = per_exit)
  
  al <- list(entry = en, exit = ex, entry.status = l$entry.status,
             exit.status = l$exit.status, data = x)
  al[sapply(al, is.null)] <- NULL
  
  do.call(Epi::Lexis, args = c(al, ...))
}







get_breaks <- function(x) {
  UseMethod("get_breaks")
}

get_breaks.survtab <- function(x) {
  
  ss <- attributes(x)$survtab.meta$surv.scale
  sb <- attributes(x)$survtab.meta$surv.breaks
  
  l <- list(sb)
  names(l) <- ss
  as.list(l)
  
}


get_breaks.aggre <- function(x) {
  
  as.list(attributes(x)$aggre.meta$breaks)
  
}

get_breaks.Lexis <- function(x) {
  as.list(attributes(x)$breaks)
}

get_breaks.default <- function(x) {
  NULL
}


select_breaks <- function(data, ...) {
  UseMethod("select_breaks")
}

select_breaks.default <- function(data, ts, br = NULL, ...) {
  br <- do_select_breaks(data = data, ts = ts, br = br)
  if (is.null(br)) {
    stop("Data did not contain breaks and no breaks were supplied ",
         "by hand.")
  }
  br
}

select_breaks.aggre <- function(data, ts, br = NULL, ...) {
  
  
  br <- do_select_breaks(data = data, ts = ts, br = br)
  
  select_breaks_subcheck(br, get_breaks(data)[[ts]], 
                         "Manually supplied breaks were not a ",
                         "subset of the breaks in aggre data. ",
                         "Data has breaks as a result of being split and ",
                         "aggregated; see ?as.aggre and ?aggre")
  
  if (is.null(br)) {
    stop("aggre object did not contain breaks and no breaks were supplied ",
         "by hand.")
  }
  
  br
}

select_breaks.Lexis <- function(data, ts, br = NULL, ...) {
  
  checkLexisData(data)
  
  br <- do_select_breaks(data = data, ts = ts, br = br)
  
  select_breaks_subcheck(br, get_breaks(data)[[ts]], 
                         "Manually supplied breaks were not a ",
                         "subset of the breaks in Lexis data. ",
                         "Data has breaks as a result of being a split Lexis ",
                         "object; see ?Lexis and e.g. ?splitMulti")
  
  if (is.null(br)) {
    stop("Lexis object did not contain breaks and no breaks were supplied ",
         "by hand.")
  }
  bl <- list(br)
  names(bl) <- ts
  checkBreaksList(data, breaks = bl)
  
  br
}


select_breaks_subcheck <- function(b1, b2, ...) {
  l1 <- list(b1)
  l2 <- list(b2)
  names(l1) <- names(l2) <- "TS"
  
  if (!is.null(b1) && !is.null(b2) && !all_breaks_in(l1, l2)) {
    stop(...)
  }
}

do_select_breaks <- function(data, ts, br = NULL) {
  # @description selects breaks from data or from br depending on
  # which one is NULL. If both exist, br must be a subset of the breaks
  # in data.
  
  stopifnot(is.data.frame(data))
  stopifnot(is.character(ts) && length(ts) == 1L && ts %in% names(data))
  
  dbr <- get_breaks(data)[[ts]]
  
  dbl <- list(dbr)
  bl <- list(br)
  names(dbl) <- names(bl) <- "TS"
  
  
  
  if (is.null(br)) br <- dbr
  
  br
}




breaks_in_data <- function(br, ts, data) {
  ## note: last break does not usually appear in data, unless intentionally
  ## limiting from e.g. 0:5 to 0:4
  stopifnot(length(ts) == 1 && ts %in% names(data))
  u <- unique(data[[ts]])
  
  br <- sort(unique(br))
  if (length(br)<2) stop("There must be at least two breaks to form intervals")
  
  br <- if (max(br) <= max(u)) br else br[-length(br)]
  all(br %in% u)
  
}



















