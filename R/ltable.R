#' @title Tabulate counts and other functions with multiple factors into a long-format table
#' @author Joonas Miettinen, Matti Rantanen
#' @description \code{ltable} makes use of \code{data.table} capabilities to tabulate frequencies or 
#' arbitrary functions of given variables into a long format \code{data.table}/\code{data.frame}.
#' @param data individual-level or aggregated data
#' @param by.vars names of variables that are used for categorization, 
#' as a character vector, e.g. \code{c('sex','agegroup')}
#' @param expr object or a list of objects where each object is a function of a variable (see: details)
#' @param subset a logical condition; data is limited accordingly before
#' evaluating \code{expr}
#' @param use.levels logical; if \code{TRUE}, uses factor levels of given variables if present; 
#' if you want e.g. counts for levels
#' that actually have zero observatios but are levels in a factor variable, use this
#' @param na.rm logical; if \code{TRUE}, drops rows in table that have more than zero \code{NA} values on 
#' any \code{by.vars} column
#' @param robust logical; if \code{TRUE}, runs the outputted data's \code{by.vars} columns 
#' through \code{robust_values} before outputting
#' 
#' @import data.table 
#' 
#' @details 
#' 
#' Returns \code{expr} for each unique combination of given \code{by.vars}.
#' 
#' By default makes use of any and all \code{\link{levels}} present for 
#' each variable in  \code{by.vars}. This is useful,
#' because even if a subset of the data does not contain observations 
#' for e.g. a specific age group, those age groups are 
#' nevertheless presented in the resulting table; e.g. with the default 
#' \code{expr = list(obs = .N)} all age group levels
#' are represented by a row and can have  \code{obs = 0}.
#' 
#' The function differs from the
#' vanilla \code{\link{table}} by giving a long format table of values
#'  regardless of the number of \code{by.vars} given.
#' Make use of e.g. \code{\link{cast_simple}} if data needs to be 
#' presented in a wide format (e.g. a two-way table).
#' 
#' The rows of the long-format table are effectively Cartesian products 
#' of the levels of each variable in  \code{by.vars},
#' e.g. with  \code{by.vars = c("sex", "area")} all levels of  
#' \code{area} are repeated for both levels of  \code{sex}
#' in the table.
#' 
#' The \code{expr} allows the user to apply any function(s) on all 
#' levels defined by  \code{by.vars}. Here are some examples:
#' \itemize{
#'   \item .N or list(.N) is a function used inside a \code{data.table} to calculate counts in each group
#'   \item list(obs = .N), same as above but user assigned variable name
#'   \item list(sum(obs), sum(pyrs), mean(dg_age)), multiple objects in a list
#'   \item list(obs = sum(obs), pyrs = sum(pyrs)), same as above with user defined var names
#' }
#' 
#' If  \code{use.levels = FALSE}, no \code{levels} information will
#'  be used. This means that if e.g. the  \code{agegroup}
#' variable is a factor and has 18 levels defined, but only 15 levels
#'  are present in the data, no rows for the missing
#' levels will be shown in the table.
#' 
#' \code{na.rm} simply drops any rows from the resulting table where 
#' any of the  \code{by.vars} values was \code{NA}. 
#' 
#' @seealso
#' \code{\link{table}}, \code{\link{cast_simple}}, \code{\link{melt}}
#' 
#' @export ltable
#' 
#' @examples
#' sr <- copy(sire)
#' sr$agegroup <- cut(sr$dg_age, breaks=c(0,45,60,75,85,Inf))
#' ## counts by default
#' ltable(sr, "agegroup")
#' 
#' ## any expression can be given
#' ltable(sr, "agegroup", list(mage = mean(dg_age)))
#' ltable(sr, "agegroup", list(mage = mean(dg_age), vage = var(dg_age)))
#' 
#' ## also returns levels where there are zero rows (expressions as NA)
#' ltable(sr, "agegroup", list(obs = .N, minage = min(dg_age), maxage = max(dg_age)), 
#'        subset = dg_age < 85)
#' 

ltable <- function(data, 
                   by.vars='sex',
                   expr=list(obs=.N),
                   subset = NULL,
                   use.levels=TRUE,
                   na.rm=FALSE,
                   robust = TRUE) {
  if (is.null(by.vars)) {
    message('No category variables given!')
    return(nrow(data))
  }
  all_names_present(data, by.vars, stops=T)
  
  if (!is.data.frame(data)) stop("only data.frame or data.table allowed as data")
  
  ## subsetting: no copy of data -----------------------------------------------
  subset <- substitute(subset)
  subset <- evalLogicalSubset(data = data, substiset = subset)
  
  ## use either original levels or simply unique values
  if (use.levels) {
    levsfun <- function(x) {
      if (is.factor(x)) return(levels(x))
      return(sort(unique(x), na.last=TRUE))
    }
  } else {
    levsfun <- function(x) sort(unique(x), na.last=TRUE)
  }
  
  ## collect levels of by.vars in list
  levs <- list()
  for (k in by.vars) {
    levs[[k]] <- levsfun(data[[k]])
  }
  
  ## table for picking rows
  cj <- do.call(CJ, levs)
  
  ## treatment of NA values in by.vars
  if (na.rm & !use.levels) {
    na_cj <- na.omit(cj)
    drop_rows<- nrow(cj) - nrow(na_cj)
    if (drop_rows >0) {
      hav <- nrow(data)
      na_hav <- data.table(data[subset,], key=by.vars)[na_cj, .N, by=.EACHI]
      na_hav <- sum(na_hav$N)
      message(paste("NOTE:", hav-na_hav,"observations in original data ignored due to missingness, resulting in", 
                    drop_rows, "fewer rows in the table"))
      cj <- na_cj
    }
  }
  
  ## make the table
  e <- substitute(expr)
  if (!is.data.table(data)) {
    ## data.frame needs to take a copy unfortunately...
    ## setDT & setDF possible but unrobust
    data <- data[subset,]
    setDT(data)
    setkeyv(data, by.vars)
    tab <- data[cj, eval(e, envir = .SD), by =.EACHI]
    rm(data)
  } else {
    old_key <- key(data)
    tmpSSV <- makeTempVarName(data, pre = "subset_")
    on.exit(setcolsnull(data, delete = tmpSSV, soft = TRUE))
    data[, c(tmpSSV) := subset]
    rm(subset)
    setkeyv(data, by.vars)
    tab <- data[data[[tmpSSV]], ][cj, eval(e, envir = .SD), by = .EACHI]
    if (length(old_key) > 0) setkeyv(data, old_key)
  }
  
  ## robust values output where possible ---------------------------------------
  if (robust) {
    for (k in by.vars) {
      set(tab, j = k, value = robust_values(tab[[k]], force = FALSE, messages = FALSE))
    }
    
  }
  
  setDT(tab)
  setkeyv(tab,by.vars)
  if (!getOption("popEpi.datatable")) setDFpe(tab)
  tab[]
}




#' @title Tabulate counts and other functions with multiple factors into a long-format table
#' @author Joonas Miettinen
#' @description Like \code{ltable} except faster and unrobust. For advanced users.
#' @import data.table 
#' @param data a data.table
#' @param by.vars a character string vector specifying the names of variables,
#' by the combinations of which to evaluate the given \code{expr}
#' @param expr an arbitrary expression utilizing any available function
#' on any variable available in the data
#' @param subset a logical condition; data is limited accordingly before
#' evaluating \code{expr}
#' @param .SDcols advanced; passed to inside the data.table's brackets 
#' \code{DT[, , ...]}; see \code{\link{data.table}}
#' @param ... advanced; other arguments passed to inside the 
#' data.table's brackets \code{DT[, , ...]}; see \code{\link{data.table}}
#' @export expr.by.cj
#' @examples
#' sr <- copy(sire)
#' sr$agegroup <- cut(sr$dg_age, breaks=5)
#' ## counts by default
#' expr.by.cj(sr, "agegroup")
#' 
#' ## any arbitrary expression can be given
#' expr.by.cj(sr, "agegroup", list(mage = mean(dg_age)))
#' expr.by.cj(sr, "agegroup", list(mage = mean(dg_age), vage = var(dg_age)))
#' 
#' ## only uses levels of by.vars present in data
#' expr.by.cj(sr, "agegroup", list(mage = mean(dg_age), vage = var(dg_age)), 
#'            subset = dg_age < 70)
#' 

expr.by.cj <- function(data, 
                       by.vars='sex',
                       expr=list(obs=.N),
                       subset = NULL,
                       .SDcols,
                       ...) {
  if (!is.data.table(data)) stop("data must be a data.table; try ltable instead?")
  
  ## subsetting: no copy of data -----------------------------------------------
  subset <- substitute(subset)
  subset <- evalLogicalSubset(data = data, substiset = subset)
  
  old_key <- key(data)
  
  levs <- lapply(data[subset, c(by.vars), with=F], unique)
  cj <- do.call(CJ, levs)
  
  tmpSSV <- makeTempVarName(data, pre = "subset_")
  on.exit(setcolsnull(data, delete = tmpSSV, soft = TRUE))
  data[, c(tmpSSV) := subset]
  rm(subset)
  
  setkeyv(data, by.vars)
  e = substitute(expr)
  tab <- data[data[[tmpSSV]], ][cj, eval(e, envir = .SD), keyby=.EACHI, .SDcols = .SDcols, ...]
  
  setDT(tab)
  if (length(old_key) > 0) setkeyv(data, old_key)
  tab[]
}

