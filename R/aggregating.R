
#' @title Set 'aggre' attributes to an object by modifying in place
#' @author Joonas Miettinen
#' @param x a \code{data.frame} or \code{data.table}
#' @param obs a character string; the name of the event counts variable
#' @param pyrs a character string; the name of the person-time
#' @param obs.exp a character string; the name of the expected event counts variable
#' @param by a character string vector; the names of variables by which \code{obs} and \code{pyrs}
#' have been tabulated; optional, since the default value usually works
#' @details 
#' 
#' \code{setaggre} sets \code{x} to the \code{aggre} class in place without taking a copy
#' as e.g. \code{as.data.frame.XXX} functions do; see e.g. \code{\link[data.table]{setDT}}.
#' 
#' @seealso 
#' \code{\link{as.aggre.data.frame}}
#' 
#' @export setaggre
#' @examples 
#' df <- data.frame(sex = rep(c("male", "female"), each = 5), 
#'                  obs = rpois(10, rep(7,5, each=5)), 
#'                  pyrs = rpois(10, lambda = 10000))
#' setaggre(df, obs = "obs", pyrs = "pyrs", by = "sex")
setaggre <- function(x, obs = "obs", pyrs = "pyrs", obs.exp = NULL, by = setdiff(names(x), c(obs, pyrs, obs.exp))) {
  ## input: aggregated data in data.frame or data.table format
  ## intention: any user can define their data as an aggregated data set
  ## which will be usable by survtab / sir / other
  ## output: no need to do x <- setaggre(x); instead modifies attributes in place;
  ## sets "aggreVars" attribute, a list of names of various variables.
  ## survtab for aggregated data will need this attribute to work.
  all_names_present(x, c(obs, pyrs, obs.exp, by))
  
  if (!inherits(x, "aggre")) {
    cl <- class(x)
    wh <- which(cl %in% c("pe", "data.table", "data.frame"))
    wh <- min(wh)
    
    ## yes, from zero: in case only one class
    cl <- c(cl[0:(wh-1)], "aggre", cl[wh:length(cl)])
    setattr(x, "class", cl)
  }
  
  
  setattr(x, "aggreVars", list(obs = obs, pyrs = pyrs, by = by, obs.exp = obs.exp))
  invisible(x)
}

#' @title Coercion to class \code{aggre}
#' @author Joonas Miettinen
#' @param x an R object to coerce to \code{aggre}; must be a \code{data.frame} or \code{data.table}
#' @param obs a character string; the name of the event counts variable
#' @param pyrs a character string; the name of the person-time
#' @param obs.exp a character string; the name of the expected event counts variable
#' @param by a character string vector; the names of variables by which \code{obs} and \code{pyrs}
#' have been tabulated; optional, since the default value usually works
#' @param ... arguments passed to or from methods
#' @seealso 
#' \code{\link{setaggre}} for modifying in place
#' 
#' 
#' @examples 
#' df <- data.frame(sex = rep(c("male", "female"), each = 5), 
#'                  obs = rpois(10, rep(7,5, each=5)), 
#'                  pyrs = rpois(10, lambda = 10000))
#' dt <- as.data.table(df)
#' 
#' df <- as.aggre(df, obs = "obs", pyrs = "pyrs", by = "sex")
#' dt <- as.aggre(dt, obs = "obs", pyrs = "pyrs", by = "sex")
#' 
#' class(df)
#' class(dt)
#' 
#' @export as.aggre
as.aggre <- function(x, ...) {
  UseMethod("as.aggre", x)
}

#' @export
#' @describeIn as.aggre
as.aggre.data.frame <- function(x, obs = "obs", pyrs = "pyrs", obs.exp = NULL, by = setdiff(names(x), c(obs, pyrs, obs.exp)), ...) {
  x <- copy(x)
  setaggre(x, obs = obs, pyrs = pyrs, obs.exp = obs.exp, by = by, ...)
  x
}

#' @export
#' @describeIn as.aggre
as.aggre.default <- function(x, ...) {
  stop(gettextf("cannot coerce class \"%s\" to 'aggre'", deparse(class(x))), 
       domain = NA)
}



laggre <- function(lex, aggre = list(sex, agegroup = age), breaks = NULL, type = c("non-empty", "unique", "full"), substituted = FALSE) {
  ## a generalized aggregator for splitted Lexis objects
  ## input: lex: a Lexis object that has been split somehow; 
  ##        aggre: an expression / list of expressions / a character vector of names;
  ##        may have been substituted, but user must use substitued = TRUE then
  ##        some way to define style of aggregation
  ## output: a long-format data.frame or data.table, where transitions and person-time is tabulated.
  
  ## TODO: 
  ## - usage of time scales (aggregate automatically into intervals)
  ##   * this requires detection of variables in aggre intelligently
  ##     BEFORE evaluating!
  ## - aggre might be substitute()'d before using this function?
  
  type <- match.arg(type[1], c("non-empty", "unique", "full"))
  
  allScales <- attr(lex, "time.scales")
  if (is.null(breaks)) breaks <- attr(lex, "breaks") 
  checkBreaksList(lex, breaks)
  
  ## non-empty and unique only need the substituted argument to pass to DT[, by]
  ## full needs to evaluate and compile CJ
  
  if (!substituted) ags <- substitute(aggre)
  
  argType <- popArgType(ags)
  
  if (argType == "character") {
    av <- aggre
  } else {
    av <- all.vars(ags)
  }
  ## more convenient with only list or char
  if (argType == "expression") ags <- substitute(list(aggre))
  
  if (!any(av %in% names(lex))) {
    ags <- deparse(ags)
    stop("none of the variables used in aggre were found in lex; the aggre expression was ", ags)
  }
  
  aggScales <- intersect(allScales, av)
  ## need to cut() time scales for aggregating?
  cut_ts <- if (length(allScales) > 1 && length(aggScales) > 0) TRUE else FALSE
  if (cut_ts) {
    ## change names of cuttable time scales temporarily to avoid using them;
    ## instead temp cutted vars will be used (with tmpdt)
    whNames <- sapply(aggScales, function(x) which(x == names(lex)))
    on.exit({
      setnames(lex, whNames, names(whNames))
    })
    
    tmpdt <- list()
    for (k in aggScales) {
      tmpdt[[k]] <- lex[[k]]
    }
    setDT(tmpdt)
    setnames(lex, whNames, makeTempVarName(lex, pre =  names(whNames)))
    for (k in aggScales) {
      set(tmpdt, j = k, value = cut(tmpdt[[k]] + .Machine$double.eps^0.5,
                                    breaks = breaks[[k]], right = FALSE,
                                    labels = breaks[[k]][-length(breaks[[k]])]))
      set(tmpdt, j = k, value = try2int(fac2num(tmpdt[[k]])))
    }
    
  } else {
    tmpdt <- NULL
  }
  
  pyrs <- with(tmpdt,{
    lex[, .(pyrs = sum(lex.dur)), keyby = ags]
  })
  mv <- names(pyrs)[-length(names(pyrs))] ## for merging etc.
  
  pyrs[is.na(pyrs), pyrs := 0]
  if (type == "non-empty") pyrs <- pyrs[pyrs > 0]
  
  ## need to modify call to get lex.Cst & lex.Xst transitions
  if (argType == "character") {
    ags <- unique(c(aggre, "lex.Cst", "lex.Xst"))
  } else {
    ags$lex.Cst <- quote(lex.Cst)
    ags$lex.Xst <- quote(lex.Xst) 
  }
  
  trans <- with(tmpdt,{
    lex[, list(obs = .N), keyby = ags]
  })
  tmpTr <- makeTempVarName(trans, pre = "trans_")
  trans[, (tmpTr) := paste0("from", lex.Cst, "to", lex.Xst)]
  transitions <- trans[, unique(get(tmpTr))]
  trans[, c("lex.Cst", "lex.Xst") := NULL]
  trans <- cast_simple(trans, columns = tmpTr, rows = mv, values = "obs")
  
  setkeyv(trans, mv); setkeyv(pyrs, mv)
  trans <- trans[pyrs]
  
  setaggre(trans, obs = transitions, pyrs = "pyrs", by = mv, obs.exp = NULL)
  trans  
}


