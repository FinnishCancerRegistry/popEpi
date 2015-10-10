
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



laggre <- function(lex, aggre = NULL, breaks = NULL, type = c("non-empty", "unique", "full"), subset = NULL, substituted = FALSE, verbose = T) {
  ## a generalized aggregator for splitted Lexis objects
  ## input: lex: a Lexis object that has been split somehow; 
  ##        aggre: an expression / list of expressions / a character vector of names;
  ##        may have been substituted, but user must use substitued = TRUE then
  ##        some way to define style of aggregation
  ## output: a long-format data.frame or data.table, where transitions and person-time is tabulated.
  ## TODO: "full" type aggregating
  allTime <- proc.time()
  
  type <- match.arg(type[1], c("non-empty", "unique", "full", "cross-product"))
  if (type == "cross-product") type <- "full"
  
  allScales <- attr(lex, "time.scales")
  if (is.null(breaks)) breaks <- attr(lex, "breaks")
  checkBreaksList(lex, breaks)
  
  subset <- substitute(subset)
  subset <- evalLogicalSubset(lex, subset)
  
  ## non-empty and unique only need the substituted argument to pass to DT[, by]
  ## full needs to evaluate and compile CJ
  
  ags <- if (!substituted) substitute(aggre) else aggre
  
  argType <- popArgType(ags)
  if (verbose) cat("Used aggre argument:", deparse(ags),"\n")
  if (verbose) cat("Type of aggre argument:", argType, "\n")
  if (argType != "NULL") {
    if (argType == "character") {
      av <- aggre
    } else {
      av <- all.vars(ags)
    } 
    
    ## more convenient with only list or char
    if (argType == "expression") ags <- substitute(list(aggre))
    
    if (!any(av %in% names(lex))) {
      ags <- deparse(ags)
      stop("none of the variables used in aggre were found in lex; the aggre expression was '", ags, "'")
    }
    
  } else {
    av <- NULL
    ags <- substitute(list())
    type <- "non-empty"
  }

  
  ## need to cut() time scales for aggregating?
  aggScales <- intersect(allScales, av)
  if (any(!aggScales %in% names(breaks))) {
    aggScales <- paste0("'", setdiff(aggScales, names(breaks)), "'", collapse = ", ")
    stop("requested aggregating by time scale(s) by which data has not been split: ", aggScales)
  }
  if (length(aggScales) > 0) {
    cutTime <- proc.time()
    ## change names of cuttable time scales temporarily to avoid using them;
    ## instead temp cutted vars will be used (with tmpdt)
    whNames <- sapply(aggScales, function(x) which(x == names(lex)))
    on.exit({
      setnames(lex, whNames, names(whNames))
    })
    
    tmpdt <- list()
    for (k in aggScales) {
      tmpdt[[k]] <- lex[subset, ][[k]]
    }
    setDT(tmpdt)
    tmpScales <- makeTempVarName(lex, pre =  names(whNames))
    setnames(lex, whNames, tmpScales)
    for (k in aggScales) {
      set(tmpdt, j = k, value = cutLow(tmpdt[[k]], breaks = breaks[[k]]))
    }
    
    if (verbose) cat("Time taken by cut()'ting time scales: ", timetaken(cutTime), "\n")
  } else {
    tmpdt <- NULL
  }
  
  if (type != "full") {
    
    ## unique / non-empty pyrs -------------------------------------------------
    pyrsTime <- proc.time()
    pyrs <- with(tmpdt,{
      lex[subset, .(pyrs = sum(lex.dur)), keyby = ags]
    })
    if (verbose) cat("Time taken by aggregating pyrs: ", timetaken(pyrsTime), "\n")
    mv <- names(pyrs)[-length(names(pyrs))] ## for merging etc.
    
    pyrs[is.na(pyrs), pyrs := 0]
    if (type == "non-empty") pyrs <- pyrs[pyrs > 0]
    
  } else {
    
    ## cross-product pyrs ------------------------------------------------------
    pyrsTime <- proc.time()
    
    if (is.null(tmpdt)) tmpdt <- list()
    for (k in setdiff(av, names(tmpdt))) {
      tmpdt[[k]] <- lex[subset,][[k]]
    }
    setDT(tmpdt)
    
    bytab <- evalPopArg(tmpdt, arg = ags, DT = TRUE)
    mv <- copy(names(bytab))
    
    cj <- lapply(bytab, function(x) {if (is.factor(x)) levels(x) else unique(x)})
    cj <- do.call(CJ, cj)
    setDT(cj)
    
    for (k in c("lex.Cst", "lex.Xst", "lex.dur")) {
      set(bytab, j = k, value = lex[subset,][[k]])
    }
    
    setkeyv(bytab, mv); setkeyv(cj, mv)
    pyrs <- bytab[cj, .(pyrs = sum(lex.dur)), keyby = .EACHI]
    pyrs[is.na(pyrs), pyrs := 0]
    
    rm(bytab)
    
    if (verbose) cat("Time taken by aggregating pyrs: ", timetaken(pyrsTime), "\n")
    
  }
  
  ## transitions ---------------------------------------------------------------
  
  transTime <- proc.time()
  ## need to modify call to get lex.Cst & lex.Xst transitions
  if (argType == "character") {
    ags <- unique(c(aggre, "lex.Cst", "lex.Xst"))
  } else {
    ags <- ags
    ags$lex.Cst <- quote(lex.Cst)
    ags$lex.Xst <- quote(lex.Xst) 
  }
  
  
  trans <- with(tmpdt,{
    lex[subset, list(obs = .N), keyby = ags]
  })
  rm(tmpdt)
  
  mv <- names(trans)[0:length(mv)] ## old might include e.g. V1, while this won't
  setnames(pyrs, setdiff(names(pyrs), "pyrs"), mv)
  
  tmpTr <- makeTempVarName(trans, pre = "trans_")
  trans[, (tmpTr) := paste0("from", lex.Cst, "to", lex.Xst)]
  transitions <- trans[, unique(get(tmpTr))]
  trans[, c("lex.Cst", "lex.Xst") := NULL]
  
  if (verbose) cat("Time taken by aggregating transitions: ", timetaken(transTime), "\n")
  
  ## casting &merging ----------------------------------------------------------
  
  mergeTime <- proc.time()
  
  ## note: need tmpDum if aggre = NULL for correct casting & merging
  tmpDum <- makeTempVarName(trans)
  mv <- c(mv, tmpDum)
  trans[, (tmpDum) := 1L]
  pyrs[, (tmpDum) := 1L]
  trans <- cast_simple(trans, columns = tmpTr, rows = mv, values = "obs")
  
  setkeyv(trans, mv); setkeyv(pyrs, mv)
  trans <- trans[pyrs]
  
  trans[, (tmpDum) := NULL]
  mv <- setdiff(mv, tmpDum)
  setcolorder(trans, c(mv, "pyrs", transitions))
  if (verbose) cat("Time taken by merging pyrs & transitions: ", timetaken(mergeTime), "\n")
  
  ## final touch ---------------------------------------------------------------
  setaggre(trans, obs = transitions, pyrs = "pyrs", by = mv, obs.exp = NULL)
  setattr(trans, "breaks", breaks)
  
  if (verbose) cat("Time taken by laggre(): ", timetaken(allTime), "\n")
  trans[]
}












