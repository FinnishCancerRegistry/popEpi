
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



#' @export
#' @title Aggregation of splitted \code{Lexis} data
#' @author Joonas Miettinen
#' @param lex a \code{Lexis} object splitted with e.g. 
#' \code{\link[Epi]{splitLexis}} or \code{\link{splitMulti}}
#' @param aggre expression(s) or variables to aggregate by; can be 
#' 1) a character string vector of variable names (e.g. \code{aggre = c("sex", "area")});
#' 2) an expression or symbol (e.g. \code{aggre = sex} or 
#' \code{aggre = factor(sex, 0:1, c("m", "f"))});
#' 3) a list of expressions or symbols (e.g. \code{aggre = list(gender = sex, area)}); 
#' see Details and Examples
#' @param breaks if \code{NULL}, uses the breaks that were used to split the data
#' to categorize time scales mentioned in \code{aggre} using \code{cut}; otherwise
#' uses the supplied named list of breaks; see Details
#' @param type determines outputted levels to which data is aggregated varying
#' from returning only rows with \code{pyrs > 0} (\code{"non-empty"}) to
#' returning all possible combinations of variables given in \code{aggre} even
#' if those combinations are not represented in data (\code{"full"}); see Details
#' @param subset a logical condition to subset by before computations
#' @param substituted \code{logical}, advanced; if \code{TRUE}, the supplied
#' \code{aggre} is a \code{call} object as a result of using \code{substitute}
#' or \code{quote}; useful for when using this function within another function
#' @param verbose \code{logical}; if \code{TRUE}, the function returns timings
#' and some information useful for debugging along the aggregation process
laggre <- function(lex, aggre = NULL, breaks = NULL, type = c("non-empty", "unique", "full"), subset = NULL, substituted = FALSE, verbose = FALSE) {
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
  
  if (is.null(breaks)) breaks <- copy(attr(lex, "breaks"))
  checkBreaksList(lex, breaks)
  
  foundScales <- copy(attr(lex, "time.scales"))
  if (length(foundScales) == 0) foundScales <- names(breaks)
  if (length(foundScales) == 0 ) stop("could not determine names of time scales; is the data a Lexis object?")
  
  
  subset <- substitute(subset)
  subset <- evalLogicalSubset(lex, subset)
  
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
  aggScales <- intersect(foundScales, av)
  if (any(!aggScales %in% names(breaks))) {
    aggScales <- paste0("'", setdiff(aggScales, names(breaks)), "'", collapse = ", ")
    stop("requested aggregating by time scale(s) by which data has not been split: ", aggScales)
  }
  if (length(aggScales) > 0) {
    cutTime <- proc.time()
    catAggScales <- paste0("'", aggScales, "'", collapse = ", ")
    if (verbose) cat("Following time scales mentioned in aggre argument and will be categorized for aggregation:", catAggScales, "\n")
    
    ## change names of cuttable time scales temporarily to avoid using them;
    ## instead temp cutted vars will be used (with tmpdt)
    whNames <- sapply(aggScales, function(x) which(x == names(lex)))
    on.exit({
      setnames(lex, whNames, names(whNames))
    })
    
    ## tmpdt: contains cut()'d time scales if appropriate
    
    tmpdt <- list()
    for (k in aggScales) {
      tmpdt[[k]] <- lex[subset, ][[k]]
    }
    setDT(tmpdt)
    tmpScales <- makeTempVarName(lex, pre =  names(whNames))
    setnames(lex, whNames, tmpScales)
    for (k in aggScales) {
      br_k <- unique(c(-Inf, breaks[[k]], Inf))
      set(tmpdt, j = k, value = cutLow(tmpdt[[k]], breaks = br_k))
    }
    if (verbose) cat("Time taken by cut()'ting time scales: ", timetaken(cutTime), "\n")
  } else {
    tmpdt <- NULL
  }
  
  
  othVars <- setdiff(av, aggScales)
  if (verbose && length(othVars) > 0) {
    catOthVars <- paste0("'", othVars, "'", collapse = ", ")
    cat("Detected the following non-time-scale variables to be utilized in aggregating:", catOthVars, "\n")
  }
  
  if (type != "full") {
    ## unique / non-empty pyrs -------------------------------------------------
    pyrsTime <- proc.time()
    pyrs <- with(tmpdt,{
      if (!is.data.table(lex)) {
        ## for some reason calling by string variable names does not work directly
        as.data.table(lex[, which(names(lex) %in% c(av, "lex.dur"))])[subset, .(pyrs = sum(lex.dur)), keyby = ags]
      } else {
        lex[subset, .(pyrs = sum(lex.dur)), keyby = ags]
      }
    })
    if (verbose) cat("Time taken by aggregating pyrs: ", timetaken(pyrsTime), "\n")
    
    byNames <- setdiff(names(pyrs), "pyrs") ## this will always be as intended
    
    pyrs[is.na(pyrs), pyrs := 0]
    if (type == "non-empty") pyrs <- pyrs[pyrs > 0]
    
  } else {
    ## cross-product pyrs ------------------------------------------------------
    pyrsTime <- proc.time()
    
    ## tmpdt will now contain both cut()'d time scales and any other variables
    ## from all.vars(aggre); tmpdt already may have contained cut()'d time scales!
    if (is.null(tmpdt)) tmpdt <- list()
    for (k in setdiff(av, names(tmpdt))) {
      tmpdt[[k]] <- lex[subset,][[k]]
    }
    setDT(tmpdt)
    
    bytab <- evalPopArg(tmpdt, arg = ags, DT = TRUE)
    
    cj <- lapply(bytab, function(x) {if (is.factor(x)) levels(x) else unique(x)})
    
    ## we want to use the breaks of cut()'d time scales for full utilization
    ## (cj above does not contain levels not present in data)
    if (argType == "character") {
      whUseScales <- NULL
      usedScales <- varsUsingScales <- intersect(aggre, aggScales)
      
    } else if (argType != "NULL") {
      whUseScales <- sapply(ags[-1], function(x) any(all.vars(x) %in% aggScales))
      usedScales <- names(bytab)[whUseScales]
    }
    
    cj[usedScales] <- lapply(breaks[aggScales], function(x) unique(c(x)))
    
    cj <- do.call(CJ, cj)
    
    if (verbose) cat("Table of variable levels for which pyrs are computed looks like this: \n")
    if (verbose) print(cj, topn = 2, nrows = 4)
    
    for (k in c("lex.Cst", "lex.Xst", "lex.dur")) {
      set(bytab, j = k, value = lex[subset,][[k]])
    }
    
    ## this will always be what intended thanks to data.table
    byNames <- setdiff(names(bytab), c("pyrs", "lex.dur", "lex.Cst", "lex.Xst")) 
    
    setkeyv(bytab, byNames); setkeyv(cj, byNames)
    pyrs <- bytab[cj, .(pyrs = sum(lex.dur)), keyby = .EACHI]
    pyrs[is.na(pyrs), pyrs := 0]
    
    rm(bytab, cj)
    
    
    if (verbose) cat("Time taken by aggregating pyrs: ", timetaken(pyrsTime), "\n")
    
  }
  
  
  pyrsDiff <- pyrs[, sum(pyrs)] - sum(lex[subset, ]$lex.dur)
  if (!isTRUE(all.equal(pyrsDiff, 0L))) {
    warning("Found discrepancy in total aggregated pyrs compared to sum(lex$lex.dur); compare results by hand and make sure settings are right \n")
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
  
  ## sadly, transition computations requires information about
  ## 1) transitions (easy) and 2) end points (harder).
  ## end points requires sorting at some point!
  
  ## note: following takes copy of some columns if data was DF; else
  ## tmplex only an alias for lex
  ## note: all this is due to avoiding using set() to modify the original
  ## data in place. that may actually be a pretty good idea...
  tmplex <- if (!is.data.table(lex)) {
    as.data.table(lex[, which(names(lex) %in% c("lex.id", tmpScales[1], "lex.Cst", "lex.Xst"))])
  } else lex
  
  tmpOrder <- makeTempVarName(tmplex, pre = "order_")
  tmplex[, (tmpOrder) := 1:.N]
  old_key <- key(tmplex) ## note: as.data.table(DF) makes no key
  setkeyv(tmplex, c("lex.id", tmpScales[1]))
  tmpTrans <- makeTempVarName(tmplex, pre = "trans_")
  tmplex[, (tmpTrans) := lex.Cst != lex.Xst | !duplicated(lex.id, fromLast = TRUE)]
  setorderv(tmplex, tmpOrder)
  if (length(old_key) > 0) setkeyv(tmplex, old_key)
  tmpdt <- tmpdt[subset & tmplex[[tmpTrans]]]
  
  print(tmpdt)
  print(tmplex)
  print(ags)
  
  trans <- with(tmpdt,{
      tmplex[subset & tmplex[[tmpTrans]], list(obs = .N), keyby = ags]
  })
  rm(tmpdt, tmplex)
  
  
  if (verbose) cat("Time taken by aggregating transitions: ", timetaken(transTime), "\n")
  
  ## casting &merging ----------------------------------------------------------
  
  mergeTime <- proc.time()
  
  ## tmpTr to be used in casting
  tmpTr <- makeTempVarName(trans, pre = "trans_")
  trans[, (tmpTr) := paste0("from", lex.Cst, "to", lex.Xst)]
  transitions <- trans[, unique(get(tmpTr))]
  trans[, c("lex.Cst", "lex.Xst") := NULL]
  
  ## note: need tmpDum if aggre = NULL for correct casting & merging
  tmpDum <- makeTempVarName(trans)
  byNames <- c(byNames, tmpDum)
  trans[, (tmpDum) := 1L]
  pyrs[, (tmpDum) := 1L]
  
  trans <- cast_simple(trans, columns = tmpTr, rows = byNames, values = "obs")
  
  setkeyv(trans, byNames); setkeyv(pyrs, byNames)
  trans <- trans[pyrs]
  
  trans[, (tmpDum) := NULL]
  byNames <- setdiff(byNames, tmpDum)
  setcolorder(trans, c(byNames, "pyrs", transitions))
  
  for (t in transitions) {
    trans[is.na(get(t)), (t) := 0L]
  }
  
  if (verbose) cat("Time taken by merging pyrs & transitions: ", timetaken(mergeTime), "\n")
  
  ## final touch ---------------------------------------------------------------
  setaggre(trans, obs = transitions, pyrs = "pyrs", by = byNames, obs.exp = NULL)
  setattr(trans, "breaks", breaks)
  if (!getOption("popEpi.datatable")) setDFpe(trans)
  if (verbose) cat("Time taken by laggre(): ", timetaken(allTime), "\n")
  
  
  
  trans[]
}












