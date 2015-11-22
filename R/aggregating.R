
#' @title Set 'aggre' attributes to an object by modifying in place
#' @author Joonas Miettinen
#' @description Coerces an R object to an \code{aggre} object, identifying
#' the object as one containing aggregated counts, person-years and other
#' information. \code{setaggre} modifies in place without taking any copies
#' and retaining all other attributes.
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
#' \code{\link{as.aggre}}
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
#' @description Coerces an R object to an \code{aggre} object, identifying
#' the object as one containing aggregated counts, person-years and other
#' information. 
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
as.aggre <- function(x, obs = "obs", pyrs = "pyrs", obs.exp = NULL, by = setdiff(names(x), c(obs, pyrs, obs.exp)), ...) {
  UseMethod("as.aggre", x)
}

#' @export
#' @describeIn as.aggre
as.aggre.data.frame <- function(x, obs = "obs", pyrs = "pyrs", obs.exp = NULL, by = setdiff(names(x), c(obs, pyrs, obs.exp)), ...) {
  x <- copy(x)
  setaggre(x, obs = obs, pyrs = pyrs, obs.exp = obs.exp, by = by, ...)
  x[]
}

#' @export
#' @describeIn as.aggre
as.aggre.default <- function(x, ...) {
  stop(gettextf("cannot coerce class \"%s\" to 'aggre'", deparse(class(x))), 
       domain = NA)
}



#' @export laggre
#' @title Aggregation of split \code{Lexis} data
#' @author Joonas Miettinen
#' @description Aggregates a split \code{Lexis} object by given variables 
#' and / or expressions into a long-format table of person-years and 
#' transitions / end-points. Automatic aggregation over time scales
#' by which data has been split if the respective time scales are mentioned
#' in the aggregation argument to e.g. intervals of calendar time, follow-up time
#' and/or age.
#' @param lex a \code{Lexis} object split with e.g. 
#' \code{\link[Epi]{splitLexis}} or \code{\link{splitMulti}}
#' @param aggre expression(s) or variables to aggregate by; can be 
#' 1) a character string vector of variable names (e.g. \code{aggre = c("sex", "area")});
#' 2) an expression or symbol (e.g. \code{aggre = sex} or 
#' \code{aggre = factor(sex, 0:1, c("m", "f"))});
#' 3) a list of expressions or symbols (e.g. \code{aggre = list(gender = sex, area)});
#' automatic aggregation over Lexis time scales mentioned here; 
#' see Details and Examples
#' @param breaks if \code{NULL}, uses the breaks that were used to split the data
#' to categorize time scales mentioned in \code{aggre} using \code{cut}; otherwise
#' uses the supplied named list of breaks; see Details
#' @param type determines outputted levels to which data is aggregated varying
#' from returning only rows with \code{pyrs > 0} (\code{"unique"}) to
#' returning all possible combinations of variables given in \code{aggre} even
#' if those combinations are not represented in data (\code{"full"}); see Details
#' @param subset a logical condition to subset by before computations;
#' e.g. \code{subset = area \%in\% c("A", "B")}
#' @param substituted \code{logical}, advanced; if \code{TRUE}, the supplied
#' \code{aggre} is a \code{call} object as a result of using \code{substitute}
#' or \code{quote}; useful for when using this function within another function
#' @param verbose \code{logical}; if \code{TRUE}, the function returns timings
#' and some information useful for debugging along the aggregation process
#' @details 
#' 
#' \strong{Basics}
#' 
#' \code{laggre} is intented for aggregation of split \code{Lexis} data only.
#' See \code{\link[Epi]{Lexis}} for forming \code{Lexis} objects by hand
#' and e.g. \code{\link[Epi]{splitLexis}}, \code{\link{splitLexisDT}}, and
#' \code{\link{splitMulti}} for splitting the data. \code{\link{lexpand}}
#' may be used for simple data sets to do both steps as well as aggregation
#' in the same function call.
#' 
#' Here aggregation refers to computing person-years and the appropriate events
#' (state transitions and end points in status) for the subjects in the data.
#' Hence, it computes e.g. deaths (end-point and state transition) and 
#' censorings (end-point) as well as events in a multi-state setting
#' (state transitions).
#' 
#' The result is a long-format \code{data.frame} or \code{data.table}
#' (depending on \code{options("popEpi.datatable")}; see \code{?popEpi})
#' with the columns \code{pyrs} and the appropriate transitions named as
#' \code{fromXtoY}, e.g. \code{from0to0} and \code{from0to1} depending
#' on the values of \code{lex.Cst} and \code{lex.Xst}.
#' 
#' 
#' \strong{The aggre argument}
#' 
#' The \code{aggre} argument determines the length of the table, i.e.
#' the combinations of variables to which data is aggregated.  
#' \code{aggre} is relatively flexible, as it can be supplied as
#' 
#' \itemize{
#'  \item{a character string vector, e.g. \code{c("sex", "area")}, naming variables existing in \code{lex}}
#'  \item{an expression, e.g. \code{factor(sex, 0:1, c("m", "f"))} using any variable found in \code{lex}}
#'  \item{a list (fully or partially named) of expressions, e.g. \code{list(gender = factor(sex, 0:1, c("m", "f"), area)}}
#' }
#' 
#' Note that expressions effectively allow a variable to be supplied simply as
#' e.g. \code{aggre = sex} (as a symbol/name in R lingo).
#' 
#' The data is then aggregated to the levels of the given variables 
#' or expression(s). Variables defined to be time scales in the supplied 
#' \code{Lexis} are processed in a special way: If any are mentioned in the
#' \code{aggre} argument, intervals of them are formed based on the breaks
#' used to split the data: e.g. if \code{age} was split using the breaks 
#' \code{c(0, 50, Inf)}, mentioning \code{age} in \code{aggre} leads to
#' creating the \code{age} intervals \code{[0, 50)} and \code{[50, Inf)}
#' and aggregating to them. The intervals are identified in the output
#' as the lower bounds of the appropriate intervals.
#' 
#' It is possible but not recommended to also supply the argument \code{breaks}, 
#' a list of breaks as in \code{splitMulti}, but this may go wrong;
#' It is mainly included for when the meta information about the 
#' breaks used to split the data is lost due to modifying \code{lex}
#' in certain ways after splitting.
#' 
#' \strong{Aggregation types (styles)}
#' 
#' It is almost always enough to aggregate the data to variable levels
#' that are actually represented in the data 
#' (default \code{aggre = "unique"}; alias \code{"non-empty"}). 
#' For certain uses it may be useful
#' to have also "empty" levels represented (resulting in some rows in output
#' with zero person-years and events); in these cases supplying
#' \code{aggre = "full"} (alias \code{"cartesian"}) causes \code{aggre}
#' to determine the Cartesian product of all the levels of the supplied \code{aggre}
#' variables or expressions and aggregate to them. As an example
#' of a Cartesian product, try
#' 
#' \code{merge(1:2, 1:5)}.
#' 
#' @return 
#' A long \code{data.frame} or \code{data.table} of aggregated person-years 
#' (\code{pyrs}), numbers of subjects at risk (\code{at.risk}), and events
#' formatted \code{fromXtoY}, where \code{X} and \code{X} are states transitioning
#' from and to or states at the end of each \code{lex.id}'s follow-up 
#' (implying \code{X} = \code{Y}). Subjects at risk are computed in the beginning
#' of an interval defined by any Lexis time scales and mentioned in \code{aggre},
#' but events occur at any point within an interval.
#' @examples 
#' 
#' ## form a Lexis object
#' library(Epi)
#' data(sibr)
#' x <- sibr[1:10,]
#' x[1:5,]$sex <- 0 ## pretend some are male
#' x <- Lexis(data = x,
#'            entry = list(AGE = dg_age, CAL = get.yrs(dg_date)),
#'            exit = list(CAL = get.yrs(ex_date)),
#'            entry.status=0, exit.status = status)
#' x <- splitMulti(x, breaks = list(CAL = seq(1993, 2013, 5), 
#'                                  AGE = seq(0, 100, 50)))
#' 
#' ## these produce the same results (with differing ways of determining aggre)
#' a1 <- laggre(x, aggre = list(gender = factor(sex, 0:1, c("m", "f")), 
#'              agegroup = AGE, period = CAL))
#' 
#' a2 <- laggre(x, aggre = c("sex", "AGE", "CAL"))
#' 
#' a3 <- laggre(x, aggre = list(sex, agegroup = AGE, CAL))
#' 
#' ## returning also empty levels
#' a4 <- laggre(x, aggre = c("sex", "AGE", "CAL"), type = "full")
#' 
laggre <- function(lex, aggre = NULL, breaks = NULL, type = c("unique", "full"), subset = NULL, substituted = FALSE, verbose = FALSE) {
  ## a generalized aggregator for split Lexis objects
  ## input: lex: a Lexis object that has been split somehow; 
  ##        aggre: an expression / list of expressions / a character vector of names;
  ##        may have been substituted, but user must use substitued = TRUE then
  ##        some way to define style of aggregation
  ## output: a long-format data.frame or data.table, where transitions and person-time is tabulated.
  ## TODO: "full" type aggregating
  allTime <- proc.time()
  
  type <- match.arg(type[1], c("non-empty", "unique", "full", "cartesian"))
  if (type == "cartesian") type <- "full"
  if (type == "non-empty") type <- "unique"
  
  if (verbose) cat("Aggregation type: ", type, " \n")
  
  if (is.null(breaks)) breaks <- copy(attr(lex, "breaks"))
  checkBreaksList(lex, breaks)
  
  foundScales <- copy(attr(lex, "time.scales"))
  if (length(foundScales) == 0) foundScales <- names(breaks)
  if (length(foundScales) == 0 ) stop("could not determine names of time scales; is the data a Lexis object?")
  
  ## subset & drop -------------------------------------------------------------
  subset <- substitute(subset)
  subset <- evalLogicalSubset(lex, subset)
  
  ## aggre argument ------------------------------------------------------------
  ags <- if (!substituted) substitute(aggre) else aggre
  aggre <- evalPopArg(lex[1,], arg = ags, DT = TRUE) ## for checking aggre argument type
  
  argType <- if (!is.null(aggre)) attr(aggre, "evalPopArg") else "NULL"
  if (verbose) cat("Used aggre argument:", deparse(ags),"\n")
  if (verbose) cat("Type of aggre argument:", argType, "\n")
  if (argType != "NULL") {
    if (argType == "character") {
      av <- eval(ags, envir = parent.frame(1L))
    } else {
      av <- all.vars(ags)
    } 
    
    ## maybe below is a bad idea to check, might use only variables from
    ## global env instead of data...
#     if (any(!av %in% names(lex))) {
#       badAggre <- deparse(ags)
#       stop("none of the variables used in aggre were found in lex; the aggre expression was '", badAggre, "'")
#     }
    
  } else {
    av <- NULL
    ags <- substitute(list())
    type <- "unique"
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
    
    ## NEW METHOD: modify time scales in place 
    ## (while taking copies of old values and returning them at exit)
    oldScaleValues <- with(lex, mget(aggScales))
    
    on.exit({
      set(lex, j = aggScales, value = oldScaleValues)
    }, add = TRUE)
    
    for (sc in aggScales) {
      set(lex, j = sc, value = cutLow(lex[[sc]], breaks = breaks[[sc]]))
    }
    
    if (verbose) cat("Time taken by cut()'ting time scales: ", timetaken(cutTime), "\n")
  }
  
  othVars <- setdiff(av, aggScales)
  if (verbose && length(othVars) > 0) {
    catOthVars <- paste0("'", othVars, "'", collapse = ", ")
    cat("Detected the following non-time-scale variables to be utilized in aggregating:", catOthVars, "\n")
  }
  
  
  aggre <- evalPopArg(if (all(subset)) lex else lex[subset, ], arg = ags, DT = TRUE)
  
  ## computing pyrs ------------------------------------------------------------
  pyrsTime <- proc.time()
  if (!is.data.table(lex)) {
    ## need to take copy of DF... might do this in the beginning of the function?
    ## for some reason calling by string variable names does not work directly
    DFtemp <- lex[subset, which(names(lex) %in% c(av, "lex.dur", "lex.id"))]
    setDT(DFtemp)
    pyrs <- DFtemp[, .(pyrs = sum(lex.dur), at.risk = sum(!duplicated(lex.id))), keyby = aggre]
    rm(DFtemp)
    } else {
      pyrs <- lex[subset, .(pyrs = sum(lex.dur), at.risk = sum(!duplicated(lex.id))), keyby = aggre]
    }
  
  if (verbose) cat("Time taken by aggregating pyrs: ", timetaken(pyrsTime), "\n")
  
  byNames <- setdiff(names(pyrs), c("pyrs", "at.risk")) ## this will always be as intended
  
  pyrs[is.na(pyrs), pyrs := 0]
  pyrs <- pyrs[pyrs > 0]
  
  
  pyrsDiff <- pyrs[, sum(pyrs)] - sum(lex$lex.dur[subset])
  if (!isTRUE(all.equal(pyrsDiff, 0L))) {
    warning("Found discrepancy in total aggregated pyrs compared to sum(lex$lex.dur); compare results by hand and make sure settings are right \n")
  }
  
  ## cartesian output ----------------------------------------------------------
  if (type == "full") {
    carTime <- proc.time()
    
    varsUsingScales <- NULL
    
    ## which variables used one time scale? and which one?
    ## will only be used in cartesian stuff.
    if (argType == "character") {
      varsUsingScales <- intersect(aggre, aggScales)
      whScaleUsed <- varsUsingScales
    } else if (argType != "NULL") {
      ## note: ags a substitute()'d list at this point always if not char
      whScaleUsed <- lapply(ags[-1], function(x) intersect(all.vars(x), aggScales))
      ## only one time scale should be used in a variable!
      oneScaleTest <- any(sapply(whScaleUsed, function(x) length(x) > 1L))
      if (oneScaleTest) stop("Only one Lexis time scale can be used in any one variable in aggre argument!")
      varsUsingScales <- byNames[sapply(whScaleUsed, function (x) length(x) == 1L)]
      whScaleUsed <- unlist(whScaleUsed)
    }
    
    ceejay <- lapply(aggre, function(x) if (is.factor(x)) levels(x) else sort(unique(x)))
    if (length(aggScales) > 0) {
      ## which variables in ceejay used the Lexis time scales from lex?
      
      ceejay[varsUsingScales] <- lapply(breaks[whScaleUsed], function(x) x[-length(x)])
    }
    
    ceejay <- do.call(CJ, ceejay)
    setkeyv(ceejay, byNames)
    setkeyv(pyrs, byNames)
    
    pyrs <- pyrs[ceejay]
    rm(ceejay)
    
    if (verbose) cat("Time taken by making aggregated data large in the cartesian product sense: ", timetaken(carTime), "\n")
  }
  
  
  ## computing events ----------------------------------------------------------
  
  transTime <- proc.time()
  
  for (var in c("lex.Cst", "lex.Xst")) {
    set(aggre, j = var, value = lex[[var]][subset])
  }
  
  ## sadly, event computations requires information about
  ## 1) transitions (easy) and 2) end points (harder).
  ## end points requires sorting at some point!
  
  ## note: following takes copy of some columns if data was DF; else
  ## tmplex only an alias for lex
  ## note: unfortunately need to take copy of DF here
  
  firstScale <- attr(lex, "time.scales")[1]
  if (!is.data.table(lex)) {
    copyVars <- which(names(lex) %in% c(av, "lex.id", "lex.dur", unique(c(firstScale, aggScales)), "lex.Cst", "lex.Xst"))
    tmplex <- lex[, c(copyVars)]
    setDT(tmplex)
  } else tmplex <- lex
  
  tmpOrder <- makeTempVarName(tmplex, pre = "order_")
  tmplex[, (tmpOrder) := 1:.N]
  
  on.exit({
    if (exists("tmplex")) setcolsnull(tmplex, c(tmpTrans, tmpOrder))
  }, add = TRUE)
  
  old_key <- key(tmplex) ## note: if lex was data.frame, tmplex no key
  setkeyv(tmplex, c("lex.id", firstScale))
  
  tmpTrans <- makeTempVarName(tmplex, pre = "trans_")
  tmplex[, (tmpTrans) := lex.Cst != lex.Xst | !duplicated(lex.id, fromLast = TRUE)]
  
  ## only an event if time line not cut short due to splitting or otherwise
  if (length(aggScales) > 0) {
    tmpCens <- tmplex[, lapply(mget(aggScales), function(x) x + lex.dur)]
    for (k in aggScales) {
      if (length(breaks[[k]]) < 2) stop("length of breaks less 0 or 1 for following time scale by which aggregation was requested: '", k, "'")
      set(tmpCens, j = k, value = tmpCens[[k]] + .Machine$double.eps^0.5 < max(breaks[[k]]))
    }
    tmpCens <- rowSums(tmpCens)
    tmpCens <- tmpCens > 0 & !is.na(tmpCens)
    tmplex[, (tmpTrans) := tmpCens & get(tmpTrans)]
    rm(tmpCens)
  }
  if (length(old_key) > 0) {
    setkeyv(tmplex, old_key) 
  } else {
    setorderv(tmplex, tmpOrder)
  }
  tmplex[, (tmpTrans) := get(tmpTrans) & subset]
  aggre <- aggre[tmplex[[tmpTrans]]]
  
  trans <- tmplex[tmplex[[tmpTrans]], list(obs = .N), keyby = aggre]
  
  setcolsnull(tmplex, c(tmpTrans, tmpOrder))
  rm(tmplex, aggre)
  
  if (verbose) cat("Time taken by aggregating events: ", timetaken(transTime), "\n")
  
  ## casting & merging ---------------------------------------------------------
  
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
  trans <- trans[pyrs]; rm(pyrs)
  
  trans[, (tmpDum) := NULL]
  byNames <- setdiff(byNames, tmpDum)
  setcolorder(trans, c(byNames, "pyrs", "at.risk", sort(transitions)))
  
  if (verbose) cat("Time taken by merging pyrs & transitions: ", timetaken(mergeTime), "\n")
  
  
  for (var in c(transitions)) {
    trans[is.na(get(var)), (var) := 0L]
  }
  trans[is.na(pyrs), pyrs := 0]
  
  
  ## final touch ---------------------------------------------------------------
  setaggre(trans, obs = transitions, pyrs = "pyrs", by = byNames, obs.exp = NULL)
  setattr(trans, "breaks", breaks)
  if (!getOption("popEpi.datatable")) setDFpe(trans)
  if (verbose) cat("Time taken by laggre(): ", timetaken(allTime), "\n")
  
  
  
  trans[]
}












