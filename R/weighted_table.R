#' @title Make a \code{data.table} of Tabulated, Aggregated Values and Weights
#' @description An internal function that aggregates a table
#' and merges in weights.
#' @param data DF/DT; passed to \code{envir} in \code{eval}
#' @param values values to tabulate. Anything \code{evalPopArg} can evaluate.
#' @param print variables to tabulate by and include in \code{prVars} in attributes
#' @param adjust variables to tabulate by and include in \code{adVars} in attributes
#' @param formula a formula such as \code{fot ~ sex} or \code{Surv(fot, lex.Xst) ~ sex}
#' @param Surv.response logical, if \code{TRUE} throws error if response in
#' \code{formula} is not a \code{Surv} object and vice versa
#' @param by.other other variables to tabulate by and include 
#' in \code{boVars} in attributes
#' @param custom.levels a named list of values. When "inflating" the data
#' in the cross-join / cartesian join sense (try e.g. \code{merge(1:5, 1:2)}),
#' one can supply the levels to inflate by using this to ensure inflation is full.
#' E.g. data might only have levels present to do inflation analogous to
#' \code{merge(2:5, 1:2)} although \code{merge(1:5, 1:2)} is intended and 
#' needed. 
#' @param weights a named list or long-form data.frame of weights. See Examples.
#' @param internal.weights.values the variable to use to compute internal
#' weights; only used if \code{weights = "internal"}.
#' @param enclos the enclosing environment passed on to \code{eval}. Variables
#' not found in \code{data} or searched for here.
#' @param NA.text a character string to display in a \code{warning}
#' if there are any rows with missing \code{values} or \code{adjust} values.
#' \strong{special:} key phrase \code{\%\%NA_COUNT\%\%} in text is replaced
#' with the count of missing observations.
#' E.g. \code{"Missing \%\%NA_COUNTS\%\% observations due to derpness."}
#' @examples 
#' makeWeightsDT <- popEpi:::makeWeightsDT ## this avoids errors during tests
#' 
#' sire <- copy(popEpi::sire)
#' set.seed(1L)
#' sire$sex <- rbinom(nrow(sire), 1, 0.5)
#' ag <- lexpand(sire, birth = "bi_date", entry = "dg_date", exit = "ex_date",
#'               status = status %in% 1:2, pophaz = popmort, pp = FALSE,
#'               aggre = list(sex, agegr = cut(dg_age, c(0,50,75,Inf)), fot), 
#'               fot = seq(0, 5, 1/12))
#' ps <- quote(list(sex, fot))
#' as <- quote(list(agegr))
#' vs <- quote(list(pyrs, at.risk))
#' ws <- list(agegr = c(0.2,0.4,0.4))
#' 
#' #### custom.levels usage
#' fb <- seq(0, 5-1/12, 1/12)
#' ag2 <- ag[fot > 0.5,]
#' # repeats fot intervals < 0.5 as empty rows
#' # may be the safest way to do this
#' dt <- makeWeightsDT(ag2, print = ps, adjust = as, 
#'                     values = vs, weights = ws,
#'                     custom.levels = list(fot = fb))
#' 
#' #### use of enclos
#' TF <- environment()
#' gender <- factor(ag$sex)
#' dt <- makeWeightsDT(ag, print = quote(gender), adjust = as, 
#'                     values = vs, weights = ws, enclos = TF)
#' ## or
#' dt <- makeWeightsDT(ag, print = quote(gender), adjust = quote(gender), 
#'                     values = vs, weights = ws,
#'                     enclos = parent.frame(1L))
#' 
#' #### formula usage
#' form <- Surv(fot, factor(from0to1))~gender
#' dt <- makeWeightsDT(ag, formula = form, Surv.response = TRUE,
#'                     adjust = as, values = vs, weights = ws,
#'                     enclos = parent.frame(1L))
#'                     
#' ## or
#' form <- Surv(fot, factor(from0to1))~gender + adjust(agegr)
#' dt <- makeWeightsDT(ag, formula = form, Surv.response = TRUE,
#'                     adjust = NULL, values = vs, weights = ws,
#'                     enclos = parent.frame(1L))
#'                     
#' ## or   
#' form <- from0to1 ~ fot + gender + adjust(agegr)
#' dt <- makeWeightsDT(ag, formula = form, Surv.response = FALSE,
#'                     adjust = NULL, values = vs, weights = ws,
#'                     enclos = parent.frame(1L))            
#' 
#' form <- from0to1 ~ fot + gender + adjust(agegr) + adjust(sex)
#' ws2 <- list(agegr = c(0.33, 0.33, 0.33), sex = c(0.5, 0.5))
#' dt <- makeWeightsDT(ag, formula = form, Surv.response = FALSE,
#'                     adjust = NULL, values = vs, weights = ws2,
#'                     enclos = parent.frame(1L))
makeWeightsDT <- function(data, values = NULL, print = NULL, adjust = NULL, formula = NULL, Surv.response = TRUE, by.other = NULL, custom.levels = NULL, weights = NULL, internal.weights.values = NULL, enclos = parent.frame(1L), NA.text = NULL) {
  
  # environmentalism -----------------------------------------------------------
  TF <- environment()
  PF <- parent.frame(1L)
  if (missing(enclos) || is.null(enclos)) enclos <- PF else 
    if (!is.environment(enclos)) stop("enclos is not an environment")
  
  ## dataism -------------------------------------------------------------------
  if (!is.data.frame(data)) stop("data must be a data.frame")
  ## tmpDum for convenience. will be deleted in the end. (if no tabulating vars)
  origData <- data
  tmpDum <- makeTempVarName(origData, pre = "dummy_")
  data <- data.table(rep(1L, nrow(origData)))
  setnames(data, 1, tmpDum)

  # pre-check weights argument -------------------------------------------------
  weights <- eval(weights, envir = enclos)
  if (is.character(weights)) weights <- match.arg(weights, "internal")
  
  # formula: vars to print and adjust by ---------------------------------------
  
  adSub <- adjust
  adjust <- evalPopArg(data = origData, arg = adSub, DT = TRUE, enclos = enclos)
  
  if (!is.null(formula)) {
    
    foList <- usePopFormula(formula, adjust = adjust, 
                            data = origData, enclos = enclos, 
                            Surv.response = Surv.response)
    print <- foList$print
    adjust <- foList$adjust
  } else {
    
    prSub <- substitute(print)
    print <- evalPopArg(data = origData, arg = prSub, DT = TRUE, enclos = enclos)
    
    
  }
  
  # variables to print by ----------------------------------------------------
  prVars <- tmpDum
  if (length(print) > 0) {
    prVars <- names(print)
    data[, c(prVars) := TF$print]
    data[, c(tmpDum) := NULL]
  } 
  rm(print)
  
  # standardization ----------------------------------------------------------
  ## note: adjust evaluated above with formula
  adVars <- NULL
  if (length(adjust) > 0) {
    adVars <- names(adjust)
    data[, c(adVars) := TF$adjust]
  } 
  rm(adjust)
  
  # variables to sum -----------------------------------------------------------
  if (!is.list(values)) stop("Argument 'values' must be a list ",
                             "(internal error: complain to the package",
                             "maintainer if you see this)")
  values <- lapply(values, function(x) {
    evalPopArg(data = origData, arg = x, DT = TRUE, enclos = enclos)
  })
  for (dt in setdiff(seq_along(values), 1L)) {
    values[[1L]] <- cbind(values[[1L]], values[[dt]])
  }
  values <- values[[1L]]
  vaVars <- NULL
  if (length(values) > 0) {
    vaVars <- names(values)
    data[, c(vaVars) := TF$values]
  } else {
    stop("no values given to sum!")
  }
  rm(values)
  
  # additionally, values to compute internal weights by: -----------------------
  iwVar <- NULL
  if (is.character(weights) && weights == "internal") {
    iw <- substitute(internal.weights.values)
    iw <- evalPopArg(data = origData, iw, DT = TRUE,
                     enclos = enclos, recursive = TRUE,
                     types = c("character", "expression", "list", "NULL"))
    
    if (length(iw) > 1L) stop("Argument 'internal.weights.values' ",
                              "must produce only one column.")
    if (length(iw) == 1L && is.character(weights) && weights == "internal") {
      iwVar <- makeTempVarName(names=c(names(data), names(origData)), pre = "iw_")
      data[, c(iwVar) := TF$iw]
    }
    
    if (length(iwVar) == 0L) {
      stop("Requested computing internal weights, but no values to compute ",
           "internals weights with were supplied (internal error: If you see ",
           "this, complain to the package maintainer).")
    }
    rm(iw)
  }
  
  
  # other category vars to keep ------------------------------------------------
  boSub <- by.other
  by.other <- evalPopArg(data = origData, arg = boSub, DT = TRUE, enclos = enclos)
  boVars <- NULL
  if (length(by.other) > 0) {
    boVars <- names(by.other)
    data[, c(boVars) := TF$by.other]
  } 
  rm(by.other)
  
  # check for aliased columns --------------------------------------------------
  aliased_cols(data, cols = c(prVars, adVars, boVars))
  
  # check for conflicting column names -----------------------------------------
  dupCols <- c(prVars, adVars, boVars, vaVars, iwVar)
  dupCols <- unique(dupCols[duplicated(dupCols)])
  if (length(dupCols) > 0L) {
    dupCols <- paste0("'", dupCols, "'", collapse = ", ")
    stop("Following column names duplicated (columns created by arguments ", 
         "print, adjust, etc.): ", dupCols, ". If you see this, please ensure ",
         "you are not passing e.g. the same column to both for adjusting ",
         "and stratification (printing).")
  }
  
  # check for NA values --------------------------------------------------------
  ## NOTE: NA values of print/by.other are OK. values/adjust are NOT.
  
    NAs <- data[, lapply(.SD, function(x) is.na(x)), .SDcols = c(vaVars, iwVar, adVars)]
    NAs <- rowSums(NAs) > 0L
    if (sum(NAs)) {
      if (!is.null(NA.text)) {
        NA.text <- gsub(x = NA.text, pattern = "%%NA_COUNT%%", 
                        replacement = sum(NAs))
        warning(NA.text)
      }
      data <- data[!NAs]
    }

  # inflate data ---------------------------------------------------------------
  ## on the other hand we aggregate data to levels of print, adjust and 
  ## by.other; on the other hand the data will always have tabulating variables
  ## represented as cross-joined, e.g. merge(1:5, 1:2).
  ## this means some rows might have zeroes as values in the 'values'
  ## columns.
  ## (necessary for correct standardization with weights)
  
  ## NOTE: have to do CJ by hand: some levels of adjust or something may not
  ## have each level of e.g. fot repeated!
  
  sortedLevs <- function(x) {
    if (!is.factor(x)) return(sort(unique(x)))
    
    factor(levels(x), levels(x), levels(x))
  }
  cj <- list()
  cj <- lapply(data[, .SD, .SDcols =  c(prVars, adVars, boVars)], sortedLevs)
  
  
  if (length(custom.levels) > 0) cj[names(custom.levels)] <- custom.levels
  cj <- do.call(function(...) CJ(..., unique = FALSE, sorted = FALSE), cj)
  
  setkeyv(data, c(prVars, adVars, boVars))
  data <- data[cj, lapply(.SD, sum), .SDcols = c(vaVars, iwVar), by = .EACHI]
  
  for (k in c(vaVars, iwVar)) {
    data[is.na(get(k)), (k) := 0]
  }
  
  setcolsnull(data, tmpDum)
  prVars <- setdiff(prVars, tmpDum); if (length(prVars) == 0) prVars <- NULL
  
  ## merge in weights ----------------------------------------------------------
  
  if (!is.null(weights)) {
    
    ## NOTE: adjust used here to contain levels of adjust arguments only
    adjust <- list()
    if (length(adVars) > 0L) {
      adjust <- lapply(data[, eval(adVars), with = FALSE], sortedLevs)
    }
    
    if (is.character(weights)) {
      
      if (weights == "internal") {
        
        weights <- mapply(function(levs, colname) {
          setkeyv(data, colname)
          data[.(levs), lapply(.SD, sum), .SDcols = eval(iwVar), by = .EACHI]
        }, 
        colname = names(adjust), levs = adjust,
        SIMPLIFY = FALSE)
        
        weights <- lapply(weights, function(x) {
          x[[iwVar]]
        })
        
      }
      setcolsnull(data, iwVar)
      setkeyv(data, c(prVars, adVars, boVars))
    } 
    
    if (!is.data.frame(weights) && is.vector(weights)) {
      ## note: lists are vectors
      if (!is.list(weights)) {
        weights <- list(weights) ## was a vector of values
        if (length(adjust) != 1) {
          stop("Argument 'weights' is a vector of weights, but there are more ",
               "than one variables to adjust by; make sure 'adjust' is a ", 
               "character vector of length one naming an adjusting variable ", 
               "in data, an expression, or a list of ", 
               "expressions of length one.")
        }
        setattr(weights, "names", adVars[1])
      }
      
      ## check adjust and weights arguments' congruence ------------------------
      ## check numbers of variables
      if (length(adjust) != length(weights)) 
        stop("Mismatch in numbers of variables (NOT necessarily in the numbers of levels/values within the variables) in adjust (", length(adjust), " variables) and weights (", length(weights)," variables); make sure each given weights vector has a corresponding variable in adjust and vice versa.")
      
      ## check names of variables
      weVars <- names(weights)
      
      badAdVars <- setdiff(adVars, weVars)
      badWeVars <- setdiff(weVars, adVars)
      if (length(badAdVars) > 0)
        stop("Mismatch in names of variables in adjust and weights; following adjust variables not mentioned in weights: ", paste0("'", badAdVars, "'", collapse = ", "))
      if (length(badWeVars) > 0)
        stop("Mismatch in names of variables in adjust and weights; following weights variables not mentioned in adjust: ", paste0("'", badWeVars, "'", collapse = ", "))
      
      weights <- weights[names(adjust)]
      
      ## check variable levels
      weLen <- sapply(weights, length)
      adLen <- sapply(adjust, length)
      badLen <- names(adjust)[weLen != adLen]
      if (length(badLen) > 0) 
        stop("Mismatch in numbers of levels/unique values in adjusting variables and lengths of corresponding weights vectors. Names of mismatching variables: ", paste0("'", badLen, "'", collapse = ", "))
      
      adjust <- do.call(function(...) CJ(..., unique = FALSE, sorted = FALSE), adjust)
      weights <- do.call(function(...) CJ(..., unique = FALSE, sorted = FALSE), weights)
      
      weVars <- paste0(weVars, ".w")
      setnames(weights, adVars, weVars)
      weights[, (adVars) := adjust]
      
      set(weights, j = "weights", value = 1L)
      for (k in weVars) {
        set(weights, j = "weights", value = weights$weights * weights[[k]])
      }
      setcolsnull(weights, delete = weVars, soft = FALSE)
      
      ## NOTE: weights will be repeated for each level of print,
      ## and for each level of print the weights must sum to one for things
      ## to work.
      weights[, weights := weights/sum(weights)]
      
    }
    
    if (!is.data.frame(weights)) stop("Something went wrong: 'weights' was not collated into a data.frame to merge with data. Blame the package maintainer please!")
    
    weights <- data.table(weights)
    weights[, weights := weights/sum(weights)]
    
    ## it's a data.frame of weights and has corresponding vars to merge by
    
    data <- merge(data, weights, by = adVars, all.x = TRUE, all.y = FALSE)
    
    
    
  }
  setattr(data, "makeWeightsDT", list(prVars = prVars, adVars = adVars, boVars = boVars, vaVars = vaVars, NAs = NAs))
  
  return(data[])
  
}