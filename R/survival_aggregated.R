# ag <- lexpand(sire, birth = "bi_date", entry = "dg_date", exit = "ex_date",
#               status = status %in% 1:2, pophaz = popmort, pp = TRUE,
#               aggre = list(sex, agegr = cut(dg_age, c(0,50,75,Inf)), fot), 
#               fot = seq(0, 5, 1/12))
# ps <- substitute(list(sex, fot))
# as <- substitute(list(agegr))
# vs <- substitute(list(pyrs, at.risk))
# ws <- substitute(list(agegr = c(0.2,0.4,0.4)))
# dt <- makeWeightsDT(ag, print = ps, adjust = as, values = vs, weights = ws, custom.levels = list(fot = (0:59)/12))

makeWeightsDT <- function(data, values = NULL, print = NULL, adjust = NULL, by.other = NULL, custom.levels = NULL, weights = NULL, n = 1L) {
  ## input: data and substitute()'d weights and adjust arguments
  ## custom.levels: for when in CJ expansion a variable should use levels other than
  ## the ones found in data (such as a time scale of survival, for which each
  ## break used to split data should be represented by a row), supply a named list
  ## referring to variables named in print and/or adjust
  ## output: a prepared data.table for merging with aggregated or other data
  ## OR a character string vector naming the weights variable already in data
  ## to compute weighted results with
  ## 'adjust' argument for defining adjusting vars
  ## and 'weights' as: 
  ## * character string name of weights variable in data;
  ## * character string naming ICSS1-3;
  ## * a data.frame that has lower bounds of categories and weights;
  ## * a list of named weights vectors which will be collated into a data.frame of weights.
  ## * list might allow for e.g. weights = list(sex = c(0.5, 0.5), agegroup = "ICSS1")
  
#   if (!is.language(print)) stop("print must be substituted!")
#   if (!is.language(adjust)) stop("adjust must be substituted!")
#   if (!is.language(weights)) stop("weights must be substituted!")
  
  origData <- data
  tmpDum <- makeTempVarName(origData, pre = "dummy_")
  data <- data.table(rep(1L, nrow(origData)))
  setnames(data, 1, tmpDum)
  
  TF <- environment()
  PF <- parent.frame(2L)
  
  # variables to print by ------------------------------------------------------
  prSub <- print
  print <- evalPopArg(data = origData, arg = prSub, DT = TRUE, enclos = PF)
  if (length(print) > 0) {
    prVars <- names(print)
    data[, (prVars) := print]
    data[, (tmpDum) := NULL]
  } else {
    prVars <- tmpDum
  }
  rm(print)
  
  # variables to sum -----------------------------------------------------------
  vaSub <- values
  values <- evalPopArg(data = origData, arg = vaSub, DT = TRUE, enclos = PF)
  if (length(values) > 0) {
    vaVars <- names(values)
    data[, (vaVars) := values]
  } else {
    stop("no values given to sum!")
  }
  rm(values)
  
  # pre-check of weights argument ----------------------------------------------
  weightsTest <- eval(envir  = origData[1,], expr = weights, enclos = PF)
  if (is.data.frame(weightsTest) && length(adjust) == 0) {
    if (!"weights" %in% names(weightsTest)) stop("column 'weights' not found in weights data.frame; if you supplied a data.frame of weights, please check that it has a column named 'weights'; otherwise contact the package maintainer because this was supposed to work.")
    
    adjust <- setdiff(names(weightsTest), "weights")
    badAdjust <- setdiff(adjust, names(origData))
    if (length(badAdjust) > 0) stop("adjust was NULL and weights was a data.frame, but following variables present in weights DF not present in data: ", paste0("'", badAdjust, "'", collapse = ", "))
  } else if (is.character(weightsTest)) {
    stop("argument 'weights' cannot currently be supplied as a character string naming a column in data to ensure correct scaling of weights (to one by every combination of variables in 'print'); please use a list, a vector of weights, or a data.frame of weights instead.")
  }
  rm(weightsTest)
  
  # standardization ------------------------------------------------------------
    adSub <- adjust
  adjust <- evalPopArg(data = origData, arg = adSub, DT = TRUE, enclos = PF)
  if (length(adjust) > 0) {
    adVars <- names(adjust)
    data[, (adVars) := adjust]
  } else {
    adVars <- NULL
  }
  # rm(adjust)
  
  
  # other category vars to keep ------------------------------------------------
  boSub <- by.other
  by.other <- evalPopArg(data = origData, arg = boSub, DT = TRUE, enclos = PF)
  if (length(print) > 0) {
    boVars <- names(by.other)
    data[, (boVars) := by.other]
  } else {
    boVars <- NULL
  }
  rm(by.other)
  
  # aggregate data to smallest number of rows according to print & adjust ------
  
  ## NOTE: have to do CJ by hand: some levels of adjust or something may not
  ## have each level of e.g. fot repeated!
  cj <- list()
  cj <- lapply(data[, mget(c(prVars, adVars, boVars))], 
               function(x) if (is.factor(x)) levels(x) else sort(unique(x)))
  
  if (length(custom.levels) > 0) cj[names(custom.levels)] <- custom.levels
  cj <- do.call(CJ, cj)
  
  
  setkeyv(data, c(prVars, adVars, boVars))
  data <- data[cj, lapply(.SD, sum), .SDcols = vaVars, by = .EACHI]
  
  for (k in vaVars) {
    data[is.na(get(k)), (k) := 0]
  }
  
  setcolsnull(data, tmpDum)
  prVars <- setdiff(prVars, tmpDum); if (length(prVars) == 0) prVars <- NULL
  
  
  ## merge in weights ----------------------------------------------------------
  
  weSub <- weights
  weights <- evalPopArg(data  = origData, arg = weSub, enclos = PF, DT = FALSE)
  if (!is.null(weights)) {
    
    if (is.character(weights)) {
      if (length(weights) > 1) stop("When given as a character string naming a variable in data, the weights argument can only be of length one.")
      all_names_present(origData, weights)
      weights <- with(origData, get(weights))
      ## now as if weights was an expression or symbol, and handled below.
      
    } 
    
    if (!is.data.frame(weights) && is.vector(weights)) {
      ## note: lists are vectors
      if (!is.list(weights)) {
        weights <- list(weights) ## was a vector of values
        if (length(adjust) != 1) stop("Argument 'weights' is a vector of weights, but there are more than one variables to adjust by; make sure 'adjust' is a character vector of length one naming an adjusting variable in data, an expression, or a list of expressions of length one.")
        setattr(weights, "names", adVars[1])
      }
      
      adjust <- lapply(adjust, function(x) if (is.factor(x)) levels(x) else sort(unique(x)))
      
      if (length(adjust) != length(weights)) 
        stop("Mismatch in numbers of elements (variables) in adjust (", length(adjust), ") and weights (", length(weights),"); make sure each given weights has a corresponding variable in adjust and vice versa.")
      weLen <- sapply(weights, length)
      adLen <- sapply(adjust, length)
      badLen <- names(adjust)[weLen != adLen]
      if (length(badLen) > 0) 
        stop("Mismatch in lengths of following adjust and weights arguments' element(s). Names of adjust elements not matching in length with weigths: ", paste0("'", badLen, "'", collapse = ", "))
      
      weVars <- names(weights)
      
      badAdVars <- setdiff(adVars, weVars)
      badWeVars <- setdiff(weVars, adVars)
      if (length(badAdVars) > 0)
        stop("Mismatch in names of elements in adjust and weights; following adjust elements not mentioned in weights: ", paste0("'", badAdVars, "'", collapse = ", "))
      if (length(badWeVars) > 0)
        stop("Mismatch in names of elements in adjust and weights; following weights elements not mentioned in adjust: ", paste0("'", badWeVars, "'", collapse = ", "))
      
      weights <- do.call(function(...) CJ(..., unique = FALSE, sorted = FALSE), weights)
      adjust <- do.call(function(...) CJ(..., unique = FALSE, sorted = FALSE), adjust)
      
      weVars <- paste0(weVars, ".w")
      setnames(weights, adVars, weVars)
      weights[, (adVars) := adjust]
      
      weights[, weights := 1L]
      for (k in weVars) {
        set(weights, j = "weights", value = weights$weights * weights[[k]])
      }
      setcolsnull(weights, delete = weVars, soft = FALSE)
      
      ## NOTE: weights will be repeated for each level of print,
      ## and for each level of print the weights must sum to one for things
      ## to work.
      weights[, weights := weights/sum(weights)]
      
    }
    
    if (is.data.frame(weights)) {
      ## it's a data.frame of weights and corresponding vars to merge by
      data <- merge(data, weights, by = adVars, all.x = TRUE, all.y = TRUE)
    } else {
      stop("Something went wrong: 'weights' was not collated into a data.frame to merge with data. Blame the package maintainer please!")
    }
    
    
  }
  setattr(data, "makeWeightsDT", list(prVars = prVars, adVars = adVars, boVars = boVars, vaVars = vaVars))
  
  return(data[])
  
}
globalVariables("weights")

# ag <- lexpand(sire, birth = "bi_date", entry = "dg_date", exit = "ex_date",
#               status = status %in% 1:2, pophaz = popmort, pp = TRUE,
#               aggre = list(sex, fot), fot = seq(0, 5, 1/12))
# ag[, d.exp := pmax(0L, from0to1 - 3L)]
# st <- survtab_ag(ag, surv.type = "surv.obs", surv.method = "hazard")
# st <- survtab_ag(ag, surv.type = "surv.cause", surv.method = "hazard", d = list(a = from0to1-3, b = 3))

# ag <- lexpand(sire, birth = "bi_date", entry = "dg_date", exit = "ex_date",
#               status = status %in% 1:2, pophaz = popmort, pp = TRUE,
#               aggre = list(sex, agegr = cut(dg_age, c(0,60,70,80, Inf), labels = FALSE), fot), fot = seq(0, 5, 1/12))
# wdt <- data.table(agegr = 1:4, weights = c(0.2, 0.4, 0.3, 0.1))
# st <- survtab_ag(ag, surv.type = "surv.obs", surv.method = "hazard", adjust = "agegr", weights = wdt$weights)
# st <- survtab_ag(ag, surv.type = "surv.obs", surv.method = "hazard", adjust = "agegr", weights = wdt)
# st <- survtab_ag(ag, surv.type = "surv.obs", surv.method = "hazard", adjust = NULL, weights = wdt)
# ag2 <- merge(ag, wdt, by = "agegr")
# st <- survtab_ag(ag, surv.type = "surv.obs", surv.method = "hazard", adjust = NULL, weights = "weights")

## probably cannot allow pre-merging weights into data and supplying name of weights
## column in data since data may not contain all the rows that a cross-join
## (cartesian product, e.g. CJ(var1, var2)) would contain; hence the weights
## will not always sum to one in the right way.

survtab_ag <- function(data, 
                       surv.breaks=NULL, 
                       surv.scale="fot",
                       
                       print = NULL,
                       adjust = NULL,
                       weights = NULL,
                       
                       n = "at.risk",
                       d = "from0to1",
                       n.cens = "from0to0",
                       pyrs = "pyrs",
                       d.exp = "d.exp",
                       
                       surv.type="surv.rel", 
                       surv.method="hazard", 
                       relsurv.method="e2",  
                       
                       subset = NULL,
                       
                       conf.level = 0.95, 
                       conf.type = "log-log",
                       format=TRUE,
                       verbose=FALSE) {
  
  
  # check data -----------------------------------------------------------------
  if (missing(data) || nrow(data) == 0) stop("data missing or has no rows")
  
  if (inherits(data, "aggre") && is.null(attr(data, "aggre")))
    stop("data is an aggre object, but no meta-information about event indicators etc. is present; did you modify data after setting the data to be an aggre object? You may set it again using e.g. as.aggre")
  
  # check arguments ------------------------------------------------------------
  
  surv.type <- match.arg(surv.type, c("surv.obs","surv.rel","surv.cause", "cif.obs", "cif.rel"))
  surv.method <- match.arg(surv.method, c("lifetable","hazard"))
  relsurv.method <- match.arg(relsurv.method, c("e2", "pp", "EdererII", "Pohar-Perme", "pohar-perme", "edererII", "ederer2"))
  if (relsurv.method %in% c("EdererII", "edererII", "ederer2")) relsurv.method <- "e2"
  if (relsurv.method %in% c("Pohar-Perme", "pohar-perme")) relsurv.method <- "pp"
  relsurv.method <- match.arg(relsurv.method, c("e2", "pp"))
  conf.type <- match.arg(conf.type, c("log","log-log","plain"))
  if (verbose) {starttime <- proc.time()}
  
  
  # handle breaks in attributes ------------------------------------------------
  
  found_breaks <- NULL
  attrs <- attributes(data)
  if (is.null(attrs$breaks) && is.null(surv.breaks)) {
    stop("Data does not have breaks information and surv.breaks not defined; this would hold if data is output from laggre or lexpand")
  } else {
    breaks_names <- names(attrs$breaks)
    
    if (!surv.scale %in% breaks_names) {
      stop(paste0("no breaks information found for given surv.scale '", surv.scale, "'"))
    }
    
    found_breaks <- attrs$breaks[[ surv.scale ]]
  }
  
  
  
  if (is.null(surv.breaks) && !is.null(found_breaks)) {
    surv.breaks <- found_breaks
  } else if (any(!surv.breaks %in% found_breaks)) {
    stop("given surv.breaks is not a subset of the breaks used to split data; cannot proceed.")
  }
  surv.breaks <- sort(unique(surv.breaks))
  
  # data prep & subsetting -----------------------------------------------------
  ## todo: make survtab work without taking copy
  subset <- substitute(subset)
  subset <- evalLogicalSubset(data, subset)
  
  origData <- data
  
  data <- if (!all(subset)) data[subset, ] else copy(data)
  setDT(data)
  
  rm("subset", pos=-1L, inherits = FALSE)
  
  # handle count etc. variables ------------------------------------------------
  TF <- environment()
  PF <- parent.frame(1L)
  
  valVars <- c("d")
  valVars <- c(valVars, if (surv.method == "hazard")  "pyrs" else c("n", "n.cens"))
  
  valVars <- c(valVars, if (surv.type == "surv.rel" && relsurv.method == "e2")  "d.exp" else NULL)
  
  valVars <- c(valVars, if (surv.type == "cif.rel")  "d.exp" else NULL)
  
  valVars <- c(valVars, if (surv.type == "surv.rel" && relsurv.method == "pp")  
    c("d.pp", "d.exp.pp", "d.pp.2",if (surv.method == "hazard") "pyrs.pp" else "n.eff.pp") else NULL)
  
  
  fo <- formals("survtab_ag")
  mc <- as.list(match.call())[-1]
  mc <- c(mc, fo[!names(fo) %in% names(mc)])
  
  mc <- mc[valVars]
  
  mc <- lapply(mc, function(x) try(evalPopArg(data = data, arg = x, DT = TRUE, enclos = PF), silent = TRUE))
  mc[sapply(mc, function(x) is.null(x) || is.language(x) || inherits(x, "try-error"))] <- NULL
  
  lackVars <- setdiff(valVars, names(mc))
  if (length(lackVars) > 0) stop("Following arguments were NULL or could not be evaluated but are required: ", paste0("'", lackVars, "'", collapse = ", "), ". Usual suspects: arguments are NULL or refer to variables that cannot be found in data or elsewhere.")
  
  eventVars <- NULL
  ## NOTE: not sure if other arguments than 'd' should be allowed to be of 
  ## length > 1 (cause-specific 'd'); restricted for now to 'd' but easy to
  ## allow in the procedure below.
  for (k in 1:length(mc)) {
    jay <- argName <- names(mc[k])
    cn <- names(mc[[k]])
    
    if (length(cn) > 1) jay <- paste0(jay, "_", cn) ## e.g. d_1, d_2, ...
    if (argName == "d") eventVars <- jay else 
      if (length(cn) > 1) stop("'", argName, "' is of length ", length(cn), "; only 'd' may be of length > 1 of the value arguments")
    setnames(mc[[k]], cn, jay)
    set(mc[[1]], j = jay, value = mc[[k]])
  }
  mc <- mc[[1]]
  
  if (!is.null(eventVars)) {
    set(mc, j = "d", value = rowSums(mc[, mget(eventVars)]))
    valVars <- unique(c(valVars, "d", eventVars))
  }
  
  all_names_present(mc, valVars)
  setcolorder(mc, valVars)
  
  # making weighted table of aggregated values ---------------------------------
  prSub <- substitute(print)
  adSub <- substitute(adjust)
  adType <- popArgType(adSub, data = origData)
  vaSub <- substitute(mc)
  weSub <- substitute(weights)
  weType <- popArgType(weSub, data = origData)
  ssSub <- list(origData[[surv.scale]])
  setattr(ssSub, "names", surv.scale)
  ssSub[[surv.scale]] <- cutLow(ssSub[[surv.scale]], breaks = surv.breaks)
  ssSub <- substitute(ssSub)
  
  ## NOTE: while ssSub will pass the whole column of e.g. fot values, which will
  ## not limit the data to e.g. up 5 years of follow-up if original data went 
  ## further, surv.breaks may be only up to 5 years and will limit the data
  ## in makeWeightsDT using a CJ-merge-trick appropriately (via custom.levels).
  bl <- list(surv.breaks[-length(surv.breaks)])
  setattr(bl, "names", surv.scale)
  
  data <- makeWeightsDT(data = origData, values = vaSub, n = 0L,
                        print = prSub, adjust = adSub, 
                        by.other = ssSub,
                        custom.levels = bl, weights = weSub)
  allVars <- attr(data, "makeWeightsDT")
  prVars <- allVars$prVars
  adVars <- allVars$adVars
  # boVars <- allVars$boVars ## this is surv.scale
  valVars <- allVars$vaVars
  
  byVars <- c(prVars, adVars)
  
  # formulate some needed variables --------------------------------------------
  setkeyv(data, c(byVars, surv.scale))
  data[, Tstop := surv.breaks[-1]]
  setnames(data, surv.scale, "Tstart")
  data[, delta := Tstop - Tstart]
  data[, surv.int := 1:.N, by = byVars]
  setcolorder(data, c(byVars, "surv.int", "Tstart", "Tstop", "delta", valVars, intersect(names(data), "weights")))
  
  if (surv.method == "lifetable") {
    testEvents <- data[, n - shift(n, n = 1, type = "lead", fill = NA), by = byVars ]
    testEvents <- data$n.cens + data$d - testEvents
    
    if (sum(abs(testEvents), na.rm = TRUE)) warning("given n.cens and d do not sum to total number of events and transitions based on n alone; check your variables?")
    rm(testEvents)
    data[, n.eff := n - n.cens/2L]
  }
  
  
  # compute observed survivals  ------------------------------------------------
  if (verbose) ostime <- proc.time()
  
  if (surv.method=="lifetable") {
    comp.st.surv.obs.lif(surv.table = data, surv.by.vars = byVars)
  }
  if (surv.method=="hazard") {
    comp.st.surv.obs.haz(surv.table = data, surv.by.vars = byVars)
  }
  
  data <- comp.st.conf.ints(data, al=1-conf.level, surv="surv.obs", transform = conf.type)
  
  if (verbose) cat("Time taken by computing observed survivals:", timetaken(ostime), "\n")
  
  
  ## empty surv.int checking ---------------------------------------------------
  
  ## age group weighting should go wrong (NA) sometimes, but
  ## otherwise estimated survival should just end if no one left in an interval
  
  testVar <- if (surv.method == "lifetable") "n" else "pyrs"
  tmpTV <- makeTempVarName(data, pre = "testValues_")
  tmpDiff <- makeTempVarName(data, pre = "diff_")
  
  ## test for consecutively empty surv.ints summed over all adVars -------------
  if (length(adVars) > 0) {
    ## with e.g. age group weighting, if all age groups have 0 pyrs/n in some
    ## strata-surv.ints, drop those only
    
    
    ## first check empty surv.ints are all consecutive...
    testVar <- if (surv.method == "lifetable") "n" else "pyrs"
    setkeyv(data, c(byVars, "surv.int"))
    ## note: by used to be c("surv.int", prVars) for some reason, lets see if that was intentional
    conse_test <- data[, sum(get(testVar)), keyby = c(prVars, "surv.int")]
    setnames(conse_test, length(conse_test), tmpTV)
    conse_test <- conse_test[get(tmpTV) > 0, diff(surv.int), keyby = prVars]
    setnames(conse_test, length(conse_test), tmpDiff)
    conse_test <- conse_test[get(tmpDiff)>1]
    ## keep non-consecutively bad surv.int stratas in entirety for inspection
    if (nrow(conse_test) > 0) {
      message("Some survival intervals summed over age groups were empty 
              non-consecutively; keeping all survival intervals with 
              some estimates as NA for inspection")
    } else {
      ## note: by used to be c("surv.int", prVars) for some reason, lets see if that was intentional
      data[, (tmpTV) := sum(get(testVar)), by=c(prVars, "surv.int")]
      data <- data[get(tmpTV) > 0]
      setcolsnull(data, tmpTV)
    }
    rm(conse_test)
  }
  
  ## other non-consecutive empty surv.ints -------------------------------------
  setkeyv(data, c(byVars, "surv.int"))
  conse_test <- data[get(testVar) > 0][, diff(surv.int), by = byVars]
  setnames(conse_test, length(conse_test), tmpDiff)
  conse_test <- conse_test[get(tmpDiff) > 1]
  
  ## keep non-consecutively bad surv.int stratas in entirety for inspection
  if (nrow(conse_test) > 0) {
    if (length(adVars) > 0) {
      
      message("Some survival intervals were empty non-consecutively 
              in at least one combination of print and adjust variables; this 
              will lead to NA cumulative estimates; for a closer 
              look you may e.g. supply what is in adjust to print and check that output")
      
    } else {
      
      message("Some survival intervals were empty non-consecutively; 
              this will lead to NA cumulative estimates; please check 
              function output (for e.g. zero person-years in survival 
              intervals) and rethink function arguments")
      if (length(prVars) > 0) {
        setkeyv(data, prVars)
        setkeyv(conse_test, prVars)
        keep_bad <- conse_test[data]
        keep_bad[, (tmpDiff) := NULL]
      } else {
        keep_bad <- data[get(tmpTV) == 0]
      }
      
      keep_good <- data[get(tmpTV) > 0]
      setcolorder(keep_bad, names(data))
      setcolorder(keep_good, names(data))
      data <- rbindlist(list(keep_bad, keep_good))
      setkeyv(data, c(byVars, "surv.int"))
      data <- unique(data)
      rm(keep_bad, keep_good)
      
    }
    
  } else {
    if (length(adVars) == 0) data <- data[get(testVar) > 0]
  }
  rm(conse_test)
  
  
  
  # create and print table of bad surv.ints ------------------------------------
  if (!is.null(byVars)) {
    if (data[surv.obs == 0 | is.na(surv.obs), .N] > 0) {
      
      zerotab <- data[surv.obs == 0 | is.na(surv.obs), 
                      list(first.bad.surv.int = min(as.integer(surv.int)), 
                           last.bad.surv.int = max(as.integer(surv.int)), 
                           surv.obs=min(surv.obs)), keyby = byVars]
      
      
      message("Some cumulative surv.obs were zero or NA in the following strata:")
      print(zerotab)
      if (surv.method == "lifetable" && data[surv.obs == 0, .N] > 0) {
        message("Zero surv.obs leads to zero relative survivals as well.")
        message("Adjusting with weights WILL use the zero surv.obs / relative survival values.")
      }
      
    }
    
  }
  
  # compute cause-specific survivals  ------------------------------------------
  if (surv.type == "surv.cause") {
    
    ## NOTE: these related to adjusting life-table estimates for delayed entry...
    #       data[, n.eff := n - n.cens/2 + n.de/2 + n.de.cens/4] # + d.de/2
    #       n.cens_1 := n.cens + (d-d_1)
    #       n.de.cens := n.de.cens + (d.de - d.de_1)
    
    if (surv.method == "lifetable") {
      for (k in eventVars) {
        k <- gsub(pattern = "d_", replacement = "", x = k)
        d_k <- paste0("d_", k)
        # d.de_k <- paste0("d.de_",k)
        
        n.eff_k <- paste0("n.eff_",k)
        
        ## old: " := n - (n.cens + (d-", d_k,")/2 + n.de/2 + (n.de.cens + d.de - ", d.de_k,")/4 )"
        # expr <- paste0(n.eff_k, " := n - (n.cens + (d-", d_k,")/2 )")
        
        set(data, j = c(n.eff_k), value = data$n.eff + (data$d - data[[d_k]])/2L ) # + d.de/2
        # data[,  eval(parse(text = expr), envir = .SD)]
        
      }
    }
    
    
    surv_names <- names(data)[grep("surv.obs", names(data))]
    surv_names <- c("d", "n.eff", surv_names)
    setnames(data, surv_names, paste0(surv_names, ".orig"))
    
    for (k in eventVars) {
      
      k <- gsub(pattern = "d_", replacement = "", x = k)
      setnames(data, paste0("d_",k), "d")
      
      if (surv.method=="hazard") {
        comp.st.surv.obs.haz(surv.table = data, surv.by.vars = byVars)
      } else {
        setnames(data, paste0("n.eff_", k), "n.eff")
        comp.st.surv.obs.lif(surv.table = data, surv.by.vars = byVars)
      }
      os.table <- comp.st.conf.ints(data, al=1-conf.level, surv="surv.obs", transform = conf.type)
      
      new_surv_names <- setdiff(surv_names, c("d", "n.eff"))
      new_surv_names <- gsub("surv.obs", paste0("surv.obs.", k), new_surv_names)
      new_surv_names <- c(paste0(c("d.", "n.eff."), k), new_surv_names)
      setnames(data, surv_names, new_surv_names)
      
      
    }
    setnames(data, paste0(surv_names, ".orig"), surv_names)
  }
  
  # compute cause-specifc/excess-case CIFs -------------------------------------
  if (surv.type %in% c("cif.obs", "cif.rel")) {
    
    data[, lag1_surv.obs := shift(surv.obs, n = 1L, type = "lag", fill = 1), by = byVars]
    data[, p.obs := surv.obs/lag1_surv.obs]
    
    if (surv.type == "cif.obs") {
      for (k in eventVars) {
        
        k <- gsub("d_", "", x = k)
        d_k <- paste0("d_", k)
        
        d_var <- paste0("d_",k)
        q_var <- paste0("q_", k)
        CIF_var <- paste0("CIF_", k)
        data[, (q_var)   := (1-p.obs)*get(d_var)/d]
        data[get(d_var) == 0L | d == 0L, (q_var) := 0]
        data[, (CIF_var) := cumsum(lag1_surv.obs*get(q_var)), by = byVars]
      }
    }
    
    if (surv.type == "cif.rel") {
      ## assuming d.exp in data
      data[, CIF.rel := (1-p.obs)*(d-d.exp)/d]
      data[d.exp>d, CIF.rel := NA]
      data[, CIF.rel := cumsum(lag1_surv.obs*CIF.rel), by = byVars]
    }
    
    ## SEs currently not known for CIFs; impute 0 to make adjusting work
    CIF_vars <- names(data)[substr(names(data),1,3) == "CIF"]
    data[, c(paste0("SE.", CIF_vars)) := 0L]
    
    
  }
  
  
  # relative survivals ---------------------------------------------------------
  if (surv.type == "surv.rel") {
    
    # compute r.e2 -------------------------------------------------------------
    comp.st.rs <- function(rs.table, rs.by.vars = byVars) {
      
      ## EdererII
      
      ##-------------
      if (surv.method == "hazard") {
        rs.table[, p.exp := exp(-delta*d.exp/pyrs)] 
        rs.table[, surv.exp := cumprod(p.exp), by = rs.by.vars]
        comp.st.r.e2.haz(surv.table = rs.table, surv.by.vars = rs.by.vars)
      } else {
        
        rs.table[, p.exp := d.exp/n]
        rs.table[, surv.exp := cumprod(p.exp), by = rs.by.vars]
        
        if (rs.table[, min(surv.obs, na.rm=T) == 0]) {
          rs.table[surv.obs == 0, surv.exp := 1]
        }
        
        comp.st.r.e2.lif(surv.table = rs.table, surv.by.vars = rs.by.vars)
        
        if (rs.table[, min(surv.obs, na.rm=T) == 0]) {
          rs.table[surv.obs == 0, intersect(c("surv.exp","r.e2","SE.r.e2","r.e2.lo","r.e2.hi"), names(rs.table)) := 0]
        }
      }
      
      ## ------------
      
      rs.table <- comp.st.conf.ints(rs.table, al=1-conf.level, surv="r.e2", transform = conf.type)
      
      return(rs.table)
    }
    
    data <- comp.st.rs(rs.table = data)
    
    
  }
  
  # compute r.pp ---------------------------------------------------------------
  if (surv.type == "surv.rel" & relsurv.method == "pp") {
    
    all_names_present(data, c("d.pp", "d.exp.pp", "d.pp.2"))
    ## pohar perme: analysis weighted by expected cumulative survival
    comp.st.pp <- function(pp.table, by.vars = byVars) {
      ## relative survival
      if (surv.method == "hazard") {
        all_names_present(data, c("pyrs.pp"))
        comp.st.r.pp.haz(surv.table = pp.table, surv.by.vars = by.vars)
      } else {
        all_names_present(data, c("n.eff.pp"))
        comp.st.r.pp.lif(surv.table = pp.table, surv.by.vars = by.vars)
        
        if (pp.table[, min(surv.obs, na.rm=T) == 0]) {
          pp.table[surv.obs == 0, intersect(c("r.pp","SE.r.pp","r.pp.lo","r.pp.hi"), names(pp.table)) := 0]
        }
      }
      
      pp.table <- comp.st.conf.ints(pp.table, al=1-conf.level, surv="r.pp", transform = conf.type )
      
      return(pp.table)
    }
    data <- comp.st.pp(pp.table = data)
  }
  
  # compute adjusted estimates -------------------------------------------------
  if ("weights" %in% names(data)) {
    adEsts <- c("surv.obs", "r.e2", "r.pp", names(data)[substr(names(data),1,3)=="CIF"])
    adEsts <- intersect(adEsts, names(data))
    adSEs <- paste0("SE.", adEsts)
    
    data.w <- data[, lapply(mget(c(adEsts, adSEs)), function(x) sum(x*weights)), keyby = c(prVars, "surv.int")]
    data <- data[, lapply(mget(valVars), sum), keyby = c(prVars, "surv.int", "Tstart", "Tstop", "delta")]
    data <- merge(data, data.w, by = c(prVars, "surv.int"), all = TRUE)
    setnames(data, c(adEsts, adSEs), paste0(c(adEsts, adSEs), ".as"))
    
    for (var in paste0(adEsts, ".as")) {
      data <- comp.st.conf.ints(data, al=1-conf.level, surv=var, transform =conf.type)
    }
    
    
  }
  
  # clean-up -------------------------------------------------------------------
  ## reorder table
  if (format) {
    order <- c("surv.int", "Tstart", "Tstop","delta","pyrs","pyrs.pp","n","d","n.cens","d.pp","d.exp","d.exp.pp",
               "surv.obs.lo","surv.obs","surv.obs.hi","SE.surv.obs",
               "r.e2.lo","r.e2","r.e2.hi","SE.r.e2",
               "r.pp.lo","r.pp","r.pp.hi","SE.r.pp",
               "surv.obs.as.lo","surv.obs.as","surv.obs.as.hi","SE.surv.obs.as",
               "r.e2.as.lo","r.e2.as","r.e2.as.hi","SE.r.e2.as",
               "r.pp.as.lo","r.pp.as","r.pp.as.hi","SE.r.pp.as")
    order <- unique(c(prVars, order))
    CIF_vars <- names(data)[substr(names(data),1,3)=="CIF"]
    order <- c(order, CIF_vars)
    surv.obs.vars <- names(data)[substr(names(data), 1,8) == "surv.obs"]
    order <- c(order, surv.obs.vars)
    
    order <- unique(order)
    order <- intersect(order, names(data))
    
    setcolsnull(data, setdiff(names(data), order))
    setcolorder(data,order)
    
    
    setkeyv(data, c(prVars, "surv.int"))
    
    #       data[, surv.int :=paste("[", format(round(Tstart,2)),", ", format(round(Tstop,2)),"[", sep="")]
    
    
    ## rounding & formatting
    signif_vars <- setdiff(order, c("surv.int", prVars, "n", "n.eff", "n.cens", "d"))
    signif_vars <- union(signif_vars, c("Tstart", "Tstop"))
    signif_vars <- intersect(signif_vars, names(data))
    signiff <- function(x) {
      if (is.numeric(x)) {
        signif(x, digits=4)
      } else {
        x
      }
      
    }
    
    data[, c(signif_vars) := lapply(.SD, signiff), .SDcols = c(signif_vars)]
    
    NWO <- c(prVars, "surv.int","Tstart", "Tstop", setdiff(names(data), c(prVars,  "surv.int","Tstart", "Tstop")))
    setcolorder(data, NWO)
  }
  
  # attributes -----------------------------------------------------------------
  setkeyv(data, c(prVars, "surv.int"))
  setattr(data, "class", c("survtab", "pe", "data.table", "data.frame"))
  if (!getOption("popEpi.datatable")) setDFpe(data)
  setattr(data, "surv.breaks", surv.breaks)
  if (length(prVars) == 0) prVars <- NULL ## might be character(0) 
  setattr(data, "by.vars", prVars)
  
  if (verbose) cat("Time taken by whole process: ", timetaken(starttime), "\n")
  data[]
}