


## ok, here's how it goes:
## aggregate data to smallest number of rows according to adjust and print
## merge in weights
## compute weighted estimate
## ???
## profit


makeWeightsDT <- function(data, weights = NULL, adjust = NULL) {
  ## input: data and substitute()'d weights and adjust arguments
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
  
  if (!is.data.table(data)) stop("makeWeightsDT() data must be a data.table; if you see this, blame the maintainer")
  
  if (is.character(weights) && all_names_present(data, weights)) return(weights)
  
  weights <- evalPopArg(data = data, arg = substitute(weights), DT = FALSE)
  
  adjust <- evalPopArg(data = data, arg = substitute(adjust), DT = TRUE)
  adVars <- NULL
  
  if (length(adjust) > 0) {
    adVars <- makeTempVarName(data, pre = names(adjust))
    on.exit(setcolsnull(data, adVars, soft = TRUE), add = TRUE)
    data[, (adVars) := adjust]
  }
  rm(adjust)
  
  
  
  ## Need: 1) weights in DT format; 2) cuts for harmonizing weights and data adjust variables
  tmpWE <- makeTempVarName(data, pre = "weights_")
  if (is.data.frame(weights)) {
    ## data.frame requirements:
    ## - one variable named "weights"
    ## - other variables have corresponding variables in data (the same names)
    ## --> cutting merge will be performed
    all_names_present(weights, "weights")
    setDT(weights)
    
    weVars <- setdiff(names(weights), "weights")
    all_names_present(data, weVars)
    
    tmpWV <- makeTempVarName(data, pre = weVars)
    whNumVars <- weights[, sapply(.SD, is.numeric), .SDcols = weVars]
    numVars <- weVars[whNumVars]
    tmpNumVars <- tmpWV[whNumVars]
    
    ## now numVars e.g. c("fot", "age") & 
    ## tmpNumVars e.g. paste0(c("fot", "age"), "V123456789")
    
    on.exit(setcolsnull(data, setdiff(tmpWV, tmpNumVars), soft = TRUE))
    
    if (length(numVars) > 0) {
      setnames(data, numVars, tmpNumVars)
      
      
      on.exit({
        setcolsnull(data, numVars, soft = TRUE)
        setnames(data, tmpNumVars, numVars)
      }, add = TRUE)
      ## create cutLow()'d variables of numeric vars to merge by
      ## (ensures equivalence of variable levels)
      cuts <- lapply(weights[, mget(numVars)], function(x) sort(unique(x)))
      cuts <- lapply(cuts, function(x) unique(c(x, Inf)))
      
      for (k in 1:length(tmpNumVars)) {
        TNV <- tmpNumVars[k]
        NV <- numVars[k]
        set(data, j = NV, value = cutLow(data[[TNV]], cuts[[NV]]))
      }
      rm(cuts)
    }
    
    data <- merge(data, weights, all.x = TRUE, all.y = FALSE, by = weVars)
    
    return(data[])
  } else if (is.list(weights)) {
    weights <- do.call(function(...) CJ(..., unique = FALSE, sorted = FALSE), weights)
    
    weights[, weights := 1L]
    for (k in weVars) {
      set(weights, j = (weVars), value = weights$weights * weights[[k]])
    }
    weights[, (weVars) := NULL]
    
  } 
  
}
globalVariables("weights")

# ag <- lexpand(sire, birth = "bi_date", entry = "dg_date", exit = "ex_date",
#               status = status %in% 1:2, pophaz = popmort, pp = TRUE,
#               aggre = list(sex, fot), fot = seq(0, 5, 1/12))
# ag[, d.exp := pmax(0L, from0to1 - 3L)]
# st <- survtab_ag(ag, surv.type = "surv.obs", surv.method = "hazard")
# st <- survtab_ag(ag, surv.type = "surv.cause", surv.method = "hazard", d = list(a = from0to1-3, b = 3))
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

  data <- if (!all(subset)) data[subset, ] else copy(data)
  setDT(data)
  
  rm("subset", pos=-1L, inherits = FALSE)
  
  # handle count etc. variables ------------------------------------------------
  valVars <- c("d")
  valVars <- c(valVars, if (surv.method == "hazard")  "pyrs" else c("n", "n.cens"))
  
  valVars <- c(valVars, if (surv.type == "surv.rel" && relsurv.method == "e2")  "d.exp" else NULL)
  
  valVars <- c(valVars, if (surv.type == "surv.rel" && relsurv.method == "pp")  
    c("d.pp", "d.exp.pp", "d.pp.2",if (surv.method == "hazard") "pyrs.pp" else "n.eff.pp") else NULL)
  
  
  fo <- formals("survtab_ag")
  mc <- as.list(match.call())[-1]
  mc <- c(mc, fo[!names(fo) %in% names(mc)])
  
  mc <- mc[valVars]
  
  mc <- lapply(mc, function(x) evalPopArg(data = data, arg = x, DT = TRUE))
  mc[sapply(mc, is.null)] <- NULL
  
  lackVars <- setdiff(valVars, names(mc))
  if (length(lackVars) > 0) stop("following variables needed but missing in specs: ", paste0("'", lackVars, "'", collapse = ", "))
  
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
  tmpValVars <- makeTempVarName(data, pre = valVars)
  
  data[, (tmpValVars) := mc]
  rm(mc)
  
  ## limit data to given surv.ints ---------------------------------------------
  data[, (surv.scale) := cutLow(get(surv.scale), breaks = surv.breaks)]
  data <- data[!is.na(get(surv.scale))]
  
  # variables to print by ------------------------------------------------------
  prSub <- substitute(print)
  print <- evalPopArg(data = data, arg = prSub, DT = TRUE)
  if (length(print) > 0) {
    prVars <- names(print)
    tmpPrVars <- makeTempVarName(data, pre = names(print))
    data[, (tmpPrVars) := print]
  } else {
    prVars <- tmpPrVars <- NULL
  }
  rm(print)
  
  # standardization ------------------------------------------------------------
  ## have 'adjust' argument for defining adjusting vars
  ## and 'weights' as: 
  ## * character string name of weights variable in data;
  ## * character string naming ICSS1-3;
  ## * a data.frame that has lower bounds of categories and weights;
  ## * a list of named weights vectors which will be collated into a data.frame of weights.
  ## * list might allow for e.g. weights = list(sex = c(0.5, 0.5), agegroup = "ICSS1")
  adSub <- substitute(adjust)
  adjust <- evalPopArg(data = data, arg = adSub, DT = TRUE)
  if (length(adjust) > 0) {
    adVars <- names(adjust)
    tmpAdVars <- makeTempVarName(data, pre = adVars)
    data[, (tmpAdVars) := adjust]
  } else {
    adVars <- tmpAdVars <- NULL
  }
  rm(adjust)
  
  # aggregate data to smallest number of rows according to print & adjust ------
  
  ## NOTE: have to do CJ by hand: some levels of adjust or something may not
  ## have each level of e.g. fot repeated!
  cj <- list()
  if (length(c(tmpPrVars, tmpAdVars)) > 0) {
    cj <- lapply(data[, mget(c(tmpPrVars, tmpAdVars))], 
                 function(x) if (is.factor(x)) levels(x) else sort(unique(x)))
  }
  
  cj[[surv.scale]] <- surv.breaks[-length(surv.breaks)]
  cj <- do.call(CJ, cj)
  
  
  setkeyv(data, c(tmpPrVars, tmpAdVars, surv.scale))
  data <- data[cj, lapply(.SD, sum), .SDcols = tmpValVars, by = .EACHI]
  
  for (k in tmpValVars) {
    data[is.na(get(k)), (k) := 0]
  }
  
  ## merge in weights ----------------------------------------------------------
  
  weSub <- substitute(weights)
  weType <- popArgType(weSub)
  if (weType != "NULL") {
    
    data <- makeWeightsDT(data, weights = weSub, adjust = adSub)
    
  }
  ## at this point weights merged in
  
  # keep only necessary columns ------------------------------------------------
  ## this only after evaluating print, weights and adjust!
  ## also use useful names from now on.
  byVars <- c(prVars, adVars)
  setnames(data, c(tmpPrVars, tmpAdVars, tmpValVars), c(byVars, valVars))
  
  data[, Tstop := surv.breaks[-1L]]
  setnames(data, surv.scale, "Tstart")
  
  setkeyv(data, c(byVars, "Tstart"))
  delta <- surv.int  <- Tstop <- Tstart <- NULL
  data[, delta := surv.breaks[-1] - surv.breaks[-length(surv.breaks)]]
  data[, surv.int := 1:.N, by = byVars]
  
  setcolsnull(data, keep=c(byVars, "surv.int", "Tstart", "Tstop", "delta", valVars), colorder = TRUE, soft = FALSE)
  
  setkeyv(data, c(byVars, "surv.int"))
  
  # formulate some needed variables --------------------------------------------
  
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
      data[, (tmpTV) := sum(get(tmpTV)), by=c(prVars, "surv.int")]
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
  
#   # compute cause-specifc/excess-case CIFs -------------------------------------
#   if (surv.type %in% c("cif.obs", "cif.rel")) {
#     comp.st.cif <- function(cif.table, cif.by.vars=byVars) {
  # cif.table[, lag1_surv.obs := shift(surv.obs, n = 1L, type = "lag", fill = 1), by = cif.by.vars]
  
  
#       cif.table[is.na(lag1_surv.obs), lag1_surv.obs := 1]
#       cif.table[, p.obs := surv.obs/lag1_surv.obs]
#       
#       if (surv.type == "cif.obs") {        
#         d_k <- paste0("d", event.values)
#         for (k in event.values) {
#           d_var <- paste0("d",k)
#           q_var <- paste0("q_", k)
#           CIF_var <- paste0("CIF_", k)
#           cif.table[, (q_var)   := (1-p.obs)*get(d_var)/d]
#           cif.table[get(d_var) == 0L | d == 0L, (q_var) := 0]
#           cif.table[, (CIF_var) := cumsum(lag1_surv.obs*get(q_var)), by = cif.by.vars]
#         }
#       }
#       
#       if (surv.type == "cif.rel") {
#         ## assuming d.exp in cif.table
#         cif.table[, CIF.rel := (1-p.obs)*(d-d.exp)/d]
#         cif.table[d.exp>d, CIF.rel := NA]
#         cif.table[, CIF.rel := cumsum(lag1_surv.obs*CIF.rel), by = cif.by.vars]
#       }
#       
#       ## SEs currently not known for CIFs; impute 0 to make comp.st.as() to work
#       CIF_vars <- names(cif.table)[substr(names(cif.table),1,3) == "CIF"]
#       cif.table[, c(paste0("SE.", CIF_vars)) := 0L]
#       
#       return(cif.table)
#       
#     }
#     
#     
#     
#     data <- comp.st.cif(data)
#   }
  
  
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
    
    data <- data[, lapply(mget(c(adEsts, adSEs)), function(x) x*weights), by = c(prVars, surv.scale)]
    
  }
  byVars <- setdiff(byVars, adVars)
  
  
  
  # clean-up -------------------------------------------------------------------
  post.tab <- function(tab) {
    if (format) {
      
      
      ## reorder table
      order <- c("surv.int", "Tstart", "Tstop","delta","pyrs","pyrs.pp","n","d","n.cens","d.pp","d.exp","d.exp.pp",
                 "surv.obs.lo","surv.obs","surv.obs.hi","SE.surv.obs",
                 "r.e2.lo","r.e2","r.e2.hi","SE.r.e2",
                 "r.pp.lo","r.pp","r.pp.hi","SE.r.pp",
                 "surv.obs.as.lo","surv.obs.as","surv.obs.as.hi","SE.surv.obs.as",
                 "r.e2.as.lo","r.e2.as","r.e2.as.hi","SE.r.e2.as",
                 "r.pp.as.lo","r.pp.as","r.pp.as.hi","SE.r.pp.as")
      order <- unique(c(prVars, order))
      CIF_vars <- names(tab)[substr(names(tab),1,3)=="CIF"]
      order <- c(order, CIF_vars)
      surv.obs.vars <- names(tab)[substr(names(tab), 1,8) == "surv.obs"]
      order <- c(order, surv.obs.vars)
      
      order <- unique(order)
      order <- intersect(order, names(tab))
      
      setcolsnull(tab, setdiff(names(tab), order))
      setcolorder(tab,order)
      
      
      setkeyv(tab, c(prVars, "surv.int"))
      
      #       tab[, surv.int :=paste("[", format(round(Tstart,2)),", ", format(round(Tstop,2)),"[", sep="")]
      
      
      signif_vars <- setdiff(order, c("surv.int", prVars, "n", "n.eff", "n.cens", "d"))
      signif_vars <- union(signif_vars, c("Tstart", "Tstop"))
      signif_vars <- intersect(signif_vars, names(tab))
      signiff <- function(x) {
        if (is.numeric(x)) {
          signif(x, digits=4)
        } else {
          x
        }
        
      }
      
      ## format surv.int into intervals
      tab[, c(signif_vars) := lapply(.SD, signiff), .SDcols = c(signif_vars)]
      
      NWO <- c(prVars, "surv.int","Tstart", "Tstop", setdiff(names(tab), c(prVars,  "surv.int","Tstart", "Tstop")))
      setcolorder(tab, NWO)
    }
    
    
    
    tab
  }
  data <- post.tab(data)
  
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





globalVariables(c("lex.Xst", "lex.Cst", "lex.dur", "agegr", "ageint_start", "event.values",
                  "lex.id", "lex.multi", "entry_age", "age", "fot", "per", "agegr.w", "surv.int",
                  "Tstart", "Tstop", "delta", "entered_late", "entered_int_late", 
                  "mv1", "v1", "weights", "byVars", "tabw", "w", "ints", "agegr.w.breaks", "agegr.w.weights"))

globalVariables(c("n", "d", "lex.Xst", "n.cens", "surv.int", 
                  "d.exp", "pop.haz", "d.exp.pp", "d.exp", "pp", "d.pp", "d.pp.2", "n.eff.pp", "pyrs.pp"))
globalVariables(c("ICSS", "n.eff", "pyrs", "test_pyrs", "surv.obs", "valVars",
                  "lag1_surv.obs", "p.obs", "surv.obs", "CIF.rel", "p.exp", "surv.exp", "obs", "agestd"))