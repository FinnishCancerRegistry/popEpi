


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

survtab_aggre <- function(data, 
                          surv.breaks=NULL, 
                          surv.scale=NULL,
                          
                          print = NULL,
                          adjust = NULL,
                          weights = NULL,
                          
                          event.values = NULL,  
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
  
  # check arguments ------------------------------------------------------------
  
  surv.type <- match.arg(surv.type, c("surv.obs","surv.rel","surv.cause", "cif.obs", "cif.rel"))
  surv.method <- match.arg(surv.method, c("lifetable","hazard"))
  if (relsurv.method == "EdererII") relsurv.method <- "e2"
  if (relsurv.method == "Pohar-Perme") relsurv.method <- "pp"
  relsurv.method <- match.arg(relsurv.method, c("e2", "pp"))
  conf.type <- match.arg(conf.type, c("log","log-log","plain"))
  if (verbose) {starttime <- proc.time()}
  
  ## if event.values is not defined, use first level
  pot.event.values <- unique(data$lex.Xst)
  if (is.numeric(data$lex.Cst)) {
    pot.entry.value <- intersect(0, unique(data$lex.Cst))
    if (length(pot.entry.value) == 0) pot.entry.value  <- unique(data$lex.Cst)[1L]
    pot.event.values <- robust_values(pot.event.values, force = FALSE, messages = FALSE)
    pot.entry.value <- robust_values(pot.entry.value, force = FALSE, messages = FALSE)
  } else {
    if (is.factor(data$lex.Cst)) pot.entry.value <- factor(levels(data$lex.Cst)[1], levels = levels(data$lex.Cst))
    if (length(pot.entry.value) == 0) pot.entry.value  <- unique(data$lex.Cst)[1L]
  }
  
  
  
  
  if (is.null(event.values)) {
    event.values <- setdiff(pot.event.values, pot.entry.value)
    message("event.values was NULL, so chose ", pot.entry.value, " as non-event value")
  } else {
    event.values <- intersect(event.values, pot.event.values)
    
    if (length(event.values) == 0) {
      stop("none of the given event.values were present in data's lex.Xst")
    }
  }
  rm(pot.event.values, pot.entry.value)
  
  # handle breaks in attributes ------------------------------------------------
  
  found_breaks <- NULL
  attrs <- attributes(data)
  if (is.null(attrs$breaks) && is.null(surv.breaks)) {
    stop("Data does not have breaks information and surv.breaks not defined; the former would hold if data is output from laggre or lexpand")
  } else {
    checkBreaksList(data, breaks = attrs$breaks)
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
  
  
  # data prep & subsetting -----------------------------------------------------
  ## todo: make survtab work without taking copy
  subset <- substitute(subset)
  subset <- evalLogicalSubset(data, subset)
  
  all_names_present(data, c("lex.multi","surv.int","delta","Tstart","Tstop"))
  
  
  data <- data[subset, ]
  setDT(data)
  
  rm("subset", pos=-1L, inherits = FALSE)
  
  sutab <- NULL
  
  ## limit data to given surv.ints ---------------------------------------------
  tmpSI <- makeTempVarName(data, pre = "surv.int_")
  data[, (tmpSI) := cutLow(fot, breaks = surv.breaks)]
  data <- data[!is.na(get(tmpSI))]
  
  # variables to print by ------------------------------------------------------
  print <- evalPopArg(data = data, arg = substitute(print), DT = TRUE)
  if (ncol(print) > 0) {
    prVars <- makeTempVarName(data, pre = names(print))
    data[, (prVars) := print]
  } else {
    prVars <- NULL
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
  adjust <- evalPopArg(data = data, arg = substitute(adjust), DT = TRUE)
  if (ncol(adjust) > 0) {
    adVars <- makeTempVarName(data, pre = names(adjust))
    data[, (adVars) := adjust]
  } else {
    adVars <- NULL
  }
  rm(adjust)
  
  # aggregate data to smallest number of rows according to print & adjust ------
  ## NOTE: valVars not yet defined; should contain:
  ##  * always: pyrs OR n, d
  ##  * EdererII: d.exp
  ##  * pp: pyrs.pp OR n.pp, d.exp.pp, d.pp, ...
  data <- data[, lapply(.SD, sum), keyby = c(prVars, adVars), .SDcols = valVars]
  
  weights <- evalPopArg(data, substitute(weights), DT = FALSE)
  
 
  if (!is.data.frame(weights) && is.list(weights)) {
    
    ## need to think if cartesian multiplication is correct...
    weights <- do.call(CJ, weights, args = list(unique = FALSE, sorted = FALSE))
    weVars <- names(weights)
    
    weights[, weights := 1]
    for (k in setdiff(names(weights), "weights")) {
      set(weights, j = "weights", value = weights$weights * weights[[k]])
    }
    weights[, weights := weights / sum(weights)]
    
    weCuts <- data[, lapply(.SD, function(x) if (is.factor(x)) levels(x) else sort(unique(x))), .SDCols = weVars]
    
    ## need to turn old weights to levels of adjust variables here for merging...
    
  } else if (is.data.frame) {
    weights <- copy(weights); setDT(weights)
    weVars <- setdiff(names(weights), "weights")
    weCuts <- weights[, lapply(.SD, function(x) sort(unique(x))), .SDcols = weVars]
    
  } else if (is.character(weights)) {
    if (weights %in% paste0("ICSS", 1:3)) {
      ## note: this will require a known name for the age group variable...
      ## possibly this should only be part of survtab.as
      wena <- weights
      weights <- copy(ICSS)
      weights[, age := c(0, seq(15,85, 5))]
      setnames(weights, c(wena), c("weights"))
      setcolsnull(weights, keep = c("age", "weights"), colorder = TRUE, soft = FALSE)
    } else {
      weVars <- weights
    }
  }
  
  
  ## in essence:
  # data[, mv1 := cutLow(v1, ...)]
  data <- merge(data, weights, by = weVars, all.x = TRUE, all.y = TRUE)
  
  ## this only after evaluating print and adjust!
  # keep only necessary columns ------------------------------------------------
  reqVars <- "obs"
  reqVars <- c(reqVars, if (surv.method == "lifetable") "n" else "pyrs")
  reqVars <- c(reqVars, if (relsurv.method == "e2") "d.exp" else "d.pp")
  ## etc.
  setcolsnull(data, keep=c(prVars, adVars, tmpSI, reqVars), colorder = TRUE, soft = FALSE)
  all_names_present(data, c(prVars, adVars, tmpSI, reqVars))
  
  
  setkeyv(data, c(prVars, adVars, tmpSI))
  
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
  
  return(data)
  
  ## empty surv.int checking ---------------------------------------------------
  
  ## age group weighting should go wrong (NA) sometimes, but
  ## otherwise estimated survival should just end if no one left in an interval
  
  ## test for consecutively empty surv.ints summed over all age groups ---------
  if ("agegr.w" %in% survtab_by_vars) {
    ## with age group weighting, if all age groups have 0 pyrs in some
    ## strata-surv.ints, drop those only
    
    ## first check empty surv.ints are all consecutive...
    setkeyv(sutab, c(survtab_by_vars, "surv.int"))
    conse_test <- sutab[, list(test_pyrs=sum(pyrs)), by=c("surv.int", setdiff(survtab_by_vars, "agegr.w"))]
    conse_test <- conse_test[test_pyrs>0, list(diff=diff(surv.int)), by=setdiff(survtab_by_vars, "agegr.w")]
    conse_test <- conse_test[diff>1]
    ## keep non-consecutively bad surv.int stratas in entirety for inspection
    if (nrow(conse_test) > 0) {
      message("Some survival intervals summed over age groups were empty 
              non-consecutively; returning all survival intervals with 
              some estimates as NA; for closer inspection manually create age
              groups to supply to by.vars")
    } else {
      sutab[, test_pyrs := sum(pyrs), by=c("surv.int", setdiff(survtab_by_vars, "agegr.w"))]
      sutab <- sutab[test_pyrs > 0]
      setcolsnull(sutab, "test_pyrs")
    }
    rm(conse_test)
  }
  
  
  ## other non-consecutive empty surv.ints -------------------------------------
  setkeyv(sutab, c(survtab_by_vars, "surv.int"))
  conse_test <- sutab[pyrs > 0][, list(diff=diff(surv.int)), by= survtab_by_vars]
  conse_test <- conse_test[diff > 1]
  
  ## keep non-consecutively bad surv.int stratas in entirety for inspection
  if (nrow(conse_test) > 0) {
    if ("agegr.w" %in% survtab_by_vars) {
      
      message("Some survival intervals were empty non-consecutively 
              in at least one agegroup-by.vars combination; this 
              will lead to NA cumulative estimates; for a closer 
              look you may e.g. create your own agegroup variable
              and supply it to by.vars")
      
    } else {
      
      message("Some survival intervals were empty non-consecutively; 
              this will lead to NA cumulative estimates; please check 
              function output (for e.g. zero person-years in survival 
              intervals) and rethink function arguments")
      if (length(survtab_by_vars) > 0) {
        setkeyv(sutab, survtab_by_vars)
        setkeyv(conse_test, survtab_by_vars)
        keep_bad <- conse_test[sutab]
        keep_bad[, diff := NULL]
      } else {
        keep_bad <- sutab[pyrs == 0]
      }
      
      keep_good <- sutab[pyrs > 0]
      setcolorder(keep_bad, names(sutab))
      setcolorder(keep_good, names(sutab))
      sutab <- rbindlist(list(keep_bad, keep_good))
      setkeyv(sutab, c(survtab_by_vars, "surv.int"))
      sutab <- unique(sutab)
      rm(keep_bad, keep_good)
      
    }
    
  } else {
    if (!"agegr.w" %in% survtab_by_vars) sutab <- sutab[pyrs>0]
  }
  rm(conse_test)
  
  
  # create and print table of bad surv.ints ------------------------------------
  if (!is.null(survtab_by_vars)) {
    if (sutab[surv.obs == 0 | is.na(surv.obs), .N] > 0) {
      
      zerotab <- sutab[surv.obs == 0 | is.na(surv.obs), 
                       list(first.bad.surv.int = min(as.integer(surv.int)), 
                            last.bad.surv.int = max(as.integer(surv.int)), 
                            surv.obs=min(surv.obs)), keyby = survtab_by_vars]
      
      
      message("Some cumulative surv.obs were zero or NA in the following strata:")
      print(zerotab)
      if (surv.method == "lifetable" && sutab[surv.obs == 0, .N] > 0) {
        message("Zero surv.obs leads to zero relative survivals as well.")
        message("Age group weighting WILL use the zero surv.obs / relative survival values.")
      }
      
    }
    
  }
  
  # compute cause-specific survivals  ------------------------------------------
  if (surv.type == "surv.cause") {
    comp.st.cs <- function(cs.table, cs.by.vars = survtab_by_vars) {
      #       gs.data[, n.eff := n.start - n.cens/2 + n.de/2 + n.de.cens/4] # + d.de/2
      # n.cens_1 := n.cens + (d-d_1)
      # n.de.cens := n.de.cens + (d.de - d.de_1)
      for (k in event.values) {
        d_k <- paste0("d", k)
        #         d.de_k <- paste0("d.de",k)
        
        n.eff_k <- paste0("n.eff",k)
        
        ## old: " := n.start - (n.cens + (d-", d_k,")/2 + n.de/2 + (n.de.cens + d.de - ", d.de_k,")/4 )"
        expr <- paste0(n.eff_k, " := n.start - (n.cens + (d-", d_k,")/2 )")
        
        cs.table[,  eval(parse(text= expr))] # + d.de/2
        
      }
      
      surv_names <- names(cs.table)[grep("surv.obs", names(cs.table))]
      surv_names <- c("d", "n.eff", surv_names)
      setnames(cs.table, surv_names, paste0(surv_names, ".orig"))
      
      for (k in event.values) {
        setnames(cs.table, paste0(c("d", "n.eff"),k), c("d", "n.eff"))
        
        if (surv.method=="lifetable") {
          comp.st.surv.obs.lif(surv.table = cs.table, surv.by.vars = cs.by.vars)
        }
        if (surv.method=="hazard") {
          comp.st.surv.obs.haz(surv.table = cs.table, surv.by.vars = cs.by.vars)
        }
        os.table <- comp.st.conf.ints(cs.table, al=1-conf.level, surv="surv.obs", transform = conf.type)
        
        new_surv_names <- setdiff(surv_names, c("d", "n.eff"))
        new_surv_names <- gsub("surv.obs", paste0("surv.obs", k), new_surv_names)
        new_surv_names <- c(paste0(c("d", "n.eff"), k), new_surv_names)
        setnames(cs.table, surv_names, new_surv_names)
        
        
      }
      setnames(cs.table, paste0(surv_names, ".orig"), surv_names)
    }
    
    sutab <- comp.st.cs(sutab)
  }
  
  
  # compute cause-specifc/excess-case CIFs -------------------------------------
  if (surv.type %in% c("cif.obs", "cif.rel")) {
    comp.st.cif <- function(cif.table, cif.by.vars=survtab_by_vars) {
      cif.table <- shift.var(cif.table, id.vars = c(cif.by.vars), shift.var = "surv.int", value.vars = "surv.obs", shift.value=-1L)
      cif.table[is.na(lag1_surv.obs), lag1_surv.obs := 1]
      cif.table[, p.obs := surv.obs/lag1_surv.obs]
      
      if (surv.type == "cif.obs") {        
        d_k <- paste0("d", event.values)
        for (k in event.values) {
          d_var <- paste0("d",k)
          q_var <- paste0("q_", k)
          CIF_var <- paste0("CIF_", k)
          cif.table[, (q_var)   := (1-p.obs)*get(d_var)/d]
          cif.table[get(d_var) == 0L | d == 0L, (q_var) := 0]
          cif.table[, (CIF_var) := cumsum(lag1_surv.obs*get(q_var)), by = cif.by.vars]
        }
      }
      
      if (surv.type == "cif.rel") {
        ## assuming d.exp in cif.table
        cif.table[, CIF.rel := (1-p.obs)*(d-d.exp)/d]
        cif.table[d.exp>d, CIF.rel := NA]
        cif.table[, CIF.rel := cumsum(lag1_surv.obs*CIF.rel), by = cif.by.vars]
      }
      
      ## SEs currently not known for CIFs; impute 0 to make comp.st.as() to work
      CIF_vars <- names(cif.table)[substr(names(cif.table),1,3) == "CIF"]
      cif.table[, c(paste0("SE.", CIF_vars)) := 0L]
      
      return(cif.table)
      
    }
    
    
    
    sutab <- comp.st.cif(sutab)
  }
  
  
  # relative survivals ---------------------------------------------------------
  if (surv.type == "surv.rel") {
    
    # compute r.e2 -------------------------------------------------------------
    comp.st.rs <- function(rs.table, rs.by.vars = survtab_by_vars) {
      if (verbose) {rst <- proc.time()}
      
      ## EdererII
      rs.table[,p.exp := exp(-delta*d.exp/pyrs)]
      rs.table[,surv.exp := cumprod(p.exp), by = rs.by.vars]
      
      ##-------------
      if (surv.method == "hazard") {
        comp.st.r.e2.haz(surv.table = rs.table, surv.by.vars = rs.by.vars)
      } else {
        
        if (rs.table[, min(surv.obs, na.rm=T) == 0]) {
          rs.table[surv.obs == 0, surv.exp := 1]
        }
        
        comp.st.r.e2.lif(surv.table = rs.table, surv.by.vars = rs.by.vars)
        
        if (rs.table[, min(surv.obs, na.rm=T) == 0]) {
          rs.table[surv.obs == 0, c("surv.exp","r.e2","SE.r.e2","r.e2.lo","r.e2.hi") := 0]
        }
      }
      
      ## ------------
      
      rs.table <- comp.st.conf.ints(rs.table, al=1-conf.level, surv="r.e2", transform = conf.type)
      
      if (verbose) {cat("Time taken by comp.st.rs(): ", timetaken(rst), "\n")}
      return(rs.table)
    }
    
    sutab <- comp.st.rs(rs.table = sutab)
    
    
  }
  
  # compute r.pp ---------------------------------------------------------------
  if (surv.type == "surv.rel" & relsurv.method == "pp") {
    ## pohar perme: analysis weighted by expected cumulative survival
    comp.st.pp <- function(pp.table, by.vars = survtab_by_vars) {
      if (verbose) {t <- proc.time()}
      
      ## relative survival
      if (surv.method == "hazard") {
        comp.st.r.pp.haz(surv.table = pp.table, surv.by.vars = by.vars)
      } else {
        comp.st.r.pp.lif(surv.table = pp.table, surv.by.vars = by.vars)
        
        if (pp.table[, min(surv.obs, na.rm=T) == 0]) {
          pp.table[surv.obs == 0, c("r.pp","SE.r.pp","r.pp.lo","r.pp.hi") := 0]
        }
      }
      
      pp.table <- comp.st.conf.ints(pp.table, al=1-conf.level, surv="r.pp", transform = conf.type )
      
      if (verbose) {cat("Time taken by comp.st.pp():", timetaken(t), "\n")}
      return(pp.table)
    }
    sutab <- comp.st.pp(pp.table = sutab)
  }
  
  
  # compute age group -weighted estimates --------------------------------------
  if ("agegr.w" %in% survtab_by_vars) {
    
    comp.st.as <- function(as.table, agegr.w.breaks=NULL, agegr.w.weights=NULL, as.by.vars = survtab_by_vars) {
      if (verbose) {t <- proc.time()}
      
      agestd_by_vars <- setdiff(as.by.vars, "agegr.w")
      
      if (is.null(agegr.w.weights)) {
        tabw[,w:=obs/sum(obs), by = agestd_by_vars]
      } else {
        tabw[,w:=agegr.w.weights/sum(agegr.w.weights), by = agestd_by_vars]
      }
      tabw[, obs := NULL]
      ## weights now sum to 1.
      
      ## get agegr.w weights
      tabgr <- merge(as.table, tabw,by=c(as.by.vars), all=T)
      
      ## calculate age standardized figures
      agestd.vars <- c("r.e2", "r.pp")
      CIF_vars <- c("CIF.rel", paste0("CIF_", event.values))
      agestd.vars <- c(agestd.vars, CIF_vars)
      surv_vars <- c("surv.obs", paste0("surv.obs", event.values))
      agestd.vars <- c(agestd.vars, surv_vars)
      agestd.vars <- intersect(agestd.vars, names(tabgr))
      
      agestd.expr <- paste0(paste0(agestd.vars, ".as"), " = sum(", agestd.vars, "*w)", collapse = ", ")
      
      ags.var.expr <- paste0("SE.", agestd.vars)
      ags.var.expr <- paste0(paste0(ags.var.expr, ".as"), "= sqrt(sum((w^2)*", ags.var.expr,"^2 ))")
      ags.var.expr <- paste0(ags.var.expr, collapse = ", ")
      
      agestd_by_vars <- setdiff(c(as.by.vars,"surv.int"), "agegr.w")
      agestd.expr <- paste0("agestd <- tabgr[, list(", agestd.expr, ", ", ags.var.expr, "), by = c(agestd_by_vars)]")
      eval(parse(text=agestd.expr))
      
      ## combine with original table that has other summary figures
      ## need to sum over age group-specific figures first.
      sum_vars <- c("pyrs", "d", "n.cens", "n.start", "n.eff")
      sum_vars <- intersect(names(as.table), sum_vars)
      as.table <- as.table[, lapply(.SD, sum), by = c(agestd_by_vars), .SDcols = sum_vars]
      
      as.table <- merge(as.table, ints[, list(surv.int, delta)], by = "surv.int", all.x=T, all.y=F)
      as.table <- merge(as.table, agestd, by = c(agestd_by_vars), all.x=T, all.y=T)
      
      ## SE not defined for CIFs
      agestd.vars <- setdiff(agestd.vars, CIF_vars)
      
      for (k in paste0(agestd.vars, ".as")) {
        as.table <- comp.st.conf.ints(as.table, al=1-conf.level, surv=k, transform =conf.type)
      }
      
      
      if (verbose) {cat("Time taken by comp.st.as():", timetaken(t), "\n")}
      return(as.table)
    }
    sutab <- comp.st.as(sutab, agegr.w.breaks=agegr.w.breaks, agegr.w.weights=agegr.w.weights)
  }
  
  survtab_by_vars <- setdiff(survtab_by_vars, "agegr.w")
  
  # clean-up -------------------------------------------------------------------
  post.tab <- function(tab) {
    if (format) {
      
      
      ## reorder table
      order <- c("surv.int", "Tstop","delta","pyrs","pyrs.pp","n.start","d","n.cens","d.pp","d.exp","d.exp.pp",
                 "surv.obs.lo","surv.obs","surv.obs.hi","SE.surv.obs",
                 "r.e2.lo","r.e2","r.e2.hi","SE.r.e2",
                 "r.pp.lo","r.pp","r.pp.hi","SE.r.pp",
                 "surv.obs.as.lo","surv.obs.as","surv.obs.as.hi","SE.surv.obs.as",
                 "r.e2.as.lo","r.e2.as","r.e2.as.hi","SE.r.e2.as",
                 "r.pp.as.lo","r.pp.as","r.pp.as.hi","SE.r.pp.as")
      order <- unique(c(survtab_by_vars, order))
      CIF_vars <- names(tab)[substr(names(tab),1,3)=="CIF"]
      order <- c(order, CIF_vars)
      surv.obs.vars <- names(tab)[substr(names(tab), 1,8) == "surv.obs"]
      order <- c(order, surv.obs.vars)
      
      order <- unique(order)
      order <- intersect(order, names(tab))
      
      setcolsnull(tab, setdiff(names(tab), order))
      setcolorder(tab,order)
      
      
      
      if (any(!names(ints) %in% names(tab))) {
        addvars <- setdiff(names(ints), names(tab))
        addvars <- c(addvars, "surv.int")
        setkey(tab, surv.int); setkey(ints, surv.int)
        tab <- ints[,c(addvars), with=FALSE][tab]
      }
      setkeyv(tab, c(survtab_by_vars, "surv.int"))
      
      #       tab[, surv.int :=paste("[", format(round(Tstart,2)),", ", format(round(Tstop,2)),"[", sep="")]
      
      
      signif_vars <- setdiff(order, c("surv.int", survtab_by_vars, "n.start", "n.eff", "n.cens", "d"))
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
      
      
      setcolorder(tab, c(survtab_by_vars, c("surv.int","Tstart", "Tstop"), setdiff(names(tab), c(survtab_by_vars,  c("surv.int","Tstart", "Tstop")))))
    }
    
    
    
    tab
  }
  sutab <- post.tab(sutab)
  
  # attributes -----------------------------------------------------------------
  setkeyv(sutab, c(survtab_by_vars, "surv.int"))
  setattr(sutab, "class", c("survtab", "pe", "data.table", "data.frame"))
  if (!getOption("popEpi.datatable")) setDFpe(sutab)
  setattr(sutab, "surv.breaks", surv.breaks)
  if (length(survtab_by_vars) == 0) survtab_by_vars <- NULL ## might be character(0) 
  setattr(sutab, "by.vars", survtab_by_vars)
  
  if (verbose) {cat("Time taken by whole process: ", timetaken(starttime), "\n")}
  sutab[]
}





globalVariables(c("lex.Xst", "lex.Cst", "lex.dur", "agegr", "ageint_start", 
                  "lex.id", "lex.multi", "entry_age", "age", "fot", "per", "agegr.w", "surv.int",
                  "Tstart", "Tstop", "delta", "entered_late", "entered_int_late", 
                  "mv1", "v1", "weights", "byVars", "tabw", "w", "ints", "agegr.w.breaks", "agegr.w.weights"))

globalVariables(c("n.start", "d", "lex.Xst", "n.cens", "surv.int", 
                  "d.exp", "pop.haz", "d.exp.pp", "d.exp", "pp", "d.pp", "d.pp.2", "n.eff.pp", "pyrs.pp"))
globalVariables(c("ICSS", "n.eff", "pyrs", "test_pyrs", "surv.obs", "valVars",
                  "lag1_surv.obs", "p.obs", "surv.obs", "CIF.rel", "p.exp", "surv.exp", "obs", "agestd"))