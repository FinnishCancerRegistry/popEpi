
# library(survival)
# library(Epi)
# ## NOTE: recommended to use factor status variable
# x <- Lexis(entry = list(FUT = 0, AGE = dg_age, CAL = get.yrs(dg_date)),
#            exit = list(CAL = get.yrs(ex_date)),
#            data = sire[sire$dg_date < sire$ex_date, ],
#            exit.status = factor(status, levels = 0:2,
#                                 labels = c("alive", "canD", "othD")),
#            merge = TRUE)
# 
# ## phony variable
# set.seed(1L)
# x$group <- rbinom(nrow(x), 1, 0.5)
# x$agegr <- cut(x$dg_age, c(0,45,60,Inf), right=FALSE)
# 
# ## observed survival
# pm <- copy(popEpi::popmort)
# names(pm) <- c("sex", "CAL", "AGE", "haz")
# st <- survmean_lex(Surv(FUT, lex.Xst != "alive") ~ group + adjust(agegr),
#                    pophaz = pm, data = x, weights = "internal",
#                    breaks = list(FUT = seq(0, 10, 1/12)))
# st <- survmean_lex(Surv(FUT, lex.Xst != "alive") ~ agegr,
#                    pophaz = pm, data = x, weights = NULL,
#                    breaks = list(FUT = seq(0, 10, 1/12)))
# st <- survmean_lex(Surv(FUT, lex.Xst != "alive") ~ 1,
#                    pophaz = pm, data = x, weights = NULL,
#                    breaks = list(FUT = seq(0, 10, 1/12)))
survmean_lex <- function(formula, data, adjust = NULL, weights = NULL, breaks=NULL, pophaz = NULL, 
                         ext.breaks = NULL, ext.pophaz = pophaz, r = 1.00, 
                         subset = NULL, verbose = FALSE, surv.method = "hazard") {
  pt <- proc.time()
  TF <- environment()
  PF <- parent.frame(1L)
  
  surv.method <- match.arg(surv.method, c("hazard", "lifetable"))
  
  if(!requireNamespace("survival")) stop("Need to load package survival to proceed")
  
  checkLexisData(data, check.breaks = FALSE)
  checkPophaz(data, pophaz, haz.name = "haz")
  checkPophaz(data, ext.pophaz, haz.name = "haz")
  pophaz <- setDT(copy(pophaz))
  ext.pophaz <- setDT(copy(ext.pophaz))
  
  if (!is.numeric(r) && !is.character(r)) {
    stop("r must be either 'auto' or a numeric value giving the assumed ",
         "relative survival ratio to use in extrapolation, e.g. r = 0.95.",
         "See ?survmean_lex for more information.")
  }
  if (is.numeric(r) && r < 0L) stop("numeric r must be > 0, e.g. r = 0.95")
  if (is.character(r)) r <- match.arg(r, "auto")
  
  allScales <- attr(data, "time.scales")
  oldBreaks <- attr(data, "breaks")
  
  
  
  ## breaks --------------------------------------------------------------------
  
  if (!is.null(oldBreaks)) checkBreaksList(data, oldBreaks)
  if (is.null(breaks)) breaks <- oldBreaks
  
  checkBreaksList(data, breaks)
  
  ## hmm - will later on set breaks on the found survival scale
  if (!is.null(ext.breaks))  checkBreaksList(data, ext.breaks)
  
  ## prep & subset data --------------------------------------------------------
  subset <- substitute(subset)
  subset <- evalLogicalSubset(data, subset)
  
  x <- copy(data[subset, ])
  setDT(x)
  setattr(x, "class", c("Lexis", "data.table", "data.frame"))
  
  ## ensure variables to merge pophaz datas by are kept ------------------------
  ## NOTE: temp var names avoid conflicts down the line
  avoid <- unique(c(names(data), names(x), names(pophaz), names(ext.pophaz)))
  
  phNames <- c(names(pophaz), names(ext.pophaz))
  phNames <- setdiff(phNames, c(allScales, "haz"))
  phNames <- intersect(phNames, names(x))
  tmpPhNames <- makeTempVarName(names = avoid, pre = phNames)
  if (!length(phNames)) {
    tmpPhNames <- NULL
  } else {
    phna <- which(phNames %in% names(pophaz))
    if (sum(phna)) setnames(pophaz, phNames[phna], tmpPhNames[phna])
    phna <- which(phNames %in% names(ext.pophaz))
    if (sum(phna)) setnames(ext.pophaz, phNames[phna], tmpPhNames[phna])
    x[, c(tmpPhNames) := copy(.SD), .SDcols = phNames]
  }
  
  ## determine printing & adjusting vars ---------------------------------------
  adSub <- substitute(adjust)
  foList <- usePopFormula(formula, adjust = adSub, data = x, enclos = PF)
  
  ## will avoid conflicts using temp names for tabulating variables
  adNames <- names(foList$adjust)
  prNames <- names(foList$print)
  byNames <- c(prNames, adNames)
  
  avoid <- unique(c(names(data), names(x), names(pophaz), names(ext.pophaz)))
  tmpAdNames <- makeTempVarName(names = avoid, pre = adNames)
  if (!length(adNames)) tmpAdNames <- NULL
  avoid <- unique(c(names(data), names(x), names(pophaz), names(ext.pophaz)))
  tmpPrNames <- makeTempVarName(names = avoid, pre = prNames)
  if (!length(prNames)) tmpPrNames <- NULL
  tmpByNames  <- c(tmpPrNames, tmpAdNames)
  
  
  lexVars <- c("lex.id", allScales, "lex.dur", "lex.Cst", "lex.Xst")
  setcolsnull(x, keep = c(lexVars, tmpPhNames), soft = FALSE)
  if (length(adNames) > 0L) x[, c(tmpAdNames) := foList$adjust]
  if (length(prNames) > 0L) x[, c(tmpPrNames) := foList$print]
  
  ## formula for survtab_lex: we estimate survivals by all levels of both
  ## print and adjust; adjusting here means computing directly adjusted
  ## estimates of the mean survival time, so mean survival times are
  ## weighted later on.
  
  formula <- paste0(deparse(formula[[2L]]), " ~ ")
  if (length(c(tmpAdNames, tmpPrNames)) > 0L) {
    formula <- paste0(formula, paste0(c(tmpPrNames, tmpAdNames), collapse = " + "))
  } else {
    formula <- paste0(formula, "1")
  }
  formula <- as.formula(formula)
  
  ## detect survival time scale ------------------------------------------------
  allScales <- attr(data, "time.scales")
  survScale <- allScales[x[, unlist(lapply(.SD, function(x) {
    identical(x, foList$y$time)
  })), .SDcols = allScales]]
  
  if (length(survScale) == 0L) {
    survScale <- allScales[x[, unlist(lapply(.SD, function(x) {
      all.equal(x, foList$y$time)
    }))]]
  }
  if (length(survScale) == 0L) {
    stop("Could not determine which time scale was used. The formula MUST ",
         "include the time scale used within a Surv() call (or a Surv object)",
         ", e.g. Surv(FUT, lex.Xst) ~ sex. Note that the 'time' argument is ",
         "effectively (and exceptionally) used here to denote the times at ",
         "the beginning of follow-up to identify the time scale existing in ",
         "the supplied data to use. If you are sure you are mentioning a time ",
         "scale in the formula in this manner, complain to the ",
         "package maintainer.")
  }
  
  no_ss <- paste0("Internal error: could not determine survival time scale. ",
                  "Make sure the formula contains e.g. ",
                  "Surv(time = fot, event = lex.Xst), where fot is the time ",
                  "scale you want to compute survivals over. If you are ",
                  "using this right, complain to the package maintainer.")
  all_names_present(x, survScale, msg = no_ss)
  rm(no_ss)
  
  tol <- .Machine$double.eps^0.5
  
  ## check weights & adjust ----------------------------------------------------
  ## this is also used at the end for YPLL
  N_subjects <- x[!duplicated(lex.id) & x[[survScale]] < tol, list(obs=.N), 
                  keyby=eval(TF$tmpByNames)]
  
  if (length(byNames)) setnames(N_subjects, tmpByNames, byNames)
  mwDTtest <- makeWeightsDT(N_subjects, values = list("obs"), print = prNames,
                            adjust = adNames, weights = weights, 
                            internal.weights.values = "obs")
  if (length(byNames)) setnames(N_subjects, byNames, tmpByNames)
  
  ## figure out extrapolation breaks -------------------------------------------
  ## now that the survival time scale is known this can actually be done.
  
  if (is.null(ext.breaks)) {
    ext.breaks <- list(c(seq(0, 1, 1/12), 1.25, 1.5, 2:19, seq(20,50, 5)))
    names(ext.breaks) <- survScale
    checkBreaksList(x, ext.breaks)
  }
  if (!survScale %in% names(ext.breaks)) {
    stop("The survival time scale must be included in the list of breaks ",
         "to extrapolate by ('ext.breaks').")
  }
  if (verbose) {
    cat("Time taken by prepping data:", timetaken(pt), "\n")
  }
  
  ## split ---------------------------------------------------------------------
  ## NOTE: handy to split once here and use that for extrapolation as well
  ## (otherwise would do the same split twice)
  pt <- proc.time()
  xs <- splitMulti(x, breaks = breaks, drop = FALSE, merge = TRUE)
  setDT(xs)
  setattr(xs, "class", c("Lexis", "data.table", "data.frame"))
  
  ## compute observed survivals ------------------------------------------------
  ## NOTE: do not adjust here; adjust in original formula means weighting
  ## the mean survival time results.
  
  st <- survtab_lex(formula, data = xs, breaks = breaks, pophaz = pophaz,
                    surv.type = "surv.rel", surv.method = surv.method)
  
  bareVars <- c(tmpByNames, "Tstop", "surv.obs", "r.e2")
  all_names_present(st, bareVars, msg = 
                      paste0("Internal error: expected to have variables ",
                             "%%VARS%% after computing observed survivals ",
                             "but didn't. Blame the package maintainer if you ",
                             "see this."))
  setcolsnull(st, keep = bareVars)
  setDT(st)
  setkeyv(st, c(tmpByNames, "Tstop"))
  
  if (r == "auto") {
    ## figure out 'r' to use for each stratum; for now simply the last
    ## estimate of RSR, i.e. p_i / p*_i
    autoR <- st[, list(r = r.e2[.N]/r.e2[.N-1L]), keyby = eval(tmpByNames)]
    autoR[, r := pmin(1, r)]
    if (verbose) {
      cat("Estimated relative survival ratio(s) to use to multiply ",
          "extrapolated survivals (note: allowed to be at most 1.00): \n")
      if (length(byNames)) setnames(autoR, tmpByNames, byNames)
      print(autoR)
      if (length(byNames)) setnames(autoR, byNames, tmpByNames)
    }
    
  }
  st[, r.e2 := NULL]
  # st[, surv.int := 1:.N, by = eval(tmpByNames)]
  st[, Tstart := c(0, Tstop[-.N]), by = eval(tmpByNames)]
  st[, delta := Tstop - Tstart]
  ## decumulate for later cumulation
  st[, surv.obs := surv.obs/c(1, surv.obs[-.N]), by= eval(tmpByNames)]
  setnames(st, "surv.obs", "surv.exp")
  
  if (verbose) {
    cat("Time taken by estimating observed survival curves:", 
        timetaken(pt), "\n")
  }
  
  ## compute overall expected survival -----------------------------------------
  ## NOTE: need to do this twice since also observed curve has two steps
  ## and therefore there are two sets of breaks / pophaz's.
  ## 1) compute Ederer I expected survival the same way as observed survival
  ##    was computed
  pt <- proc.time()
  e1a <- comp_e1(xs, breaks = breaks, pophaz = pophaz, immortal = TRUE, 
                 survScale = survScale, by = tmpByNames)
  
  ## 2) extrapolate from there normally; extrapolation starts individually from
  ##    where the curve from 1) would hypothetically end, e.g. if 1) ends at
  ##    T = 10, continue from T + 10
  setkeyv(xs, c("lex.id", survScale))
  setkeyv(xs, c("lex.id"))
  xs <- unique(xs)
  
  mb <- max(breaks[[survScale]])
  xs[, c(allScales) := lapply(.SD, function(col) col + TF$mb), 
     .SDcols = c(allScales)]
  
  ## set survScale to zero while maintaining variable type
  set(xs, j = survScale, value = xs[[survScale]] - xs[[survScale]])
  
  storage.mode(xs$lex.dur) <- "double"
  set(xs, j = "lex.dur", value = Inf)
  
  ## split this as well in advance; can use the same file to extrapolate
  ## observed survival later
  empty_list <- lapply(allScales, function(el) NULL)
  names(empty_list) <- allScales
  setDT(xs)
  forceLexisDT(xs, breaks = empty_list, allScales = allScales)
  xs <- splitMulti(xs, breaks = ext.breaks, drop = FALSE, merge = TRUE)
  setDT(xs)
  forceLexisDT(xs, breaks = ext.breaks, allScales = allScales)
  
  ## compute "extrapolation" to expected survival curve, 
  ## e.g. from T = 10 to T = 100.
  e1b <- comp_e1(xs, breaks = ext.breaks, pophaz = ext.pophaz, immortal = TRUE, 
                 survScale = survScale, by = tmpByNames)
  
  ## 3) combine 1) and 2)
  m1 <- max(breaks[[survScale]])
  m2 <- max(ext.breaks[[survScale]])
  
  e1 <- list(e1a, e1b)
  e1 <- mapply(function(dt, sh, fills) {
    dt <- setDT(copy(dt))
    setnames(dt, survScale, "Tstop")
    setorderv(dt, c(tmpByNames, "Tstop"))
    dt[, Tstop := Tstop + sh]
    dt[, Tstart := c(0,Tstop[-.N]), by = eval(tmpByNames)]
    ## de-cumulate surv.exp for appropriate rbinding
    dt[, surv.exp := surv.exp/c(1,surv.exp[-.N]), by = eval(tmpByNames)]
    setcolorder(dt, c(tmpByNames,"Tstart", "Tstop", "surv.exp"))
    dt
  }, dt = e1, fills = list(m1, m1+m2), sh = list(0, m1), SIMPLIFY = FALSE)
  e1 <- rbindlist(e1)
  setDT(e1)
  e1[, delta := Tstop - Tstart]
  print(e1)
  
  if (verbose) {
    cat("Time taken by computing overall expected survival curves:", 
        timetaken(pt), "\n")
  }
  ## figure out if need to extrapolate observed survival -----------------------
  ## which lex.id's survived beyond 
  extr_IDS <- x[lex.dur + x[[survScale]] >= max(TF$breaks[[survScale]]),
                unique(lex.id)]
  
  ## extrapolate observed survival if needed -----------------------------------
  if (!length(extr_IDS)) {
    
    cat("No extrapolation done since all subjects exited follow-up within ",
        "the range of surv.breaks used to compute observed survivals. \n")
    st.ext <- st[0]
  } else {
    pt <- proc.time()
    xs <- xs[lex.id %in% TF$extr_IDS, ]
    setDT(xs)
    forceLexisDT(xs, breaks = ext.breaks, allScales = allScales)
    st.ext <- comp_e1(xs, breaks = ext.breaks, pophaz = ext.pophaz, 
                      survScale = survScale, by = tmpByNames, immortal = TRUE)
    setnames(st.ext, survScale, "Tstop")
    st.ext[, Tstop := Tstop + max(TF$breaks[[survScale]])]
    st.ext[, Tstart := c(0, Tstop[-.N]), by = eval(tmpByNames)]
    st.ext[, delta := Tstop-Tstart]
    ## decumulate for later cumulation
    st.ext[, surv.exp := surv.exp/c(1, surv.exp[-.N]), by= eval(tmpByNames)]
    
    if (verbose) {
      cat("Time taken by extrapolating observed survival curves:", 
          timetaken(pt), "\n")
    }
  } 
  
  
  ## combine all estimates into one data set -----------------------------------
  ## first multiply the extrapolated survival curve with RSR
  pt <- proc.time()
  if (length(r)) {
    if (is.numeric(r)) {
      st.ext[, surv.exp := surv.exp * TF$r]
    } else if (r == "auto") {
      if (length(tmpByNames)) {
        st.ext <- merge(st.ext, autoR, by = tmpByNames, all.x=TRUE, all.y=FALSE)
      } else {
        ## autoR only has one row.
        st.ext[, r := TF$autoR$r]
      }
      
      st.ext[, surv.exp := surv.exp * st.ext$r]
      st.ext[, r := NULL]
    }
  }
  
  st[, survmean_type := "est"]
  st.ext[, survmean_type := "est"]
  e1[, survmean_type := "exp"]
  x <- rbindlist(list(st, st.ext, e1), use.names = TRUE)
  
  setkeyv(x, c(tmpByNames, "survmean_type",  "Tstop"))
  setcolorder(x, c(tmpByNames, "survmean_type",
                   "Tstart", "Tstop", "delta", "surv.exp"))
  ## cumulate 
  setkeyv(x, c(tmpByNames, "survmean_type",  "Tstop"))
  if (any(duplicated(x))) {
    stop("Internal error: rows were duplicated when computing cumulative ",
         "survival curves. Complain to the package maintainer if you see ",
         "this.")
  }
  x[, surv.exp := cumprod(surv.exp), by = eval(c(tmpByNames, "survmean_type"))]
  
  ## check curve convergence to zero -------------------------------------------
  ## a good integration is based on curves that get very close to 
  ## zero in the end
  mi <- x[, .(surv.exp = round(min(surv.exp),4)*100), 
          keyby = eval(c(tmpByNames, "survmean_type"))]
  if (any(mi$surv.exp > 1)) {
    warning("One or several of the curves used to compute mean survival times ",
            "or expected mean survival times was > 1 % at the lowest point. ",
            "Mean survival estimates may be significantly biased. To avoid ",
            "this, supply breaks to 'ext.breaks' which make the curves longer ",
            ", e.g. ext.breaks = list(FUT = 0:150) when using time scale FUT ",
            "as the survival time scale.")
  }
  mi[, surv.exp := paste0(surv.exp, " %")]
  mi[, survmean_type := factor(survmean_type, c("est", "exp"),
                               c("Observed", "Expected"))]
  setnames(mi, c("survmean_type", "surv.exp"), 
           c("Obs./Exp. curve", "Lowest value"))
  setnames(mi, tmpByNames, byNames)
  if (verbose) {
    cat("Lowest points in observed / expected survival curves by strata:\n")
    print(mi)
  }
  
  ## integrating by trapezoid areas --------------------------------------------
  ## trapezoid area: WIDTH*(HEIGHT1 + HEIGHT2)/2
  ## so we compute "average interval survivals" for each interval t_i
  ## and multiply with interval length.
  
  setkeyv(x, c(tmpByNames, "survmean_type",  "Tstop"))
  sm <- x[, .(survmean = sum(delta*(surv.exp + c(1, surv.exp[-.N]))/2L)), 
          keyby = c(tmpByNames, "survmean_type")]
  
  ## cast ----------------------------------------------------------------------
  
  sm <- cast_simple(sm, columns = "survmean_type", 
                    rows = tmpByNames, values = "survmean")
  
  ## add numbers of subjects, compute YPLL -------------------------------------
  setkeyv(sm, tmpByNames); setkeyv(N_subjects, tmpByNames)
  sm[, "obs" := N_subjects$obs]
  sm[, "YPLL" := (exp-est)*obs]
  
  
  ## adjusting -----------------------------------------------------------------
  ## TODO: check weights in the beginning somehow, use makeWeightsDT() here.
  
  sm <- makeWeightsDT(sm, values = list(c("est", "exp", "obs", "YPLL")),
                      print = tmpPrNames, adjust = tmpAdNames,
                      weights = weights, internal.weights.values = "obs")
  if (length(adNames)) {
    vv <- c("est", "exp", "obs", "YPLL")
    sm[, c(vv) := lapply(.SD, function(col) col*sm$weights), .SDcols = vv]
    sm <- sm[, lapply(.SD, sum), .SDcols = vv, by = eval(tmpPrNames)]
  }
  
  if (verbose) {
    cat("Time taken by final touches:", timetaken(pt), "\n")
  }
  
  ## final touch ---------------------------------------------------------------
  if (length(prNames)) setnames(sm, tmpPrNames, prNames)
  
  this_call <- match.call()
  at <- list(call = this_call, print = prNames, adjust = adNames, 
             breaks = breaks, 
             ext.breaks = ext.breaks, survScale = survScale,
             curves = copy(x))
  setattr(sm, "class", c("survmean","data.table", "data.frame"))
  setattr(sm, "survmean.meta", at)
  if (!getOption("popEpi.datatable")) setDFpe(sm)
  return(sm[])
}

