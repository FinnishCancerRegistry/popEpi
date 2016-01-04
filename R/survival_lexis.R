# library(survival)
# dt[, fot := as.integer(ex_date-dg_date)/365.24]
# s <- dt[, Surv(time = rep(0, nrow(dt)), time2 = fot, event = status %in% 1:2)]


survtab_lex <- function(data, print = NULL, adjust = NULL, breaks = NULL, pophaz = NULL, weights = NULL, event.values = NULL, surv.type = "surv.rel", surv.method = "hazard", relsurv.method = "e2", subset = NULL, verbose = FALSE, ...) {
  
  ## checks --------------------------------------------------------------------
  checkLexisData(data)
  
  PF <- parent.frame()
  e <- environment() ## will refer to this explicitly in DT[] to avoid conflicts
  allScales <- attr(data, "time.scales")
  splitScales <- names(breaks)
  
  if (is.null(event.values)) {
    event.values <- if (is.factor(data$lex.Xst)) levels(data$lex.Xst) else sort(unique(data$lex.Xst))
  }
  event.values <- intersect(event.values, unique(data$lex.Xst))
  event.values <- setdiff(event.values, unique(data$lex.Cst))
  cens.values <- setdiff(unique(data$lex.Cst), event.values)
  if (length(cens.values) == 0) stop("could not determine which values of lex.Cst / lex.Xst imply censoring; supply event.values by hand and make sure data has meaningful lex.Cst and lex.Xst values")
  if (length(event.values) == 0) stop("could not determine which values of lex.Xst imply events; supply event.values by hand and make sure data has meaningful lex.Cst and lex.Xst values")
  
  ## ensure breaks make sense --------------------------------------------------
  checkBreaksList(data, breaks = breaks)
  oldBreaks <- attr(data, "breaks")
  if (!is.null(oldBreaks)) checkBreaksList(data, breaks = oldBreaks)
  ## match break types to time scale types
  ## (don't try to match time scales to breaks)
  splitScales <- names(breaks)
  for (k in splitScales) {
    breaks[[k]] <- matchBreakTypes(data, breaks = breaks[[k]], timeScale = k)
  }
  
  comp_pp <- FALSE
  drop <- TRUE
  if (surv.type == "surv.rel" && relsurv.method == "pp") comp_pp <- TRUE
  if (comp_pp) drop <- FALSE
  
  ## data & subset -------------------------------------------------------------
  subset <- evalLogicalSubset(data, substitute(subset))
  x <- data[subset, ]; rm(subset)
  setDT(x)
  forceLexisDT(x, breaks = NULL, allScales = allScales, key = TRUE)
  
  
  ## simplify event and censoring indicators -----------------------------------
  if (!surv.type %in% c("cif.obs", "surv.cause")) {
    ## this simplifies computations
    x[, lex.Cst := 0L]
    x[, lex.Xst := as.integer(lex.Xst %in% event.values)]
    cens.values <- 0L
    event.values <- 1L
  }
  
  ## pre-crop data to speed up computations ------------------------------------
  cropBreaks <- breaks
  if (surv.type == "surv.rel" && relsurv.method == "pp")  {
    ## pp-weights have to be computed from entry to follow-up till roof of breaks;
    ## can only crop along the survival time scale
    cropBreaks <- breaks[1L]
    cb <- protectFromDrop(cropBreaks[[1L]], lower = TRUE)
    cb <- c(min(cb), max(cropBreaks[[1L]]))
    cropBreaks[[1L]] <- cb
  }
  intelliCrop(x = x, breaks = cropBreaks, allScales = allScales, cropStatuses = TRUE)
  x <- intelliDrop(x, breaks = cropBreaks, dropNegDur = TRUE, check = TRUE)
  setDT(x)
  forceLexisDT(x, breaks = oldBreaks, allScales = allScales, key = TRUE)
  
  print <- evalPopArg(x, substitute(print), DT = TRUE, recursive = TRUE, enclos = PF)
  adjust <- evalPopArg(x, substitute(adjust), DT = TRUE, recursive = TRUE, enclos = PF)
  pophazVars <- setdiff(names(pophaz), "haz")
  
  setcolsnull(x, keep = c("lex.id", "lex.dur", allScales, "lex.Cst", "lex.Xst", pophazVars))
  
  if (!is.null(print)) x[, names(print) := print]
  if (!is.null(adjust)) x[, names(adjust) := adjust]
  print <- names(print) ## note: names(NULL) equals NULL
  adjust <- names(adjust)
  
  ## includes time scale to compute survivals over
  aggreVars <- c(print, adjust, names(breaks)[1L]) 
  
  
  splitTime <- proc.time()
  setDT(x)
  forceLexisDT(x, breaks = oldBreaks, allScales = allScales, key = TRUE)
  x <- splitMulti(x, breaks = breaks, drop = FALSE, merge = TRUE)
  setDT(x)
  forceLexisDT(x, breaks = breaks, allScales = allScales, key = TRUE)
  if (verbose) cat("Time taken by splitting Lexis data: ", timetaken(splitTime), "\n")
  
  
#   ## date time scales? ---------------------------------------------------------
#   ## this actually needs to also handle breaks in an intellgent way!
#   areDates <- x[, sapply(.SD, is.Date), .SDcols = allScales]
#   areDifftimes <- x[, sapply(.SD, function(x) inherits(x, "difftime") && units(x) == "days"), .SDcols = allScales]
#   if (any(areDates)) x[, (allScales[areDates]) := lapply(.SD, get.yrs, year.length = "actual"), .SDcols = allScales[areDates]]
#   if (any(areDifftimes)) x[, (allScales[areDifftimes]) := lapply(.SD, function(x) x/365.242199), .SDcols = allScales[areDifftimes]]
  
  if (!is.null(pophaz)) {
    hazTime <- proc.time()
    haz <- NULL ## appease R CMD CHECK
    x <- cutLowMerge(x, pophaz, by = pophazVars, 
                     mid.scales = intersect(pophazVars, allScales))
    forceLexisDT(x, breaks = breaks, allScales =allScales, key = TRUE)
    if (verbose) cat("Time taken by merging population hazards with split Lexis data: ", timetaken(hazTime), "\n")
  }
  
  
  ## still need to compute pp-weighted figures below. they all have to be done
  ## on the level of the splitted observations!
  
  ppNames <- d.pp <- d.pp.2 <- d.exp.pp <- ptime.pp <- n.cens.pp <- NULL
  if (comp_pp) {
    ppTime <- proc.time()
    comp_pp_weights(x, surv.scale = splitScales[1L], 
                    breaks = breaks[[1L]], haz = "haz", 
                    style = "delta", verbose = FALSE)
    setDT(x)
    forceLexisDT(x, breaks = breaks, allScales = allScales, key = TRUE)
    if (verbose) cat("Time taken by computing Pohar-Perme weights: ", timetaken(ppTime), "\n")
    
    intelliCrop(x = x, breaks = breaks, allScales = allScales, cropStatuses = TRUE)
    x <- intelliDrop(x, breaks = breaks, dropNegDur = TRUE, check = TRUE)
    forceLexisDT(x, breaks = breaks, allScales = allScales, key = TRUE)
    
    ppTime <- proc.time()
    pp <- comp_pp_weighted_figures(x, haz = "haz", pp = "pp", by = "lex.id")
    ppNames <- makeTempVarName(x, pre = names(pp))
    x[, c(e$ppNames) := e$pp] ## note: e$pp avoid conflict with possibly existing pp column
    rm(pp)
    
    d.pp.2 <- ppNames[substr(ppNames, 1, 13) == "from0to1.pp.2"]
    d.pp <- ppNames[substr(ppNames, 1, 11) == "from0to1.pp"]
    d.pp <- setdiff(d.pp, d.pp.2)
    d.exp.pp <- ppNames[substr(ppNames, 1, 8) == "d.exp.pp"]
    ptime.pp <- ppNames[substr(ppNames, 1, 8) == "ptime.pp"]
    n.cens.pp <- ppNames[substr(ppNames, 1, 11) == "from0to0.pp"]
    
    if (verbose) cat("Time taken by computing Pohar-Perme weighted counts and person-times: ", timetaken(ppTime), "\n")
  }
  
  d.exp <- NULL
  if (surv.type %in% c("surv.rel", "cif.rel") && "haz" %in% names(x)) {
    d.exp <- makeTempVarName(x, pre = "d.exp_")
    x[, c(e$d.exp) := lex.dur * haz]
  }
  
  
  aggreTime <- proc.time()
  setDT(x)
  forceLexisDT(x, breaks = breaks, allScales = allScales, key = TRUE)
  if (verbose) cat("** verbose messages from laggre: \n")
  x <- laggre(x, by = aggreVars, verbose = verbose,
              sum.values = c(d.exp, ppNames))
  if (verbose) cat("** end of  verbose messages from laggre \n")
  setDT(x)
  setattr(x, "class", c("aggre", "data.table", "data.frame"))
  if (verbose) cat("Time taken by aggregating split Lexis data: ", timetaken(aggreTime), "\n")
  
  dn <- CJ(C = cens.values, X = event.values)
  dn <- paste0("from",dn$C, "to", dn$X)
  dn <- intersect(dn, names(x))
  
  survTime <- proc.time()
  st <- survtab_ag(x, surv.scale = names(breaks)[1L], 
                   adjust = adjust,
                   print = print, 
                   weights = weights, 
                   
                   d = dn, pyrs = pyrs, d.exp = d.exp, 
                   
                   d.pp = d.pp, d.exp.pp = d.exp.pp, d.pp.2 = d.pp.2, 
                   n.cens.pp = n.cens.pp, pyrs.pp = ptime.pp,
                   
                   surv.type = surv.type,
                   surv.method = surv.method,
                   relsurv.method = relsurv.method)
  if (verbose) cat("Time taken by computing survivals with aggregated data: ", timetaken(survTime), "\n")
  st
}
# library(Epi)
# library(popEpi)
# dt <- copy(sire)[dg_date < ex_date,]
# dt[, agegr := cut(dg_age, c(0,50,75,Inf))]
# dt[, sex := rbinom(n = .N, size = 1, prob = 0.5)]
# dt <- Lexis(data = dt, entry = list(FUT = 0, AGE = dg_age, CAL = get.yrs(dg_date)),
#             exit = list(CAL = get.yrs(ex_date)), entry.status = 0L, exit.status = status, merge = TRUE)
# pm <- copy(popEpi::popmort)
# setnames(pm, c("agegroup", "year"), c("AGE", "CAL"))
# st <- survtab_lex(dt, print = NULL, #adjust = "agegr", 
#                   # pophaz = pm,
#                   surv.type = "surv.obs",
#                   # weights = list(agegr = c(0.2,0.4,0.4)),
#                   breaks = list(FUT = seq(0,5,1/12)))
# st <- survtab_lex(dt, print = NULL, #adjust = "agegr", 
#                   pophaz = pm,
#                   surv.type = "surv.rel",
#                   relsurv.method = "pp",
#                   # weights = list(agegr = c(0.2,0.4,0.4)),
#                   breaks = list(FUT = seq(0,5,1/12)))
# st <- survtab_lex(dt, print = NULL, adjust = c("sex","agegr"), 
#                   pophaz = pm,
#                   surv.type = "surv.rel",
#                   relsurv.method = "pp",
#                   weights = list(sex = c(0.5, 0.5), agegr = c(0.2,0.4,0.4)),
#                   breaks = list(FUT = seq(0,5,1/12)))


# ag <- lexpand(sire, birth = "bi_date", entry = "dg_date", exit = "ex_date",
#               status = status %in% 1:2, pophaz = popmort, pp = TRUE,
#               fot = seq(0, 5, 1/12))
# pm2 <- copy(popEpi::popmort)
# setnames(pm2, c("year", "agegroup"), c("per", "age"))
# st <- survtab_lex(ag, print = NULL, #adjust = c("sex","agegr"), 
#                   pophaz = pm2,
#                   surv.type = "surv.rel",
#                   relsurv.method = "pp",
#                   #weights = list(sex = c(0.5, 0.5), agegr = c(0.2,0.4,0.4)),
#                   breaks = list(fot = seq(0,5,1/12)))
detectEvents <- function(x, breaks, tol = .Machine$double.eps^0.5, by = "lex.id") {
  ## INTENTION: given a Lexis object, determines which rows
  ## have an event (a transition or end-point) within the window
  ## determined by breaks (a list of breaks as supplied to e.g. splitMulti).
  ## Usable with split and unsplit data, though it is best to do this
  ## before splitting for efficiency.
  ## NOTE: by should be a character vector specifying variables that identify
  ## unique subjects; the idea is that each subject only has one end-point
  ## and some transitions
  ## NOTE: breaks should be a list of breaks or NULL; if it is NULL,
  ## it is NOT checked whether observations were cut short by the breaks used.
  ## observations cut short are not any kind of events.
  ## OUTPUT: an integer vector coding events as follows:
  ## 0: no event within breaks (row cut short by breaks or subject 
  ##    has multiple rows, of which this is not an event)
  ## 1: transition within breaks
  ## 2: original end-point within breaks and no transition occured (i.e. censoring)
  if (!is.data.table(x)) stop("x must be a data.table; if you see this, send the package maintainer an email")
  # checkLexisData(x)
  if (!inherits(x, "Lexis")) stop("data not a Lexis object")
  if (!is.null(breaks)) {
    if (!is.list(breaks)) stop("breaks must be a named list of breaks vectors")
    if (length(breaks) != length(setdiff(names(breaks), ""))) stop("all elements in breaks list are not named")
  }
  
  tmp <- list()
  oldKey <- key(x)
  if (length(oldKey) == 0L) {
    tmp$order <- makeTempVarName(x, pre = "order_")
    on.exit(setorderv(x, tmp$order), add = TRUE)
    on.exit(setcolsnull(x, tmp$order, soft = TRUE), add = TRUE)
    set(x, j = tmp$order, value = 1:nrow(x))
  } else on.exit(setkeyv(x, oldKey), add = TRUE)
    
  
  setkeyv(x, c(by, names(breaks)[1L]))
  setkeyv(x, by)
  ## rows that actually can be events: transitions and last rows by subject
  whTr <- x[, lex.Cst != lex.Xst]
  whLa <- !duplicated(x, fromLast = TRUE)
  whEv <- whTr | whLa
  
  if (!is.null(breaks)) {
    
    splitScales <- names(breaks)
    if (any(!splitScales %in% names(x))) stop("Following time scales missing from data that data was split by: ", paste0("'", setdiff(splitScales, names(x)), "'", collapse = ", "))
    
    brmax <- lapply(breaks, max)
    brmin <- lapply(breaks, min)
    
    ## detect rows residing within breaks window
    for (sc in splitScales) {
      z <- (x$lex.dur + x[[sc]])[whEv]
      tol_sc <- if (is.double(z)) tol else 0L
      
      ## NOTE: if max of orig values within breaks window, then all may be events
      if (!(max(z) + tol_sc < brmax[[sc]])) whEv[whEv] <- z < brmax[[sc]] - tol_sc
      if (!(min(z) - tol_sc > brmin[[sc]])) whEv[whEv] <- z > brmin[[sc]] + tol_sc
      
    }
    ## whEv now indicates rows that may be events AND which reside within breaks window. 
  }
  
  ## censorings are not transitions, but must reside within breaks window.
  whCe <- whLa & !whTr & whEv
  
  ## need to add event indicator to data since it has been reordered,
  ## reorder back old order, and return the event indicator.
  tmp$ind <- makeTempVarName(x, pre = "event_indicator_")
  on.exit(setcolsnull(x, delete = tmp$ind, soft = TRUE), add = TRUE)
  evInd <- as.integer(whEv)
  evInd <- ifelse(whCe, 2L, evInd)
  set(x, j = tmp$ind, value = evInd)
  
  if (length(oldKey) == 0L) {
    setkeyv(x, NULL)
    setorderv(x, tmp$order)
    set(x, j = tmp$order, value = NULL)
  } else setkeyv(x, oldKey)
  
  
  evInd <- x[[tmp$ind]]
  set(x, j = tmp$ind, value = NULL)
  on.exit(expr = {}, add = FALSE) ## removes on.exit expressions from earlier
  
  
  if (!identical(oldKey, key(x))) stop("keys do not match at function end; send an email to package maintainer if you see this")
  
  evInd
}









