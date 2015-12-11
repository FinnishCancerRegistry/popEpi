# library(survival)
# dt[, fot := as.integer(ex_date-dg_date)/365.24]
# s <- dt[, Surv(time = rep(0, nrow(dt)), time2 = fot, event = status %in% 1:2)]


survtab_lex <- function(data, print = NULL, adjust = NULL, breaks = NULL, pophaz = NULL, weights = NULL, event.values = NULL, surv.type = "surv.rel", relsurv.method = "e2", subset = NULL, ...) {
  
  ## checks --------------------------------------------------------------------
  if (!inherits(data, "Lexis")) stop("data is not a Lexis object")
  
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
  ## match break types to time scale types
  ## (don't try to match time scales to breaks)
  for (k in allScales) {
    breaks[[k]] <- matchBreakTypes(data, breaks = breaks[[k]], timeScale = k)
  }
  
  comp_pp <- FALSE
  drop <- TRUE
  if (surv.type == "surv.rel" && relsurv.method == "pp") comp_pp <- TRUE
  if (comp_pp) drop <- FALSE
  
  ## data & subset -------------------------------------------------------------
  subset <- evalLogicalSubset(data, substitute(subset))
  
  x <- if (all(subset)) copy(data) else data[subset,]
  forceLexisDT(x, breaks = attr(data, "breaks"), allScales = allScales, key = TRUE)
  
  print <- evalPopArg(x, substitute(print), DT = TRUE)
  adjust <- evalPopArg(x, substitute(adjust), DT = TRUE)
  
  setcolsnull(x, keep = c("lex.id", "lex.dur", allScales, "lex.Cst", "lex.Xst", setdiff(names(pophaz), "haz")))
  
  if (!is.null(print)) x[, names(print) := print]
  if (!is.null(adjust)) x[, names(adjust) := adjust]
  print <- names(print) ## note: names(NULL) equals NULL
  adjust <- names(adjust)
  av <- c(print, adjust, names(breaks)[1L])
  
  x <- splitMulti(x, breaks = breaks, drop = drop, merge = TRUE)
  forceLexisDT(x, breaks = breaks, allScales = allScales, key = TRUE)
  
#   ## date time scales? ---------------------------------------------------------
#   ## this actually needs to also handle breaks in an intellgent way!
#   areDates <- x[, sapply(.SD, is.Date), .SDcols = allScales]
#   areDifftimes <- x[, sapply(.SD, function(x) inherits(x, "difftime") && units(x) == "days"), .SDcols = allScales]
#   if (any(areDates)) x[, (allScales[areDates]) := lapply(.SD, get.yrs, year.length = "actual"), .SDcols = allScales[areDates]]
#   if (any(areDifftimes)) x[, (allScales[areDifftimes]) := lapply(.SD, function(x) x/365.242199), .SDcols = allScales[areDifftimes]]
  
  if (!is.null(pophaz)) {
    x <- cutLowMerge(x, pophaz, by = setdiff(names(pophaz), "haz"), 
                     mid.scales = intersect(names(pophaz), allScales))
    forceLexisDT(x, breaks = breaks, allScales = allScales, key = TRUE)
  }
  
  
  ## still need to compute pp-weighted figures below. they all have to be done
  ## on the level of the splitted observations!
  
  if (comp_pp) {
    comp_pp_weights(x, surv.scale = names(breaks)[1L], 
                    breaks = breaks[[1L]], haz = "haz", 
                    style = "delta", verbose = FALSE)
    forceLexisDT(x, breaks = breaks, allScales = allScales, key = TRUE)
    
    intelliCrop(x = x, breaks = breaks, allScales = allScales, cropStatuses = TRUE)
    x <- intelliDrop(x, breaks = breaks, dropNegDur = TRUE, check = TRUE)
    forceLexisDT(x, breaks = breaks, allScales = allScales, key = TRUE)
    pp <- vector("list", 4)
    names(pp) <- c("d.pp", "d.exp.pp", "d.pp.2", if (surv.method == "hazard") "pyrs.pp" else "n.eff.pp")
    
    #### TODO ############
    ## - find out which rows are events (transitions)
    ## - find out which rows are censorings (NOTE: exit time < max(breaks))
    ## - multiply d, d.exp, etc. with pp and aggregate.
    
  }
  
  # c("d.pp", "d.exp.pp", "d.pp.2",if (surv.method == "hazard") "pyrs.pp" else "n.eff.pp") else NULL)
  haz <- NULL ## appease R CMD CHECK
  x <- laggre(x, aggre = c(print, adjust, names(breaks)[1]), verbose = FALSE,
              expr = if (surv.type %in% c("surv.rel", "cif.rel") && "haz" %in% names(x)) list(d.exp = sum(haz*lex.dur)) else NULL)
  
  dn <- CJ(C = cens.values, X = event.values)
  dn <- paste0("from",dn$C, "to", dn$X)
  dn <- intersect(dn, names(x))
  
  st <- survtab_ag(x, surv.scale = names(breaks)[1L], adjust = adjust,
                   d = dn, pyrs = pyrs, 
                   print = print, weights = weights, surv.type = surv.type,
                   d.exp = if (surv.type %in% c("surv.rel", "cif.rel")) "d.exp" else NULL)
  
}
# library(Epi)
# library(popEpi)
# dt <- copy(sire)[dg_date < ex_date,]
# dt[, agegr := cut(dg_age, c(0,50,75,Inf))]
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
  checkLexisData(x)
  if (!is.null(breaks)) checkBreaksList(x, breaks = breaks)
  
  tmp <- list()
  oldKey <- key(x)
  if (length(oldKey) == 0L) {
    tmp$order <- makeTempVarName(x, pre = "order_")
    on.exit(setorderv(x, tmp$order), add = TRUE)
    on.exit(setcolsnull(x, tmp$order, soft = TRUE), add = TRUE)
    set(x, j = tmp$order, value = 1:nrow(x))
  } else 
    on.exit(setkeyv(x, oldKey), add = TRUE)
  
  setkeyv(x, c(by, attr(x, "time.scales")[1L]))
  setkeyv(x, by)
  ## rows that actually can be events: transitions and last rows by subject
  whTr <- x[, lex.Cst != lex.Xst]
  whLa <- !duplicated(x, fromLast = TRUE)
  whEv <- whTr | whLa
  
  if (!is.null(breaks)) {
    
    splitScales <- names(breaks)
    
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
    setorderv(x, tmp$order)
    set(x, j = tmp$order, value = NULL)
  } else 
    setkeyv(x, oldKey)
  
  evInd <- x[[tmp$ind]]
  set(x, j = tmp$ind, value = NULL)
  on.exit(expr = {}, add = FALSE) ## removes on.exit expressions from earlier
  
  evInd
}









