


#' @title Compute mean survival times using extrapolation
#' @description Computes mean survival times based on survival estimation up to
#' a point in follow-up time (e.g. 10 years), after which survival is extrapolated
#' using an appropriate hazard data file (\code{pophaz}) to yield the "full"
#' survival curve. The area under the full survival curve is the mean survival.
#' @author Joonas Miettinen, Karri Seppa
#' @param data a data set of splitted records; e.g. output of \code{\link{lexpand}}
#' @param surv.breaks passed on to \code{\link{survtab}}; if \code{NULL}, uses
#' the existing breaks along time scale \code{"fot"} in \code{data}
#' @param by.vars a character vector of variables names;
#'  e.g. \code{by.vars = "sex"} will calculate 
#' mean survival separately for each unique combination of these variables
#' @param pophaz a data set of appropriate population hazards as given to 
#' \code{lexpand}; will be used in extrapolation - see Details
#' @param r a numeric of length one; multiplies population hazard in \code{pophaz}
#' by this number; used e.g. \code{r = 1.1} if excess hazard of 10 percent should persist
#' in extrapolation
#' @param agegr.w.breaks a numeric vector of fractional years as \code{[a,b)}
#' breaks as in \code{survtab}; will be used to determine standardization age group
#' @param agegr.w.weights a numeric vector of weights
#' breaks as in \code{survtab}; will be used to standardize by age group
#' @param subset a logical condition; e.g. \code{subset = sex == 1}; 
#' subsets the data before computations
#' @param verbose \code{logical}; if \code{TRUE}, the function is returns
#' some messages and results along the run, which may be useful in debugging
#' @param ... any other arguments passed on to \code{survtab} such as
#' \code{surv.method = "lifetable"} for actuarial estimates of observed survival
#' @param ext.breaks advanced; a list of breaks (see \code{\link{lexpand}});
#' used as breaks for extrapolation; see Details
#' @details
#' \strong{Basics}
#' 
#' \code{survmean} computes mean survival times. 
#' This is done using a) observed survival estimates computed with \code{survtab}
#' and b) extrapolated survival probabilities using EdererI method expected
#' survivals for subjects surviving beyond the roof of \code{surv.breaks},
#' up to 100 years forward but only up to the 125th birthday by default. The area under
#' the resulting extrapolated curve is computed via trapezoidal integration,
#' which is the mean survival time.
#' 
#' For extrapolation, the user must supply a \code{pophaz} data set of population
#' hazards. The extrapolation itself is essentially done  
#' by splitting the extrapolated observations and merging population hazards
#' to those records using \code{lexpand}.
#' 
#' The user may compute age-standardized mean survival time estimates using the
#' \code{agegr.w.breaks} and \code{agegr.w.weights}
#' parameters, though this is also fairly simple to do by hand via using the
#' \code{by.vars} argument and merging in the weights yourself.
#' 
#' Note that mean survival is based by default on hazard-based estimates of
#' observed survival as outlined in \code{survtab}. Unlike with actuarial
#' estimates, observed survival can never fall to zero using this method.
#' However, the bias caused by this is likely to be small, and hazard-based
#' estimation allows for e.g. period method estimates of mean survival time.
#' 
#' \strong{Extrapolation tweaks}
#' 
#' One may tweak the accuracy and length of extrapolation by using \code{ext.breaks}:
#' By default the survivals of any survivors beyond the roof of \code{surv.breaks} 
#' are extrapolated up to 100 years from the roof of \code{surv.breaks} 
#' or up to their 125th birthday, whichever comes first. The extrapolation
#' is by default based on the assumption that population hazards supplied
#' by \code{pophaz} are constant in time periods of length 1/12, 0.25, or 1 years:
#' if \code{ext.breaks = NULL}, it is internally substituted by
#' 
#' \code{list(fot = c(0:6/12, 0.75, 1:100), age = c(0, 125))}
#' 
#' to be supplied internally to a \code{lexpand} call.
#' 
#' Hence, alternate specifications allow for longer/shorter and more/less
#' accurate extrapolations. E.g.
#' 
#' \code{ext.breaks = list(fot = seq(0,100,1/12), age = 0:125, per = 1900:2100)}
#' 
#' would ensure a smooth extrapolation and perfect usage of \code{pophaz}.
#' This will probably not produce results much different from the default, though.
#' 
#' Note that \code{ext.breaks} should always include some meaningful definitions
#' of survival intervals (over \code{fot}) to correctly integrate the survival curve.
#' 
#' @return 
#' Returns a \code{data.frame} or \code{data.table} (depending on 
#' \code{getOptions("popEpi.datatable")}; see \code{?popEpi}) containing the
#' following columns:
#' \itemize{
#'   \item{est}{The estimated mean survival time}
#'   \item{exp}{The computed expected survival time}
#'   \item{obs}{Counts of subjects in data}
#'   \item{YPLL}{Years of Potential Life Lost, computed as (\code{(exp-est)*obs})}
#' }
#' The data may also have columns specifying the levels of \code{by.vars}
#' if any are used. Additionally, when age-adjusted estimates were requested,
#' the output values that have been age-adjusted include the suffix \code{.as}
#' in the appropriate column names. 
#' 
#' 
#' @examples
#' 
#' ## take first 3000 subjects in sire data for demonstration
#' sr <- sire[1:3000, ]
#' sr$agegr <- cut(sr$dg_age, c(0,45,60,Inf), right=FALSE)
#' x <- lexpand(sr, birth = bi_date, entry = dg_date, exit = ex_date,
#'              status = status %in% 1:2,
#'              breaks=list(fot=seq(0,10,1/12)), pophaz=popmort)
#' sm <- survmean(x, pophaz=popmort)
#' ## for each level of "agegr" separately:
#' #sma<- survmean(x, pophaz=popmort, by.vars="agegr") 
#' ## automated age-standardised results:
#' #sms<- survmean(x, pophaz=popmort, agegr.w.breaks=c(0,45,60,Inf))
#' 
#' ## visual inspection of how realistic extrapolation is for each stratum;
#' ## grey vertical line points to start of extrapolation;
#' ## solid lines are observed and extrapolated survivals;
#' ## dashed lines are expected survivals
#' plot(sm)
#' # plot(sma)
#' # plot(sms) plots precisely the same as plot(sma)
#' 
#' ## for finer control of plotting these curves, you may extract
#' ## from the survmean object using
#' attr(sm, "curves")
#' ## or
#' attributes(sm)$curves
#' 
#' @export survmean
#' 
#' 
#' 
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
# st <- survmean(Surv(FUT, lex.Xst != "alive") ~ group + adjust(agegr),
#                    pophaz = pm, data = x, weights = "internal",
#                    breaks = list(FUT = seq(0, 10, 1/12)))
# st <- survmean(Surv(FUT, lex.Xst != "alive") ~ agegr,
#                    pophaz = pm, data = x, weights = NULL,
#                    breaks = list(FUT = seq(0, 10, 1/12)))
# st <- survmean(Surv(FUT, lex.Xst != "alive") ~ 1,
#                    pophaz = pm, data = x, weights = NULL,
#                    breaks = list(FUT = seq(0, 10, 1/12)))
survmean <- function(formula, data, adjust = NULL, weights = NULL, breaks=NULL, pophaz = NULL, 
                     e1.breaks = NULL, e1.pophaz = pophaz, r = "auto", 
                     subset = NULL, verbose = FALSE, surv.method = "hazard") {
  pt <- proc.time()
  TF <- environment()
  PF <- parent.frame(1L)
  
  surv.method <- match.arg(surv.method, c("hazard", "lifetable"))
  
  if(!requireNamespace("survival")) stop("Need to load package survival to proceed")
  
  ## appease R CMD CHECK (due to using vars in DT[] only)
  r.e2 <- last.r.e2 <- surv <- survmean_type <- est <- NULL
  
  checkLexisData(data, check.breaks = FALSE)
  checkPophaz(data, pophaz, haz.name = "haz")
  checkPophaz(data, e1.pophaz, haz.name = "haz")
  pophaz <- setDT(copy(pophaz))
  e1.pophaz <- setDT(copy(e1.pophaz))
  
  if (!is.numeric(r) && !is.character(r)) {
    stop("r must be either 'auto' or a numeric value giving the assumed ",
         "relative survival ratio to use in extrapolation, e.g. r = 0.95.",
         "See ?survmean for more information.")
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
  if (!is.null(e1.breaks))  checkBreaksList(data, e1.breaks)
  
  ## prep & subset data --------------------------------------------------------
  subset <- substitute(subset)
  subset <- evalLogicalSubset(data, subset)
  
  x <- copy(data[subset, ])
  setDT(x)
  forceLexisDT(x, breaks = oldBreaks, allScales = allScales)
  
  ## ensure variables to merge pophaz datas by are kept ------------------------
  ## NOTE: temp var names avoid conflicts down the line
  avoid <- unique(c(names(data), names(x), names(pophaz), names(e1.pophaz)))
  
  phNames <- c(names(pophaz), names(e1.pophaz))
  phNames <- setdiff(phNames, c(allScales, "haz"))
  phNames <- intersect(phNames, names(x))
  tmpPhNames <- makeTempVarName(names = avoid, pre = phNames)
  if (!length(phNames)) {
    tmpPhNames <- NULL
  } else {
    phna <- which(phNames %in% names(pophaz))
    if (sum(phna)) setnames(pophaz, phNames[phna], tmpPhNames[phna])
    phna <- which(phNames %in% names(e1.pophaz))
    if (sum(phna)) setnames(e1.pophaz, phNames[phna], tmpPhNames[phna])
    x[, c(tmpPhNames) := copy(.SD), .SDcols = phNames]
  }
  
  ## determine printing & adjusting vars ---------------------------------------
  adSub <- substitute(adjust)
  foList <- usePopFormula(formula, adjust = adSub, data = x, enclos = PF)
  
  ## will avoid conflicts using temp names for tabulating variables
  adNames <- names(foList$adjust)
  prNames <- names(foList$print)
  byNames <- c(prNames, adNames)
  
  avoid <- unique(c(names(data), names(x), names(pophaz), names(e1.pophaz)))
  tmpAdNames <- makeTempVarName(names = avoid, pre = adNames)
  if (!length(adNames)) tmpAdNames <- NULL
  avoid <- unique(c(names(data), names(x), names(pophaz), names(e1.pophaz)))
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
    formula <- paste0(formula, paste0(c(tmpPrNames, tmpAdNames), 
                                      collapse = " + "))
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
  test_obs <- x[, .(obs=.N),  keyby=eval(TF$tmpByNames)]
  if (length(byNames)) setnames(test_obs, tmpByNames, byNames)
  mwDTtest <- makeWeightsDT(test_obs, values = list("obs"), print = prNames,
                            adjust = adNames, weights = weights, 
                            internal.weights.values = "obs")
  if (length(byNames)) setnames(test_obs, byNames, tmpByNames)
  
  ## figure out extrapolation breaks -------------------------------------------
  ## now that the survival time scale is known this can actually be done.
  
  if (is.null(e1.breaks)) {
    e1.breaks <- copy(breaks[survScale])
    addBreaks <- max(e1.breaks[[survScale]]) + 
      c(seq(0,1,1/12), seq(1.2, 1.8, 0.2), 2:19, seq(20, 50, 5))
    e1.breaks[[survScale]] <- unique(c(e1.breaks[[survScale]], addBreaks))
    
    checkBreaksList(x, e1.breaks)
  }
  if (!survScale %in% names(e1.breaks)) {
    stop("The survival time scale must be included in the list of breaks ",
         "to extrapolate by ('e1.breaks').")
  }
  if (!all(breaks[[survScale]] %in% e1.breaks[[survScale]])) {
    stop("The vector of breaks in 'breaks' for the survival time scale MUST",
         "be a subset of the breaks for the survival time scale in ",
         "'e1.breaks'. E.g. the former could be 0:10 and the latter 0:100.")
  }
  
  if (verbose) {
    cat("Time taken by prepping data:", timetaken(pt), "\n")
  }
  
  
  ## compute observed survivals ------------------------------------------------
  ## NOTE: do not adjust here; adjust in original formula means weighting
  ## the mean survival time results.
  
  st <- survtab_lex(formula, data = x, breaks = breaks, 
                    pophaz = pophaz,
                    relsurv.method = "e2",
                    surv.type = "surv.rel", 
                    surv.method = surv.method)
  
  bareVars <- c(tmpByNames, "Tstop", "r.e2")
  all_names_present(st, bareVars, msg = 
                      paste0("Internal error: expected to have variables ",
                             "%%VARS%% after computing observed survivals ",
                             "but didn't. Blame the package maintainer if you ",
                             "see this."))
  setcolsnull(st, keep = bareVars, colorder = TRUE)
  setDT(st)
  setkeyv(st, c(tmpByNames, "Tstop"))
  st[, Tstart := c(0, Tstop[-.N]), by = eval(tmpByNames)]
  
  ## decumulate for later cumulation
  st[, r.e2 := r.e2/c(1, r.e2[-.N]), by= eval(tmpByNames)]
  
  if (verbose) {
    cat("Time taken by estimating relative survival curves:", 
        timetaken(pt), "\n")
  }
  
  ## compute overall expected survival -----------------------------------------
  ## 1) take only those individuals that were diagnosed in the time window
  ##    defined by breaks list in argument 'breaks'
  pt <- proc.time()
  setkeyv(x, c("lex.id", survScale))
  xe <- unique(x)[x[[survScale]] < TF$tol, ]
  
  if (length(breaks) > 1L) {
    ## e.g. a period window was defined and we only use
    ## entering follow-up in the time window.
    tmpDropBreaks <- setdiff(allScales, survScale)
    tmpDropBreaks <- intersect(names(breaks), tmpDropBreaks)
    tmpDropBreaks <- breaks[tmpDropBreaks]
    
    xe <- intelliDrop(xe, breaks = tmpDropBreaks)
  }
  
  xe <- x[lex.id %in% TF$xe[, unique(lex.id)]]
  forceLexisDT(xe, breaks = oldBreaks, allScales = allScales, key = FALSE)
  
  ## 2) compute Ederer I expected survival curves from T = 0 till e.g. T = 100
  e1 <- comp_e1(xe, breaks = e1.breaks, pophaz = e1.pophaz, immortal = TRUE, 
                survScale = survScale, by = tmpByNames, id = "lex.id")
  setnames(e1, survScale, "Tstop")
  e1[, Tstart := c(0, Tstop[-.N]), by = eval(tmpByNames)]
  e1[, surv.int := cut(Tstart, breaks = e1.breaks[[survScale]], 
                       right = FALSE, labels = FALSE)]
  e1[, delta := Tstop - Tstart]
  
  ## decumulate for later cumulation
  e1[, surv.exp := surv.exp/c(1, surv.exp[-.N]), by = eval(tmpByNames)]
  
  if (verbose) {
    cat("Time taken by computing overall expected survival curves:", 
        timetaken(pt), "\n")
  }
  
  ## compute counts of subjects ------------------------------------------------
  ## these correspond to the counts of patients for which expected survival
  ## was computed. If observed survival is e.g. a period estimated curve,
  ## we only use subjects entering follow-up in the period window.
  N_subjects <- xe[!duplicated(lex.id)][, list(obs=.N), 
                  keyby=eval(TF$tmpByNames)]
  
  ## combine all estimates into one data set -----------------------------------
  pt <- proc.time()
  
  st[, surv.int := cut(Tstart, breaks = e1.breaks[[survScale]], 
                       right = FALSE, labels = FALSE)]
  
  x <- merge(e1, st[, c(tmpByNames, "surv.int", "r.e2"), with = FALSE], 
             by = c(tmpByNames,"surv.int"), all = TRUE)
  setkeyv(x, c(tmpByNames, "surv.int"))
  
  ## add last non-NA values as separate column
  if (length(tmpByNames)) {
    st <- unique(st, by = tmpByNames, fromLast = TRUE)
  } else {
    st <- st[.N]
  }
  
  st[, delta := Tstop - Tstart]
  st[, r.e2 := r.e2^(1/delta)] ## "annualized" RSRs
  setcolsnull(st, keep  = c(tmpByNames, "r.e2"), soft = FALSE)
  setnames(st, "r.e2", "last.r.e2")
  if (length(tmpByNames)) {
    x <- merge(x, st, by = tmpByNames, all = TRUE)
  } else {
    x[, last.r.e2 := st$last.r.e2]
  }
  x[, last.r.e2 := last.r.e2^(delta)] ## back to non-annualized RSRs
  ## enforce RSR in extrapolated part of observed curve to at most 1
  x[, last.r.e2 := pmin(last.r.e2, 1)]
  
  ## manually given RSR for extrapolated part of the obs.surv. curve
  if (r != "auto") {
    x[, last.r.e2 := TF$r]
  }
  
  x[is.na(r.e2), r.e2 := last.r.e2]
  x[, surv := r.e2*surv.exp]
  
  ## cumulate again
  setkeyv(x, c(tmpByNames, "surv.int"))
  x[, c("surv", "surv.exp") := lapply(.SD, cumprod),
    .SDcols = c("surv", "surv.exp"), by = eval(tmpByNames)]
  
  x2 <- copy(x)
  x[, "surv.exp" := NULL]
  x2[, "surv" := NULL]
  setnames(x2, "surv.exp", "surv")
  x <- rbind(x, x2)
  x[, survmean_type := rep(c("est", "exp"), each = nrow(x2))]
  
  setcolsnull(x, keep = c(tmpByNames, "survmean_type", 
                          "surv.int", "Tstart", "Tstop", 
                          "delta", "surv", "surv.exp"),
              colorder = TRUE)
  
  ## check curve convergence to zero -------------------------------------------
  ## a good integration is based on curves that get very close to 
  ## zero in the end
  mi <- x[, .(surv = round(min(surv),4)*100), 
          keyby = eval(c(tmpByNames, "survmean_type"))]
  if (any(mi$surv > 1)) {
    warning("One or several of the curves used to compute mean survival times ",
            "or expected mean survival times was > 1 % at the lowest point. ",
            "Mean survival estimates may be significantly biased. To avoid ",
            "this, supply breaks to 'e1.breaks' which make the curves longer ",
            ", e.g. e1.breaks = list(FUT = 0:150) when using time scale FUT ",
            "as the survival time scale.")
  }
  mi[, surv := paste0(surv, " %")]
  mi[, survmean_type := factor(survmean_type, c("est", "exp"),
                               c("Observed", "Expected"))]
  setnames(mi, c("survmean_type", "surv"), 
           c("Obs./Exp. curve", "Lowest value"))
  if (length(byNames)) setnames(mi, tmpByNames, byNames)
  if (verbose) {
    cat("Lowest points in observed / expected survival curves by strata:\n")
    print(mi)
  }
  
  ## integrating by trapezoid areas --------------------------------------------
  ## trapezoid area: WIDTH*(HEIGHT1 + HEIGHT2)/2
  ## so we compute "average interval survivals" for each interval t_i
  ## and multiply with interval length.
  
  setkeyv(x, c(tmpByNames, "survmean_type",  "Tstop"))
  sm <- x[, .(survmean = sum(delta*(surv + c(1, surv[-.N]))/2L)), 
          keyby = c(tmpByNames, "survmean_type")]
  
  ## cast ----------------------------------------------------------------------
  
  sm <- cast_simple(sm, columns = "survmean_type", 
                    rows = tmpByNames, values = "survmean")
  
  ## add numbers of subjects, compute YPLL -------------------------------------
  setkeyv(sm, tmpByNames); setkeyv(N_subjects, tmpByNames)
  sm[, "obs" := N_subjects$obs]
  sm[, "YPLL" := (exp-est)*obs]
  
  
  ## adjusting -----------------------------------------------------------------
  
  sm <- makeWeightsDT(sm, values = list(c("est", "exp", "obs", "YPLL")),
                      print = tmpPrNames, adjust = tmpAdNames,
                      weights = weights, internal.weights.values = "obs")
  if (length(adNames)) {
    vv <- c("est", "exp", "obs", "YPLL")
    sm[, c("est", "exp") := lapply(.SD, function(col) col*sm$weights), 
       .SDcols = c("est", "exp")]
    sm <- sm[, lapply(.SD, sum), .SDcols = vv, by = eval(tmpPrNames)]
  }
  
  if (verbose) {
    cat("Time taken by final touches:", timetaken(pt), "\n")
  }
  
  ## final touch ---------------------------------------------------------------
  if (length(prNames)) setnames(sm, tmpPrNames, prNames)
  
  this_call <- match.call()
  at <- list(call = this_call, 
             print = prNames, adjust = adNames, 
             tprint = tmpPrNames, tadjust = tmpAdNames,
             breaks = breaks, 
             e1.breaks = e1.breaks, survScale = survScale,
             curves = copy(x))
  setattr(sm, "class", c("survmean","data.table", "data.frame"))
  setattr(sm, "survmean.meta", at)
  if (!getOption("popEpi.datatable")) setDFpe(sm)
  return(sm[])
}

