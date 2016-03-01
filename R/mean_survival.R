


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



survmean <- function(data, surv.breaks=NULL, by.vars = NULL, pophaz = NULL, 
                     r = 1.00,
                     agegr.w.breaks=NULL, agegr.w.weights=NULL, ext.breaks = NULL,
                     subset = NULL, verbose = FALSE, ...) {
  
  if (is.null(pophaz)) stop("need a pophaz data set")
  all_names_present(data, by.vars)
  all_names_present(data, c("lex.id", "lex.dur", "lex.Xst", "lex.Cst", "fot", "per", "age"))
  
  ## prep & subset data --------------------------------------------------------
  ## take copy of data for now... need to rewrite this whole mess at some point
  attrs <- attributes(data)
  attrs <- attrs[intersect(names(attrs), c("breaks","time.scales"))]
  
  subset <- substitute(subset)
  subset <- evalLogicalSubset(data, subset)
  
  data <- if (all(subset)) copy(data) else data[subset,]
  setDT(data)
  setattr(data, "class", c("Lexis", "data.table", "data.frame"))
  
  ## age group weighting -------------------------------------------------------
  if (!is.null(agegr.w.breaks)) {
    by.vars <- c(by.vars, "ms_agegr_w")
    
    dup_vec <- duplicated(data, by="lex.id")
    data[!dup_vec, entry_age := age-fot]
    data[, entry_age := na.omit(entry_age), by=lex.id]
    data[, ms_agegr_w := cut(entry_age, breaks=agegr.w.breaks, right=FALSE)]
    
    agetab <- data.table(ms_agegr_w = sort(unique(data$ms_agegr_w)))
    setkey(agetab, ms_agegr_w)
    if (is.null(agegr.w.weights)) agegr.w.weights <- data[!dup_vec, .N, keyby=ms_agegr_w]$N
    agetab[, agr.w := agegr.w.weights/(sum(agegr.w.weights))]
    
  }
  
  N_subjects <- data[!duplicated(lex.id) & fot == 0, list(obs=.N), keyby=by.vars]
  
  ## compute survivals ---------------------------------------------------------
  st <- survtab(data, surv.breaks=surv.breaks, 
                surv.type = "surv.rel", by.vars=by.vars, 
                relsurv.method = "e2",
                format = FALSE,
                ...)
  
  subr <- attr(st, "surv.breaks")
  n_si <- length(subr) - 1 ## maximum surv.int value
  setDT(st)
  
  if (verbose) cat("Table of estimated observed survivals: \n")
  if (verbose) print(st)
  
  ## melt: surv.exp will be used to determine disease-free mean survival
  
  ft <- st[, c("surv.int", by.vars, "Tstart", "Tstop", "delta"), with=FALSE]
  ft <- rbindlist(list(ft, ft))
  ft[, survmean_type := rep(c("est", "exp"), each = nrow(ft)/2L)]
  ft[, survmean_type := factor(survmean_type)]
  
  st <- melt(st, id.vars=by.vars, measure.vars=c("surv.obs", "surv.exp"), 
             variable.name = "survmean_type", value.name="surv.obs", variable.factor = TRUE)
  st[, survmean_type := factor(survmean_type, levels=c("surv.obs","surv.exp"), labels=c("est","exp"))]
  by.vars <- c("survmean_type", by.vars)
  
  setkeyv(st, by.vars); setkeyv(ft, by.vars)
  st <- cbind(ft, st[, list(surv.obs)])
  setcolorder(st, c(by.vars, setdiff(names(st), by.vars)))
  rm(ft)
  
  ## get only values at the end of known follow-up -----------------------------
  ## get copy of data
  setkeyv(data, c("lex.id", attr(data, "time.scales")[1L]))
  data <- unique(data, by=c("lex.id"), fromLast = TRUE)
  data[, fot := fot+lex.dur]
  data <- data[fot >= max(subr)]
  ## if all subjects left follow-up before the roof of surv.breaks, 
  ## now nrow(data) == 0. meaning we do not extrapolate
  
  if (nrow(data) > 0) {
    if (verbose) cat("Computing EdererI expected (extrapolated) survivals for ", nrow(data), "subjects... \n")
    
    ## extrapolate and add expected survivals after observed survivals
    
    setcolsnull(data, keep = c("lex.id", "fot", "per", "age", "lex.dur", by.vars, names(pophaz)))
    
    ## preparations for extrapolation ------------------------------------------
    ## more values at the end of known follow-up
    data[, per := per+lex.dur]
    data[, age := age+lex.dur]
    
    ## compute values in 100 years or up to 125th birthday
    ## this elongates observations
    ## TODO: use temporary variable names to avoid conflicts
    BL <- list(fot = c(0:6/12, 0.75, 1:100), age = c(0, 125), per = c(-Inf, Inf))
    if (!is.null(ext.breaks)) BL <- ext.breaks
    for (k in c("per", "age", "fot")) {
      if (is.null(BL[[k]])) BL[[k]] <- c(if (k == "per") -Inf else 0, Inf)
    }
    
    expRange <- lapply(BL, range)
    expRange <- lapply(expRange, function(x) paste0(x, collapse = " - "))
    expRange <- mapply(function(x,y) paste0(y, ": ", x, ";"), x = expRange, y = names(expRange), SIMPLIFY = TRUE)
    if (verbose) cat("Extrapolation range:", paste0(expRange, collapse = " "),"\n")

    ## NOTE: e.g. age value here at the end of interval (i.e. age + lex.dur)
    ## NOTE: from here on fot will be considered to be zero for everyone 
    MS_BIRTH_YRS_ <- NULL ## this appeases R CMD CHECK
    data[, lex.dur := pmin(pmax(max(BL$age) - age, 0L), pmax(max(BL$per) - per, 0L), max(BL$fot))]
    data <- data[lex.dur > 0L]
    data[, MS_BIRTH_YRS_ := per-age]
    
    setnames(data, "per", "MS_ENTRY_YRS_")
    setcolsnull(data, keep = c("MS_BIRTH_YRS_", "MS_ENTRY_YRS_", by.vars, names(pophaz)), colorder = TRUE)
    if (verbose) cat("extrapolation data just before splitting: \n")
    if (verbose) print(data)
    
    ## split & merge elongated observations ------------------------------------
    pophaz <- copy(pophaz)
    set(pophaz, j = "haz", value = pophaz$haz * r)
    
    data <- lexpand(data, birth = "MS_BIRTH_YRS_", entry = "MS_ENTRY_YRS_", exit = 1e6L, 
                    status = 0L, entry.status = 0L,
                    breaks = BL,
                    pophaz = pophaz, pp = FALSE, merge= TRUE, drop = TRUE)
    setDT(data)
    rm(pophaz)
    
    if (verbose) cat("Extrapolation data just after splitting: \n")
    if (verbose) print(data)
    
    setcolsnull(data, keep = c(by.vars, "fot","lex.dur","lex.id","lex.multi","pop.haz"))
    setkey(data, lex.id, fot)
    data[, lex.multi := 1:.N, by = lex.id]
    setkey(data, lex.id, lex.multi)
    
    ## compute subject-specific expected survival curves for EdererI method ----
    ## NOTE: survmean_type not in data
    by.vars <- setdiff(by.vars, "survmean_type")
    if (length(by.vars)==0) by.vars <- NULL
    data[, surv.int := cut(fot, BL$fot,right=FALSE,labels=FALSE)]
    setkeyv(data, c(by.vars, "surv.int", "lex.id", "lex.multi"))
    
    
    
    ## NOTE: following repeats cumulative hazard within surv.int for each row there by id
    data[, pop.haz  := sum(pop.haz*lex.dur), by = c(by.vars, "surv.int", "lex.id")]
    data[, p.exp    := exp(-pop.haz)] ## surv.int level for each subject
    data <- unique(data, by = c("surv.int", "lex.id"))
    data[, surv.exp := cumprod(p.exp)/p.exp, by = list(lex.id)] ## till start of interval
    data <- data[, list(haz.exp.e1 = sum(surv.exp*pop.haz)/sum(surv.exp)), by = c(by.vars, "surv.int")] # EdererI weighting
    data[, p.exp.e1 := exp(-haz.exp.e1)]
    setkeyv(data, c(by.vars, "surv.int"))
    setnames(data, "p.exp.e1", "surv.obs") ## cumulative computed later
    
    
    ## prepare to add after actual surv.obs estimates / expected survivals
    by.vars <- c("survmean_type", by.vars)
    data <- rbindlist(list(data, data))
    data[, survmean_type := factor(rep(c("est", "exp"), each = nrow(data)/2L))]
    
#     new_si_levs <- st[, max(surv.int)+1L]
#     new_si_levs <- new_si_levs:(new_si_levs+99L)
    
#     data[, surv.int := factor(surv.int, levels = sort(unique(surv.int)), labels = new_si_levs)]
#     data[, surv.int := fac2num(surv.int)]
    setorderv(st, c(by.vars, "Tstop"))
    st[, surv.int := 1:.N, by = by.vars]
    data[, surv.int := surv.int + max(st$surv.int)]
    
    SItab <- data.table(surv.int = max(st$surv.int) + 1:(length(BL$fot)-1), delta = diff(BL$fot))
    data <- merge(data, SItab, by = "surv.int", all.x=TRUE, all.y=FALSE)

    setcolsnull(data, keep = c(by.vars, "surv.int","surv.obs","delta"), colorder=TRUE, soft=FALSE)
    setcolsnull(st,   keep = c(by.vars, "surv.int","surv.obs","delta"), colorder=TRUE, soft=FALSE)
        
    ## roll back cumulative surv.obs to cumulate later properly
    setkeyv(st, c(by.vars, "surv.int"))
    st[, lag1_surv.obs := shift(surv.obs, n = 1L, type = "lag", fill = 1), by = by.vars]
    
    st[, surv.obs := surv.obs/lag1_surv.obs]
    st[, lag1_surv.obs := NULL]

    data <- rbindlist(list(st, data))
    rm(st)
    
    setkeyv(data, c(by.vars,"surv.int"))
    data[, surv.obs := cumprod(surv.obs), by=by.vars]
    if (verbose) cat("EdererI extrapolation done. \n")
  } else {
    cat("No extrapolation done since all subjects exited follow-up within the range of surv.breaks used to compute observed survivals. \n")
    data <- st
    rm(st)
    setcolsnull(data, keep = c(by.vars, "surv.int","surv.obs","delta",
                               "surv.int.start","surv.int.stop","SE.surv.obs"))
  }
  
  ## integrating by trapezoid areas --------------------------------------------
  ## need lag1 values
  setkeyv(data, c(by.vars, "surv.int"))
  data[, lag1_surv.obs := shift(surv.obs, n = 1L, type = "lag", fill = 1), by = by.vars]
  
  
  data[, dum := 1L]
  data[!duplicated(data, by=c(by.vars,"dum"), fromLast=TRUE), surv.obs := 0]
  data[is.na(lag1_surv.obs), lag1_surv.obs := 1]
  data[, surv.obs := (surv.obs+lag1_surv.obs)/2]
  setcolsnull(data, "dum")
  
  bkup <- data
  setkeyv(bkup, c(by.vars, "surv.int"))
  bkup[, Tstop := cumsum(delta), by=by.vars]

  data <- data[, list(survmean = sum(surv.obs*delta)), keyby = by.vars]

  ## cast ----------------------------------------------------------------------

  setkeyv(data, by.vars)
  
  by.vars <- setdiff(by.vars, "survmean_type")
  if (length(by.vars) == 0) {
    by.vars <- "temp_dummy"
    data[, temp_dummy := 1L]
  }
  data <- cast_simple(data, columns = "survmean_type", rows=by.vars, values = "survmean")
  setcolsnull(data, "temp_dummy")
  by.vars <- setdiff(by.vars, "temp_dummy")
  if (length(by.vars) == 0) by.vars <- NULL

  ## add numbers of subjects, compute YPLL -------------------------------------
  setkeyv(data, by.vars); setkeyv(N_subjects, by.vars)
  data[, "obs" := N_subjects$obs]
  data[, "YPLL" := (exp-est)*obs]
  
  
  ## age group weighting -------------------------------------------------------
  if ("ms_agegr_w" %in% by.vars) {
    by.vars <- setdiff(by.vars, "ms_agegr_w")
    if (length(by.vars) == 0) by.vars <- NULL
    setkey(data, ms_agegr_w)
    setkey(agetab, ms_agegr_w)
    data <- agetab[data]
    data <- data[, list(est.as = sum(est*agr.w), exp.as = sum(exp*agr.w), obs = sum(obs), YPLL.as = sum(YPLL*agr.w)), keyby = by.vars]
  }
  if (verbose) cat("survmean computations finished. \n")

  prVars <- setdiff(by.vars, "ms_agegr_w")
  setattr(bkup, "print", prVars)
  adVars <- intersect(by.vars, "ms_agegr_w")
  setattr(bkup, "adjust", adVars)
  setattr(bkup, "surv.breaks", subr)
  setattr(bkup, "survmean_type", "survmean_type")
  setattr(data, "class", c("survmean","data.table", "data.frame"))
  if (!getOption("popEpi.datatable")) setDFpe(data)
  setattr(data, "curves", bkup)
  return(data[])
}


globalVariables(c('ms_agegr_w',
                  'agr.w',
                  'survmean_type',
                  'per.end',
                  'ms_bi_yrs',
                  'ms_stat',
                  'p.exp.e1',
                  'haz.exp.e1',
                  'dum',
                  'temp_dummy',
                  'YPLL',
                  'est'))


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
  
  ## compute survivals ---------------------------------------------------------
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
  print(xs)
  print(ext.breaks)
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
  print(summary(e1a))
  print(summary(e1b))
  
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
  mibr <- c(0, 0.001, 0.01, 0.05, 0.10, 0.5, 1)
  mi[, surv.exp := cut(surv.exp, mibr, right = FALSE, labels = FALSE)]
  mila <- c("<0.001", "0.001-0.01", "0.01-0.05", "0.1-0.5", ">0.5")
  mi[, surv.exp := TF$mila[surv.exp]]
  if (verbose) {
    cat("Lowest points in observed / expected survival curves by strata:\n")
    print(mi)
  }
  
  ## integrating by trapezoid areas --------------------------------------------
  ## trapezoid area: WIDTH*(HEIGHT1 + HEIGHT2)/2
  ## so we compute "average interval survivals" for each interval t_i
  ## and multiply with interval length.
  
  x[, surv.exp.mean := delta*(surv.exp + c(1, surv.exp[-.N]))/2L,
    by = eval(c(tmpByNames,"survmean_type"))]
  
  sm <- x[, .(survmean = sum(surv.exp.mean*delta)), 
          keyby = c(tmpByNames, "survmean_type")]
  
  x[, surv.exp.mean := NULL]
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











