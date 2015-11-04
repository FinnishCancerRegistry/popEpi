


#' @title Compute mean survival times using extrapolation
#' @author Joonas Miettinen, Karri Seppa
#' @param data a data set of splitted records; e.g. output of \code{\link{lexpand}}
#' @param surv.breaks passed on to \code{\link{survtab}}; see that help
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
#' @param ... any other arguments passed on to \code{survtab} such as
#' \code{surv.method = "lifetable"} for actuarial estimates of observed survival
#' @param ext.breaks advanced; a list of breaks (see \code{\link{lexpand}});
#' used as breaks for extrapolation; see Details
#' @details
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
#' @examples
#' 
#' ## take first 5000 subjects in sire data for demonstration
#' sr <- sire[1:5000, ]
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
#' @export survmean
#' 
#' 
#' 



survmean <- function(data, surv.breaks=NULL, by.vars = NULL, pophaz = NULL, 
                     r = 1.00,
                     agegr.w.breaks=NULL, agegr.w.weights=NULL, ext.breaks = NULL,
                     subset = NULL, ...) {
  
  if (is.null(pophaz)) stop("need a pophaz data set")
  all_names_present(data, by.vars)
  all_names_present(data, c("lex.id", "lex.dur", "lex.Xst", "lex.Cst", "fot", "per", "age"))
  
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
  ## prep & subset data --------------------------------------------------------
  ## no copy of data
  attrs <- attributes(data)
  attrs <- attrs[intersect(names(attrs), c("breaks","time.scales"))]
  
  subset <- substitute(subset)
  subset <- evalLogicalSubset(data, subset)
  
#   if (!"lex.multi" %in% names(data)) {
#     setkey(data, lex.id, fot)
#     data[, lex.multi := 1:.N, by = lex.id]
#   }
#   setkey(data, lex.id, lex.multi)
  setDT(data) ## for now need to require DT
  N_subjects <- data[subset & !duplicated(lex.id) & fot == 0, list(obs=.N), keyby=by.vars]
  
  ## compute survivals ---------------------------------------------------------
  st <- survtab(data, surv.breaks=surv.breaks, 
                surv.type = "surv.rel", by.vars=by.vars, 
                subset = subset,
                relsurv.method = "e2",
                format = FALSE,
                ...)
  subr <- attr(st, "surv.breaks")
  n_si <- length(subr) - 1 ## maximum surv.int value
  
  ## melt: surv.exp will be used to determine disease-free mean survival
  setDT(st)
  ft <- st[, c("surv.int", by.vars, "Tstart", "Tstop", "delta"), with=FALSE]
  ft <- rbindlist(list(ft, ft))
  ft[, meansurv_type := rep(c("est", "exp"), each = nrow(ft)/2L)]
  ft[, meansurv_type := factor(meansurv_type)]
  
  st <- melt(st, id.vars=by.vars, measure.vars=c("surv.obs", "surv.exp"), 
             variable.name = "meansurv_type", value.name="surv.obs", variable.factor = TRUE)
  st[, meansurv_type := factor(meansurv_type, levels=c("surv.obs","surv.exp"), labels=c("est","exp"))]
  by.vars <- c("meansurv_type", by.vars)
  
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
    ## extrapolate and add expected survivals after observed survivals
    
    ## preparations for extrapolation ------------------------------------------
    ## more values at the end of known follow-up
    data[, per := per+lex.dur]
    data[, age := age+lex.dur]
    
    ## compute values in 100 years or up to 125th birthday
    ## this elongates observations
    ## TODO: use temporary variable names to avoid conflicts
    BL <- list(fot = c(0:6/12, 0.75, 1:100), age = c(0, 125))
    if (!is.null(ext.breaks)) BL <- ext.breaks
    ageRoof <- max(BL$age)
    fotRoof <- max(BL$fot)
    data[, lex.dur := pmin(pmax(ageRoof, age) - age, fotRoof)]
    data[, per.end := lex.dur + per]
    data[, ms_bi_yrs := per-age]
    data[, ms_stat := 0L]
    
    setcolsnull(data, c("fot","age","lex.id","lex.multi", 
                        "lex.Cst", "lex.Xst","lex.dur","pop.haz","pp"))
    setnames(data, "per", "ms_per")
    
    ## split & merge elongated observations ------------------------------------
    set(pophaz, j = "haz", value = pophaz$haz * r)
    data <- lexpand(data, birth = "ms_bi_yrs", entry = "ms_per", exit = "per.end", 
                status = ms_stat, 
                breaks = BL,
                pophaz = pophaz, pp = FALSE, merge= TRUE, drop = TRUE)
    setDT(data)
    set(pophaz, j = "haz", value = pophaz$haz / r)
    
    setcolsnull(data, keep = c(by.vars, "fot","lex.dur","lex.id","lex.multi","pop.haz"))
    setkey(data, lex.id, fot)
    data[, lex.multi := 1:.N, by = lex.id]
    setkey(data, lex.id, lex.multi)
    
    ## compute subject-specific expected survival curves for EdererI method ----
    ## meansurv_type not in data
    by.vars <- setdiff(by.vars, "meansurv_type")
    if (length(by.vars)==0) by.vars <- NULL
    
    data[, surv.int := cut(fot, 0:100,right=FALSE,labels=FALSE)]
    setkeyv(data, c(by.vars, "surv.int", "lex.id", "lex.multi"))
    data[, pop.haz  := sum(pop.haz*lex.dur), by = c(by.vars, "surv.int", "lex.id")] ## surv.int level for each subject
    data[, p.exp    := exp(-pop.haz)] ## surv.int level for each subject
    data <- unique(data, by = c("surv.int", "lex.id"))
    data[, surv.exp := cumprod(p.exp)/p.exp, by = list(lex.id)] ## till start of interval
    data <- data[, list(haz.exp.e1 = sum(surv.exp*pop.haz)/sum(surv.exp)), by = c(by.vars, "surv.int")] # EdererI weighting
    data[, p.exp.e1 := exp(-haz.exp.e1)]
    setkeyv(data, c(by.vars, "surv.int"))
    setnames(data, "p.exp.e1", "surv.obs") ## cumulative computed later
    
    ## prepare to add after actual surv.obs estimates / expected survivals
    by.vars <- c("meansurv_type", by.vars)
    data <- rbindlist(list(data, data))
    data[, meansurv_type := factor(rep(c("est", "exp"), each = nrow(data)/2L))]
    
    new_si_levs <- st[, max(surv.int)+1L]
    new_si_levs <- new_si_levs:(new_si_levs+99L)
    
    data[, surv.int := factor(surv.int, levels = 1:100, labels = new_si_levs)]
    data[, surv.int := fac2num(surv.int)]
    
    data[, delta := 1L]
#     data[, SE.surv.obs := 0]

    setcolsnull(data, keep = c(by.vars, "surv.int","surv.obs","delta"), colorder=TRUE, soft=FALSE)
    setcolsnull(st,   keep = c(by.vars, "surv.int","surv.obs","delta"), colorder=TRUE, soft=FALSE)
        
    ## roll back cumulative surv.obs to cumulate later properly
    st <- shift.var(st, id.vars = by.vars, shift.var = "surv.int", value.vars = "surv.obs",shift.value = -1L)
    st[is.na(lag1_surv.obs), lag1_surv.obs := 1]
    st[, surv.obs := surv.obs/lag1_surv.obs]
    st[, lag1_surv.obs := NULL]

    data <- rbindlist(list(st, data))
    rm(st)
    
    setkeyv(data, c(by.vars,"surv.int"))
    data[, surv.obs := cumprod(surv.obs), by=by.vars]
  } else {
    data <- st
    rm(st)
    setcolsnull(data, keep = c(by.vars, "surv.int","surv.obs","delta",
                               "surv.int.start","surv.int.stop","SE.surv.obs"))
  }
  
  ## integrating by trapezoid areas --------------------------------------------
  ## need lag1 values
  data <- shift.var(data, id.vars = by.vars, shift.var = "surv.int", 
                    value.vars = "surv.obs", shift.value=-1L)
  setkeyv(data, c(by.vars, "surv.int"))
  
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
  
  by.vars <- setdiff(by.vars, "meansurv_type")
  if (length(by.vars) == 0) {
    by.vars <- "temp_dummy"
    data[, temp_dummy := 1L]
  }
  data <- cast_simple(data, columns = "meansurv_type", rows=by.vars, values = "survmean")
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
  

  
  setattr(bkup, "by.vars", by.vars)
  setattr(bkup, "surv.breaks", subr)
  setattr(data, "class", c("meansurv","pe","data.table", "data.frame"))
  if (!getOption("popEpi.datatable")) setDFpe(data)
  setattr(data, "curves", bkup)
  return(data[])
}


globalVariables(c('ms_agegr_w',
                  'agr.w',
                  'meansurv_type',
                  'per.end',
                  'ms_bi_yrs',
                  'ms_stat',
                  'p.exp.e1',
                  'haz.exp.e1',
                  'dum',
                  'temp_dummy',
                  'YPLL',
                  'est'))

plot.meansurv <- function(obj, ...) {
  curves <- attr(obj, "curves")
  if (is.null(curves)) stop("no curves information in obj; usually lost if obj altered after using meansurv")
  
  plot(curves$surv.obs ~ curves$Tstop, type="n",
       xlab = "Years from entry", ylab = "Survival")
  lines.meansurv(obj, ...)
  
  subr <- attr(curves, "surv.breaks")
  abline(v = max(subr), lty=2, col="grey")
  
}

lines.meansurv <- function(obj, ...) {
  curves <- attr(obj, "curves")
  if (is.null(curves)) stop("no curves information in obj; usually lost if obj altered after using meansurv")
  
  by.vars <- attr(curves, "by.vars")
  by.vars <- unique(c(c("meansurv_type", "ms_agegr_w"), by.vars))
  by.vars <- intersect(by.vars, names(curves))
  
  curves <- data.table(curves)
  setkeyv(curves, c(by.vars,"surv.int"))
  type_levs <- length(levels(interaction(curves[, (by.vars), with=FALSE])))/2L
  if (length(by.vars) > 1) {
    other_levs <- length(levels(interaction(curves[, setdiff(by.vars, "meansurv_type"), with=FALSE])))
  } else {
    other_levs <- 1L
  }
  
  curves <- cast_simple(curves, columns = by.vars, rows = "Tstop", values = "surv.obs")
  matlines(x=curves$Tstop, y=curves[, setdiff(names(curves), "Tstop"), with=FALSE],  
          lty = rep(1:2, each=type_levs), col = 1:other_levs, ...)
}




