

makeWeightsDT <- function(data, weights, adjust) {
  ## input: data and substitute()'d weights and adjust arguments
  ## output: a prepared data.table for merging with aggregated or other data
  ## to compute weighted results with
  ## 'adjust' argument for defining adjusting vars
  ## and 'weights' as: 
  ## * character string name of weights variable in data;
  ## * character string naming ICSS1-3;
  ## * a data.frame that has lower bounds of categories and weights;
  ## * a list of named weights vectors which will be collated into a data.frame of weights.
  ## * list might allow for e.g. weights = list(sex = c(0.5, 0.5), agegroup = "ICSS1")
  
  
  ## TODO: not sure if adjust should already be in DT format when supplied
  ## weights might also be pre-evaluated.
  
  weights <- copy(eval(weights, envir = data, enclos = parent.frame(1L)))
  if (!is.data.frame(weights)) adjust <- copy(eval(adjust, envir = data, enclos = parent.frame(1L)))
  
  ## Need: 1) weights in DT format; 2) cuts for harmonizing weights and data adjust variables
  if (is.data.frame(weights)) {
    setDT(weights)
    weVars <- setdiff(names(weights), "weights")
    cuts <- lapply(weights[, mget(weVars)], function(x) if (is.factor(x)) levels(x) else sort(unique(x)))
    
  } else if (is.list(weights)) {
    weights <- do.call(function(...) CJ(..., unique = FALSE, sorted = FALSE), weights)
    
    
    
    weights[, weights := 1L]
    for (k in weVars) {
      set(weights, j = (weVars), value = weights$weights * weights[[k]])
    }
    weights[, (weVars) := NULL]
    
  }
  
}





#' @title Survival tables
#' @author Joonas Miettinen, Karri Seppa
#' @description Given a data set processed by \code{lexpand}, estimates various 
#' survival time functions as requested by the user. 
#' @param data a dataset preferably processed by \code{\link{lexpand}}
#' @param surv.breaks breaks as explicitly 
#' left inclusive and right exclusive - e.g. \code{[a,b)}.
#' @param by.vars a character string vector; defines names of 
#' variables by which survivals are calculated
#' separately; e.g. \code{by.vars = c('sex', 'area')} computes 
#' survivals separately for each combination
#' of \code{'sex'} and \code{'area'}.
#' @param event.values a vector of values present 
#' in the datasets's \code{lex.Xst} column; 
#' if \code{NULL}, uses all but the first 
#' level of \code{as.factor(lex.Xst)} as \code{event.values};
#' these values are events and all other values are considered to be censoring
#' @param surv.type either \code{'surv.obs'},
#' \code{'surv.cause'}, \code{'surv.rel'}, 
#' \code{'cif.obs'} or \code{'cif.rel'}; 
#' defines what kind of survival time function(s) is/are estimated; see Details
#' 
#' @param surv.method either \code{'lifetable'} or \code{'hazard'}; determines
#' the method of calculating survival time functions
#' @param relsurv.method  either \code{'e2'} or \code{'pp'}; 
#' defines whether to compute relative survival using the
#' EdererII method or using Pohar-Perme weighting;
#' ignored if \code{surv.type != "surv.rel"}
#'  
#' @param subset a logical condition; e.g. \code{subset = sex == 1}; 
#' subsets the data before computations
#'  
#' 
#' @param agegr.w.breaks optional; if not \code{NULL}, given breaks will be 
#' used to define age groups in age group
#' weighting; given as left inclusive and right exclusive, e.g. [a,b)
#' @param agegr.w.weights optional; if \code{agegr.w.breaks} is not \code{NULL},
#'  the user can define a vector of weights
#' to give to each age group defined by \code{agegr.w.breaks}; 
#' if \code{agegr.w.weights} is \code{NULL},
#' internal weights (see Examples) are used if 
#' \code{agegr.w.breaks} is not \code{NULL}; 
#' can also be one of \code{"ICSS1"}, \code{"ICSS2"}, and \code{"ICSS3"} 
#' for internationally used standard weights ---
#' see References
#' 
#' @param conf.level confidence level used in confidence intervals; 
#' e.g. \code{0.95} for 95 percent confidence intervals
#' @param conf.type character string; must be one of \code{"plain"}, 
#' \code{"log-log"} and \code{"log"}; 
#' defines the transformation used on the survival (and/or relative survival)
#'  function to yield confidence 
#' intervals via the delta method
#' @param format logical; if \code{TRUE}, output is formatted into a neat table;
#' otherwise you get all the raw results
#' @param verbose logical; if \code{TRUE}, the function is chatty and
#'  returns some messages and timings along the process
#' 
#' @details
#' 
#' \strong{survtab may change in significant ways in future releases and
#' should not be considered to be stable.}
#' 
#' \strong{Basics}
#' 
#' \code{survtab} creates survival tables using data split with e.g. 
#' \code{\link{lexpand}}. We recommend using \code{lexpand} since
#' it is well tested and one usually needs to merge in population hazards
#' to compute relative survivals.
#'  
#'  By default
#' \code{survtab} makes use of the exact same breaks that were used in 
#' splitting (with e.g. \code{lexpand}), so it is not necessary to specify any
#' \code{surv.breaks}. If specified, the 
#' \code{surv.breaks} must be a subset of the pertinent 
#' breaks given in \code{lexpand}.
#' 
#' The function supplies \code{surv.breaks} to \code{\link{cut}} to 
#' create survival intervals in the data, 
#' e.g. 
#' 
#' \code{surv.breaks=0:5 -> [0,1),[1,2), ..., [4,5)}. 
#' 
#' 
#' Interval lengths (\code{delta} in output) are also calculated based 
#' on \code{surv.breaks}. The upper limit of the breaks should
#' therefore be meaningful and never e.g. \code{Inf}. 
#' 
#' if \code{surv.type = 'surv.obs'}, only 'raw' observed survival 
#' is calculated over the chosen time intervals. With
#' \code{surv.type = 'surv.rel'}, also relative survival estimates 
#' are supplied in addition to observed survival figures. 
#' 
#' \code{surv.type = 'cif.obs'} requests cumulative incidence functions (CIF) 
#' to be estimated, where all unique \code{event.values}
#' are seen as competing risks indicators 
#' (others are random censoring indicators);
#' CIFs are estimated for each competing risk are computed based 
#' on a survival-interval-specific proportional hazards
#' assumption as described by Chiang (1968) using the chosen \code{surv.method}.  
#' With \code{surv.type = 'cif.rel'}, a CIF is estimated with using 
#' excess cases as the ''cause-specific'' cases.
#' 
#' if \code{surv.type = 'surv.cause'}, cause-specific survivals are estimated
#' separately for each unique value of \code{event.values}.
#' 
#' The vignette \href{../doc/survtab_examples.html}{survtab_examples} 
#' may prove valuable for learning using \code{survtab} through
#' practical examples.
#' 
#' \strong{Relative / net survival}
#'  
#' When \code{surv.type = 'surv.rel'}, the user can choose 
#' \code{relsurv.method = 'pp'}, whereupon
#' additional Pohar-Perme weighting is used 
#' to get closer to a true net survival measure.
#' By default \code{relsurv.method = 'e2'}.
#'
#'
#' \strong{Age-standardised survival}
#' 
#' The user can also apply age standardisation on top of 
#' everything else. Then the requested survival figures
#' are calculated for each age group separately, and then a 
#' weighted average of the age-group-specific survivals
#' is presented. 
#' 
#' The user must define the age-standardisation age 
#' groups and their weights with the \code{agegr.w.breaks}
#' and \code{agegr.w.weights} arguments. The numbers of
#' age groups and weights should match; e.g. 
#' with \code{agegr.w.breaks = c(0,45,65,75,Inf)} the weights vector 
#' must then have 4 elements. 
#' The \code{agegr.w.weights} do not have to sum to one as 
#' they are processed internally to do so.
#' 
#' If one wishes to use one of the three integrated international 
#' standard weighting schemes available,
#' one must specify the weighting scheme by using e.g. 
#' \code{agegr.w.weights = "ICSS1"}, and also
#' by specifying the used \code{agegr.w.breaks}. However, as 
#' the weights are available only
#' for 5-year age groups, the \code{agegr.w.breaks} must all 
#' (except the last) be divisible by 5;
#' e.g.  
#' 
#' \code{agegr.w.breaks = c(0, 45, 65, 85, Inf)}.
#' 
#' 
#' You can see the international weights integrated
#' into \pkg{popEpi} by typing \code{ICSS} into the console. 
#' See also \code{\link{ICSS}}.
#' 
#' Note that the \code{by.vars} should not be confused with age-standardisation.
#' \code{by.vars} simply determine variables, for the unique combinations of which
#' survivals are computed and outputted separately.
#' 
#' 
#' \strong{Period analysis and other data selection schemes}
#' 
#' If one wishes to calculate e.g. period analysis (delayed entry estimates), 
#' one should limit the data accordingly
#' when expanding the data; see \code{\link{lexpand}}.
#' 
#' 
#' \strong{Data requirements}
#' 
#' This function requires the data to contain,
#' at minimum, the variables 
#' \code{lex.id}, \code{lex.dur},\code{lex.Cst},
#' \code{lex.Xst},  and \code{fot}; these will be enough to calculate
#' observed survivals. 
#' 
#' Relative survivals require additional information. EdererII
#' relative survival requires the presence of a \code{pop.haz} variable in the data,
#' and Pohar-Perme weighting requires \code{pp} (the inverse cumulative population
#' survival). Both can be computed with \code{lexpand}.
#' 
#' You may take a look at a simulated cohort \code{\link{sire}} as an example of the
#' minimum required information for when processing data to be used in calculating 
#' relative survival (in the Finnish context).
#' 
#' @return
#' Returns a table of life time function values and other 
#' information with survival intervals as rows.
#' Returns some of the following estimates of survival time functions:
#' 
#' \itemize{
#'  \item \code{surv.obs} - observed (raw) survival
#'  \item \code{CIF_k} - cumulative incidence function for cause \code{k}
#'  \item \code{CIF.rel} - cumulative incidence function using excess cases
#'  \item \code{r.e2} -  relative survival, EdererII
#'  \item \code{r.pp} -  relative survival, Pohar-Perme weighted
#' }
#' The suffix \code{.as} implies agegroup-standardisation, and \code{.lo} and
#' \code{.hi} imply lower and upper confidence limits, respectively. 
#' The prefix \code{SE.} stands for standard error.
#' 
#' @import data.table
#' 
#' @export survtab
#' 
#' @seealso
#' \code{\link{splitMulti}}, \code{\link{lexpand}}, 
#' \code{\link{ICSS}}, \code{\link{sire}},
#' \href{../doc/survtab_examples.html}{The survtab_examples vignette}
#' 
#' @references
#' 
#' Perme, Maja Pohar, Janez Stare, and Jacques Estève. 
#' "On estimation in relative survival." Biometrics 68.1 (2012): 113-120.
#' 
#' Hakulinen, Timo, Karri Seppa, and Paul C. Lambert. 
#' "Choosing the relative survival method for cancer survival estimation." European Journal of Cancer 47.14 (2011): 2202-2210.
#'  
#' Seppa, Karri, Timo Hakulinen, and Arun Pokhrel. 
#' "Choosing the net survival method for cancer survival estimation." European Journal of Cancer (2013).
#' 
#' CHIANG, Chin Long. Introduction to stochastic processes in biostatistics. 1968.
#'  
#' 
#' @examples
#' ## see more examples with explanations in vignette("survtab_examples")
#' 
#' x <- lexpand(sire, birth = bi_date, entry = dg_date, exit = ex_date,
#'              status = status %in% 1:2, fot = seq(0, 5, 1/12),
#'              aggre = list(fot, agegroup = cut(dg_age,4,lab=FALSE)))
#' 

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
  data[, tmpSI := cutLow(fot, breaks = surv.breaks)]
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
  data[, mv1 := cutLow(v1, ...)]
  data <- merge(data, weigths, by = weVars, all.x = TRUE, all.y = TRUE)
  
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





globalVariables(c("lex.Xst", "lex.Cst", "lex.dur", "agegr", "ageint_start", "lex.id", "lex.multi", "entry_age", "age", "fot", "per", "agegr.w", "surv.int",
                  "Tstart", "Tstop", "delta", "entered_late", "entered_int_late"))

globalVariables(c("n.start", "d", "lex.Xst", "n.cens", "surv.int", "d.exp", "pop.haz", "d.exp.pp", "d.exp", "pp", "d.pp", "d.pp.2", "n.eff.pp", "pyrs.pp"))
globalVariables(c("ICSS", "n.eff", "pyrs", "test_pyrs", "surv.obs", "lag1_surv.obs", "p.obs", "surv.obs", "CIF.rel", "p.exp", "surv.exp", "obs", "agestd"))