#' @template survival_doc_template
#' @param formula a \code{formula}; the response 
#' must be the time scale to compute survival time function estimates
#' over, e.g. \code{fot ~ sex}. Variables on the right-hand side of the formula
#' separated by \code{+} are considered stratifying variables, for which 
#' estimates are computed separately. May contain usage of \code{adjust()} 
#' --- see Details and Examples.
#' @param data since popEpi 0.4.0, a \code{data.frame}
#' containing variables used in \code{formula} and other arguments.
#' \code{aggre} objects are recommended as they contain information on any
#' time scales and are therefore safer; for creating \code{aggre} objects see
#' \code{\link{as.aggre}} when your data is already aggregated and \code{aggre}
#' for aggregating split \code{Lexis} objects.
#' 
#' @param surv.breaks a vector of breaks on the 
#' survival time scale. Optional if \code{data} is an \code{aggre} object
#' and mandatory otherwise. Must define each intended interval;
#' e.g. \code{surv.breaks = 0:5} when data has intervals defined by 
#' breaks \code{seq(0, 5, 1/12)} will aggregate to wider intervals first.
#' It is generally recommended (and sufficient; 
#' see Seppa, Dyban and Hakulinen (2015)) to use monthly
#' intervals where applicable.
#' 
#' @param n variable containing counts of subjects at-risk at the start of a 
#' time interval; e.g. \code{n = "at.risk"}. 
#' Required when \code{surv.method = "lifetable"}.
#' \link[=flexible_argument]{Flexible input}.
#' 
#' @param d variable(s) containing counts of subjects experiencing an event. 
#' With only one type of event, e.g. \code{d = "deaths"}. With multiple types of 
#' events (for CIF or cause-specific survival estimation), supply e.g.
#' \code{d = c("canD", "othD")}. If the survival time function to be estimated
#' does not use multiple types of events, supplying more than one variable
#' to \code{d} simply causes the variables to be added together. 
#' Always required. \link[=flexible_argument]{Flexible input}.
#' 
#' @param n.cens variable containing counts of subjects censored during a 
#' survival time interval; E.g. \code{n.cens = "alive"}.
#' Required when \code{surv.method = "lifetable"}. 
#' \link[=flexible_argument]{Flexible input}.

#' @param pyrs variable containing total subject-time accumulated within a 
#' survival time interval; E.g. \code{pyrs = "pyrs"}. 
#' Required when \code{surv.method = "hazard"}. Flexible input.

#' @param d.exp variable denoting total "expected numbers of events" 
#' (typically computed \code{pyrs * pop.haz}, where 
#' \code{pop.haz} is the expected hazard level) 
#' accumulated within a survival time interval; E.g. \code{pyrs = "pyrs"}.
#' Required when computing EdererII relative survivals or 
#' CIFs based on excess counts of events. Flexible input.

#' @param n.pp variable containing total Pohar-Perme weighted counts of
#' subjects at risk in an interval,
#' supplied as argument \code{n} is supplied. 
#' Computed originally on the subject
#' level as analogous to \code{pp * as.integer(status == "at-risk")}.
#' Required when \code{relsurv.method = "pp"}. Flexible input.
#' 
#' @param d.pp variable(s) containing Pohar-Perme weighted counts of events,
#' supplied as argument \code{d} is supplied. Computed originally on the subject
#' level as analogous to \code{pp * as.integer(status == some_event)}.
#' Required when \code{relsurv.method = "pp"}. Flexible input.

#' @param d.pp.2 variable(s) containing total Pohar-Perme 
#' "double-weighted" counts of events,
#' supplied as argument \code{d} is supplied. Computed originally on the subject
#' level as analogous to \code{pp * pp * as.integer(status == some_event)}.
#' Required when \code{relsurv.method = "pp"}. Flexible input.

#' @param n.cens.pp variable containing total Pohar-Perme weighted counts 
#' censorings,
#' supplied as argument \code{n.cens} is supplied. 
#' Computed originally on the subject
#' level as analogous to \code{pp * as.integer(status == "censored")}.
#' Required when \code{relsurv.method = "pp"}. Flexible input.

#' @param pyrs.pp variable containing total Pohar-Perme weighted subject-times,
#' supplied as argument \code{pyrs} is supplied. 
#' Computed originally on the subject
#' level as analogous to \code{pp * pyrs}.
#' Required when \code{relsurv.method = "pp"}. Flexible input.

#' @param d.exp.pp variable containing total Pohar-Perme weighted counts 
#' of excess events,
#' supplied as argument \code{pyrs} is supplied. 
#' Computed originally on the subject
#' level as analogous to \code{pp * d.exp}.
#' Required when \code{relsurv.method = "pp"}. Flexible input.
#' 
#' 
#' @section Data requirements:
#' 
#' \code{survtab_ag} computes estimates of survival time functions using 
#' pre-aggregated data. For using subject-level data directly, use 
#' \code{\link{survtab}}. For aggregating data, see \code{\link{lexpand}}
#' and \code{\link{aggre}}. 
#' 
#' By default, and if data is an \code{aggre} object (not mandatory), 
#' \code{survtab_ag} makes use of the exact same breaks that were used in 
#' splitting the original data (with e.g. \code{lexpand}), so it is not 
#' necessary to specify any \code{surv.breaks}. If specified, the 
#' \code{surv.breaks} must be a subset of the pertinent 
#' pre-existing breaks. When data is not an \code{aggre} object, breaks
#' must always be specified. Interval lengths (\code{delta} in output) are 
#' also calculated based on whichever breaks ares used, 
#' so the upper limit of the breaks should
#' therefore be meaningful and never e.g. \code{Inf}. 
#' 
#' 
#' @examples
#' ## see more examples with explanations in vignette("survtab_examples")
#' 
#' #### survtab_ag usage
#' 
#' data("sire", package = "popEpi")
#' ## prepare data for e.g. 5-year "period analysis" for 2008-2012
#' ## note: sire is a simulated cohort integrated into popEpi.
#' BL <- list(fot=seq(0, 5, by = 1/12),
#'            per = c("2008-01-01", "2013-01-01"))
#' x <- lexpand(sire, birth = bi_date, entry = dg_date, exit = ex_date,
#'              status = status %in% 1:2,
#'              breaks = BL,
#'              pophaz = popmort,
#'              aggre = list(fot))
#'              
#' ## calculate relative EdererII period method
#' ## NOTE: x is an aggre object here, so surv.breaks are deduced
#' ## automatically
#' st <- survtab_ag(fot ~ 1, data = x)
#' 
#' summary(st, t = 1:5) ## annual estimates
#' summary(st, q = list(r.e2 = 0.75)) ## 1st interval where r.e2 >= 0.75 at end
#' \dontrun{
#' plot(st)
#' 
#' 
#' ## non-aggre data: first call to survtab_ag would fail
#' df <- data.frame(x)
#' # st <- survtab_ag(fot ~ 1, data = x)
#' st <- survtab_ag(fot ~ 1, data = x, surv.breaks = BL$fot)
#' 
#' ## calculate age-standardised 5-year relative survival ratio using 
#' ## Ederer II method and period approach 
#' 
#' sire$agegr <- cut(sire$dg_age,c(0,45,55,65,75,Inf),right=F)
#' BL <- list(fot=seq(0, 5, by = 1/12),
#'            per = c("2008-01-01", "2013-01-01"))
#' x <- lexpand(sire, birth = bi_date, entry = dg_date, exit = ex_date,
#'              status = status %in% 1:2,
#'              breaks = BL,
#'              pophaz = popmort,
#'              aggre = list(agegr, fot))
#' 
#' ## age standardisation using internal weights (age distribution of 
#' ## patients diagnosed within the period window)
#' ## (NOTE: what is done here is equivalent to using weights = "internal")
#' w <- aggregate(at.risk ~ agegr, data = x[x$fot == 0], FUN = sum)
#' names(w) <- c("agegr", "weights")
#' 
#' st <- survtab_ag(fot ~ adjust(agegr), data = x, weights = w)
#' plot(st, y = "r.e2.as", col = c("blue"))
#' 
#' ## age standardisation using ICSS1 weights
#' data(ICSS)
#' cut <- c(0, 45, 55, 65, 75, Inf)
#' agegr <- cut(ICSS$age, cut, right = FALSE)
#' w <- aggregate(ICSS1~agegr, data = ICSS, FUN = sum)
#' names(w) <- c("agegr", "weights")
#'
#' st <- survtab_ag(fot ~ adjust(agegr), data = x, weights = w)
#' lines(st, y = "r.e2.as", col = c("red"))
#' 
#' 
#' ## cause-specific survival
#' sire$stat <- factor(sire$status, 0:2, c("alive", "canD", "othD"))
#' x <- lexpand(sire, birth = bi_date, entry = dg_date, exit = ex_date,
#'              status = stat,
#'              breaks = BL,
#'              pophaz = popmort,
#'              aggre = list(agegr, fot))
#' st <- survtab_ag(fot ~ adjust(agegr), data = x, weights = w,
#'                  d = c("fromalivetocanD", "fromalivetoothD"),
#'                  surv.type = "surv.cause")
#' plot(st, y = "surv.obs.fromalivetocanD.as")
#' lines(st, y = "surv.obs.fromalivetoothD.as", col = "red")
#' 
#' 
#' }
#' @export
survtab_ag <- function(formula = NULL,
                       
                       data, 
                       
                       adjust = NULL,
                       weights = NULL,
                       
                       surv.breaks = NULL, 
                       
                       n = "at.risk",
                       d = "from0to1",
                       n.cens = "from0to0",
                       pyrs = "pyrs",
                       d.exp = "d.exp",
                       
                       n.pp = NULL,
                       d.pp = "d.pp",
                       d.pp.2 = "d.pp.2",
                       n.cens.pp = "n.cens.pp",
                       pyrs.pp = "pyrs.pp",
                       d.exp.pp = "d.exp.pp",
                       
                       surv.type="surv.rel", 
                       surv.method="hazard", 
                       relsurv.method="e2",  
                       
                       subset = NULL,
                       
                       conf.level = 0.95, 
                       conf.type = "log-log",
                       
                       verbose=FALSE) {
  
  if (verbose) starttime <- proc.time()
  
  Tstop <- delta <- Tstart <- surv.int <- n.eff <- n.eff.pp <- surv.obs <- 
     lag1_surv.obs <- p.obs <- CIF.rel <- NULL ## APPEASE R CMD CHECK
  
  TF <- environment()
  PF <- parent.frame(1L)
  
  this_call <- match.call()
  used_args <- as.list(this_call)[-1L]
  fl <- formals("survtab_ag")
  used_args <- c(used_args, fl[!names(fl) %in% names(used_args)])
  used_args <- used_args[names(fl)]
  rm(fl)
  
  attrs <- copy(attributes(data))
  
  # check data -----------------------------------------------------------------
  if (missing(data) || nrow(data) == 0) stop("data missing or has no rows")
  
  # check arguments ------------------------------------------------------------
  
  surv.type <- match.arg(surv.type, c("surv.obs","surv.rel","surv.cause", "cif.obs", "cif.rel"))
  surv.method <- match.arg(surv.method, c("lifetable","hazard"))
  relsurv.method <- match.arg(relsurv.method, c("e2", "pp", "EdererII", "Pohar-Perme", "pohar-perme", "edererII", "ederer2"))
  if (relsurv.method %in% c("EdererII", "edererII", "ederer2")) relsurv.method <- "e2"
  if (relsurv.method %in% c("Pohar-Perme", "pohar-perme")) relsurv.method <- "pp"
  relsurv.method <- match.arg(relsurv.method, c("e2", "pp"))
  conf.type <- match.arg(conf.type, c("log","log-log","plain"))
  
  
  ## argument 'formula' pre-check ----------------------------------------------
  if (!(inherits(formula, "formula") && length(formula) == 3L)) {
    stop("Argument 'formula' does not appear to be a two-sided formula. ",
         "Usage: e.g. fot ~ sex")
  }
  surv.scale <- deparse(formula[[2]])
  if (!surv.scale %in% names(data)) {
    stop("Left-hand-side of formula must be a column in data; e.g. ",
         "fot ~ sex, where 'fot' is the name of a column in data.")
  }
  
  ## check breaks --------------------------------------------------------------
  
  surv.breaks <- select_breaks(data = data, ts = surv.scale, br = surv.breaks)
  surv.breaks <- sort(unique(surv.breaks))
  # if (!breaks_in_data(surv.breaks, surv.scale, data)) {
  #   stop("Used breaks do not all appear to exist in data. Make sure the ",
  #        "breaks match to the values that your time scale variable has in the ",
  #        "data.")
  # }
  
  # data prep & subsetting -----------------------------------------------------
  subset <- substitute(subset)
  subset <- evalLogicalSubset(data, subset)
  
  origData <- data
  
  data <- data[subset, ]
  setDT(data)
  
  # handle count etc. variables ------------------------------------------------
  
  valVars <- c("d")
  valVars <- c(valVars, if (surv.method == "hazard") "pyrs" else c("n", "n.cens"))
  
  valVars <- c(valVars, if (surv.type == "surv.rel" && relsurv.method == "e2")  "d.exp" else NULL)
  
  valVars <- c(valVars, if (surv.type == "cif.rel")  "d.exp" else NULL)
  
  ppVars <- c("d.pp", "d.exp.pp", "d.pp.2", 
              if (surv.method == "hazard") "pyrs.pp" else c("n.cens.pp", "n.pp"))
  valVars <- c(valVars, if (surv.type == "surv.rel" && relsurv.method == "pp") ppVars else NULL)
  
  fo <- formals("survtab_ag")
  mc <- as.list(match.call())[-1]
  mc <- c(mc, fo[!names(fo) %in% names(mc)])
  
  mc <- mc[valVars]
  
  mc <- lapply(mc, function(elem) {
    evalPopArg(data = data, arg = elem, DT = TRUE, enclos = PF, recursive = TRUE)
    })
  
  ## NOTE: this does not delete but sets the value to NULL.
  mc[unlist(lapply(mc, function(x) {
    NROW(x) == 0L || is.null(x) || is.language(x) || inherits(x, "try-error")
    }))] <- NULL
  
  lackVars <- setdiff(valVars, names(mc[!unlist(lapply(mc, is.null))]))
  if (length(lackVars) > 0) {
    stop("Following arguments were NULL or could not be evaluated but are ",
         "required: ", paste0("'", lackVars, "'", collapse = ", "), ". ",
         "Usual suspects: arguments are NULL or refer to variables that ",
         "cannot be found in data.")
  }
  
  eventVars <- NULL
  ## NOTE: not sure if other arguments than 'd' should be allowed to be of 
  ## length > 1 (cause-specific 'd'); restricted for now to 'd' but easy to
  ## allow in the procedure below.
  ## nl will contain the names of the variables corresponding to each argument,
  ## e.g. d = c("d.1", "d.2"), etc.
  mc[[1]] <- data.table(mc[[1L]]) ## this avoids an exotic error in set().
  nl <- lapply(mc, names)
  for (k in 1:length(mc)) {
    jay <- argName <- names(mc[k])
    cn <- names(mc[[k]])
    
    if (length(cn) > 1) jay <- paste0(jay, ".", cn) ## e.g. d.1, d.2, ...
    if (argName %in% c("d")) {
      eventVars <- jay
      if (surv.type %in% c("surv.cause") && length(cn) == 1L) {
        stop("surv.type = 'surv.cause', but only one type of event supplied ",
             "via argument 'd'. If you want to compute cause-specific ",
             "survivals, please supply multiple types of events via ",
             "'d'; otherwise use surv.type = 'surv.obs'") 
      } else  if (length(cn) > 1 && !argName %in% c("d","d.pp", "d.pp.2", "n.pp")) {
        stop("'", argName, "' has/evaluates to ", length(cn), 
             " columns; only 'd', 'd.pp', and 'd'pp.2', 'n.pp' may evaluate ",
             "to more than one column of the value arguments")
      }
      
    }
    
    setnames(mc[[k]], cn, jay)
    set(mc[[1]], j = jay, value = mc[[k]])
    nl[[argName]] <- jay
  }
  mc <- mc[[1]]
  
  if (!is.null(eventVars)) {
    set(mc, j = "d", value = rowSums(mc[, mget(eventVars)]))
    valVars <- unique(c(valVars, "d", eventVars))
  }
  
  ## sum e.g. d.pp.1 + d.pp.2 = d.pp
  dna <- names(nl)[names(nl) %in% c("d.pp", "d.pp.2")]
  if (length(dna)) dna <- dna[unlist(lapply(nl[dna], function(x) length(x) > 1L))]
  if (length(dna)) {
    
    for (k in dna) {
      set(mc, j = k, value = mc[, rowSums(.SD), .SDcols = nl[[k]]])
    }
    setcolsnull(mc, unlist(nl[dna]))
    valVars <- setdiff(valVars, unlist(nl[dna]))
    valVars <- c(valVars, dna)
    valVars <- unique(valVars)
  }
  
  
  all_names_present(mc, valVars)
  setcolorder(mc, valVars)
  
  ## addition: internal weights use n at beginning of first interval
  
  if (is.character(weights)) {
    checkWeights(weights)
    if (!"n" %in% valVars) {
      n <- substitute(n)
      mc$n <- evalPopArg(n, data = data, enclos = PF)
      
      valVars <- unique(c(valVars, "n"))
      
      if (is.null(mc$n)) {
        
        stop("Requested internal weights to be computed and used to standardize ", 
             "estimates, but argument 'n' not supplied. This is currently ",
             "required for computing internal weights (the values of 'n' ", 
             "in the first interval will be used for this). Please supply 'n' ",
             "or supply hand-made weights (preferred for your clarity).")
      }
    } 
    
    data[, c("n") := mc$n]
    
  }
  
  
  # making weighted table of aggregated values ---------------------------------
  ## NOTE: at-risk counts require special treatment when surv.breaks
  ## are a subset of the available breaks: cannot sum at-risk figures!
  ## instead should simply pick the value at the start of the
  ## (now larger) interval. Will accomplish this by setting values not
  ## at the start of an interval to zero and summing anyway.
  if (surv.method == "lifetable") {
    wh_internal <- list(surv.breaks)
    names(wh_internal) <- surv.scale
    wh_internal <- data[wh_internal, on = eval(surv.scale), which = TRUE]
    wh_internal <- setdiff(1:nrow(data), wh_internal)
    mc[wh_internal, intersect(c("n", "n.pp"), names(mc)) := 0L]
  }
  
  ## NOTE: while ssSub will pass the whole column of e.g. fot values, which will
  ## not limit the data to e.g. up 5 years of follow-up if original data went 
  ## further, surv.breaks may be only up to 5 years and will limit the data
  ## in makeWeightsDT using a CJ-merge-trick appropriately (via custom.levels).
  bl <- list(surv.breaks)
  setattr(bl, "names", surv.scale)
  
  adjust <- evalPopArg(data, adjust, enclos = PF, naming = "model")
  
  iws <- NULL
  if (is.character(weights) && pmatch(weights, c("internal", "cohort"), 0)) {
    if (!"n" %in% names(data)) {
      stop("Need 'n' specified for when using internal weights: Internal ",
           "weights are computed as the counts of subjects at the start of ",
           "follow-up.")
    }
    iws <- makeTempVarName(data, pre = "internal_weights_")
    data[, c(iws) := 0.0]
    data[data[[surv.scale]] == surv.breaks[1], c(iws) := n]
  }
  
  data <- makeWeightsDT(data = data, values = list(mc), enclos = PF,
                        print = NULL, formula = formula, adjust = adjust, 
                        by.other = surv.scale, Surv.response = FALSE,
                        custom.levels = bl, weights = weights,
                        internal.weights.values = iws,
                        custom.levels.cut.low = surv.scale)
  
  allVars <- attr(data, "makeWeightsDT")
  allVars[] <- lapply(allVars, function(x) if (length(x) == 0L) NULL else x)
  prVars <- allVars$prVars
  adVars <- allVars$adVars
  # boVars <- allVars$boVars ## this is surv.scale
  valVars <- allVars$vaVars
  
  ## to avoid e.g. 'factor(sex, 1:2)' going bonkers
  prVars_orig <- prVars
  if (length(prVars) > 0L) {
    prVars <- makeTempVarName(names = c(names(data), adVars), 
                              pre = paste0("print_", 1:length(prVars)))
  }
  adVars_orig <- adVars
  if (length(adVars) > 0L) {
    adVars <- makeTempVarName(names = c(names(data), prVars), 
                              pre = paste0("print_", 1:length(adVars)))
  }
  if (length(c(prVars, adVars))) setnames(data, c(prVars_orig, adVars_orig), c(prVars, adVars))
  byVars <- c(prVars, adVars)
  
  # formulate some needed variables --------------------------------------------
  setkeyv(data, c(byVars, surv.scale))
  data[, Tstop := surv.breaks[-1]]
  setnames(data, surv.scale, "Tstart")
  data[, delta := Tstop - Tstart]
  data[, surv.int := 1:.N, by = eval(byVars)]
  setcolorder(data, c(byVars, "surv.int", "Tstart", "Tstop", "delta", valVars, intersect(names(data), "weights")))
  
  if (surv.method == "lifetable") {
    testEvents <- data[, n - shift(n, n = 1, type = "lead", fill = NA), by = eval(byVars)]$V1
    testEvents <- data$n.cens + data$d - testEvents
    
    if (sum(abs(testEvents), na.rm = TRUE)) {
      on.exit({
        
        data[, "n.cens + d - (n-lead1_n)" := testEvents]
        wh <- testEvents != 0L
        wh <- wh & !is.na(wh)
        if (interactive()) {
          printSD <- c(byVars, "Tstop", "d", "n", "n.cens", 
                       "n.cens + d - (n-lead1_n)")
          print(data[wh, .SD, .SDcols = printSD], top = 5, nrow = 10)
          
        }
        
      }, add = TRUE)
      
      stop("Supplied n.cens and d do not sum to total number of events and ",
           "censorings based on n alone. Note that lifetable analysis ",
           "is currently not supported for period analysis (or other ",
           "comparable limitations of data).",
           if (interactive())" See table below and check your variables.")
    }
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
  testVar <- if (surv.method == "lifetable") "n" else "pyrs"
  ## sum over adjusting variables
  data <- test_empty_surv_ints(data, by = c(prVars, adVars), 
                               show.by = c(prVars_orig, adVars_orig),
                               sum.over = adVars,
                               test.var = testVar)
  
  ## sum over nothing
  if (length(adVars) > 0L) {
    data <- test_empty_surv_ints(data, by = c(prVars, adVars), 
                                 show.by = c(prVars_orig, adVars_orig),
                                 sum.over = NULL, test.var = testVar)
  }
  
  ## if adjusting, crop all estimates by adjusting variables
  ## to shortest estimate
  if (length(adVars)) {
    adLe <- data[, list(min = min(surv.int), max = max(surv.int)), keyby = eval(adVars)]
    adLe <- c(max(adLe$min), min(adLe$max))
    data <- data[surv.int %in% `:`(adLe[1L], adLe[2L])]
  }
  
  # create and print table of bad surv.ints ------------------------------------
  
  badObsSurv <- data$surv.obs == 0 | is.na(data$surv.obs)
  if (sum(badObsSurv)) {
    
    zerotab <- data[badObsSurv, 
                    list(first.bad.surv.int = min(as.integer(surv.int)), 
                         last.bad.surv.int = max(as.integer(surv.int)), 
                         surv.obs=min(surv.obs)), keyby = eval(byVars)]
    
    
    message("Some cumulative surv.obs were zero or NA:")
    if (length(byVars)) setnames(zerotab, c(prVars, adVars), c(prVars_orig, adVars_orig))
    print(zerotab)
    if (surv.method == "lifetable" && data[surv.obs == 0, .N] > 0) {
      message("NOTE: Zero surv.obs leads to zero relative survivals as well. Adjusting with weights WILL use the zero surv.obs / relative survival values.")
    }
    
  }
  rm(badObsSurv)
  
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
    surv_names <- c("d", if (surv.method == "lifetable") "n.eff" else NULL, surv_names)
    setnames(data, surv_names, paste0(surv_names, ".orig"))
    
    for (k in eventVars) {
      
      k <- gsub(pattern = "d.", replacement = "", x = k)
      setnames(data, paste0("d.",k), "d")
      
      if (surv.method=="hazard") {
        comp.st.surv.obs.haz(surv.table = data, surv.by.vars = byVars)
      } else {
        setnames(data, paste0("n.eff_", k), "n.eff")
        comp.st.surv.obs.lif(surv.table = data, surv.by.vars = byVars)
      }
      os.table <- comp.st.conf.ints(data, al=1-conf.level, surv="surv.obs", transform = conf.type)
      
      new_surv_names <- setdiff(surv_names, c("d", if (surv.method == "lifetable") "n.eff" else NULL))
      new_surv_names <- gsub("surv.obs", paste0("surv.obs.", k), new_surv_names)
      new_surv_names <- c(paste0(c("d.", if (surv.method == "lifetable") "n.eff." else NULL), k), new_surv_names)
      setnames(data, surv_names, new_surv_names)
      
      
    }
    setnames(data, paste0(surv_names, ".orig"), surv_names)
  }
  
  # compute cause-specifc/excess-case CIFs -------------------------------------
  if (surv.type %in% c("cif.obs", "cif.rel")) {
    
    data[, lag1_surv.obs := shift(surv.obs, n = 1L, type = "lag", fill = 1), by = eval(byVars)]
    data[, p.obs := surv.obs/lag1_surv.obs]
    
    if (surv.type == "cif.obs") {
      for (k in eventVars) {
        
        k <- gsub("d.", "", x = k)
        d.k <- paste0("d.", k)
        
        d.var <- paste0("d.",k)
        q.var <- paste0("q.", k)
        CIF_var <- paste0("CIF_", k)
        data[, (q.var)   := (1-p.obs)*get(d.var)/d]
        data[get(d.var) == 0L | d == 0L, (q.var) := 0]
        data[, (CIF_var) := cumsum(lag1_surv.obs*get(q.var)), by = eval(byVars)]
      }
    }
    
    if (surv.type == "cif.rel") {
      ## assuming d.exp in data
      data[, CIF.rel := (1-p.obs)*(d-d.exp)/d]
      data[d.exp>d, CIF.rel := NA]
      data[, CIF.rel := cumsum(lag1_surv.obs*CIF.rel), by = eval(byVars)]
    }
    
    ## SEs currently not known for CIFs; impute 0 to make adjusting work
    CIF_vars <- names(data)[substr(names(data),1,3) == "CIF"]
    data[, c(paste0("SE.", CIF_vars)) := 0L]
    
    setcolsnull(data, c("lag1_surv.obs", "p.obs", paste0("q.", substr(eventVars, 3, nchar(eventVars)))))
    
  }
  
  
  # relative survivals ---------------------------------------------------------
  if (surv.type == "surv.rel" & relsurv.method == "e2") {
    
    # compute r.e2 -------------------------------------------------------------
    comp.st.rs <- function(rs.table, rs.by.vars = byVars) {
      
      p.exp <- delta <- surv.exp <- surv.obs <- n.eff.pp <- 
        surv.obs <- NULL ## APPEASE R CMD CHECK
      ## EdererII
      
      ##-------------
      if (surv.method == "hazard") {
        rs.table[, p.exp := exp(-delta*d.exp/pyrs)] 
        rs.table[, surv.exp := cumprod(p.exp), by = eval(rs.by.vars)]
        comp.st.r.e2.haz(surv.table = rs.table, surv.by.vars = rs.by.vars)
      } else {
        rs.table[, p.exp := 1 - d.exp/n]
        rs.table[, surv.exp := cumprod(p.exp), by = eval(rs.by.vars)]
        
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
        all_names_present(data, c("pyrs.pp"),
                          msg = paste0("internal error: work data did not have",
                                       " variable named pyrs.pp. Complain ",
                                       "to package maintainer if you see this."))
        comp.st.r.pp.haz(surv.table = pp.table, surv.by.vars = by.vars)
      } else {
        data[, n.eff.pp := n.pp - 0.5*n.cens.pp]
        all_names_present(data, c("n.pp", "n.cens.pp", "n.eff.pp"),
                          msg = paste0("internal error: work data did not have",
                                       " variable named n.eff.pp. Complain ",
                                       "to package maintainer if you see this."))
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
    adEsts <- names(data)[substr(names(data), 1, 8) == "surv.obs"]
    adEsts <- c(adEsts, "r.e2", "r.pp")
    adEsts <- c(adEsts,  names(data)[substr(names(data),1,3)=="CIF"])
    adEsts <- intersect(adEsts, names(data))
    adEsts <- adEsts[unlist(lapply(adEsts, function(x) !substr(x, nchar(x)-2L, nchar(x)) %in% c(".lo", ".hi")))]
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
  ## back to original names of print / adjust (used to avoid e.g. 
  ## 'factor(V1, 1:2)' going bonkers in data.table)
  if (length(c(prVars))) setnames(data, c(prVars), c(prVars_orig))
  prVars <- prVars_orig
  adVars <- adVars_orig
  
  ## reorder table, format numeric values, etc.
  
  miscVars <- intersect(names(data), c("surv.int", "Tstart", "Tstop", "delta"))
  
  survVars <- c("surv.obs.lo","surv.obs","surv.obs.hi","SE.surv.obs",
                "r.e2.lo","r.e2","r.e2.hi","SE.r.e2",
                "r.pp.lo","r.pp","r.pp.hi","SE.r.pp",
                paste0("CIF.rel.", c("lo", "", "hi")), "SE.CIF.rel",
                "surv.obs.as.lo","surv.obs.as","surv.obs.as.hi","SE.surv.obs.as",
                "r.e2.as.lo","r.e2.as","r.e2.as.hi","SE.r.e2.as",
                "r.pp.as.lo","r.pp.as","r.pp.as.hi","SE.r.pp.as",
                paste0("CIF.rel.as.", c("lo", "", "hi")), "SE.CIF.rel.as"
  )
  survVars <- intersect(survVars, names(data))
  
  ## which variables are estimates, SEs, CIs, etc.
  survVars.ca <- setdiff(names(data), c(prVars, valVars, miscVars, survVars))
  CIF_vars <- survVars.ca[substr(survVars.ca, 1,3)=="CIF" | substr(survVars.ca, 1,6)=="SE.CIF"]
  survVars <- c(survVars, CIF_vars)
  
  surv.obs.vars <- survVars.ca[substr(survVars.ca, 1,8) == "surv.obs" | substr(survVars.ca, 1,11) == "SE.surv.obs"]
  survVars <- c(survVars, surv.obs.vars)
  
  survVars <- unique(intersect(survVars, names(data)))
  
  ## remove some unuseful variables
  setcolsnull(data, c("SE.A", "SE.B"))
  setcolsnull(data, survVars[substr(survVars, 1, 6) == "SE.CIF"]) ## since they are zero for now
  survVars <- intersect(survVars, names(data))
  
  SEVars <- survVars[substr(survVars, 1, 3) == "SE."]
  CIVars <- survVars[substr(survVars, nchar(survVars) - 2L, nchar(survVars)) %in% c(".lo", ".hi")]
  estVars <- setdiff(survVars, c(SEVars, CIVars))
  
  order <- unique(c(prVars, miscVars, valVars, survVars))
  order <- intersect(order, names(data))
  
  setcolsnull(data, setdiff(names(data), order))
  setcolorder(data,order)
  
  setkeyv(data, c(prVars, "surv.int"))
  
  # attributes -----------------------------------------------------------------
  setkeyv(data, c(prVars, "surv.int"))
  setattr(data, "class", c("survtab", "data.table", "data.frame"))
  if (!return_DT()) setDFpe(data)
  if (length(prVars) == 0) prVars <- NULL ## might be character(0) 
  
  used_args$data <- origData
  used_args$formula <- formula
  used_args$weights <- evalRecursive(arg = weights, env = PF)$weights
  
  arglist <- list(call = this_call, 
                  arguments = used_args,
                  surv.scale = surv.scale,
                  surv.breaks = surv.breaks,
                  print.vars = prVars,
                  adjust.vars = adVars,
                  value.vars = valVars,
                  misc.vars = miscVars,
                  surv.vars = survVars,
                  est.vars = estVars,
                  SE.vars = SEVars,
                  CI.vars = CIVars)
  varsArgs <- substr(names(arglist), nchar(names(arglist))-4L, nchar(names(arglist))) == ".vars"
  varsArgs <- names(arglist)[varsArgs]
  arglist[varsArgs] <- lapply(arglist[varsArgs], function(x) if (length(x) == 0L) NULL else x)
                  
  setattr(data, "survtab.meta", arglist)
  
  if (verbose) cat("Time taken by whole process: ", timetaken(starttime), "\n")
  data[]
}


# ag <- lexpand(sire, birth = "bi_date", entry = "dg_date", exit = "ex_date",
#               status = status %in% 1:2, pophaz = popmort, pp = TRUE,
#               aggre = list(sex, fot), fot = seq(0, 5, 1/12))
# ag[, d.exp := pmax(0L, from0to1 - 3L)]
# st <- survtab_ag(ag, surv.type = "surv.obs", surv.method = "hazard")
# st <- survtab_ag(ag, surv.type = "surv.cause", surv.method = "hazard", d = list(a = from0to1-3, b = 3))

# sire <- copy(sire)
# sire$sex <- rbinom(nrow(sire), size = 1, prob = 0.5)
# ag <- lexpand(sire, birth = "bi_date", entry = "dg_date", exit = "ex_date",
#               status = status %in% 1:2, pophaz = popmort, pp = TRUE,
#               aggre = list(sex, agegr = cut(dg_age, c(0,60,70,80, Inf), labels = FALSE), fot), 
#               fot = seq(0, 5, 1/12))
# ag <- lexpand(sire, birth = "bi_date", entry = "bi_date", exit = "ex_date",
#               status = status %in% 1:2,
#               aggre = list(sex, age), 
#               age = seq(0, 100, 1))
# wdt <- data.table(agegr = 1:4, weights = c(0.2, 0.4, 0.3, 0.1))
# wli <- list(agegr = c(0.2, 0.4, 0.3, 0.1))
# st <- survtab_ag(fot ~ sex + adjust(agegr), data = ag, surv.type = "surv.obs", surv.method = "hazard", weights = wli)
# st <- survtab_ag(fot ~ sex + adjust(agegr), data = ag, surv.type = "surv.rel", 
#                  d.pp = "from0to1.pp", d.pp.2 = "from0to1.pp.2", 
#                  d.exp.pp = "d.exp.pp", pyrs.pp = "ptime.pp",
#                  surv.method = "hazard", weights = wli,
#                  relsurv.method = "pp")
# ag <- lexpand(sire, birth = "bi_date", entry = "dg_date", exit = "ex_date",
#               status = status, pophaz = popmort, pp = TRUE,
#               aggre = list(sex, agegr = cut(dg_age, c(0,60,70,80, Inf), labels = FALSE), fot), 
#               fot = seq(0, 5, 1/12))
# st <- survtab_ag(fot ~ sex + adjust(agegr), data = ag, 
#                  d = list(cand = from0to1, othd = from0to2),
#                  surv.type = "surv.cause", weights = wli)
# st <- survtab_ag(fot ~ sex, data = ag, surv.type = "surv.obs", surv.method = "hazard", adjust = "agegr", weights = wli)
# st <- survtab_ag(fot ~ adjust(agegr), data = ag, surv.type = "surv.obs", weights = wli)
# st <- survtab_ag(fot ~ 1, data = ag, adjust = "agegr", surv.type = "surv.obs", weights = wli)
# st <- survtab_ag(fot ~ 1, data = ag, adjust = "agegr", surv.type = "surv.obs", weights = wli)
# st <- survtab_ag(fot ~ 1, data = ag, surv.type = "surv.obs")

# wli2 <- wli
# wli$sex <- c(0.4, 0.6)
# st <- survtab_ag(fot ~ adjust(sex, agegr), data = ag, surv.type = "surv.obs", weights = wli)
# st <- survtab_ag(fot ~ adjust(agegr), data = ag, surv.type = "surv.obs", weights = wli["agegr"])
# ag[, d.exp := pmax(from0to1 - 1, 0L)]
# st <- survtab_ag(fot ~ adjust(sex, agegr), data = ag, surv.type = "surv.rel", weights = wli)
# st <- survtab_ag(fot ~ adjust(sex, agegr), data = ag, surv.type = "surv.cause", weights = wli)
# ag[, othd := pmax(from0to1 - 1L, 0L)]
# st <- survtab_ag(fot ~ adjust(sex, agegr), data = ag, d = list(cand = from0to1, othd = pmax(from0to1-1L, 0L)), surv.type = "surv.cause", weights = wli)
