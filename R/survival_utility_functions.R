globalVariables(c("tab", "SE.A", "pp.table"))

comp.st.surv <- function(surv.var = "p", surv.expr = "1-d/(n.eff-n.cens/2)", 
                         SE.expr = "sqrt(p*(1-p)/(n.eff))", cumu = TRUE) {
  
  function(surv.table = tab, surv.by.vars = NULL) {
    
    tabname <- deparse(substitute(surv.table))
    #     tabname <- "surv.table"    
    by.expr <- NULL
    if (!is.null(surv.by.vars)) {
      surv.by.vars <- paste0("'", surv.by.vars, "'", collapse = " ,")
      by.expr <- paste0(", by = c( ", surv.by.vars, " )")
    }
    
    surv.expr <-  paste0(surv.var, " := ", surv.expr)
    tab.surv.expr <- paste0(tabname, "[, ", surv.expr,  by.expr, "]")
    #   surv.expr <- parse(text=surv.expr)
    #   surv.var <- parse(text=surv.var)
    SE.var <- paste0("SE.", surv.var)
    SE.expr <- paste0(SE.var, " := ", SE.expr)
    tab.SE.expr <- paste0(tabname, "[, ", SE.expr, ", ", by.expr, "]")
    #   SE.expr <- parse(text=SE.expr)
    #   SE.var <- parse(text=SE.var)
    cumuexpr <- paste0(tabname, "[, ", surv.var, " := cumprod(", surv.var, ")", by.expr, "]" )
    
    
    ## parent.frame(2): two steps upstream in environments
    pe <- function(obj, ..., env=parent.frame(2)) {
      eval(parse(text=obj), ..., envir=env)
    }
    
    pe(tab.surv.expr)
    
    ## keep uninformative estimates NA if last informative estimate
    ## was not zero. if zero, repeat zero till the end.
    ## (in lifetable analysis the survival can drop to zero.)
    mintest.expr <- paste0(tabname, "[, mintest := FALSE]")
    pe(mintest.expr)
    mintest.expr <- paste0(tabname, "[, mintest := min(", surv.var, ", na.rm=T)==0", by.expr, "]")
    pe(mintest.expr)
    
    
    minexpr <- paste0(tabname, "[is.na(", surv.var, ") & mintest==TRUE, ", surv.var, " := 1L]")
    pe(minexpr)
    
    
    
    if (cumu) pe(cumuexpr)
    
    ## standard error
    pe(tab.SE.expr)
    
    ## zero survival leads to zero SE
    minexpr <- paste0(tabname, "[",surv.var, "== 0 & mintest==TRUE, ", SE.var, " := 0L]")
    pe(minexpr)
    
    ## remove mintest variable from used table
    mintest.expr <- paste0(tabname, "[, mintest := NULL]")
    pe(mintest.expr)
  }
}


comp.st.surv.obs.lif <- comp.st.surv(surv.var=  "surv.obs",
                                     surv.expr= "1-d/n.eff", 
                                     SE.expr=   "surv.obs*sqrt(  cumsum(  d/(n.eff*(n.eff-d))  )  )", ## old : sqrt(p*(1-p)/(n.eff))"
                                     cumu = TRUE)

comp.st.surv.obs.haz <- comp.st.surv(surv.var=  "surv.obs", 
                                     surv.expr= "exp(-delta*d/pyrs)", 
                                     SE.expr=   "surv.obs*sqrt(  cumsum(  delta^2*d/pyrs^2  )  )", ## old: sqrt(p*(1-p)/(n.eff))
                                     cumu = TRUE)


comp.st.r.e2.haz <- comp.st.surv(surv.var = "r.e2",
                                 surv.expr = "exp(-delta*(d-d.exp)/pyrs)",
                                 SE.expr = "SE.surv.obs/surv.exp",
                                 cumu=TRUE)

comp.st.r.e2.lif <- comp.st.surv(surv.var = "r.e2",
                                 surv.expr = "1-(d-d.exp)/(n.eff)",
                                 SE.expr = "SE.surv.obs/surv.exp",
                                 cumu=TRUE)

comp.st.r.pp.haz <- comp.st.surv(surv.var = "r.pp",
                                 surv.expr= "exp(-delta*(d.pp - d.exp.pp)/pyrs.pp)",
                                 SE.expr = "r.pp*sqrt(  cumsum(  delta^2*( d.pp.2)/ pyrs.pp^2  )  )",
                                 cumu=TRUE)

comp.st.r.pp.lif <- comp.st.surv(surv.var = "r.pp",
                                 surv.expr = "1-(d.pp-d.exp.pp)/(n.eff.pp)",
                                 SE.expr = "r.pp*sqrt(  cumsum(  d.pp.2/(n.eff^2)  )  )",
                                 cumu=TRUE)


## this function will calculate confidence intervals
## for obs & rel & net survivals
#' @import stats
comp.st.conf.ints <- function(tab = pp.table, al=0.05, surv="r.pp", transform ="log-log") {
  al <- 0.05
  zlo <- as.character(qnorm(al/2))
  zhi <- as.character(qnorm(1-al/2))
  SE.surv <- paste0("SE.",surv)
  surv.hi <- paste0(surv, ".hi")
  surv.lo <- paste0(surv, ".lo")
  
  pe <- function(...) {
    eval(parse(text=paste0(...)), envir=tab)
  }
  
  if (transform =="plain") {
    ## assume S(t)~N(mu, sigma)
    
    ex <- paste0(surv,  " ", zlo, "*", SE.surv)
    tab[, c(surv.lo)  := pe(ex)]
    ex <- paste0(surv, " +", zhi, "*", SE.surv)
    tab[, c(surv.hi)  := pe(ex)]
  }
  
  if (transform =="log-log") {
    ## assume log(H(t))~N(mu, sigma)
    ex <- paste0(SE.surv,"/(abs(log(",surv,"))*",surv,")")
    tab[,  SE.A := pe(ex)]
    
    ex <- paste0(surv, "^exp(", zhi, "*SE.A)")
    tab[, c(surv.lo)  := pe(ex)]
    ex <- paste0(surv, "^exp(", zlo, "*SE.A)")
    tab[, c(surv.hi)  := pe(ex)]
  }
  
  if (transform =="log") {
    ## assume log(S(t))~N(mu, sigma)
    ex <- paste0(SE.surv,"/",surv)
    tab[,  SE.A := pe(ex)]
    
    ex <- paste0(surv, "*exp(", zlo, "*SE.A)")
    tab[, c(surv.lo)  := pe(ex)]
    ex <- paste0(surv, "*exp(", zhi, "*SE.A)")
    tab[, c(surv.hi)  := pe(ex)]
  }
  
  ## zero SE means zero uncertainty means lo=hi=estimate
  tab[get(SE.surv) == 0, c(surv.lo, surv.hi) := get(surv)]
  
  tab[]
}



# x <- Lexis(data=sire[1,], entry = list(fot=0, per=get.yrs(dg_date), age=dg_age), 
#            exit=list(per=get.yrs(ex_date)), exit.status=status)
# x <- splitMulti(x, breaks = list(fot=seq(0, 5, by = 1/12), per=1994:2013, age = 0:150))
# x[, surv.int := cut(fot, seq(0, 5, 1/12) - .Machine$double.eps^0.5, labels = FALSE)]
# x <- cutLowMerge(x, popmort, by.x = c("sex","per", "age"), 
#                  by.y = c("sex", "year", "agegroup"), 
#                  mid.scales = c("per", "age"), all.x = TRUE, all.y = FALSE)
# comp_pp_weights(x, surv.scale = "fot", breaks = seq(0, 5, 1/12), haz = "haz", style = "delta")

comp_pp_weights <- function(lex, surv.scale = "fot", breaks = NULL, haz = "haz", style = "delta", verbose = FALSE) {
  ppTime <- proc.time()
  ## input: a split Lexis object (data.table) and the time scale to compute
  ## pp weights over; lex must also contain 'haz', the population
  ## (expected) hazard level for each row
  this_env_ <- environment()
  
  g <- function(x, e = .SD) {
    x <- deparse(substitute(x)) ## e.g. x = y-> x = "surv.scale"
    x <- this_env_[[x]] ## e.g. this_env_[["surv.scale"]]
    
    eval(as.symbol(x), envir = e)
  }
  
  style <- match.arg(style, c("delta", "actual"))
  if (!is.data.table(lex)) stop("lex must be a data.table")
  
  all_names_present(lex, c(haz, surv.scale, "lex.id", "lex.dur"))
  
  if ("pp" %in% names(lex)) stop("Variable named 'pp' existed in data when attempting to compute pohar-perme weights; 'pp' is reserved so you should delete or rename that variable")
  
  ## cumulative survivals needed for pp weighting.
  setorderv(lex, c("lex.id", surv.scale))
  
  breaks <- sort(unique(breaks)) - .Machine$double.eps^0.5
  
  
  ## need a bunch of temporary variable names to compute pp weights
  ## inside the data set without overwriting anything existing.
  
  tmpSI <- makeTempVarName(data = lex, pre = "surv.int_")
  tmpSIstart <- makeTempVarName(data = lex, pre = "surv.int.start_")
  tmpSIstop <- makeTempVarName(data = lex, pre = "surv.int.stop_")
  tmpSIlength <- makeTempVarName(data = lex, pre = "surv.int.length_")
  on.exit(setcolsnull(lex, delete = c(tmpSI, tmpSIstart, tmpSIstop, tmpSIlength, tmpPS, tmpPCS, tmpPCSM)), add = TRUE)
  lex[, (tmpSI) := cut(g(surv.scale, e = .SD), breaks, labels = FALSE)]
  lex[, (tmpSIstop) := breaks[-1][g(tmpSI, e = .SD)]]
  lex[, (tmpSIstart) := breaks[-length(breaks)][g(tmpSI, e = .SD)]]
  lex[, (tmpSIlength) := g(tmpSIstop, e = .SD) - g(tmpSIstart, e = .SD)]
  
  tmpPS <- makeTempVarName(data = lex, pre = "pop.surv_")
  tmpPCS <- makeTempVarName(data = lex, pre = "pop.cumsurv_")
  tmpPCSM <- makeTempVarName(data = lex, pre = "pop.cumsurv.mid_")

  ## conditional survs
  lex[, (tmpPS) := exp(-g(haz, e = .SD)*lex.dur)] 
  ## till end of each interval...
  lex[, (tmpPCS) := cumprod(g(tmpPS, e = .SD)), by = lex.id]
  ## till start of each interval
  lex[, (tmpPCS) := g(tmpPCS, e = .SD) / (g(tmpPS, e = .SD))]    
  
  ## pohar-perme weighting by expected cumulative survival. approximation:
  ## cumulative survival up to either middle of remaining surv.int (not individual-specific)
  ## or up to middle of subject's follow-up in each row (individual-specific)
  ## difference: e.g. 2 rows within a surv.int have either the same or different pp-weights
  if (style == "actual") {
    lex[, (tmpPCSM) := g(tmpPCS, e = .SD)*(g(tmpPS, e = .SD)^(1/2))]
  }
  if (style == "delta") {
    if (verbose) deltaTime <- proc.time()
    
    setkeyv(lex, c("lex.id", tmpSI))
    ## expected survival up to middle of remaining time in surv.int
    ## cumulation starting from first record for subject in each surv.int
    
    ## some records are the only one for a lex.id in a surv.int; these are easy
    first_in_surv.int <- !duplicated(lex, fromLast = FALSE)
    last_in_surv.int <- !duplicated(lex, fromLast = TRUE)
    only_in_surv.int <- first_in_surv.int & last_in_surv.int
    
    #         last_in_surv.int <- last_in_surv.int & !first_in_surv.int
    #         first_in_surv.int <- first_in_surv.int & !first_in_surv.int
    
    lex[only_in_surv.int, (tmpPCSM) := g(tmpPCS, e = .SD) * exp(-g(haz, e = .SD)*(g(tmpSIstop, e = .SD) - g(surv.scale, e = .SD))/2)]
    ## more complicated with many records in a surv.int per lex.id
    if (any(!only_in_surv.int)) {
      
      dist <- makeTempVarName(lex, pre = "dist_")
      on.exit(setcolsnull(lex, delete = dist), add = TRUE)
      
      ## distance from remaining surv.int mid-point starting from start of record, or lex.dur; for integration
      lex[, (dist) := pmin((g(tmpSIstop, e = .SD) - g(surv.scale, e = .SD))/2, lex.dur)]
      ## some records after mid-point can have negative fot.dist at this point
      lex[, (dist) := pmax(g(dist, e = .SD), 0)]
      
      ## some lex.id are censored / die before mid of surv.int; last record
      ## must reach its fot.dist at least up to the mid (or be zero due to above)
      lex[last_in_surv.int, (dist) := pmax((g(tmpSIstop, e = .SD) - g(surv.scale, e = .SD))/2, 0)]
      
      ## from start of first in surv.int till mid point
      lex[!only_in_surv.int, (tmpPCSM) := g(tmpPCS, e = .SD)[1L] * exp(-sum(g(haz, e = .SD)*g(dist, e = .SD))), by = c(tmpSI, "lex.id")]
      
      ## todo: alternate faster method for integration!
      setcolsnull(lex, delete = c(dist))
    }
    
    rm(first_in_surv.int, last_in_surv.int, only_in_surv.int)
    if (verbose) cat("Time taken by computation of Pohar-Perme weights: ", timetaken(ppTime), "\n")
  }
  
  
  lex[, pp := 1/(g(tmpPCSM, e = .SD))]
  
  invisible(lex[])
}

comp_pp_weighted_figures <- function(lex, pp = "pp") {
  
  checkLexisData(lex, check.breaks = TRUE)
  if (!is.data.table(lex)) stop("lex must be a data.table")
  
  l <- list()
  l$pp <- pp
  
}



