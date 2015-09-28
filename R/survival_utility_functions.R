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
  
  tab
}
