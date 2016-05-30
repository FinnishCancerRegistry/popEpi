



prevtab <- function(formula, data, popcounts = NULL, breaks = NULL, adjust = NULL, weights = NULL, subset = NULL, verbose = FALSE) {
  # x <- lexpand(sire, birth = bi_date, entry = dg_date, exit = ex_date,
  #              status = status %in% 1:2)
  # prevtab(per ~ sex + fot, data = x, popcounts = meanpop,
  #         breaks = list(per = 2009:2014, fot = c(0,1,5,10,Inf)),
  #         adjust = "agegroup", weights = "internal")
  # prevtab(per ~ sex + work, data = x, popcounts = meanpop,
  #         breaks = list(per = 2009:2014, work = c(0,1,5,10,Inf)),
  #         adjust = "agegroup", weights = "internal")
  
  PF <- parent.frame(1L)
  TF <- environment()
  
  checkLexisData(data)
  checkPophaz(data, popcounts, haz.name = "count")
  
  allScales <- attr(data, "time.scales")
  oldBreaks <- attr(data, "breaks")
  
  lexVars <- c(allScales, "lex.dur", "lex.id", "lex.Cst", "lex.Xst")
  lexVars <- intersect(names(data), lexVars)
  
  popVars <- setdiff(names(popcounts), "counts")
  
  prVars <- NULL
  adVars <- NULL
  
  allVars <- unique(intersect(names(data), c(lexVars, prVars, adVars, popVars)))
  
  
  ## subsetting ----------------------------------------------------------------
  sb <- substitute(subset)
  subset <- evalLogicalSubset(data, substiset = sb, enclos = PF)
  
  
  ## Splitting to ensure breaks exist; also takes copy -------------------------
  x <- setDT(mget(allVars, envir = as.environment(data)))
  x <- x[subset, ]
  forceLexisDT(x, breaks = oldBreaks, allScales = allScales)
  x <- splitMulti(data, breaks = breaks, drop = TRUE, merge = TRUE)
  
  newBreaks <- attr(x, "breaks")
  
  
  ## detect prevalence time scale ----------------------------------------------
  prevScale <- detectSurvivalTimeScale(data, eval(formula[[2]], envir = data))
  
  
  ## limit to prevalence time points -------------------------------------------
  ## since we want prevalence at certain points of time along the prevalence
  ## time scale, prevScale values not at the breaks are not considered at all.
  j <- list(newBreaks[[prevScale]])
  names(j) <- prevScale
  x <- x[j, on = prevScale]
  
  
  ## aggregate -----------------------------------------------------------------
  agVars <-  unique(c(prVars, adVars, popVars))
  ag <- aggre(x, by = agVars)
  ag <- data.table(ag)
  
  
  ## handle time scales other than prevScale -----------------------------------
  prLexVars <- intersect(lexVars, prVars)
  setkeyv(ag, c(agVars, setdiff(agVars, prLexVars)))
  
  
  ## compute prevalence rates if appropriate -----------------------------------
  ag <- merge(ag, popcounts, by = popVars)
  ag <- rate(data = ag, obs = "at.risk", pyrs = "count", print = prVars,
             adjust = adVars, weights = weights)
  
  return(ag)
}




prevtab_ag <- function(formula, data, pop.data = NULL, adjust = NULL, weights = NULL, subset = NULL, verbose = FALSE) {
  ## prevtab(per ~ sex + fot)
  
  
}
