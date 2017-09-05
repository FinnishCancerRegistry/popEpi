



prevtab <- function(
  formula, 
  data, 
  meanpop = NULL, 
  breaks = NULL, 
  adjust = NULL, 
  weights = NULL, 
  subset = NULL, 
  verbose = FALSE
) {
  
  PF <- parent.frame(1L)
  TF <- environment()
  
  checkLexisData(data)
  checkPophaz(data, meanpop, haz.name = "meanpop")
  meanpop <- data.table(meanpop)
  
  allScales <- attr(data, "time.scales")
  oldBreaks <- attr(data, "breaks")
  
  lexis_vars <- c(allScales, "lex.dur", "lex.id", "lex.Cst", "lex.Xst")
  lexis_vars <- intersect(names(data), lexis_vars)
  
  meanpop_vars <- setdiff(names(meanpop), "meanpop")
  
  print_vars <- print_vars_tmp <- NULL
  adjust_vars <- adjust_vars_tmp <- NULL
  
  all_vars <- unique(intersect(names(data), c(lexis_vars, print_vars, adjust_vars, meanpop_vars)))
  
  ## appease R CMD CHECK -------------------------------------------------------
  at.risk <- NULL
  
  
  ## subsetting ----------------------------------------------------------------
  sb <- substitute(subset)
  subset <- evalLogicalSubset(data, substiset = sb, enclos = PF)
  
  ## data with only time scales and by variables -------------------------------
  
  data = data[subset, ]
  by_data <- usePopFormula(formula, Surv.response = FALSE, 
                           data = data, enclos = PF)
  x <- setDT(mget(lexis_vars, as.environment(data)))
  if (!is.null(by_data[["print"]])) {
    print_vars <- names(by_data[["print"]])
    print_vars_tmp <- makeTempVarName(names = all_vars, pre = print_vars)
    set(x, j = print_vars_tmp, value = by_data[["print"]])
  }
  if (!is.null(by_data[["adjust"]])) {
    adjust_vars <- names(by_data[["adjust"]])
    adjust_vars_tmp <- makeTempVarName(names = all_vars, pre = adjust_vars)
    set(x, j = adjust_vars_tmp, value = by_data[["adjust"]])
  }
  forceLexisDT(x, breaks = oldBreaks, allScales = allScales)
  rm("by_data")
  x[, c("lex.Cst", "lex.Xst") := 0L]
  
  by_vars <- c(print_vars, adjust_vars)
  by_vars_tmp <- c(print_vars_tmp, adjust_vars_tmp)
  
  meanpop_vars_tmp <- makeTempVarName(
    names = c(names(x), names(data), names(meanpop)), pre = meanpop_vars
  )
  meanpop_vars_tmp <- unlist(lapply(seq_along(meanpop_vars), function(i) {
    if (!meanpop_vars[i] %in% by_vars) {
      return(meanpop_vars_tmp[i])
    }
    wh <- which(by_vars == meanpop_vars[i])
    by_vars_tmp[wh]
  }))
  
  lapply(seq_along(meanpop_vars), function(i) {
    set(x, j = meanpop_vars_tmp[i], value = data[[meanpop_vars[i]]])
  })
  
  ## Splitting to ensure breaks exist; also takes copy -------------------------
  x <- splitMulti(x, breaks = breaks, drop = TRUE, merge = TRUE)
  forceLexisDT(x, breaks = attr(x, "breaks"), allScales = allScales)
  
  newBreaks <- copy(attr(x, "breaks"))
  
  
  ## detect prevalence time scale ----------------------------------------------
  prevScale <- detectSurvivalTimeScale(data, eval(formula[[2]], envir = data))
  
  
  ## limit to prevalence time points -------------------------------------------
  ## since we want prevalence at certain points of time along the prevalence
  ## time scale, prevScale values not at the breaks are not considered at all.
  j <- list(newBreaks[[prevScale]])
  names(j) <- prevScale
  x <- x[j, on = prevScale, nomatch = 0L]
  
  ## aggregate -----------------------------------------------------------------
  aggre_vars <-  unique(c(print_vars_tmp, adjust_vars_tmp, meanpop_vars_tmp))
  print(aggre_vars)
  ag <- aggre(x, by = aggre_vars)
  ag <- setDT(ag)
  
  setkeyv(
    ag, c(aggre_vars, setdiff(aggre_vars, intersect(lexis_vars, print_vars_tmp)))
  )
  ag[, "n" := cumsum(at.risk), by = eval(aggre_vars)]
  ag <- setDT(mget(c(aggre_vars, meanpop_vars_tmp, "n"), as.environment(ag)))
  
  ## compute prevalence rates if appropriate -----------------------------------
  print(ag)
  ag <- merge(ag, meanpop, by = meanpop_vars_tmp)
  
  if (length(c(print_vars_tmp, adjust_vars_tmp))) {
    setnames(ag, c(print_vars_tmp, adjust_vars_tmp), 
             c(print_vars, adjust_vars_tmp))
  }
  ag <- rate(data = ag, obs = "n", pyrs = "meanpop", print = print_vars,
             adjust = adjust_vars, weights = weights)
  
  return(ag)
}




prevtab_ag <- function(
  formula, 
  data, 
  meanpop = NULL, 
  adjust = NULL, 
  weights = NULL, 
  subset = NULL, 
  verbose = FALSE
) {
  ## prevtab(per ~ sex + fot)
  
  
}
