# library(survival)
# dt[, fot := as.integer(ex_date-dg_date)/365.24]
# s <- dt[, Surv(time = rep(0, nrow(dt)), time2 = fot, event = status %in% 1:2)]

# library(Epi)
# library(popEpi)
# dt <- copy(sire)[dg_date < ex_date,]
# dt[, agegr := cut(dg_age, c(0,50,75,Inf))]
# dt <- Lexis(data = dt, entry = list(FUT = 0, AGE = dg_age, CAL = get.yrs(dg_date)),
#             exit = list(CAL = get.yrs(ex_date)), entry.status = 0L, exit.status = status, merge = TRUE)

survtab_lex <- function(data, print, adjust, breaks, pophaz, weights, ...) {
  
  if (!inherits(data, "Lexis")) stop("data is not a Lexis object")
  allScales <- attr(data, "time.scales")
  splitScales <- names(breaks)
  
  x <- splitMulti(data, breaks = breaks, drop = TRUE, merge = TRUE)
  print <- evalPopArg(x, substitute(print), DT = TRUE)
  adjust <- evalPopArg(x, substitute(adjust), DT = TRUE)
  setcolsnull(x, keep = c("lex.id", "lex.dur", allScales, "lex.Cst", "lex.Xst", setdiff(names(pophaz), "haz")))
  x[, names(print) := print]
  x[, names(adjust) := adjust]
  print <- names(print)
  adjust <- names(adjust)
  
  x <- cutLowMerge(x, pophaz, by = setdiff(names(pophaz), "haz"), 
                   mid.scales = intersect(names(pophaz), allScales))
  setattr(x, "breaks", breaks)
  setattr(x, "class", c("Lexis", "data.table", "data.frame"))
  
  
  av <- c(print, adjust, names(breaks)[1])
  ## still need to compute pp-weighted figures below. they all have to be done
  ## on the level of the splitted observations!
  # c("d.pp", "d.exp.pp", "d.pp.2",if (surv.method == "hazard") "pyrs.pp" else "n.eff.pp") else NULL)
  x <- laggre(x, aggre = c(print, adjust, names(breaks)[1]), verbose = FALSE,
              expr = list(d.exp = sum(haz*lex.dur)))
  
  
  st <- survtab_ag(x, surv.scale = names(breaks)[1L], adjust = adjust,
                   print = print, weights = weights, surv.type = "surv.rel",
                   d.exp = d.exp)
  
}
# pm <- copy(popEpi::popmort)
# setnames(pm, c("agegroup", "year"), c("AGE", "CAL"))
# st <- survtab_lex(dt, print = "sex", adjust = "agegr", 
#                   pophaz = pm,
#                   weights = c(0.2,0.4,0.4),
#                   breaks = list(FUT = seq(0,5,1/12)))











