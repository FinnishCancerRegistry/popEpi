
lexpand2 <- function(data, 
                     birth = NULL, 
                     entry = NULL, 
                     exit = NULL, 
                     status = NULL, 
                     entry.status = NULL, 
                     id = NULL,
                     breaks = NULL, 
                     drop = TRUE,
                     pophaz = NULL,
                     pp = TRUE,
                     subset = NULL,
                     merge = TRUE,
                     aggre = NULL,
                     aggre.type = "cartesian",
                     verbose = FALSE,
                     ...) {
  # @param data a data set
  # @param merge passed on to \code{\link{Lexis_fpa}}
  # @param aggre passed on to \code{\link{aggre}}
  # @param aggre.type passed on to \code{\link{aggre}}
  # @param drop passed on to \code{\link{splitMulti}}
  # @param breaks passed on to \code{\link{splitMulti}}
  # @examples
  # 
  # lex <- lexpand2(sire,
  #                 birth = "bi_date",
  #                 entry = dg_date,
  #                 exit = ex_date + 1L,
  #                 status = "status")
  # 

  TF <- environment()
  PF <- parent.frame(1L)
  
  ## various checks ------------------------------------------------------------
  
  allScales <- c("fot", "per", "age")
  reserved <- c(allScales, paste0("lex.", c("dur", "Xst", "Cst", "id")))
  if (!is.null(pophaz)) {
    reserved <- c(reserved, "d.exp", "pop.haz")
    if (pp) reserved <- c(reserved, "pp")
  }
  
  
  subs <- substitute(subset)
  subset <- evalLogicalSubset(data, subs, enclos = PF)
  
  stopifnot(is.logical(verbose))
  stopifnot(is.logical(drop))
  stopifnot(is.logical(merge))
  stopifnot(is.logical(pp))
  
  ## create Lexis object -------------------------------------------------------
  
  lexCols <- c("birth", "entry", "exit", "status", "entry.status", "id")
  al <- lapply(lexCols, function(stri) {
    
    e <- paste0("substitute(", stri, ", env = TF)")
    e <- eval(parse(text = e), envir = TF)
    
  })
  names(al) <- lexCols
  
  
  al <- lapply(al, function(expr) {
    
    evalPopArg(data = data, arg = expr, DT = TRUE, enclos = PF, 
               types = c("NULL", "character", "expression"))
    
  })
  
  
  al[sapply(names(al), function(stri) {
    stri %in% c("id", "entry.status") && is.null(al[[stri]])
    })] <- NULL
  
  al <- lapply(al, function(elem) {
    if (is.data.frame(elem)) return(elem[[1]])
    elem
  })
  
  al <- c(al, list(data = data, subset = subset))
  names(al)[names(al) == "status"] <- "exit.status"
  
  al$merge <- merge
  
  x <- do.call(Lexis_fpa, args = al)
  
  setDT(x)
  setattr(x, "class", c("Lexis", "data.table", "data.frame"))
  
  
  ## more checks ---------------------------------------------------------------
  if (!is.null(breaks)) checkBreaksList(x, breaks = breaks)
  pophaz <- data.table(pophaz)
  alt_phna <- c("year" = "per", "agegroup" = "age")
  lapply(names(alt_phna), function(stri) {
    if (stri %in% names(pophaz)) setnames(pophaz, stri, alt_phna[stri])
  })
  if (!is.null(pophaz)) checkPophaz(x, pophaz)
  
  
  ## splitting if needed -------------------------------------------------------
  if (!is.null(breaks)) {
    x <- splitMulti(x, breaks = breaks, drop = drop)
    breaks <- attr(x, "breaks")
  }
  
  ## merge in pophaz if needed -------------------------------------------------
  if (!is.null(pophaz)) {
    setnames(pophaz, "haz", "pop.haz")
    
    phScales <- intersect(allScales, names(pophaz))
    if (!length(phScales)) phScales <- NULL
    x <- cutLowMerge(x, pophaz, by = intersect(names(x), names(pophaz)), 
                     all.x = TRUE, all.y = FALSE, mid.scales = phScales, 
                     old.nums = TRUE)
    
    
    if (pp) {
      x <- comp_pp_weights(x, surv.scale = "fot", 
                           breaks = breaks,
                           haz = "pop.haz")
      pp_cols <- comp_pp_weighted_figures(x, haz = "pop.haz", 
                                          pp = "pp", 
                                          by = "lex.id")
    }
    
  }
  
  ## aggregate if needed -------------------------------------------------------
  ags <- substitute(aggre)
  ags_test <- evalRecursive(ags, TF, baseenv())
  if (!is.null(ags_test$arg)) x <- aggre(x, by = ags)
  
  x
}
