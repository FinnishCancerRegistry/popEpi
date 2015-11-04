#' @title Split case-level observations
#' @author Joonas Miettinen
#' @description Split a \code{Lexis} object along one time scale
#' (as \code{\link[Epi]{splitLexis}}) with speed
#' @param lex a Lexis object, splitted or not
#' @param breaks a vector of \code{[a,b)} breaks to split \code{data} by
#' @param timeScale a character string; name of the time scale to split by
#' @param merge logical; if \code{TRUE}, retains all variables 
#' from the original data - i.e. original variables are
#' repeated for all the rows by original subject
#' @param drop logical; if \code{TRUE}, drops all resulting rows 
#' after expansion that reside outside the time window
#' defined by the given breaks
#' 
#' 
#' @details 
#' 
#' \code{splitLexisDT} is in essence a \pkg{data.table} version of
#' \code{splitLexis} or \code{survSplit} for splitting along a single
#' time scale. It requires a Lexis object as input, which may have already
#' been split along some time scale.
#' 
#' Unlike \code{splitLexis}, \code{splitLexisDT} drops observed time outside
#' the roof and floor of \code{breaks} by default - with \code{drop = FALSE}
#' the functions have identical behaviour.
#'  
#' @return
#' A \code{data.table} or \code{data.frame} 
#' (depending on \code{options("popEpi.datatable")}; see \code{?popEpi}) 
#' object expanded to accommodate split observations.
#' 
#' @export splitLexisDT
#' @examples
#' library(Epi)
#' x <- Lexis(data=sire[1000:1100], 
#'            entry = list(fot=0, per=get.yrs(dg_date), age=dg_age), 
#'            exit=list(per=get.yrs(ex_date)), exit.status=status)
#' BL <- list(fot=seq(0, 5, by = 3/12), per=c(2008, 2013))
#' x2 <- splitMulti(x, breaks = BL, drop = FALSE)
#' x3 <- splitLexisDT(x, breaks = BL$fot, timeScale = "fot", drop = FALSE)
#' x3 <- splitLexisDT(x3, breaks = BL$per, timeScale = "per", drop = FALSE)
#' x4 <- splitLexis(x,  breaks = BL$fot, time.scale = "fot")
#' x4 <- splitLexis(x4, breaks = BL$per, time.scale = "per")
#' ## all produce identical results
splitLexisDT <- function(lex, breaks, timeScale, merge = TRUE, drop = TRUE) {
  if (!inherits(lex, "Lexis")) stop("'lex' needs to be a Lexis object")
  
  allScales <- attr(lex, "time.scales")
  allBreaks <- attr(lex, "breaks")
  
  if (!timeScale %in% allScales) stop("invalid timeScale")
  
  breaks <- setdiff(breaks, allBreaks[[timeScale]]) ## remove any existing breaks already split by
  breaks <- matchBreakTypes(lex, breaks, timeScale, modify.lex = TRUE)
  breaks <- sort(breaks)
  if (!drop)  breaks <- protectFromDrop(breaks)
  
  BL <- list(breaks)
  setattr(BL, "names", timeScale)
  checkBreaksList(x = lex, breaks = BL)
  
  N_expand <- length(breaks)
  N_subjects <- nrow(lex)
  
  lexVars <- c("lex.id", "lex.multi", allScales, "lex.dur", "lex.Cst", "lex.Xst")
  lexVars <- intersect(lexVars, names(lex))
  othVars <- setdiff(names(lex), lexVars)
  
  ## quick data expansion ------------------------------------------------------
  
  l <- vector(mode = "list", length = N_expand)
  l[[1]] <- data.table(lex)
  
  ## ALTERNATE METHOD
  ## this would crop time scale values to obey breaks limits
  ## and drop accordingly to save memory. however, 
  ## time scale value determination relies on each subject from lex to reside in l.
  ## might work if original lex data is subset as well. questionable if more efficient.
  #   if (drop) {
  #     intelliCrop(l[[1]], breaks = BL, allScales = allScales)
  #     l[[1]] <- intelliDrop(l[[1]], breaks = BL, dropNegDur = TRUE)
  #   }
  
  if (!merge) l[[1]][, (othVars) := NULL]
  
  tmpID <- makeTempVarName(data = l[[1]], pre = "TEMP_SPLITTING_ID")
  tmpIE <- makeTempVarName(data = l[[1]], pre = "TEMP_SPLIT_INT_END")
  
  set(l[[1]], j = tmpID, value = 1:nrow(l[[1]]))
  for (k in 2:(N_expand)) {
    l[[k]] <- l[[1]]
  }
  l <- rbindlist(l)
  
  ## time scale value determination --------------------------------------------
  set(l, j = tmpIE,  value = c(rep(breaks, each = N_subjects)) )
  set(l, j = tmpIE,  value = pmin(l[[tmpIE]], l[[timeScale]] + l$lex.dur) )
  set(l, j = timeScale, value = c(lex[[timeScale]], pmax(lex[[timeScale]], rep(breaks[-length(breaks)], each = N_subjects))) )
  
  
  ## status determination ------------------------------------------------------
  ## note: lex.dur still the original value (maximum possible)
  harmonizeStatuses(x = l, C = "lex.Cst", X = "lex.Xst")
  not_event <- l[lex[[timeScale]]+lex.dur != get(tmpIE), which = TRUE]
  l[not_event, lex.Xst := lex.Cst]
  
  set(l, j = "lex.dur", value = l[[tmpIE]] - l[[timeScale]] )
  
  
  ## other time scale values ---------------------------------------------------
  otherScales <- setdiff(allScales, timeScale)
  if (length(otherScales) > 0) {
    for (k in otherScales) {
      set(l, j = k, value = lex[[k]] + l[[tmpIE]] - lex[[timeScale]] - l$lex.dur)
    }
  }
  
  ## dropping ------------------------------------------------------------------
  
  l <- l[lex.dur > 0]
  if (drop) l <- intelliDrop(x = l, breaks = BL, dropNegDur = FALSE)
  
  
  setkeyv(l, c(tmpID, timeScale))
  #   l <- unique(l) ## unsure if needed. probably highly unlikely?
  set(l, j = c(tmpIE, tmpID), value = NULL)
  
  
  ## final touch & attributes --------------------------------------------------
  
  setcolorder(l, neworder = intersect(c(lexVars, othVars), names(l)))
  
  if (!drop) breaks <- breaks[-c(length(breaks))] ## don't keep e.g. -Inf or Inf that were added by protectFromDrop
  allBreaks[[timeScale]] <- breaks
  setattr(l, "breaks", allBreaks)
  setattr(l, "time.scales", allScales)
  setattr(l, "time.since", rep("", times = length(allScales)))
  setattr(l, "class", c("Lexis","data.table","data.frame"))
  if (!getOption("popEpi.datatable")) setDFpe(l)
  
  l[]
}

