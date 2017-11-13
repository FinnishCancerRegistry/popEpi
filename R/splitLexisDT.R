#' @title Split case-level observations
#' @author Joonas Miettinen
#' @description Split a \code{Lexis} object along one time scale
#' (as \code{\link[Epi]{splitLexis}}) with speed
#' @param lex a Lexis object, split or not
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
#' The \code{Lexis} time scale variables can be of any arbitrary 
#' format, e.g. \code{Date},
#' fractional years (see \code{\link[Epi]{cal.yr}}) and \code{\link{get.yrs}},
#' or other. However, using \code{date} variables (from package \pkg{date})
#' are not recommended, as \code{date} variables are always stored as integers,
#' whereas \code{Date} variables (see \code{?as.Date}) are typically stored
#' in double ("numeric") format. This allows for breaking days into fractions
#' as well, when using e.g. hypothetical years of 365.25 days.
#' 
#' @return
#' A \code{data.table} or \code{data.frame} 
#' (depending on \code{options("popEpi.datatable")}; see \code{?popEpi}) 
#' object expanded to accommodate split observations.
#' 
#' @export
#' @family splitting functions
#' @examples
#' library(Epi)
#' data("sire", package = "popEpi")
#' x <- Lexis(data=sire[1000:1100, ], 
#'            entry = list(fot=0, per=get.yrs(dg_date), age=dg_age), 
#'            exit=list(per=get.yrs(ex_date)), exit.status=status)
#' BL <- list(fot=seq(0, 5, by = 3/12), per=c(2008, 2013))
#' 
#' x2 <- splitMulti(x, breaks = BL, drop = FALSE)
#' 
#' x3 <- splitLexisDT(x, breaks = BL$fot, timeScale = "fot", drop = FALSE)
#' x3 <- splitLexisDT(x3, breaks = BL$per, timeScale = "per", drop = FALSE)
#' 
#' x4 <- splitLexis(x,  breaks = BL$fot, time.scale = "fot")
#' x4 <- splitLexis(x4, breaks = BL$per, time.scale = "per")
#' ## all produce identical results
#' 
#' ## using Date variables
#' x <- Lexis(data=sire[1000:1100, ], 
#'            entry = list(fot=0, per=dg_date, age=dg_date-bi_date), 
#'            exit=list(per=ex_date), exit.status=status)
#' BL <- list(fot = 0:5*365.25, per = as.Date(c("2008-01-01", "2013-01-01")))
#' 
#' x2 <- splitMulti(x, breaks = BL, drop = FALSE)
#' 
#' x3 <- splitLexisDT(x, breaks = BL$fot, timeScale = "fot", drop = FALSE)
#' x3 <- splitLexisDT(x3, breaks = BL$per, timeScale = "per", drop = FALSE)
#' 
#' ## splitLexis may not work when using Dates
splitLexisDT <- function(lex, breaks, timeScale, merge = TRUE, drop = TRUE) {
  
  do_split <- TRUE
  
  tol <- .Machine$double.eps^0.5
  checkLexisData(lex, check.breaks = FALSE)
  
  attr_list <- copy(attributes(lex)[c("time.scales", "breaks", "time.since")])
  allScales <- attr_list[["time.scales"]]
  allBreaks <- attr_list[["breaks"]]
  
  if (!timeScale %in% allScales) {
    stop("timeScale '", timeScale,"' not among following existing time scales: ", 
         paste0("'", allScales, "'", collapse = ", "))
  }
  
  ## lexVars: if !merge, will drop all but these (NOTE: checkLexisData
  ## check for existence of these)
  lexVars <- c("lex.id", "lex.multi", allScales, "lex.dur", "lex.Cst", "lex.Xst")
  lexVars <- intersect(lexVars, names(lex))
  othVars <- setdiff(names(lex), lexVars)
  
  ## basic checks on breaks
  if (drop && length(breaks) == 1L) {
    stop("Length of breaks vector is one, but argument 'drop' is TRUE. ",
         "Cannot do dropping with only one break. Either supply at least ",
         "two breaks or set drop = FALSE.")
  }
  if (length(breaks) == 0L) {
    stop("No breaks supplied (length of breaks is zero).")
  }
  
  ## remove any existing breaks already split by;
  ## NOTE: setdiff would break Date format breaks!
  orig_breaks <- copy(breaks)
  if (length(allBreaks[[timeScale]])) {
    ## because any test like (x %in% NULL) results in FALSE.
    breaks <- breaks[!breaks %in% allBreaks[[timeScale]]]
  }
  
  breaks <- matchBreakTypes(lex, breaks, timeScale, modify.lex = FALSE) 
  
  if (length(breaks) == 0L || (length(orig_breaks) == 2L && drop)) {
    ## former means no additional splitting to do. (we still crop & drop
    ## if argument drop = TRUE)
    ## latter means we only need to crop & drop.
    do_split <- FALSE
    breaks <- orig_breaks
  }
  
  breaks <- sort(breaks)
  if (!drop) breaks <- protectFromDrop(breaks)
  
  BL <- list(breaks)
  setattr(BL, "names", timeScale)
  checkBreaksList(x = lex, breaks = BL)
  
  
  ## use subset lex if dropping for efficiency
  orig_lex <- lex
  if (drop) {
    keepVars <- if (merge) NULL else lexVars ## NULL: all vars
    lex <- subsetDTorDF(lex, select = keepVars)
    rm(keepVars)
    lex <- data.table(lex)
    
    setattr(lex, "class", c("Lexis", "data.table", "data.frame"))
    
    lex <- intelliCrop(lex, breaks = BL, allScales = allScales, 
                       cropStatuses = TRUE, tol = tol)
    lex <- intelliDrop(lex, breaks = BL, dropNegDur = TRUE, 
                       check = FALSE, tol = tol)
  }
  
  if (!do_split) {
    
    l <- if (!drop) copy(lex) else lex
    
  } else {
    
    ## currently cannot handle NA values in split time scale; will add them in
    ## the end
    ts_is_na <- is.na(lex[[timeScale]])
    ts_any_na <- any(ts_is_na)
    if (ts_any_na) {
      warning("NA values in the time scale you are splitting along ('", 
              timeScale,"'). Results may deviate from that produced by ",
              "splitLexis from package Epi. For safety you may want to split ",
              "using only the data with no NA values and combine the the split",
              " data with the NA-valued data using rbind.")
      lex_na <- lex[ts_is_na, ]
      lex <- lex[!ts_is_na, ]
    }
    
    
    ## will use this due to step below (and laziness)
    ts_values <- lex[[timeScale]]
    ## Date objects are based on doubles and therefore keep the most information
    if (inherits(ts_values, c("IDate", "date", "dates"))) ts_values <- as.Date(ts_values)
    
    N_expand <- length(breaks)
    N_subjects <- nrow(lex)
    
    ## use tmp id to ensure correct status rolling -----------------------------
    id_dt <- data.table(
      tmp_id_values = 1:nrow(lex),
      orig_id_values = lex[["lex.id"]]
    )
    on.exit(set(lex, j = "lex.id", value = id_dt[["orig_id_values"]]))
    set(lex, j = "lex.id", value = id_dt[["tmp_id_values"]])
    
    ## quick data expansion ------------------------------------------------------
    
    l <- vector(mode = "list", length = N_expand)
    l[[1]] <- data.table(lex)
    
    if (!merge) setcolsnull(l[[1]], keep = lexVars, soft = FALSE)
    
    tmpID <- makeTempVarName(data = l[[1]], pre = "TEMP_SPLITTING_ID")
    tmpIE <- makeTempVarName(data = l[[1]], pre = "TEMP_SPLIT_INT_END")
    
    set(l[[1]], j = tmpID, value = 1:nrow(l[[1]]))
    if (N_expand > 1L) {
      for (k in 2:(N_expand)) {
        l[[k]] <- l[[1]]
      }
    }
    
    l <- rbindlist(l)
    
    ## time scale value determination --------------------------------------------
    set(l, j = tmpIE,  value = rep(breaks, each = N_subjects))
    set(l, j = tmpIE,  value = pmin(l[[tmpIE]], l[[timeScale]] + l$lex.dur) )
    set(l, j = timeScale, value = c(
      ts_values, 
      pmax(ts_values, rep(breaks[-length(breaks)], each = N_subjects))
    ))
    
    set(l, j = "lex.dur", value = l[[tmpIE]] - l[[timeScale]] )
    
    ## other time scale values ---------------------------------------------------
    otherScales <- setdiff(allScales, timeScale)
    if (length(otherScales) > 0) {
      ## change in timeScale
      ts_delta <- l[[timeScale]] - ts_values
      for (k in otherScales) {
        set(l, j = k, value = lex[[k]] + ts_delta)
      }
    }
    
    ## dropping ----------------------------------------------------------------
    ## drops very very small intervals as well as dur <= 0
    has_zero_dur <- l[["lex.dur"]] < tol
    if (any(has_zero_dur)) {
      l <- l[!has_zero_dur]
    }
    
    
    
    ## roll states -------------------------------------------------------------
    # this avoids duplicate deaths, etc., where appropriate.
    setkeyv(l, c("lex.id", timeScale))
    lex_id <- mget_cols(c("lex.Cst", "lex.Xst", "lex.id"), data = lex)
    setattr(lex_id, "time.scales", allScales)
    roll_lexis_status_inplace(
      unsplit.data = lex_id, split.data = l, id.var = "lex.id"
    )
    rm("lex_id")
    
    
    set(l, j = c(tmpIE, tmpID), value = NULL)
    
    ## revert to original IDs --------------------------------------------------
    set(l, j = "lex.id", value = {
      id_dt[
        i = list(tmp_id_values = l$lex.id), 
        j = .SD, 
        on = "tmp_id_values", 
        .SDcols = "orig_id_values"
        ]
    })
    
    if (ts_any_na) {
      l <- rbind(l, lex_na)
      setkeyv(l, c("lex.id", timeScale))
    }
    
  }
  
  ## harmonize statuses --------------------------------------------------------
  harmonizeStatuses(x = l, C = "lex.Cst", X = "lex.Xst")
  
  
  ## ensure time scales and lex.dur have same (ish) class as before ------------
  for (k in c(allScales, "lex.dur")) {
    
    if (inherits(orig_lex[[k]], "difftime") && !inherits(l[[k]], "difftime")){
      setattr(l[[k]], "class", "difftime")
      setattr(l[[k]], "units", attr(orig_lex[[k]], "units"))
    } else if (is.numeric(orig_lex[[k]]) && inherits(l[[k]], "difftime")) {
      set(l, j = k, value = as.numeric(l[[k]]))
    }
    
  }
  
  ## harmonize time scales -----------------------------------------------------
  ## numeric time scales are forced to the lowest common denominator:
  ## difftime -> integer -> double (though difftime is not numeric class)
  harmonizeNumericTimeScales(l, times = c(allScales, "lex.dur"))
  
  
  ## final touch & attributes --------------------------------------------------
  setcolorder(l, neworder = intersect(c(lexVars, othVars), names(l))) #merge=T/F
  if (!drop) breaks <- unprotectFromDrop(breaks)
  allBreaks[[timeScale]] <- sort(unique(c(allBreaks[[timeScale]], breaks)))
  
  allBreaks <- lapply(allScales, function(scale_nm) {
    allBreaks[[scale_nm]] ## intentionally NULL if not there
  }) 
  names(allBreaks) <- allScales
  
  setattr(l, "breaks", allBreaks)
  setattr(l, "time.scales", allScales)
  setattr(l, "time.since", attr_list[["time.since"]])
  setattr(l, "class", c("Lexis","data.table","data.frame"))
  if (!return_DT()) setDFpe(l)
  
  l[]
}

