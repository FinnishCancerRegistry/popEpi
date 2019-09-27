#' @title Split case-level observations
#' @author Joonas Miettinen
#' @description Split a \code{Lexis} object along multiple time scales
#' with speed and ease
#' @param data a Lexis object with event cases as rows
#' @param breaks a list of named numeric vectors of breaks; see Details and Examples
#' @param ... alternate way of supplying breaks as named vectors;
#' e.g. \code{fot = 0:5} instead of \code{breaks = list(fot = 0:5)};
#' if \code{breaks} is not \code{NULL}, \code{breaks} is used and any breaks
#' passed through \code{...} are NOT used; note also that due to partial 
#' matching of argument names in R, 
#' if you supply e.g. \code{dat = my_breaks} and you 
#' do not pass argument \code{data} explicitly (\code{data = my_data}), then R
#' interprets this as \code{data = my_breaks} --- so choose the names of your
#' time scales wisely
#' @param drop logical; if \code{TRUE}, drops all resulting rows 
#' after expansion that reside outside the time window
#' defined by the given breaks
#' @param merge logical; if \code{TRUE}, retains all variables 
#' from the original data - i.e. original variables are
#' repeated for all the rows by original subject
#' @param verbose logical; if \code{TRUE}, the function is chatty 
#' and returns some messages along the way
#' 
#' 
#' @details 
#' 
#' \code{splitMulti} is in essence a \pkg{data.table} version of
#'  \code{splitLexis} or \code{survSplit} for splitting along multiple
#'  time scales.
#' It requires a Lexis object as input.
#' 
#' The \code{breaks} must be a list of named vectors of the appropriate type. 
#' The breaks are fully explicit and
#' left-inclusive and right exclusive, e.g. \code{fot=c(0,5)} 
#' forces the data to only include time between
#' \code{[0,5)} for each original row (unless \code{drop = FALSE}). 
#' Use \code{Inf} or \code{-Inf} for open-ended intervals,
#'  e.g. \code{per=c(1990,1995,Inf)} creates the intervals 
#'  \code{[1990,1995), [1995, Inf)}.
#'  
#' Instead of specifying \code{breaks}, one may make use of the \code{...}
#' argument to pass breaks: e.g. 
#' 
#' \code{splitMulti(x, breaks = list(fot = 0:5))} 
#' 
#' is equivalent to
#' 
#' \code{splitMulti(x, fot = 0:5)}.
#' 
#' Multiple breaks can be supplied in the same manner. However, if both
#' \code{breaks} and \code{...} are used, only the breaks in \code{breaks}
#' are utilized within the function. 
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
#' @examples
#' #### let's prepare data for computing period method survivals
#' #### in case there are problems with dates, we first 
#' #### convert to fractional years.
#' \dontrun{
#' library("Epi")
#' library("data.table")
#' data("sire", package = "popEpi")
#' x <- Lexis(data=sire[dg_date < ex_date, ], 
#'            entry = list(fot=0, per=get.yrs(dg_date), age=dg_age), 
#'            exit=list(per=get.yrs(ex_date)), exit.status=status)
#' x2 <- splitMulti(x, breaks = list(fot=seq(0, 5, by = 3/12), per=c(2008, 2013)))
#' # equivalently:
#' x2 <- splitMulti(x, fot=seq(0, 5, by = 3/12), per=c(2008, 2013))
#' 
#' ## using dates; note: breaks must be expressed as dates or days!
#' x <- Lexis(data=sire[dg_date < ex_date, ], 
#'            entry = list(fot=0, per=dg_date, age=dg_date-bi_date), 
#'            exit=list(per=ex_date), exit.status=status)
#' BL <- list(fot = seq(0, 5, by = 3/12)*365.242199,
#'            per = as.Date(paste0(c(1980:2014),"-01-01")),
#'            age = c(0,45,85,Inf)*365.242199)
#' x2 <- splitMulti(x, breaks = BL, verbose=TRUE)
#' 
#' 
#' ## multistate example (healty - sick - dead)
#' sire2 <- data.frame(sire)
#' sire2 <- sire2[sire2$dg_date < sire2$ex_date, ]
#' 
#' set.seed(1L) 
#' not_sick <- sample.int(nrow(sire2), 6000L, replace = FALSE)
#' sire2$dg_date[not_sick] <- NA
#' sire2$status[!is.na(sire2$dg_date) & sire2$status == 0] <- -1
#' 
#' sire2$status[sire2$status==2] <- 1
#' sire2$status <- factor(sire2$status, levels = c(0, -1, 1), 
#'                        labels = c("healthy", "sick", "dead"))
#'  
#' xm <- Lexis(data = sire2, 
#'             entry = list(fot=0, per=get.yrs(bi_date), age=0), 
#'             exit = list(per=get.yrs(ex_date)), exit.status=status)
#' xm2 <- cutLexis(xm, cut = get.yrs(xm$dg_date), 
#'                 timescale = "per", 
#'                 new.state = "sick")
#' xm2[xm2$lex.id == 6L, ]
#' 
#' xm2 <- splitMulti(xm2, breaks = list(fot = seq(0,150,25)))
#' xm2[xm2$lex.id == 6L, ]
#' }
#' 
#' @import data.table 
#' @import Epi
#' 
#' @export
#' @family splitting functions
#' @seealso
#' \code{\link[Epi]{splitLexis}}, \code{\link[Epi]{Lexis}},  
#' \code{\link[survival]{survSplit}}
#' 
splitMulti <- function(data,
                       breaks = NULL,
                       ...,
                       drop=TRUE,
                       merge=TRUE,
                       verbose=FALSE) {
  
  lex.id <- lex.dur <- NULL ## APPEASE R CMD CHECK
  
  ## basic checks --------------------------------------------------------------
  if (verbose) {stime <- proc.time()}
  
  breaks <- splitMultiPreCheck(data = data, breaks = breaks, ...)
  
  ## collect necessary data ----------------------------------------------------
  attr_list <- copy(attributes(data)[c("time.scales", "breaks", "time.since")])
  allScales <- attr_list$time.scales
  splitScales <- names(breaks)
  
  keep_nms <- if (merge) names(data) else {
    intersect(
      names(data), 
      c("lex.id", "lex.Cst", "lex.Xst", allScales)
    )
  }
  # this is not a copy!
  dt <- mget_cols(keep_nms, data = data)
  forceLexisDT(dt, breaks = attr(data, "breaks"), allScales = allScales,
               key = FALSE)
  
  ## check if even need to do splitting ----------------------------------------
  
  oldBreaks <- copy(attr(data, "breaks"))
  tryCatch(checkBreaksList(data, oldBreaks), error = function(e) {
    stop("Error in splitMulti: \n",
         "Old breaks existing in Lexis data did not pass testing. Error ",
         "message from test: \n", e, call. = FALSE)
  })
  
  ## only do split if all breaks are NOT in the breaks that the data
  ## has already been split by.
  do_split <- TRUE
  do_split <- !all_breaks_in(breaks, oldBreaks, x = data)
  
  if (!do_split) {
    l <- setDT(copy(dt))
    setkeyv(l, c("lex.id", allScales[1]))
  } else {
    
    ## temp IDS ----------------------------------------------------------------
    # used to ensure correct splitting and lex status rolling
    
    id_dt <- data.table(
      orig_id_values = dt$lex.id, 
      temp_id_values = 1:nrow(dt), 
      key = "temp_id_values"
    )
    
    on.exit(set(dt, j = "lex.id", value = id_dt[["orig_id_values"]]))
    set(dt, j = "lex.id", value = id_dt[["temp_id_values"]])
    
    l <- vector(mode = "list", length = length(splitScales))
    setattr(l, "names", splitScales)
    for (v in splitScales) {
      l[[v]] <- splitLexisDT(dt, breaks = breaks[[v]], 
                             merge = merge, drop = FALSE, timeScale = v)
      breaks[[v]] <- attr(l[[v]], "breaks")[[v]]
    }
    l <- rbindlist(l)
    
    s1 <- allScales[1]
    setkeyv(l, c("lex.id", s1))
    
    if (length(splitScales) > 1L) {
      ## roll time scale values, re-compute interval lengths (lex.dur) ---------
      
      tmp_ie <- makeTempVarName(names = names(l), pre = "TEMP_INT_END_")
      l[, (tmp_ie) := shift(.SD, n = 1, type = "lead"), 
        .SDcols = s1, by = "lex.id"]
      is_last_row <- is.na(l[[tmp_ie]])
      
      l[is_last_row, (tmp_ie) := lex.dur + .SD, .SDcols = s1]
      
      set(l, j = "lex.dur", value = l[[tmp_ie]] - l[[s1]])
      set(l, j = tmp_ie, value = NULL)
    }
    
    has_zero_dur <- l[["lex.dur"]] < .Machine$double.eps^0.5
    if (any(has_zero_dur)) {
      l <- l[!has_zero_dur, ]
    }
    
    ## ensure statuses are as expected -----------------------------------------
    
    
    setkeyv(l, c("lex.id", s1))
    roll_lexis_status_inplace(
      unsplit.data = dt, split.data = l, id.var = "lex.id"
    )
    
    ## dt$lex.id from temporary values to original values ----------------------
    # merge in correct IDs also to split data
    on.exit()
    set(dt, j = "lex.id", value = id_dt$lex.id)
    
    
    tmpID <- makeTempVarName(names = names(l), pre = "TEMP_SPLITMULTI_ID_")
    setnames(l, old = "lex.id", new = tmpID)
    set(l, j = "lex.id", value = {id_dt[
      i = .(l[[tmpID]]), 
      j = .SD, 
      on = "temp_id_values",
      .SDcols = "orig_id_values"
      ]})
    set(l, j = tmpID, value = NULL)
    rm("id_dt")
    
  }
  
  if (drop) l <- intelliDrop(l, breaks = breaks, dropNegDur = FALSE)
  
  if (nrow(l) == 0) {
    warning("no data left after dropping; check breaks?")
  }
  
  order <- c("lex.id", "lex.multi", allScales, "lex.dur", "lex.Cst", "lex.Xst")
  order <- c(order, setdiff(names(l), order))
  order <- intersect(order, names(l))
  setcolorder(l, order)
  
  if (verbose) cat("time taken by splitting process: ", timetaken(stime), "\n")
  
  
  breaks <- lapply(allScales, function(scale_nm) {
    ## allowed to NULL also
    br <- c(breaks[[scale_nm]], oldBreaks[[scale_nm]])
    if (is.null(br)) return(br)
    sort(unique(br))
  }) 
  names(breaks) <- allScales
  
  setattr(l, "time.scales", allScales)
  setattr(l, "time.since", attr_list[["time.since"]])
  setattr(l, "breaks", breaks)
  setattr(l, "class", c("Lexis","data.table","data.frame"))
  if (!return_DT()) setDFpe(l)
  
  l[]
  
}

globalVariables(".")
