
#' @title Split case-level observations
#' @author Joonas Miettinen
#' @description Given subject-level data, data is split
#' by calendar time (`per`), `age`, and follow-up
#' time (`fot`, from 0 to the end of follow-up)
#' into subject-time-interval rows according to
#' given `breaks` and additionally processed if requested.
#' @param data dataset of e.g. cancer cases as rows
#' @param birth birth time in date format
#' or fractional years; string, symbol or expression
#' @param entry entry time in date format
#' or fractional years; string, symbol or expression
#' @param exit exit from follow-up time in date
#' format or fractional years; string, symbol or expression
#' @param event advanced: time of possible event differing from `exit`;
#' typically only used in certain SIR/SMR calculations - see Details;
#' string, symbol or expression
#' @param status variable indicating type of event at `exit` or `event`;
#' e.g. `status = status != 0`; expression or quoted variable name
#' @param entry.status input in the same way as `status`;
#' status at `entry`; see Details
#' @param id optional; an id variable; e.g. `id = my_id`;
#' string, symbol or expression
#' @param overlapping advanced, logical; if `FALSE` AND if `data` contains
#' multiple rows per subject,
#' ensures that the timelines of `id`-specific rows do not overlap;
#' this ensures e.g. that person-years are only computed once per subject
#' in a multi-state paradigm
#' @param aggre e.g. `aggre = list(sex, fot)`;
#' a list of unquoted variables and/or expressions thereof,
#' which are interpreted as factors; data events and person-years will
#' be aggregated by the unique combinations of these; see Details
#' @param aggre.type one of `c("unique","cartesian")`;
#' can be abbreviated; see Details
#' @param breaks a named list of vectors of time breaks;
#' e.g. `breaks = list(fot=0:5, age=c(0,45,65,Inf))`; see Details
#' @param drop logical; if `TRUE`, drops all resulting rows
#' after splitting that reside outside
#' the time window as defined by the given breaks (all time scales)
#' @param pophaz a dataset of population hazards to merge
#'  with split data; see Details
#' @param pp logical; if `TRUE`, computes Pohar-Perme weights using
#' `pophaz`; adds variable with reserved name `pp`;
#' see Details for computing method
#' @param subset a logical vector or any logical condition; data is subsetted
#' before splitting accordingly
#' @param merge logical; if `TRUE`, retains all
#' original variables from the data
#' @param verbose logical; if `TRUE`, the function is chatty and
#' returns some messages along the way
#' @param ... e.g. `fot = 0:5`; instead of specifying a `breaks` list,
#' correctly named breaks vectors can be given
#' for `fot`, `age`, and `per`; these override any breaks in the
#' `breaks` list; see Examples
#'
#'
#'
#' @details
#' **Basics**
#'
#' `[lexpand]` splits a given data set (with e.g. cancer diagnoses
#' as rows) to subintervals of time over
#' calendar time, age, and follow-up time with given time breaks
#' using `[splitMulti]`.
#'
#' The dataset must contain appropriate
#' `Date` / `IDate` format or
#' other numeric variables that can be used
#' as the time variables.
#'
#' You may take a look at a simulated cohort
#' `[sire]` as an example of the
#' minimum required information for processing data with `lexpand`.
#'
#' Many arguments can be supplied as a character string naming the appropriate
#' variable (e.g. `"sex"`), as a symbol (e.g. `sex`) or as an expression
#' (e.g. `factor(sex, 0:1, c("m", "f"))`) for flexibility.
#'
#' **Breaks**
#'
#' You should define all breaks as left inclusive and right exclusive
#' time points (e.g.`[a,b)` )
#' for 1-3 time dimensions so that the last member of a breaks vector
#' is a meaningful "final upper limit",
#'  e.g. `per = c(2002,2007,2012)`
#' to create a last subinterval of the form `[2007,2012)`.
#'
#' All breaks are explicit, i.e. if `drop = TRUE`,
#' any data beyond the outermost breaks points are dropped.
#' If one wants to have unspecified upper / lower limits on one time scale,
#' use `Inf`: e.g. `breaks = list(fot = 0:5, age = c(0,45,Inf))`.
#' Breaks for `per` can also be given in
#' `Date` / `IDate`format, whereupon
#' they are converted to fractional years before used in splitting.
#'
#' The `age` time scale can additionally
#' be automatically split into common age grouping schemes
#' by naming the scheme with an appropriate character string:
#'
#' \itemize{
#'   \item `"18of5"`: age groups 0-4, 5-9, 10-14, ..., 75-79, 80-84, 85+
#'   \item `"20of5"`: age groups 0-4, 5-9, 10-14, ..., 85-89, 90-94, 95+
#'   \item `"101of1"`: age groups 0, 1, 2, ..., 98, 99, 100+
#' }
#'
#' **Time variables**
#'
#' If any of the given time variables
#' (`birth`, `entry`, `exit`, `event`)
#' is in any kind of date format, they are first coerced to
#' fractional years before splitting
#' using `[get.yrs]` (with `year.length = "actual"`).
#'
#' Sometimes in e.g. SIR/SMR calculation one may want the event time to differ
#' from the time of exit from follow-up, if the subject is still considered
#' to be at risk of the event. If `event` is specified, the transition to
#'  `status` is moved to `event` from `exit`
#'  using `[Epi::cutLexis]`. See Examples.
#'
#' **The status variable**
#'
#' The statuses in the expanded output (`lex.Cst` and `lex.Xst`)
#' are determined by using either only `status` or both `status`
#' and `entry.status`. If `entry.status = NULL`, the status at entry
#' is guessed according to the type of variable supplied via `status`:
#' For numeric variables it will be zero, for factors the first level
#' (`levels(status)[1]`) and otherwise the first unique value in alphabetical
#' order (`sort(unique(status))[1]`).
#'
#' Using numeric or factor status
#' variables is strongly recommended. Logical expressions are also allowed
#' (e.g. `status = my_status != 0L`) and are converted to integer internally.
#'
#' **Merging population hazard information**
#'
#' To enable computing relative/net survivals with `[survtab]`
#' and `[relpois]`, `lexpand` merges an appropriate
#' population hazard data (`pophaz`) to the expanded data
#' before dropping rows outside the specified
#' time window (if `drop = TRUE`). `pophaz` must, for this reason,
#' contain at a minimum the variables named
#' `agegroup`, `year`, and `haz`. `pophaz` may contain additional variables to specify
#' different population hazard levels in different strata; e.g. `popmort` includes `sex`.
#' All the strata-defining variables must be present in the supplied `data`. `lexpand` will
#' automatically detect variables with common names in the two datasets and merge using them.
#'
#' Currently `year` must be an integer variable specifying the appropriate year. `agegroup`
#' must currently also specify one-year age groups, e.g. `popmort` specifies 101 age groups
#' of length 1 year. In both
#' `year` and `agegroup` variables the values are interpreted as the lower bounds of intervals
#' (and passed on to a `cut` call). The mandatory variable `haz`
#' must specify the appropriate average rate at the person-year level;
#' e.g. `haz = -log(survProb)` where `survProb` is a one-year conditional
#' survival probability will be the correct hazard specification.
#'
#' The corresponding `pophaz` population hazard value is merged by using the mid points
#' of the records after splitting as reference values. E.g. if `age=89.9` at the start
#' of a 1-year interval, then the reference age value is `90.4` for merging.
#' This way we get a "typical" population hazard level for each record.
#'
#' **Computing Pohar-Perme weights**
#'
#' If `pp = TRUE`, Pohar-Perme weights
#' (the inverse of cumulative population survival) are computed. This will
#' create the new `pp` variable in the expanded data. `pp` is a
#' reserved name and `lexpand` throws exception if a variable with that name
#' exists in `data`.
#'
#' When a survival interval contains one or several rows per subject
#' (e.g. due to splitting by the `per` scale),
#' `pp` is cumulated from the beginning of the first record in a survival
#' interval for each subject to the mid-point of the remaining time within that
#' survival interval, and  that value is given for every other record
#' that a given person has within the same survival interval.
#'
#' E.g. with 5 rows of duration `1/5` within a survival interval
#' `[0,1)]`, `pp` is determined for all records by a cumulative
#' population survival from `0` to `0.5`. The existing accuracy is used,
#' so that the weight is cumulated first up to the end of the second row
#' and then over the remaining distance to the mid-point (first to 0.4, then to
#' 0.5). This ensures that more accurately merged population hazards are fully
#' used.
#'
#' **Event not at end of follow-up & overlapping time lines**
#'
#' `event` may be used if the event indicated by `status` should
#' occur at a time differing from `exit`. If `event` is defined,
#' `cutLexis` is used on the data set after coercing it to the `Lexis`
#' format and before splitting. Note that some values of `event` are allowed
#' to be `NA` as with `cutLexis` to accommodate observations
#' without an event occurring.
#'
#' Additionally, setting `overlapping = FALSE` ensures that (irrespective
#' of using `event`) the each subject defined by `id` only has one
#' continuous time line instead of possibly overlapping time lines if
#' there are multiple rows in `data` by `id`.
#'
#'
#' **Aggregating**
#'
#' Certain analyses such as SIR/SMR calculations require tables of events and
#' person-years by the unique combinations (interactions) of several variables.
#' For this, `aggre` can be specified as a list of such variables
#' (preferably `factor` variables but not mandatory)
#'  and any arbitrary functions of the
#' variables at one's disposal. E.g.
#'
#' `aggre = list(sex, agegr = cut(dg_age, 0:100))`
#'
#' would tabulate events and person-years by sex and an ad-hoc age group
#' variable. Every ad-hoc-created variable should be named.
#'
#' `fot`, `per`, and `age` are special reserved variables which,
#' when present in the `aggre` list, are output as categories of the
#' corresponding time scale variables by using
#' e.g.
#'
#' `cut(fot, breaks$fot, right=FALSE)`.
#'
#' This only works if
#' the corresponding breaks are defined in `breaks` or via "`...`".
#' E.g.
#'
#' `aggre = list(sex, fot.int = fot)` with
#'
#' `breaks = list(fot=0:5)`.
#'
#' The output variable `fot.int` in the above example will have
#' the lower limits of the appropriate intervals as values.
#'
#' `aggre` as a named list will output numbers of events and person-years
#' with the given new names as categorizing variable names, e.g.
#' `aggre = list(follow_up = fot, gender = sex, agegroup = age)`.
#'
#' The output table has person-years (`pyrs`) and event counts
#' (e.g. `from0to1`) as columns. Event counts are the numbers of transitions
#' (`lex.Cst != lex.Xst`) or the `lex.Xst` value at a subject's
#' last record (subject possibly defined by `id`).
#'
#' If `aggre.type = "unique"` (alias `"non-empty"`),
#' the above results are computed for existing
#' combinations of expressions given in `aggre`, but also for non-existing
#' combinations if `aggre.type = "cartesian"` (alias `"full"`). E.g. if a
#' factor variable has levels `"a", "b", "c"` but the data is limited
#' to only have levels `"a", "b"` present
#' (more than zero rows have these level values), the former setting only
#' computes results for `"a", "b"`, and the latter also for `"c"`
#' and any combination with other variables or expression given in `aggre`.
#' In essence, `"cartesian"` forces also combinations of variables used
#' in `aggre` that have no match in data to be shown in the result.
#'
#' If `aggre` is not `NULL` and `pophaz` has been supplied,
#' `lexpand` also aggregates the expected counts of events, which
#' appears in the output data by the reserved name `d.exp`. Additionally,
#' having `pp = TRUE` causes `lexpand` to also compute various
#' Pohar-Perme weighted figures necessary for computing Pohar-Perme net survivals
#' with `[survtab_ag]`. This can be slow, so consider what is really
#' needed. The Pohar-Perme weighted figures have the suffix `.pp`.
#'
#' @return
#' If `aggre = NULL`, returns
#' a `data.table` or `data.frame`
#' (depending on `options("popEpi.datatable")`; see `?popEpi`)
#' object expanded to accommodate split observations with time scales as
#' fractional years and `pophaz` merged in if given. Population
#' hazard levels in new variable `pop.haz`, and Pohar-Perme
#' weights as new variable `pp` if requested.
#'
#' If `aggre` is defined, returns a long-format
#' `data.table`/`data.frame` with the variable `pyrs` (person-years),
#' and variables for the counts of transitions in state or state at end of
#' follow-up formatted `fromXtoY`, where `X` and `Y` are
#' the states transitioned from and to, respectively. The data may also have
#' the columns `d.exp` for expected numbers of cases and various
#' Pohar-Perme weighted figures as identified by the suffix `.pp`; see
#' Details.
#'
#'
#' @examples
#' \donttest{
#' ## prepare data for e.g. 5-year cohort survival calculation
#' x <- lexpand(sire, breaks=list(fot=seq(0, 5, by = 1/12)),
#'              birth = bi_date, entry = dg_date, exit = ex_date,
#'              status =  status != 0, pophaz=popmort)
#'
#' ## prepare data for e.g. 5-year "period analysis" for 2008-2012
#' BL <- list(fot = seq(0, 5, by = 1/12), per = c("2008-01-01", "2013-01-01"))
#' x <- lexpand(sire, breaks = BL,
#'              birth = bi_date, entry = dg_date, exit = ex_date,
#'              pophaz=popmort, status =  status != 0)
#'
#' ## aggregating
#' BL <- list(fot = 0:5, per = c("2003-01-01","2008-01-01", "2013-01-01"))
#' ag <- lexpand(sire, breaks = BL, status = status != 0,
#'              birth = bi_date, entry = dg_date, exit = ex_date,
#'               aggre=list(sex, period = per, surv.int = fot))
#'
#' ## aggregating even more
#' ag <- lexpand(sire, breaks = BL, status = status != 0,
#'               birth = bi_date, entry = dg_date, exit = ex_date,
#'               aggre=list(sex, period = per, surv.int = fot),
#'               pophaz = popmort, pp = TRUE)
#'
#' ## using "..."
#' x <- lexpand(sire, fot=0:5, status =  status != 0,
#'              birth = bi_date, entry = dg_date, exit = ex_date,
#'              pophaz=popmort)
#'
#' x <- lexpand(sire, fot=0:5, status =  status != 0,
#'              birth = bi_date, entry = dg_date, exit = ex_date,
#'              aggre=list(sex, surv.int = fot))
#'
#' ## using the "event" argument: it just places the transition to given "status"
#' ## at the "event" time instead of at the end, if possible using cutLexis
#' x <- lexpand(sire, status = status, event = dg_date,
#'              birth = bi_date, entry = dg_date, exit = ex_date,)
#'
#' ## aggregating with custom "event" time
#' ## (the transition to status is moved to the "event" time)
#' x <- lexpand(sire, status = status, event = dg_date,
#'              birth = bi_date, entry = dg_date, exit = ex_date,
#'              per = 1970:2014, age = c(0:100,Inf),
#'              aggre = list(sex, year = per, agegroup = age))
#'
#' }
#'
#' @import data.table
#' @import Epi
#' @family splitting functions
#' @family aggregation functions
#' @seealso
#' `[Epi::Lexis]`, `[popmort]`
#' @export
lexpand <- function(data,
                    birth=NULL, entry=NULL, exit=NULL, event=NULL,
                    status = status != 0,
                    entry.status = NULL,
                    breaks = list(fot=c(0,Inf)),
                    id = NULL,
                    overlapping = TRUE,
                    aggre = NULL,
                    aggre.type = c("unique", "cartesian"),
                    drop=TRUE,
                    pophaz = NULL, pp = TRUE,
                    subset = NULL,
                    merge=TRUE, verbose = FALSE,
                    ...) {
  start_time <- proc.time()

  TF <- environment()
  PF <- parent.frame(1L)

  ## data checks
  if ( missing(data) || nrow(data) == 0) stop("no data found")

  if (!is.data.frame(data)) stop("data must be a data.frame or data.table")

  ## to instate global variables to appease R CMD CHECK
  .EACHI <- lex.status <- lexpand.id <- lex.exit <- lex.birth <-
    lex.entry <- lex.event <- temp.id <- cd <- fot <- age <- per <-
    lex.id <- lex.multi <- pop.haz <- lex.Cst <- lex.Xst <- lex.dur <- NULL


  ## test conflicting variable names -------------------------------------------
  added_vars <- c("fot", "per", "age", "lex.id", "lex.dur", "lex.Xst", "lex.Cst")
  if (!is.null(pophaz)) added_vars <- if (pp) c(added_vars, "pp", "pop.haz") else c(added_vars, "pop.haz")
  conflicted_vars <- intersect(added_vars, names(data))

  if (merge && length(conflicted_vars) > 0) {
    conflicted_vars <- paste0("'", conflicted_vars, "'", collapse = ", ")
    warning("'data' already had variable(s) named ", conflicted_vars, " which lexpand will create, and you have merge = TRUE; this may result in unexpected problems. Rename the variable(s)?")
  }
  rm(added_vars, conflicted_vars)

  ## test aggre type -----------------------------------------------------------
  aggre.type <- match.arg(aggre.type[1L], c("cartesian", "non-empty", "unique", "cross-product", "full"))
  if (aggre.type == "cross-product") {
    aggre.type <- "cartesian"
    warning("aggre.type value 'cross-product' deprecated and renamed to 'cartesian'; please use that in the future")
  }

  ## subsetting-----------------------------------------------------------------
  ## no copy taken of data!
  subset <- substitute(subset)
  subset <- evalLogicalSubset(data, subset)

  ## prepping time variables ---------------------------------------------------
  l <- substitute(list(birth, entry, exit, event, status, entry.status, id))
  rm(birth, entry, exit, event, status, entry.status, id)

  lc <- unlist(lapply(l, deparse))
  lc <- lc[-1] ## first always "list"

  wh <- which(lc != "NULL")
  lex_vars <- c("lex.birth","lex.entry","lex.exit","lex.event", "lex.status", "lex.entry.status", "lexpand.id")[wh]
  if (any(!c("lex.birth", "lex.entry", "lex.exit", "lex.status") %in% lex_vars)) stop("birth, entry, exit and status are mandatory")

  l <- eval(l, envir = data[subset, ], enclos = PF)
  l[-wh] <- NULL


  ## vars can be given as character strings of variable names
  isChar  <- sapply(l, is.character, simplify = TRUE)
  if (any(isChar)) {
    isShort <- sapply(l, function(x) {length(x) == 1L}, simplify = TRUE)
    whOneChar <- which(isShort & isChar)

    whBadChar <- NULL
    if (length(whOneChar) > 0) {
      testBadChar <- unlist(l[whOneChar])
      whBadChar <- whOneChar[!testBadChar %in% names(data)]
    }

    if (length(whBadChar) > 0) {

      badChar <- l[whBadChar]
      badChar <- paste0(badChar[1:min(length(badChar), 5L)], collapse = ", ")
      stop("Variables given as a character of length one are interpreted as variable names in data,
           but some given characters were not found in data;
           check names or input as factor/Date;
           first five bad names: ", badChar)
    }

    l[whOneChar] <- lapply(l[whOneChar], function(x) {data[subset, ][[x]]})
  }


  l <- as.data.table(l)
  setnames(l, names(l), lex_vars)


  rm(lex_vars)
  if (!all(c("lex.birth","lex.entry","lex.exit","lex.status") %in% names(l))) {
    stop("birth, entry, exit and status are mandatory, but at least one was misspecified/NULL")
  }

  if (is.logical(l$lex.status)) l[, lex.status := as.integer(lex.status)]
  if (is.null(l$lexpand.id)) l[, lexpand.id  := 1:.N]

  ## checks for merging style --------------------------------------------------
  if (!is.null(pophaz)) {
    all_names_present(pophaz, c("agegroup","year","haz"))
    othMergeVars <- setdiff(names(pophaz), c("agegroup","year","haz"))
    badOthMergeVars <- setdiff(othMergeVars, names(data))
    if (length(badOthMergeVars) > 0) {
      badOthMergeVars <- paste0("'", badOthMergeVars, "'", collapse = ", ")
      stop("Following variables exist in pophaz but do not exist in data: ", badOthMergeVars, ". Make sure data and pophaz contain variables with the same names that you intend to merge by.")
    }
  }

  if (is.null(pophaz)) {
    comp_pp <- FALSE
  } else {
    comp_pp <- TRUE
  }

  ## internally we have "delta" and "actual" methods,
  ## the latter being experimental and not visible in the documentation.
  ## "delta" is always used if pp = TRUE.
  ## it is possible to choose pp = "actual" as well, though, if you know
  ## about it.
  comp_pp <- FALSE
  if (is.logical(pp) && pp) pp <- "delta"

  if (!is.null(pp) && is.character(pp)) {
    pp <- match.arg(pp, c("delta", "actual"))
    comp_pp <- TRUE
  }
  if (comp_pp && "pp" %in% names(data)) stop("variable named 'pp' in data; this is a reserved name for pohar-perme weights, please rename / remove the variable in data")

  ## ensure given breaks make any sense ----------------------------------------

  bl <- list(...)
  lna <- names(bl)
  bad_lna <- setdiff(lna, c("fot","per","age"))
  if (length(bad_lna) > 0) {
    bad_lna <- paste0("'", bad_lna, "'", collapse = ", ")
    stop("only arguments named 'fot', 'per' or 'age' currently allowed to be passed via '...'; did you mistype an argument? bad args: ", bad_lna)
  }
  lna <- intersect(names(bl), c("fot","per","age"))
  if (length(lna) > 0) {
    bl <- bl[lna]
    if (!is.null(breaks)) breaks[lna] <- NULL
    breaks <- c(breaks, bl)
  }
  rm(bl, lna)

  brna <- names(breaks)
  if (length(brna) != length(breaks)) {
    stop("all elements in breaks list must be named, e.g. list(fot = 0:5, age=c(0,45,65,Inf))")
  }

  brna <- intersect(brna, c("fot","per","age"))
  if (length(brna) == 0) {
    breaks$fot <- c(0,Inf)
  }

  if ("age" %in% brna && is.character(breaks$age)) {
    schemeNames <- c("18of5", "20of5", "101of1")
    if (!breaks$age %in% schemeNames) stop("You supplied '", breaks$age, "' as breaks for the age scale, but allowed character strings are: ", paste0("'", schemeNames, "'", collapse = ","))
    brSchemes <- list(c(seq(0, 85, 5)), c(seq(0, 95, 5), Inf), c(0:100, Inf))
    names(brSchemes) <- paste0("age_", schemeNames)
    breaks$age <- brSchemes[paste0("age_",breaks$age)]
  }


  if (any(sapply(breaks, length) == 1L)) {
    stop("any given non-null vector of breaks must have more than one break!")
  }

  # convert to fractional years ------------------------------------------------

  char2date <- function(obj) {
    if (is.character(obj)) {
      return(as.IDate(obj))
    } else {
      return(obj)
    }
  }

  date2yrs <- function(obj) {
    if (is.Date(obj)) {
      get.yrs(obj, year.length = "actual")
    } else {
      obj
    }
  }

  breaks <- lapply(breaks, char2date)
  breaks <- lapply(breaks, date2yrs)

  time_vars <- intersect(names(l), c("lex.birth", "lex.entry", "lex.exit","lex.event"))
  l[, (time_vars) := lapply(.SD, date2yrs) , .SDcols = time_vars]

  if (verbose) cat("given birth, entry, exit, status etc. variables after coercion to numeric \n")
  if (verbose) print(l)

  # check data consistency for overlapping = FALSE -----------------------------
  ## not allowed: for any one unique subject to be true for
  ## multiple rows (if overlapping = TRUE):
  ## * same event values
  ## * same entry values
  if (!overlapping) {
    if ("lex.event" %in% names(l)) {
      if (all(is.na(l$lex.event))) stop("ALL 'event' values are NA; if this is as intended, please use event = NULL instead")

      if (any(duplicated(l, by = c("lexpand.id", "lex.event")))) {
        stop("subject(s) defined by lex.id had several rows where 'event' time had the same value, which is not supported with overlapping = FALSE; perhaps separate them by one day?")
      }
      if (any(l[!is.na(lex.event), lex.entry == lex.event])) {
        stop("some rows have simultaneous 'entry' and 'event', which is not supported with overlapping = FALSE; perhaps separate them by one day?")
      }
    } else if (any(duplicated(l, by = c("lexpand.id", "lex.exit")))) {
      stop("subject(s) defined by lex.id had several rows where 'exit' time had the same value, which is not supported without 'event' defined; use 'event' or perhaps separate them by one day?")
    }


  }


  # dropping unuseful records --------------------------------------------------
  test_times <- function(condition, msg, old_subset=l_subset, DT=l) {

    condition <- substitute(condition)
    condition <- eval(condition, envir = DT, enclos = parent.frame(1L))

    new_subset <- old_subset & !(condition & !is.na(condition))
    old_n <- sum(old_subset)
    new_n <- sum(new_subset)

    if (new_n == 0L) {
      stop("dropping rows where ", msg, " resulted in zero rows left. likely problem: misdefined time variables")
    }

    if (new_n < old_n) {
      message(paste0("dropped ", old_n-new_n, " rows where ", msg))
    }
    return(new_subset)
  }

  l_subset <- rep(TRUE, nrow(l))

  l_subset <- test_times(is.na(lex.birth), "birth values are missing")
  l_subset <- test_times(is.na(lex.entry), "entry values are missing")
  l_subset <- test_times(is.na(lex.exit), "exit values are missing")

  if (!is.null(breaks$per)) {
    l_subset <- test_times(lex.exit < min(breaks$per), "subjects left follow-up before earliest per breaks value")
  }
  if (!is.null(breaks$age)) {
    l_subset <- test_times(lex.exit - lex.birth < min(breaks$age), "subjects left follow-up before lowest age breaks value")
  }
  if (!is.null(breaks$fot)) {
    l_subset <- test_times(lex.exit - lex.entry < min(breaks$fot), "subjects left follow-up before lowest fot breaks value")
  }
  l_subset <- test_times(lex.birth >= lex.exit, "birth >= exit")
  l_subset <- test_times(lex.entry == lex.exit, "entry == exit")
  l_subset <- test_times(lex.entry >  lex.exit, "entry >  exit")
  l_subset <- test_times(lex.birth >  lex.entry, "birth > entry")
  if (!is.null(l$lex.event)) {
    l_subset <- test_times(lex.event > lex.exit, "event > exit")
    l_subset <- test_times(lex.event < lex.entry, "event < entry")
  }
  l <- l[l_subset]

  if (verbose) cat("Time taken by checks, prepping and test: ", timetaken(start_time), "\n")

  # Lexis coercion -------------------------------------------------------------

  ## status definitions
  setnames(l, "lex.status", "lex.Xst")
  if ("lex.entry.status" %in% names(l)) {
    setnames(l, "lex.entry.status", "lex.Cst")
  } else {
    if (is.factor(l$lex.Xst)) {
      l[, lex.Cst := factor(levels(lex.Xst)[1L], levels=levels(lex.Xst))]
    } else if (is.double(l$lex.Xst)) {
      l[, lex.Cst := 0]
    } else if (is.integer(l$lex.Xst)) {
      l[, lex.Cst := 0L]
    } else {
      l[, lex.Cst := sort(unique(lex.Xst))[1L]]
    }

  }

  # ensure common labels for factors etc.
  harmonizeStatuses(x = l, C = "lex.Cst", X = "lex.Xst")

  ## time scales and duration
  l[, lex.dur := lex.exit - lex.entry]
  l[, fot := 0]
  setnames(l, "lex.entry", "per")
  l[, age := per-lex.birth]
  setnames(l, "lexpand.id", "lex.id")

  ## for merging data with l later
  if (merge) {
    idt <- data.table(temp.id = 1:nrow(l))
    l[, temp.id := 1:.N]
  }

  ## crop time scale values to obey breaks limits and drop if necessary
  ## NOTE: goes wrong if need to compute pp weights!
  #   if (drop && !pp) {
  #     intelliCrop(x = l, breaks = breaks, allScales = c("fot", "per", "age"), cropStatuses = TRUE)
  #     l <- intelliDrop(x = l, breaks = breaks, dropNegDur = TRUE)
  #   }


  setcolsnull(l, colorder=TRUE, soft=TRUE,
              keep = c("lex.id","fot","per","age",
                       "lex.dur", "lex.Cst", "lex.Xst", "lex.event", "temp.id"))
  setattr(l, "class", c("Lexis", "data.table", "data.frame"))
  setattr(l, "time.scales", c("fot","per","age"))
  setattr(l, "time.since", c("","",""))

  if (verbose) cat("data just after Lexis coercion: \n")
  if (verbose) print(l)

  # event not at exit time -----------------------------------------------------

  if ("lex.event" %in% names(l)) {

    if (!overlapping) {

      ## using lex.event time, ensure coherence of lex.Cst & lex.Xst
      ## before cutLexis()
      tmpFE <- makeTempVarName(l, pre = "fot_end_")
      l[, (tmpFE) := fot + lex.dur]
      setkeyv(l, c("lex.id", "lex.event", tmpFE))
      tmpLX <- makeTempVarName(l, pre = "lag_lex.Xst_")
      l[, (tmpLX) := shift(lex.Xst, n = 1, type = "lag"), by = lex.id]
      l[!is.na(get(tmpLX)), lex.Cst := get(tmpLX)]
      l[, c(tmpFE, tmpLX) := NULL]
      rm(tmpFE, tmpLX)

    }

    if (verbose) cutt <- proc.time()
    setDF(l)
    setattr(l, "class", c("Lexis", "data.frame"))
    l <- Epi::cutLexis(l, cut = l$lex.event, timescale = "per", new.state = l$lex.Xst, precursor.states = unique(l$lex.Cst))
    setDT(l)
    setattr(l, "class", c("Lexis", "data.table", "data.frame"))
    if (verbose) cat("Time taken by cutLexis when defining event time points: ", timetaken(cutt), "\n")

    if (verbose) cat("Data just after using cutLexis: \n")
    if (verbose) print(l[])

  }


  # overlapping timelines? -----------------------------------------------------

  if (!overlapping && any(duplicated(l$lex.id))) {
    tmpFE <- makeTempVarName(l, pre = "fot_end_")
    l[, (tmpFE) := fot + lex.dur]
    ## don't keep duplicated rows:
    ## same end points imply fully overlapping time lines
    ## e.g.
    ## --->
    ## ->
    ##  -->
    ## results in
    ## ->
    ## --->
    ## we only keep the longest time line with a unique end point.

    # setkeyv(l,  c("lex.id", tmpFE, "fot"))
    tmpLE <- intersect(names(l), "lex.event")
    LEval <- if (length(tmpLE) == 0) NULL else -1

    setorderv(l, c("lex.id", tmpFE, tmpLE, "fot"), c(1,1,LEval,1))
    l <- unique(l, by = c("lex.id", tmpFE))

    ## end points are kept but starting points are "rolled"
    ## from first to last row by lex.id to ensure non-overlappingness; e.g.
    ## ->
    ## --->
    ## results in
    ## ->
    ##   ->
    # setkeyv(l, c("lex.id", tmpFE))
    # setorderv(l, c("lex.id", tmpLE, tmpFE), c(1, LEval, 1))
    setkeyv(l, c("lex.id", tmpLE, tmpFE))

    if (verbose) cat("data just before fixing overlapping time lines \n")
    if (verbose) print(l)
    l[, lex.dur := get(tmpFE) - c(min(fot), get(tmpFE)[-.N]), by = lex.id]
    l[, fot := get(tmpFE) - lex.dur]
    cumDur <- l[,  list(age = min(age), per = min(per), cd = c(0, cumsum(lex.dur)[-.N])), by = lex.id]
    cumDur[, age := age+cd]
    cumDur[, per := per+cd]
    l[, age := cumDur$age]
    l[, per := cumDur$per]
    l[, (tmpFE) := NULL]; rm(cumDur)


    ## if event used, first row up to event, second row from first event to etc...
  }

  setcolsnull(l, "lex.event", soft = TRUE) ## note: lex.event needed in overlapping procedures

  if (verbose) cat("time and status variables before splitting: \n")
  if (verbose) print(l)
  if ("id" %in% ls()) rm("id")


  # splitting ------------------------------------------------------------------

  ## determine whether to drop data only after splitting and merging
  drop_after <- FALSE
  if (drop == TRUE && comp_pp) {
    drop <- FALSE
    drop_after <- TRUE
  }

  forceLexisDT(l, breaks = list(fot = NULL, per = NULL, age = NULL),
               allScales = c("fot", "per", "age"))
  if (verbose) splittime <- proc.time()
  l <- splitMulti(l,  breaks = breaks,
                  drop = drop, verbose=FALSE, merge = TRUE)
  setDT(l)
  setkey(l, lex.id, fot)
  l[, lex.multi := 1:.N, by = lex.id]
  if (verbose) cat("Time taken by splitting:", timetaken(splittime), "\n")

  # merging other variables from data ------------------------------------------

  if (merge) {
    setkey(l, temp.id)

    temp <- data.table(idt, data[subset & !is.na(subset), ][l_subset, ])
    setkey(temp, temp.id)

    l <- temp[l]

    rm(temp, idt)
    setcolsnull(l, "temp.id")

    lex_vars <- c("lex.id","lex.multi","fot","per","age", "lex.dur", "lex.Cst", "lex.Xst")
    setcolorder(l, c(lex_vars, setdiff(names(l), lex_vars)))
  }
  rm(data, subset, l_subset)

  ## aggregating checks --------------------------------------------------------
  ## NOTE: aggre evaled here using small data subset to check that all needed
  ## variables are found, etc.
  aggSub <- substitute(aggre)
  agTest <- evalPopArg(arg = aggSub, data = l[1:min(10L, .N), ],
                       enclos = PF, recursive = TRUE, DT = TRUE)
  agTy <- attr(agTest, "arg.type")
  if (is.null(agTy)) agTy <- "NULL"
  aggSub <- attr(agTest, "quoted.arg")
  agVars <- attr(agTest, "all.vars")
  rm(aggre)

  # merging pophaz and pp-weighting --------------------------------------------
  if (!is.null(pophaz)) {

    pophaztime <- proc.time()

    if (any(c("haz", "pop.haz") %in% names(l))) stop("case data had variable(s) named 'haz' / 'pop.haz', which are reserved for lexpand's internal use. rename/remove them please.")
    # merge surv.int information -----------------------------------------------
    NULL_FOT <- FALSE
    if (is.null(breaks$fot)) {
      breaks$fot <- l[, c(0, max(fot+lex.dur))]
      NULL_FOT <- TRUE
    }

    breaks$fot <- sort(unique(breaks$fot))
    # handle pophaz data -------------------------------------------------------

    if (!"haz" %in% names(pophaz)) stop("no 'haz' variable in pophaz; please rename you hazard variable to 'haz'")
    yBy <- xBy <- setdiff(names(pophaz), c("haz"))
    if (c("year") %in% yBy) xBy[yBy == "year"] <- "per"
    if (c("agegroup") %in% yBy) xBy[yBy == "agegroup"] <- "age"
    yByOth <- setdiff(yBy, c("year", "agegroup"))

    if (any(!yByOth %in% names(l)))
      stop("Following variable names not common between pophaz and data: ", paste0("'", yByOth[!yByOth %in% names(l)], "'", collapse = ", "))

    l <- cutLowMerge(x = l, y = pophaz, by.x = xBy, by.y = yBy, all.x = TRUE,
                     all.y = FALSE, mid.scales = c("per", "age"), old.nums = TRUE)
    setnames(l, "haz", "pop.haz")

    ## check if l's merging time variables were within pophaz's limits ---------
    nNA <- l[is.na(pop.haz), .N]
    if (nNA > 0) message("WARNING: after merging pophaz, ", nNA, " rows in split data have NA hazard values!")

    names(yBy) <- xBy
    names(xBy) <- yBy
    for (k in intersect(c("per", "age"), xBy)) {
      yVar <- yBy[k]
      kLo <- min(pophaz[[yVar]])
      kHi <- max(pophaz[[yVar]])
      mid <- l[, get(k) + lex.dur]
      nLo <- sum(mid < kLo - .Machine$double.eps^0.5)
      nHi <- sum(mid > kHi - .Machine$double.eps^0.5)
      if (nLo > 0) message("WARNING: ", nLo, " rows in split data have NA values due to their mid-points residing below the minimum value of '", yVar, "' in pophaz!")
      if (nHi > 0) message("NOTE: ", nHi, " rows in split data had values of '", k, "' higher than max of pophaz's '", yVar, "'; the hazard values at '", yVar, "' == ", kHi, " were used for these")
    }
    rm(mid)
    for (k in yByOth) {
      levsNotOth <- setdiff(unique(l[[k]]), unique(pophaz[[k]]))
      if (length(levsNotOth) > 0) message("WARNING: following levels (first five) of variable '", k, "' not in pophaz but exist in split data: ", paste0("'",levsNotOth[1:5],"'", collapse = ", "))
    }


    # pohar-perme weighting ----------------------------------------------------
    if (comp_pp) {
      setkeyv(l, c("lex.id", "fot"))
      comp_pp_weights(l, surv.scale = "fot", breaks = breaks$fot, haz = "pop.haz",
                      style = "delta", verbose = verbose)
    }
    merge_msg <- "Time taken by merging pophaz"
    if (comp_pp) merge_msg <- paste0(merge_msg, " and computing pp")
    merge_msg <- paste0(merge_msg, ": ")
    if (verbose) cat(paste0(merge_msg, timetaken(pophaztime), "\n"))


  }

  # dropping after merging -----------------------------------------------------
  if (drop_after) {
    l <- intelliDrop(x = l, breaks = breaks)
  }

  if (verbose) cat("Number of rows after splitting: ", nrow(l),"\n")


  # aggregating if appropriate -------------------------------------------------
  if (agTy != "NULL") {

    setcolsnull(l, keep = c("lex.id","lex.dur", "fot", "per", "age", "lex.Cst", "lex.Xst", agVars, "pop.haz", "pp"))

    sumVars <- NULL
    if ("pop.haz" %in% names(l)) {
      if ("d.exp" %in% names(l)) stop("data had variable named 'd.exp' by which to aggregate, which would be overwritten due to aggregating expected numbers of cases (you have supplied pophaz AND are aggregating); please rename / remove it first.")
      l[, c("d.exp") := pop.haz*lex.dur ]
      sumVars <- c(sumVars, "d.exp")
    }
    if ("pop.haz" %in% names(l) && comp_pp && "pp" %in% names(l)) {
      forceLexisDT(l, breaks = breaks, allScales = c("fot", "per", "age"))
      ppFigs <- comp_pp_weighted_figures(lex = l, haz = "pop.haz", pp = "pp", event.ind = NULL)
      bad_pp_vars <- intersect(names(ppFigs), names(l))
      if (length(bad_pp_vars) > 0L) {
        bad_pp_vars <- paste0("'",bad_pp_vars, "'", collapse = ", ")
        stop("Data had variable(s) named ", bad_pp_vars, ", by which to aggregate, which would be overwritten due to aggregating expected numbers of cases (you have supplied pophaz AND are aggregating); please rename / remove them first")
      }
      l[, names(ppFigs) := ppFigs]
      sumVars <- c(sumVars, names(ppFigs))
      rm(ppFigs)

    }

    if (verbose) cat("Starting aggregation of split data... \n")
    setDT(l)
    forceLexisDT(l, allScales = c("fot", "per", "age"), breaks = breaks)
    l <- try(aggre(lex = l, by = aggSub, type = aggre.type, verbose = verbose, sum.values = sumVars))
    if (inherits(l, "try-error")) stop("Something went wrong when calling aggre() within lexpand(). Usual suspect: bad 'by' argument. Error message from aggre():
                                       ", paste0(l[[1]]))
    if (verbose) cat("Aggregation done. \n")

    if (!return_DT() && is.data.table(l)) setDFpe(l)

  } else {


    # last touch-up --------------------------------------------------------------
    ## sometimes problems with releasing memory
    gc()

    breaks <- lapply(c("fot","per","age"), function(ts_nm) {
      breaks[[ts_nm]]
    })
    names(breaks) <- c("fot","per","age")

    ## handle attributes
    setkeyv(l, c("lex.id", "lex.multi"))
    set(l, j = "lex.multi", value = NULL)
    setattr(l, "time.scales", c("fot","per","age"))
    setattr(l, "time.since", c("","",""))
    setattr(l, "breaks", breaks)
    setattr(l, "class", c("Lexis","data.table","data.frame"))
    if (!return_DT() && is.data.table(l)) setDFpe(l)



  }

  if (verbose) cat("Time taken by lexpand(): ", timetaken(start_time), "\n")

  return(l[])
}


globalVariables(c('.EACHI', "dg_date", "ex_date", "bi_date"))

