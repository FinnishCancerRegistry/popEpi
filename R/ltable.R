#' @title Tabulate Counts and Other Functions by Multiple Variables into a
#' Long-Format Table
#' @author Joonas Miettinen, Matti Rantanen
#' @description `ltable` makes use of `data.table`
#' capabilities to tabulate frequencies or
#' arbitrary functions of given variables into a long format
#' `data.table`/`data.frame`. `expr.by.cj` is the
#' equivalent for more advanced users.
#' @param data a `data.table`/`data.frame`
#' @param by.vars names of variables that are used for categorization,
#' as a character vector, e.g. `c('sex','agegroup')`
#' @param expr object or a list of objects where each object is a function
#' of a variable (see: details)
#' @param subset a logical condition; data is limited accordingly before
#' evaluating `expr` - but the result of `expr` is also
#' returned as `NA` for levels not existing in the subset. See Examples.
#' @param use.levels logical; if `TRUE`, uses factor levels of given
#' variables if present;  if you want e.g. counts for levels
#' that actually have zero observations but are levels in a factor variable,
#' use this
#' @param na.rm logical; if `TRUE`, drops rows in table that have
#' `NA` as values in any of `by.vars` columns
#' @param robust logical; if `TRUE`, runs the output data's
#' `by.vars` columns through `robust_values` before outputting
#' @param .SDcols advanced; a character vector of column names
#' passed to inside the data.table's brackets
#' `DT[, , ...]`; see `[data.table]`; if `NULL`,
#' uses all appropriate columns. See Examples for usage.
#' @param enclos advanced; an environment; the enclosing
#' environment of the data.
#' @param ... advanced; other arguments passed to inside the
#' data.table's brackets `DT[, , ...]`; see `[data.table]`
#'
#' @import data.table
#'
#' @details
#'
#' Returns `expr` for each unique combination of given `by.vars`.
#'
#' By default makes use of any and all `[levels]` present for
#' each variable in  `by.vars`. This is useful,
#' because even if a subset of the data does not contain observations
#' for e.g. a specific age group, those age groups are
#' nevertheless presented in the resulting table; e.g. with the default
#' `expr = list(obs = .N)` all age group levels
#' are represented by a row and can have  `obs = 0`.
#'
#' The function differs from the
#' vanilla `[table]` by giving a long format table of values
#' regardless of the number of `by.vars` given.
#' Make use of e.g. `[cast_simple]` if data needs to be
#' presented in a wide format (e.g. a two-way table).
#'
#' The rows of the long-format table are effectively Cartesian products
#' of the levels of each variable in  `by.vars`,
#' e.g. with  `by.vars = c("sex", "area")` all levels of
#' `area` are repeated for both levels of  `sex`
#' in the table.
#'
#' The `expr` allows the user to apply any function(s) on all
#' levels defined by  `by.vars`. Here are some examples:
#' \itemize{
#'   \item .N or list(.N) is a function used inside a `data.table` to
#'   calculate counts in each group
#'   \item list(obs = .N), same as above but user assigned variable name
#'   \item list(sum(obs), sum(pyrs), mean(dg_age)), multiple objects in a list
#'   \item list(obs = sum(obs), pyrs = sum(pyrs)), same as above with user
#'   defined variable names
#' }
#'
#' If  `use.levels = FALSE`, no `levels` information will
#'  be used. This means that if e.g. the  `agegroup`
#' variable is a factor and has 18 levels defined, but only 15 levels
#'  are present in the data, no rows for the missing
#' levels will be shown in the table.
#'
#' `na.rm` simply drops any rows from the resulting table where
#' any of the  `by.vars` values was `NA`.
#'
#' @seealso
#' `[table]`, `[cast_simple]`, `[melt]`
#'
#' @return
#' A `data.table` of statistics (e.g. counts) stratified by the columns defined
#' in `by.vars`.
#'
#' @export ltable
#'
#' @examples
#' data("sire", package = "popEpi")
#' sr <- sire
#' sr$agegroup <- cut(sr$dg_age, breaks=c(0,45,60,75,85,Inf))
#' ## counts by default
#' ltable(sr, "agegroup")
#'
#' ## any expression can be given
#' ltable(sr, "agegroup", list(mage = mean(dg_age)))
#' ltable(sr, "agegroup", list(mage = mean(dg_age), vage = var(dg_age)))
#'
#' ## also returns levels where there are zero rows (expressions as NA)
#' ltable(sr, "agegroup", list(obs = .N,
#'                             minage = min(dg_age),
#'                             maxage = max(dg_age)),
#'        subset = dg_age < 85)
#'
#' #### expr.by.cj
#' expr.by.cj(sr, "agegroup")
#'
#' ## any arbitrary expression can be given
#' expr.by.cj(sr, "agegroup", list(mage = mean(dg_age)))
#' expr.by.cj(sr, "agegroup", list(mage = mean(dg_age), vage = var(dg_age)))
#'
#' ## only uses levels of by.vars present in data
#' expr.by.cj(sr, "agegroup", list(mage = mean(dg_age), vage = var(dg_age)),
#'            subset = dg_age < 70)
#'
#' ## .SDcols trick
#' expr.by.cj(sr, "agegroup", lapply(.SD, mean),
#'            subset = dg_age < 70, .SDcols = c("dg_age", "status"))

ltable <- function(data,
                   by.vars = NULL,
                   expr = list(obs = .N),
                   subset = NULL,
                   use.levels = TRUE,
                   na.rm = FALSE,
                   robust = TRUE) {

  PF <- parent.frame()
  TF <- environment()

  e <- substitute(expr)

  ## eval subset ---------------------------------------------------------------
  subset <- substitute(subset)
  subset <- evalLogicalSubset(data, subset, enclos = PF)

  ## create table --------------------------------------------------------------
  res <- expr.by.cj(data = data,
                    by.vars = by.vars,
                    expr = e,
                    subset = subset,
                    use.levels = use.levels,
                    na.rm = na.rm,
                    robust = robust)


  ## final touch ---------------------------------------------------------------

  if (!return_DT()) {
    setDFpe(res)
  }
  res

}




#' @describeIn ltable Somewhat more streamlined `ltable` with
#' defaults for speed. Explicit determination of enclosing environment
#' of data.
#' @export expr.by.cj

expr.by.cj <- function(data,
                       by.vars = NULL,
                       expr = list(obs = .N),
                       subset = NULL,
                       use.levels = FALSE,
                       na.rm = FALSE,
                       robust = FALSE,
                       .SDcols = NULL,
                       enclos = parent.frame(1L),
                       ...) {

  PF <- enclos
  TF <- environment()


  ## checks --------------------------------------------------------------------
  if (!is.data.frame(data)) {
    stop("Argument 'data' must be data.frame (data.table is fine too)")
  }

  stopifnot(is.environment(enclos))
  stopifnot(is.logical(na.rm))
  stopifnot(is.logical(use.levels))

  stopifnot(is.character(by.vars) || is.null(by.vars))
  all_names_present(data, c(by.vars))

  stopifnot(is.character(.SDcols) || is.null(.SDcols))
  all_names_present(data, .SDcols)

  tab <- data.table(data[1:min(10, nrow(data)),])
  e <- substitute(expr)
  e <- tab[, evalRecursive(e, env = .SD, enc = PF)$argSub]

  ## eval subset ---------------------------------------------------------------
  subset <- substitute(subset)
  subset <- evalLogicalSubset(data, subset, enclos = PF)

  ## retrieve data to use without taking copy ----------------------------------

  tabVars <- unique(c(by.vars, all.vars(e), .SDcols))
  tabVars <- intersect(names(data), tabVars)

  tab <- mget(tabVars, envir = as.environment(data))
  setDT(tab)

  tmpDum <- makeTempVarName(data, pre = "dummy_")
  if (!length(by.vars)) {
    if (!length(tab)) {
      ## no by.vars nor variables in expr
      tab <- data.table(rep(1L, nrow(data)))
      setnames(tab, "V1", tmpDum)
    } else {
      tab[, c(tmpDum) := 1L]
    }
    by.vars <- tmpDum
  }

  ## create joining table ------------------------------------------------------
  lev_fun <- function(x) {
    if (use.levels && is.factor(x)) {
      factor(levels(x), levels = levels(x))
    } else {
      sort(unique(x), na.last = TRUE)
    }
  }
  cj <- lapply(as.list(tab)[by.vars], lev_fun)
  cj <- do.call(CJ, c(cj, unique = FALSE, sorted = FALSE))
  if (na.rm) cj <- na.omit(cj)

  ## eval expression -----------------------------------------------------------
  tabe <- "tab[subset][cj, eval(e),
                       on = by.vars,
                       by = .EACHI, ..."
  tabe <- if (is.null(.SDcols)) tabe else paste0(tabe, ", .SDcols = .SDcols")
  tabe <- paste0(tabe ,"]")
  res <- eval(parse(text = tabe))

  setcolsnull(res, delete = tmpDum, soft = TRUE)
  by.vars <- setdiff(by.vars, tmpDum)

  ## final touch ---------------------------------------------------------------
  if (length(res)) setcolorder(res, c(by.vars, setdiff(names(res), by.vars)))
  if (length(by.vars)) setkeyv(res, by.vars)
  if (!return_DT()) {
    setDFpe(res)
  }
  res

}

