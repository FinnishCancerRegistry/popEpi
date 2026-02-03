#' @title `Lexis` Datasets
#' @description
#' Make `[Epi::Lexis]` objects.
#' @name lexis_funs
NULL

#' @eval codedoc::pkg_doc_fun("popEpi::Lexis_fpa", "lexis_funs")
#' @param data a `data.frame`; mandatory
#' @param birth the time of birth; A character string naming the variable in
#' data or an expression to evaluate - see
#' [Flexible input][flexible_argument]
#' @param entry the time at entry to follow-up; supplied the
#' same way as `birth`
#' @param exit the time at exit from follow-up; supplied the
#' same way as `birth`
#' @param entry.status passed on to `[Epi::Lexis]` if not `NULL`;
#' supplied the same way as `birth`
#' @param exit.status passed on to `[Epi::Lexis]` if not `NULL`;
#' supplied the same way as `birth`
#' @param subset a logical condition to subset by before passing data
#' and arguments to `[Epi::Lexis]`
#' @param ... arguments passed to `[Epi::Lexis]`
#' @examples
#'
#' # popEpi::Lexis_fpa
#' data("sire", package = "popEpi")
#' lex <- Lexis_fpa(sire,
#'                  birth = "bi_date",
#'                  entry = dg_date,
#'                  exit = ex_date + 1L,
#'                  exit.status = "status")
#'
#' ## some special cases
#' myVar <- "bi_date"
#' l <- list(myVar = "bi_date")
#' sire$l <- sire$myVar <- 1
#'
#' ## conflict: myVar taken from data when "bi_date" was intended
#' lex <- Lexis_fpa(sire,
#'                  birth = myVar,
#'                  entry = dg_date,
#'                  exit = ex_date + 1L,
#'                  exit.status = "status")
#'
#' ## no conflict with names in data
#' lex <- Lexis_fpa(sire,
#'                  birth = l$myVar,
#'                  entry = dg_date,
#'                  exit = ex_date + 1L,
#'                  exit.status = "status")
#' stopifnot(
#'   identical(class(lex), c("Lexis", "data.table", "data.frame")),
#'   Epi::timeScales(lex) %in% c("fot", "per", "age")
#' )
#'
Lexis_fpa <- function(data,
                      birth = NULL,
                      entry = NULL,
                      exit = NULL,
                      entry.status = NULL,
                      exit.status = NULL,
                      subset = NULL,
                      ...) {
  # @codedoc_comment_block popEpi::Lexis_fpa
  # `popEpi::Lexis_fpa` collects data from its inputs to to call `[Epi::Lexis]`.
  # This is a convenience function for making a `Lexis` object with the
  # time scales `fot`, `per`, and `age`.
  # @codedoc_comment_block popEpi::Lexis_fpa
  TF <- environment()
  PF <- parent.frame(1L)

  checkVars <- c("fot", "per", "age",
                 paste0("lex.", c("dur", "Xst", "Cst", "id")))
  checkVars <- intersect(names(data), checkVars)
  if (length(checkVars)) {
    stop("Following variable name(s) reserved but exist in data: ",
         paste0(checkVars, collapse = ", "))
  }


  sb <- substitute(subset)
  subset <- evalLogicalSubset(data, sb, enclos = PF)
  if (all(subset)) subset <- NULL
  x <- subsetDTorDF(data = data, subset = subset)
  setDT(x)

  an <- c("birth", "entry", "exit", "entry.status", "exit.status")

  l <- vector("list", length(an))
  names(l) <- an
  for (stri in an) {
    e <- paste0("substitute(", stri, ", env = TF)")
    e <- parse(text = e)[[1]]
    e <- eval(e, envir = TF) ## e.g. result of substitute(birth)
    e <- evalPopArg(data = x, arg = e, enclos = PF)[[1]]
    l[[stri]] <- e
  }

  l[sapply(l, is.null)] <- NULL

  missVars <- setdiff(c("birth", "entry", "exit"), names(l))
  if (length(missVars)) {
    stop("Following mandatory arguments were NULL: ",
         paste0(missVars, collapse = ", "))
  }

  fot <- l$entry - l$entry
  per <- l$entry
  age <- l$entry - l$birth
  per_exit <- l$exit

  en <- list(fot = fot, per = per, age = age)
  ex <- list(per = per_exit)

  al <- list(entry = en, exit = ex, entry.status = l$entry.status,
             exit.status = l$exit.status, data = x)
  al[sapply(al, is.null)] <- NULL

  # @codedoc_comment_block return(popEpi::Lexis_fpa)
  # Returns a `Lexis` object with the additional class `data.table`. It has
  # the usual columns that `Lexis` objects
  # have, and with time scale columns `fot`, `per`, and `age`.
  # They are calculated as
  #
  # `fot = entry - entry` (to ensure correct format, e.g. difftime)
  #
  # `per = entry`
  #
  # and
  #
  # `age = entry - birth`.
  # @codedoc_comment_block return(popEpi::Lexis_fpa)
  out <- do.call(Epi::Lexis, args = c(al, ...))
  # @codedoc_comment_block news("popEpi::Lexis_fpa", "2026-02-03", "0.5.0")
  # `Lexis_fpa` output is now also a `data.table`, so the complete class vector
  # is `c("Lexis", "data.table", "data.frame")`.
  # @codedoc_comment_block news("popEpi::Lexis_fpa", "2026-02-03", "0.5.0")
  data.table::setDT(out)
  data.table::setattr(out, "class", union("Lexis", class(out)))
  data.table::setkeyv(out, c("lex.id", Epi::timeScales(out)))
  return(out[])
}


#' @eval codedoc::pkg_doc_fun("popEpi::Lexis_dt", "lexis_funs")
#' @examples
#'
#' # popEpi::Lexis_dt
#' lex_1 <- popEpi::Lexis_dt(
#'   data = popEpi::sire,
#'   entry = list(
#'     ts_fut = 0L,
#'     ts_age = as.integer(dg_date - bi_date),
#'     ts_cal = as.integer(dg_date)
#'   ),
#'   exit = list(ts_cal = as.integer(ex_date)),
#'   entry.status = 0L,
#'   exit.status = status
#' )
#' stopifnot(
#'   class(lex_1) == c("Lexis", "data.table", "data.frame"),
#'   Epi::timeScales(lex_1) %in% c("ts_fut", "ts_age", "ts_cal")
#' )
#'
#' lex_2 <- popEpi::Lexis_dt(
#'   data = popEpi::sire,
#'   entry = list(
#'     ts_fut = 0L,
#'     ts_age = as.integer(dg_date - bi_date),
#'     ts_cal = as.integer(dg_date)
#'   ),
#'   duration = as.integer(ex_date - dg_date),
#'   entry.status = 0L,
#'   exit.status = status
#' )
#' stopifnot(
#'   class(lex_2) == c("Lexis", "data.table", "data.frame"),
#'   Epi::timeScales(lex_2) %in% c("ts_fut", "ts_age", "ts_cal")
#' )
#'
Lexis_dt <- function(
  ...
) {
  # @codedoc_comment_block news("popEpi::Lexis_dt", "2026-02-03", "0.5.0")
  # New function `Lexis_dt`, a wrapper of `Epi::Lexis` which also sets class to
  # `c("Lexis", "data.table", "data.frame")`.
  # @codedoc_comment_block news("popEpi::Lexis_dt", "2026-02-03", "0.5.0")
  # @codedoc_comment_block popEpi::Lexis_dt
  # `popEpi::Lexis_dt` performs the following steps:
  #
  # - Calls `[Epi::Lexis]`.
  # @codedoc_comment_block popEpi::Lexis_dt
  call_env <- parent.frame(1L)
  lexis_call <- match.call()
  lexis_call[[1]] <- quote(Epi::Lexis)
  out <- eval(lexis_call, envir = call_env)
  # @codedoc_comment_block popEpi::Lexis_dt
  # - Calls `[data.table::setDT]` to make output into a `data.table`,
  #   `[data.table::setattr]` to set its class to
  #   `c("Lexis", "data.table", "data.frame")`, and
  #   `[data.table::setkeyv]` to sort the output by `lex.id` and the time
  #   scales.
  # - Returns the `Lexis` / `data.table` object.
  # @codedoc_comment_block popEpi::Lexis_dt
  data.table::setDT(out)
  data.table::setattr(out, "class", union("Lexis", class(out)))
  data.table::setkeyv(out, c("lex.id", Epi::timeScales(out)))
  # @codedoc_comment_block return(popEpi::Lexis_dt)
  # Returns a `Lexis` object that is also a `data.table` ---
  # therefore output has the classes `c("Lexis", "data.table", "data.frame")`.
  # @codedoc_comment_block return(popEpi::Lexis_dt)
  return(out[])
}
