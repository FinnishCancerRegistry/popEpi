#' @title Confidence intervals for the rate ratios
#' @author Matti Rantanen
#' @description Calculate rate ratio with confidence intervals for rate objects or observations and person-years.
#'
#' @details Calculate rate ratio of two age standardized rate objects (see `[rate]`).
#' Multiple rates for each objects is supported if there are an equal number of rates.
#' Another option is to set `x` and `y` as a vector of two.
#' \enumerate{
#'   \item rate and its standard error, and  set `SE.method = TRUE`.
#'   \item observations and person-year, and  set `SE.method = FALSE`.
#' }
#' See examples.
#'
#'
#' @param x `[rate, integer, numeric]` (no default)
#'
#' Rate data.
#'
#' - `rate`: An object as produced by `[rate]`. If one of `x` and `y` is a
#'   `rate` object then both have to be or an error is raised.
#' - `integer` / `numeric`: A vector of length two. Here you have two options
#'   which you can choose using argument `SE.method`:
#'   + `SE.method = FALSE`: The first element is the number of events and the
#'     second the at-risk time (Poisson offset). This causes confidence
#'     intervals to be produced via `[stats::poisson.test]`.
#'   + `SE.method = TRUE`: The first element is the rate estimate and second
#'     the standard error. Confidence intervals are produced using the delta
#'     method with the log-transform.
#' @param y `[rate, integer, numeric]` (no default)
#'
#' See the documentation for argument `x`.
#'
#' @param crude `[logical]` (default `NULL`)
#'
#' This argument only used when `x` and `y` are `rate` objects as produced by
#' `[rate]`.
#'
#' - `TRUE` causes column `rate` to be used from the output object of `[rate]`.
#' - `FALSE` causes column `rate.adj` to be used.
#'
#' @param SE.method `[logical]` (default `FALSE`)
#'
#' Ignored when `x` and `y` are objects of class `rate` as produced by `[rate]`.
#' See the documention for argument `x`.
#'
#' @examples
#' # this data.table::setDTthreads call is included here only to
#' # conform to the CRAN submission requirement to only use at most 2
#' # threads. you do not need to set this to use popEpi.
#' # however some long calculations may benefit from using more threads.
#' data.table::setDTthreads(2L)
#' \donttest{
#' # two rate ratios; silly example with female rectal / breast cancer
#' ## mortality rates
#' data("sire", package = "popEpi")
#' data("sibr", package = "popEpi")
#'
#' BL <- list(per = 2000:2005)
#'
#' re <- lexpand(sire, birth = "bi_date", entry = "dg_date", exit = "ex_date",
#'               status = status == 1, breaks = BL, aggre = list(per))
#' br <- lexpand(sibr, birth = "bi_date", entry = "dg_date", exit = "ex_date",
#'               status = status == 1, breaks = BL, aggre = list(per))
#'
#' r_re <- rate(re, obs = "from0to1", pyrs = "pyrs")
#' r_br <- rate(br, obs = "from0to1", pyrs = "pyrs")
#'
#' rate_ratio(r_re, r_br, SE.method = TRUE)
#' }
#'
#' # using rates (0.003 and 0.005) and their SEs (0.001 and 0.002)
#' rate_ratio(x= c(0.003, 0.001), y= c(0.005, 0.002), SE.method = TRUE)
#'
#' # using event numbers (10 and 20) and person-years (30000 and 40000)
#' rate_ratio(x = c(10, 30000), y = c(20, 40000), SE.method = FALSE)
#'
#' @seealso `[rate]`
#'
#' @family rate functions
#'
#' @return A vector length of three: rate_ratio, and lower and upper confidence intervals.
#'
#' @export rate_ratio
#'
#' @import data.table
#' @import stats
rate_ratio <- function(x, y, crude = FALSE, SE.method = FALSE) {
  stopifnot(
    inherits(x, c("rate", "numeric", "integer")),
    inherits(y, c("rate", "numeric", "integer")),
    identical(inherits(x, "rate"), inherits(y, "rate")),
    inherits(x, "rate") || length(x) == 2,
    inherits(y, "rate") || length(y) == 2,

    is.logical(crude),
    length(crude) == 1,
    !is.na(crude),

    is.logical(SE.method),
    length(SE.method) == 1,
    !is.na(SE.method)
  )
  if (inherits(x, "rate")) {
    if (!crude & (!'rate.adj' %in% names(x) | !'rate.adj' %in% names(y))) {
      stop(
        "`crude = FALSE` but column `rate.adj` not present in input ",
        "object `x` and/or `y` but "
      )
    }
  }

  x <- prep.rate.input(x, crude = crude, SE = SE.method)
  y <- prep.rate.input(y, crude = crude, SE = SE.method)

  if (SE.method) {
    # @codedoc_comment_block news("popEpi::rate_ratio", "2026-08-17", "0.6.0")
    # Fixed the confidence interval formula for when `SE.method = TRUE`.
    # @codedoc_comment_block news("popEpi::rate_ratio", "2026-08-17", "0.6.0")
    # delta method for variance
    rr <- x[[1]] / y[[1]]
    rr_var <- (1 / x[[1]])^2 * x[[2]]^2 + (1 / y[[1]])^2 * y[[2]]^2
    z <- qnorm(p = 0.975)
    lo <- exp(log(rr) - sqrt(rr_var) * z)
    hi <- exp(log(rr) + sqrt(rr_var) * z)
    out <- round(data.frame(rate_ratio = ratio, lower = lo, upper = hi), 3)
  } else {
    # x and y vector of two:, pyrs
    pt <- list()
    out <- data.frame()
    j <- 1
    for (j in 1:length(x[[1]])) {
      pt[[j]] <- stats::poisson.test(
        x = c(x[[1]][j], y[[1]][j]),
        T = c(x[[2]][j], y[[2]][j])
      )
      out <- rbind(
        out,
        round(
          data.frame(
            rate_ratio = pt[[j]]$estimate,
            lower = pt[[j]]$conf.int[1],
            upper = pt[[j]]$conf.int[2]
          ),
          3
        )
      )
    }
  }
  if (any(out < 0)) {
    warning(
      'Negative estimate or confidence intervals. Tip: set SE.method to FALSE when using observations and person-years.'
    )
  }
  return(out)
}


prep.rate.input <- function(z, crude, SE) {
  # this one modulates input to rate_ratio function
  if (is.vector(z) && length(z) == 2) {
    # z is obs and pyrs OR rate and SE
    return(list(z[1], z[2]))
  } else if (inherits(z, 'rate')) {
    if (!SE) {
      # obs and pyrs
      att <- attributes(z)
      setDT(z)
      a <- z[, get(att$rate.meta$obs)]
      b <- z[, get(att$rate.meta$pyrs)]
    } else {
      if (crude) {
        a <- z[, rate]
        b <- z[, SE.rate]
      } else {
        # z is a rate object
        a <- z[, rate.adj]
        b <- z[, SE.rate.adj]
      }
    }
  } else {
    stop('Input is not correct: its neighter a vector of two nor a rate object')
  }
  return(list(a, b))
}
