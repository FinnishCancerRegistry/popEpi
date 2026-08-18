#' @title Confidence intervals for the rate ratios
#' @author Matti Rantanen
#' @description
#' Estimate the rate ratio.
#'
#' @details
#' `rate_ratio` was originally written for the purpose of estimating the
#' rate ratio when only the rates and their standard errors are known
#' (`SE.method = TRUE`). For instance one could estimate the rate ratio of two
#' age-adjusted rates in this manner. However, we have also implemented the
#' possibility of estimating the rate ratio from count data.
#'
#' @param x `[rate, integer, numeric, list]` (no default)
#'
#' Rate data.
#'
#' - `rate`: An object as produced by `[rate]`. If one of `x` and `y` is a
#'   `rate` object then both have to be or an error is raised.
#' - `integer` / `numeric`: A vector of length two. For what the elements should
#'   be, see the documentation for argument `SE.method`.
#' - `list`: A `list` of length two. Each element is a vector of of numbers,
#'   whose intended contents depend on argument `SE.method`.
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
#' @param SE.method `[logical, character]` (default `FALSE`)
#'
#' Ignored when `x` and `y` are objects of class `rate` as produced by `[rate]`.
#'
#' - `FALSE` / `"poisson.test":
#'   The first element of `x` and `y` is the number of
#'   events and the
#'   second the at-risk time (Poisson offset). This causes confidence
#'   intervals to be produced via `[stats::poisson.test]`.
#' - `TRUE` / `"se_delta_method"`:
#'   The first element is the rate estimate and second
#'   the standard error. Confidence intervals are produced using the delta
#'   method with the log-transform.
#' - `SE.method = "glm"`: `x` and `y` contain event numbers and at-risk times.
#'   We fit a tiny Poisson regression model using
#'   `[stats::glm]` to compute exact confidence intervals. This is the most
#'   efficient and accurate method.
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
#' rate_ratio(r_re, r_br, crude = TRUE, SE.method = TRUE)
#' }
#'
#' # using rates (0.003 and 0.005) and their SEs (0.001 and 0.002)
#' # the CIs come from the delta method
#' rate_ratio(x = c(0.003, 0.001), y = c(0.005, 0.002),
#'            SE.method = TRUE)
#' rate_ratio(x = c(0.003, 0.001), y = c(0.005, 0.002),
#'            SE.method = "se_delta_method")
#' rate_ratio(
#'   x = list(c(0.003, 0.004), c(0.001, 0.001)),
#'   y = list(c(0.005, 0.006), c(0.002, 0.002)),
#'   SE.method = "se_delta_method"
#' )
#'
#'
#' # using event numbers (10 and 20) and person-years (30000 and 40000)
#' rate_ratio(x = c(10, 30000), y = c(20, 40000), SE.method = FALSE)
#' rate_ratio(x = c(10, 30000), y = c(20, 40000), SE.method = "poisson.test")
#' rate_ratio(
#'   x = list(c(10, 20), c(30000, 30000)),
#'   y = list(c(20, 30), c(40000, 40000)),
#'   SE.method = "poisson.test"
#' )
#'
#' # using event numbers (10 and 20) and person-years (30000 and 40000)
#' # and exact confidence intevals
#' rate_ratio(x = c(10, 30000), y = c(20, 40000), SE.method = "glm")
#' rate_ratio(
#'   x = list(c(10, 20), c(30000, 30000)),
#'   y = list(c(20, 30), c(40000, 40000)),
#'   SE.method = "glm"
#' )
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
    inherits(x, c("rate", "numeric", "integer", "list")),
    inherits(y, c("rate", "numeric", "integer", "list")),
    identical(inherits(x, "rate"), inherits(y, "rate")),
    inherits(x, "rate") || length(x) == 2,
    inherits(y, "rate") || length(y) == 2,

    is.logical(crude),
    length(crude) == 1,
    !is.na(crude),

    length(SE.method) == 1,
    SE.method %in% list(TRUE, FALSE, "se_delta_method", "glm", "poisson.test")
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

  if (SE.method %in% list(TRUE, "se_delta_method")) {
    # @codedoc_comment_block news("popEpi::rate_ratio", "2026-08-17", "0.6.0")
    # Fixed the confidence interval formula for when `SE.method = TRUE`.
    # @codedoc_comment_block news("popEpi::rate_ratio", "2026-08-17", "0.6.0")
    # delta method for variance
    rr <- x[[1]] / y[[1]]
    rr_var <- (1 / x[[1]])^2 * x[[2]]^2 + (1 / y[[1]])^2 * y[[2]]^2
    z <- qnorm(p = 0.975)
    lo <- exp(log(rr) - sqrt(rr_var) * z)
    hi <- exp(log(rr) + sqrt(rr_var) * z)
    out <- data.frame(rate_ratio = rr, lower = lo, upper = hi)
  } else if (SE.method %in% list(FALSE, "poisson.test")) {
    out <- data.table::rbindlist(lapply(seq_along(x[[1]]), function(i) {
      pt <- stats::poisson.test(
        x = c(x[[1]][i], y[[1]][i]),
        T = c(x[[2]][i], y[[2]][i])
      )
      data.frame(
        rate_ratio = pt$estimate,
        lower = pt$conf.int[1],
        upper = pt$conf.int[2]
      )
    }))
  } else if (SE.method == "glm") {
    out <- rate_ratio_glm__(
      d1 = x[[1]],
      y1 = x[[2]],
      d2 = y[[1]],
      y2 = y[[2]]
    )
    data.table::set(x = out, j = "rr_se", value = NULL)
    data.table::setnames(
      out,
      old = c("rr", "rr_lo", "rr_hi"),
      new = c("rate_ratio", "lower", "upper")
    )
  }
  data.table::setDF(out)
  out <- round(out, 3)
  return(out)
}


prep.rate.input <- function(z, crude, SE) {
  # this one modulates input to rate_ratio function
  if ((is.vector(z) || is.list(z)) && length(z) == 2) {
    # z is obs and pyrs OR rate and SE
    return(list(z[[1]], z[[2]]))
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
