#' @title Confidence intervals for the ratio of two SIRs/SMRs
#' @author Matti Rantanen
#' @description Calculate ratio of two SIRs/SMRs and the confidence intervals of the ratio.
#' 
#' @details Function works with pooled sir-objects i.e. the \code{print} argument in \code{sir} is ignored.
#' Also \code{x} and \code{y} can be a vector of two where first index is the
#' observed cases and second is expected cases (see examples).
#' Note that the ratio of two SIR's is only applicable when the age distributions are similar
#' in both populations.
#'
#' \strong{Formula}
#' 
#' The observed number of first sir \code{O1} is considered as a Binomial variable with sample 
#' size of \code{O1+O2}. The confidence intervals for Binomial proportion \code{A} 
#' is solved using \code{exact} or \code{asymptotic} 
#' method. Now the CI for ratio \code{O1/O2} is \code{B = A/(1 - A)}. And further the CI for SIR/SMR 
#' is B*E2/E1. (Ederer and Mantel)
#' 
#' @param x a sir-object or a vector of two; observed and expected cases.
#' @param y a sir-object or a vector of two; observed and expected cases.
#' @param conf.level the type-I error in confidence intervals, default 0.95 for 95\% CI.
#' @param type How the binomial confidence intervals are calculated (default:) \code{exact} or \code{asymptotic}.
#' @param alternative The null-hypothesis test: (default:) \code{two.sided}, \code{less}, \code{greater}
#' @param digits number of digits in the output
#' 
#' @note
#' Parameter \code{alternative} is always \code{two.sided} when parameter 
#' \code{type} is set to \code{asymptotic}.
#' 
#' @examples 
#' ## Ratio for sir-object and the same values given manually:
#' 
#' 
#' ## create example dataset
#' dt1 <- data.frame(obs = rep(c(5,7), 10),
#'                   pyrs = rep(c(250,300,350,400), 5),
#'                   var = 1:20)
#' Ref <- data.frame(obs = rep(c(50,70,80,100), 5),
#'                  pyrs = rep(c(2500,3000,3500,4000), 5),
#'                  var = 1:20)
#' ## sir using the function
#' s1 <- sir(coh.data = dt1, coh.obs = obs, coh.pyrs = pyrs, 
#'           ref.data = Ref, ref.obs = obs, ref.pyrs = pyrs,
#'           adjust = var)
#'
#' ## Ratio is simply 1:
#' sir_ratio(s1, c(120, 150))
#' 
#' @seealso \code{\link{sir}}
#' \href{../doc/sir.html}{A SIR calculation vignette}
#' 
#' @references Statistics with Confidence: Confidence Intervals and Statistical Guidelines, Douglas Altman
#' 
#' @family sir_related
#' 
#' @return A vector length of three: sir_ratio, and lower and upper confidence intervals.
#' 
#' @export sir_ratio
#' 
#' @import data.table
#' @import stats


sir_ratio <- function(x, y, digits = 3, alternative = 'two.sided', 
                      conf.level = 0.95, type = 'exact') {
  # prepare input values: x
  # Tests are located in test_sir script.
  if(inherits(x = x, what = 'sir')){
    O1 <- x[[1]]$obs
    E1 <- x[[1]]$exp
  }
  else if(is.vector(x) && length(x) == 2) {
    O1 <- x[1]
    E1 <- x[2]
  }
  else{
    stop('Input x is not correct: x is neighter a vector of 2 nor sir-object')
  }
  # prepare y:
  if(inherits(y,'sir')){
    O2 <- y[[1]]$obs
    E2 <- y[[1]]$exp
  }
  else if(is.vector(y) && length(y) == 2) {
    O2 <- y[1]
    E2 <- y[2]
  }
  else{
    stop('Input y is not correct: y is neighter a vector of 2 nor sir-object')
  }
  
  type <- match.arg(type, c('asymptotic', 'exact'), several.ok = FALSE)
  alternative <- match.arg(alternative, c('two.sided','less', 'greater'), several.ok = FALSE)
  # conf.level
  
  p <- O1/(O1+O2)
  if(type == 'asymptotic') {
    alpha <- (1 - conf.level)/2
    Ex <- p + c(-qnorm(1-alpha),qnorm(1-alpha)) * sqrt((1/(O1+O2))*p*(1-p))
    if( alternative != 'two.sided') {
      message('Test changed to two.sided when asymptotic.')
      alternative <- 'two.sided'
    }
  }
  if(type == 'exact') {
    Ex <- binom.test(c(O1,O2), p = 0.5, alternative = alternative, conf.level = conf.level)$conf.int
  }
  B = Ex/(1-Ex)
  
  res <- round(c(sir_ratio = (O1/E1)/(O2/E2), lower=(B*(E2/E1))[1], upper = (B*(E2/E1))[2]), digits = digits)
  return(res)
}

