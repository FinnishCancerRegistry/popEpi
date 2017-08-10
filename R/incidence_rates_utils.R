

#' @title Confidence intervals for the rate ratios
#' @author Matti Rantanen
#' @description Calculate rate ratio with confidence intervals for rate objects or observations and person-years.
#' 
#' @details Calculate rate ratio of two age standardized rate objects (see \code{\link{rate}}). 
#' Multiple rates for each objects is supported if there are an equal number of rates. 
#' Another option is to set \code{x} and \code{y} as a vector of two.
#' \enumerate{
#'   \item rate and its standard error, and  set \code{SE.method = TRUE}.
#'   \item observations and person-year, and  set \code{SE.method = FALSE}.
#' }
#' See examples.
#' 
#' 
#' @param x a rate-object, vector of two; rate and standard error or observed and person-years.
#' @param y a rate-object, vector of two; rate and standard error or observed and person-years.
#' @param crude set TRUE to use crude rates; default is FALSE.
#' @param SE.method default TRUE; if \code{x} and \code{y} are vectors of observed and 
#' person-years, this must be changed to FALSE.
#' 
#' @examples 
#' \dontrun{
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
#' # manually set rates (0.003 and 0.005) and SEs (0.001 and 0.002)
#' # so that x = y = c('rate', 'SE')
#' rate_ratio(x= c(0.003, 0.001), y= c(0.005, 0.002), SE.method = TRUE) 
#' 
#' # observed numbers (10 and 20) and person-years (30000 and 40000):
#' rate_ratio(x = c(10, 30000), y = c(20, 40000), SE.method = FALSE)
#' 
#' @seealso \code{\link{rate}}
#' 
#' @family rate functions
#' 
#' @return A vector length of three: rate_ratio, and lower and upper confidence intervals.
#' 
#' @export rate_ratio
#' 
#' @import data.table
#' @import stats
rate_ratio <- function(x, y, crude = FALSE, SE.method = TRUE) {
  if( inherits(x, 'rate') | inherits(y, 'rate') ) {
    if(!crude & (!'rate.adj' %in% names(x) | !'rate.adj' %in% names(y))) {
      crude <- TRUE
      message('Crude rates used')
    }
  }
  
  x <- prep.rate.input(x, crude = crude, SE = SE.method)
  y <- prep.rate.input(y, crude = crude, SE = SE.method)
  
  if(SE.method) {
    ratio <- x[[1]]/y[[1]]
    
    # delta method for variance
    v0 <-  (1/x[[1]])^2*x[[2]]^2 + (1/y[[1]])^2*y[[2]]^2
    
    
    lo <- ratio - v0*1.96 #exp(log(ratio)-log(v0)*1.96)
    hi <- ratio + v0*1.96 #exp(log(ratio)+log(v0)*1.96)
    out <- round(data.frame(rate_ratio = ratio, lower = lo, upper = hi), 3)
  }
  else {
    # x and y vector of two:, pyrs
    pt <- list()
    out <- data.frame()
    j <- 1
    for(j in 1:length(x[[1]])) {
      pt[[j]] <- poisson.test(x = c(x[[1]][j], y[[1]][j]), T = c(x[[2]][j],y[[2]][j]))
      out <- rbind(out, round(data.frame(rate_ratio = pt[[j]]$estimate, 
                                         lower = pt[[j]]$conf.int[1], 
                                         upper = pt[[j]]$conf.int[2]),3) )
    }
  }
  if(any(out<0)) {
    warning('Negative estimate or confidence intervals. Tip: set SE.method to FALSE when using observations and person-years.')
  }
  return(out)
}



prep.rate.input <- function(z, crude, SE) {
  # this one modulates input to rate_ratio function
  if(is.vector(z) && length(z) == 2) {
    # z is obs and pyrs OR rate and SE
    return(list(z[1], z[2]))
  }
  else if(inherits(z,'rate')){
    if(!SE) { # obs and pyrs
      att <- attributes(z)
      setDT(z)
      a <- z[, get(att$rate.meta$obs)]
      b <- z[, get(att$rate.meta$pyrs)]
    }
    else {
      if(crude) {
        a <- z[,rate]
        b <- z[,SE.rate]
      } 
      else {
        # z is a rate object
        a <- z[,rate.adj]
        b <- z[,SE.rate.adj]
      }
    }
  }
  else{
    stop('Input is not correct: its neighter a vector of two nor a rate object')
  }
  return(list(a,b))
}

  
  