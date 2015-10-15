#' @title Standardised incidence/mortality rates (direct method)
#' @author Matti Rantanen, Joonas Miettinen
#'
#' @description \code{rate} calculates adjusted rates using
#' preloaded weights data or user specified weights.
#'
#' @param data aggregated data (see: \code{\link{lexband}})
#' @param pyrs person-years variable name in data, quoted or unquoted
#' @param obs observations variable name in data, quoted or unquoted.
#' @param adjust variable for adjusting the rates, quoted or unquoted
#' @param print variable name to stratify the rates. Vector or a list. Functions
#' can be in list, \code{list( year.cat := cut(year, c(1990,2000,2010)) )}
#' @param weight.data character string; select standard population: 
#' \code{'nordic', 'world66_5','europe', 'world00_1', 'world00_18of5', 'world00_20of5'} or \code{'cohort'}
#' If NULL, the weights are fetched from \code{weights}.
#' @param weights user defined weights in a list. length of the list should 
#' equal to variable levels in \code{adjust}. See examples.  (currenty only one dimension supported)
#' @param subset a logical expression to subset data.
#' 
#' @details Input data needs to be in aggregated format with observations 
#' and person-years. For individual data use \code{\link{lexband}}. Or
#' \code{\link{ltable}} and merge person-years manually.
#' 
#' \strong{Weights}
#' 
#' User can set weights in a named list. These weights are merged with unique
#' levels of variable in \code{adjust}. \strong{Multiple weights support coming soon.}
#' 
#' \strong{Standard populations}
#' 
#' Rates can be standardized either by preloaded standard
#' population or cohorts person-years. For the world standard population, 
#' four possibilities are currently available:
#' The old world standard population from 1966 (\code{'world66_5'}), 
#' the new equivalent from 2000 with 18 or 20 age groups
#'  (\code{'world00_18of5'} or \code{'world00_20of5'}) 
#' and the 1-year age groups (\code{'world00_1'}).
#' 
#' Differences between rates using the old and new population standards will 
#' arise due to the shift in world population - the proportion of young people 
#' has fallen. The new standard is arguably more useful when interpretability 
#' and generalizability are concerned. However, the old standard is used still 
#' in many contexts in order to assure comparability with old results.
#' 
#' Available age group standardization options:
#' \itemize{
#' \item 'europe' - european std. popupulation, 18 age groups
#' \item 'nordic' - nordic std. popupulation, 18 age groups
#' \item 'world66_5' - world 1966 standard, 18 age groups
#' \item 'world00_18of5' - world 2000 standard, 18 agegroups
#' \item 'world00_20of5' - world 2000 standard, 20 agegroups 
#' \item 'world00_1' - world 2000 standard, 101 agegroups
#' \item 'cohort' - weights are calculated from cohort person-years. Same 
#' age specific weights are applied for each stratum, i.e. weights are 
#' aggreagated from the whole cohort.
#' }
#'
#' Age groups are coded in integer values 1-18, 1-20 and 1-101. Data is 
#' derived from datasets included in \pkg{popEpi}: \code{\link{stdpop18}} and 
#' \code{\link{stdpop101}}.
#' 
#' Note that age group variable should naturally have a correct number of 
#' levels for the rate you want to calculate.
#' 
#' @return Returns a \code{data.table} with observations, person-years, rates and
#' adjusted rates, if availble. Results are stratified by \code{print}.
#' Adjusted rates are identified with suffix \code{.adj} and  
#' \code{.lo} and \code{.hi} are for confidence intervals lower and upper 
#' 95\% bounds, respectively.
#' The prefix \code{SE.} stands for standard error.
#'  
#' 
#' @references
#' Source of the Nordic standard population in 5-year age groups (also contains European & 1966 world standards):
#' \url{http://www-dep.iarc.fr/NORDCAN/english/glossary.htm}
#' 
#' A discussion (and source) of the old European standard population: 
#' \url{http://epp.eurostat.ec.europa.eu/cache/ITY_OFFPUB/KS-RA-13-028/EN/KS-RA-13-028-EN.PDF}
#' 
#' A comparison of the 1966 vs. 2000 world standard populations in 5-year age groups:
#' \url{http://www3.ha.org.hk/cancereg/e_asr.asp}
#' 
#' Source of 2000 world standard population in 1-year age groups:
#' \url{http://seer.cancer.gov/stdpopulations/stdpop.singleages.html}
#' 
#' @seealso \code{\link{lexpand}}, \code{\link{ltable}}
#' 
#' @examples 
#' ## Prepare data with lexband and then reformat agegroup.
#' x <- lexpand(sibr, birth = bi_date, entry = dg_date, exit = ex_date,  
#'              breaks = list(per = c(1990,2000,2010,2020), age = c(0:17*5,Inf)),
#'              aggre = list(agegroup = age, year.cat = per),
#'              status =  status != 0)
#'
#' x$agegroup <- findInterval(x$agegroup,  c(0:17*5,Inf))
#'
#' ## calculate rates for selected periods with Nordic 2000 weights:
#' r1 <- rate( data = x, obs = from0to1, pyrs = pyrs, print = year.cat, 
#'             adjust = agegroup, weight.data = 'nordic')
#' r1
#'
#' r2 <- rate( data = x, obs = from0to1, pyrs = pyrs, print = year.cat, 
#'             adjust = agegroup, weight.data = NULL,
#'             weights = list( agegroup = c(4,5,5,5,5,4,4,3,3,2,2,1,1) ))
#' r2
#'
#' @import data.table
#' @export rate

rate <- function( data,
                  obs,
                  pyrs,
                  print = NULL,
                  adjust = NULL, 
                  weights = NULL,
                  weight.data = 'world66_5',
                  subset = NULL
) {
  data <- copy(data)
  setDT(data)
  ## subsetting ------------------------------------------------------------
  ## no copy taken of data!
  subset <- substitute(subset)
  subset <- evalLogicalSubset(data = data, substiset = subset)
  data <- data[subset,]
  
  # evalPopArg
  obs <- substitute(obs)
  inc.obs <- evalPopArg(data = data, arg = obs)
  obs <- names(inc.obs)
  
  pyrs <- substitute(pyrs)
  inc.pyr <- evalPopArg(data = data, arg = pyrs)
  pyrs <- names(inc.pyr)
  
  print <- substitute(print)
  inc.pri <- evalPopArg(data = data, arg = print)
  print <- names(inc.pri)
  
  adjust <- substitute(adjust)
  inc.adj <- evalPopArg(data = data, arg = adjust)
  adjust <- names(inc.adj)
  
  ## collect data
  data <- cbind(inc.obs, inc.pyr)
  if(!is.null(print))  data <- cbind(data, inc.pri) 
  if(!is.null(adjust)) data <- cbind(data, inc.adj)
  
  
  if(missing(weights)) weights <- NULL
  
  ## user specified weights
  if (popArgType(substitute(weights)) == 'list' ) {
    if (length(names(inc.adj)) > 1 | length(eval(weights)) > 1  ) { 
      stop('Only one weigth currently supported')
    }
    map <- cbind(as.data.table(eval(weights)), unique(inc.adj))
    setnames(map, 1, 'Reference.Weights')
    
    weights <- 'Reference.Weights'
    data <- merge(data, map, by = adjust)
  }

  # merge WHO std data
  data <- rate_table(data = data, 
                     obs = obs,
                     pyrs = pyrs,
                     adjust = adjust,
                     print = print,
                     weight.data = weight.data,
                     weights = weights)
  data <- rate_est(data = data,
                   obs = obs,
                   pyrs = pyrs,
                   print = print,
                   weights = 'reference')
  # class
  setattr(data, 'class',c('rate','pe',class(data)))
  
  # data.frame output option  
  if (!getOption("popEpi.datatable")) {
    setDFpe(data)
  }
  
  return(data)
}



stdr.weights <- function(wp = 'world00_1'){
  
  ## This one returns the standard population
  ## output: data.table with colnames: agegroup, reference
  ## standard populations are from datasets: stdpop18 and stdpop101
  if (length(wp) > 1) {
    stop('Standard population name is not a scalar.')
  }
  else if (wp %in% c('world66_5','europe','nordic')){
    # get standard pop
    sr <- data.table(stdpop18)
    setnames(sr, 1:4, c('agegroup','world66_5','europe','nordic'))
    sr[,agegroup := 1:18]
    sr[,colnames(sr)[which(!colnames(sr) %in% c('agegroup', wp))]:=NULL]
    setnames(sr, wp, 'reference')
  }
  else if (wp %in% c("world00_1","world00_20of5","world00_18of5") ){
    sr <- data.table(stdpop101)
    if (wp == "world00_18of5"){
      sr[,agegroup := cut(agegroup, breaks=c(0:17*5,Inf), right=FALSE, labels=FALSE)]
      sr <- sr[,list(world_std = sum(world_std)), by="agegroup"]
    }
    if (wp == 'world00_20of5'){
      sr[,agegroup := cut(agegroup, breaks=c(0:19*5,Inf), right=FALSE, labels=FALSE)]
      sr <- sr[,list(world_std = sum(world_std)), by="agegroup"]
    }
    else {
      sr <- sr[,list(world_std = sum(world_std)), by="agegroup"]
    }
    setnames(sr, "world_std", "reference")
  }
  else {
    stop("Invalid standard populaiton name.")
  }
  sr[]
}



rate_table <- function(data, 
                       obs = 'obs',
                       pyrs = 'pyrs',
                       adjust = NULL,
                       print = NULL,
                       weight.data = 'world66_5',
                       weights = NULL
){
  ## This one fetches and merges the standard population
  ## or transforms the population already in the data to standard format.
  
  colsum1 <- function(c) c/sum(c)
  # merge WHO weights to data
  # Everything should sum to one on each level of print
  data <- data.table(data)
  if (!is.null(weights) && all(weights %in% colnames(data)) ) {
    ## use predefined weights
    data[, reference := colsum1(.SD), .SDcols = weights, by = c(print)]
    data[, (weights) := NULL]
  }
  else if ( !is.null(adjust) ) { # add: if ( !is.null(adjust) )
    ## aggregate data before adding weights
    eval0 <- paste0('list(obs = sum(',obs,',na.rm=TRUE),pyrs = sum(',pyrs,', na.rm=TRUE))')
    eval0 <- parse(text = eval0)
    data <- data[, eval(eval0), by = c(adjust, print) ] 
    setnames(data, c('obs','pyrs'), c(obs, pyrs))
    stdr.list <- c('world66_5','europe','nordic',"world00_1",
                   "world00_20of5","world00_18of5")
    
    if ( !is.null(weight.data) && weight.data %in% stdr.list) {
      ## get preloaded WHO std data
      
      if(length(adjust) > 1) stop('Set only one variable name for indicating age group')

      wd <- stdr.weights(wp = weight.data)
      wd <- wd[, reference := colsum1(.SD), .SDcols = 'agegroup']
      setnames(wd, 'agegroup', adjust)
    }
  
    else if ( (is.null(weight.data) || weight.data=='cohort')  && is.null(weights)) {
      # get cohort std
      p1 <- paste0('sum(',pyrs,', na.rm=TRUE)')
      p1 <- parse(text = p1)
      wd <- data[,list( pyrs = eval(p1)), by = c(unique(adjust))]
       # expr.by.cj(data = data, by.vars = unique(adjust), expr = list(pyrs = sum(eval(p1))))
       # removed for easier syntax...
      wd[, reference := colsum1(.SD), .SDcols = 'pyrs']
      wd[,c('pyrs') := NULL]
    }
    else {
      return(data)
    }
    data <- merge(x  = data, y  = wd[, c('reference', adjust) , with=FALSE],
                  by = adjust, all.x = TRUE)
  }
  else {
    if(!is.null(print)) {
      data <- data[, list(obs = sum(get(obs), na.rm=TRUE), pyrs = sum(get(pyrs), na.rm=TRUE)), by = c(print) ]
      setnames(data, c('obs','pyrs'), c(obs, pyrs))
    }
  }
  return(data)
}


rate_est <- function(data = data, 
                     obs = 'obs', 
                     pyrs = 'pyrs', 
                     print = NULL, 
                     weights = NULL
) {
  ## This one estimates the rates and calculates CI's and SE's.
  
  data <- data.table(data)
  if( is.null(weights) |  !weights %in% colnames(data)) {
    weights <- NULL
  }
  
  if (all(!is.null(weights), !is.null(obs), !is.null(pyrs))) {
    # rate.adj
    
    f2 <- function(list) list[[1]]/list[[2]]*list[[3]]
    funx <- function(n,d,w,fun)  eval(parse(text=fun))
    

    # variance rate.adj for each strata A
    fun1 <- '(d/n^2) * w^2'
    fun2 <- 'd / n * w'
    
    make_fun <- function(n = NA, d = NA, w = NA, fun) {
      fun <- gsub(pattern = "n", replacement = n, x = fun)
      fun <- gsub(pattern = "d", replacement = d, x = fun)
      fun <- gsub(pattern = "w", replacement = w, x = fun)
      parse(text = fun)
    }
    eval.me1 <- make_fun(d = obs, n = pyrs, w=weights, fun = fun1)
    eval.me2 <- make_fun(d = obs, n = pyrs, w=weights, fun = fun2)
    data[, var.temp := eval(eval.me1)]
    data[, lam.temp := eval(eval.me2)]
    # add std weighted rates and variances
    #data[, ':='(var.temp = funx(d=get(obs), n=get(pyrs), w=get(weights), fun = fun1),
    #            lam.temp = funx(d=get(obs), n=get(pyrs), w=get(weights), fun = fun2)) ]
    data[, rate.adj := f2(.SD), .SDcols= c(obs, pyrs, weights)]

    # aggregate data
    ie <- paste0('list(', obs, '=sum(',obs,',na.rm=TRUE), ', pyrs, '=sum(',pyrs,',na.rm=TRUE),',
                 'rate.adj=sum(rate.adj,na.rm=TRUE),' ,'lam.temp=sum(lam.temp,na.rm=TRUE), var.temp=sum(var.temp,na.rm=TRUE))') 
    l <- parse(text = ie)

    data <- data[, eval(l), by=c(print)]
    # rate.adj: S.E.
    data[, SE.rate.adj := exp( sqrt((1/lam.temp)^2 * var.temp)) ]
    # rate.adj: CI
    data[, ':='(rate.adj.lo = exp(log(rate.adj)-log(SE.rate.adj)*1.96),
                rate.adj.hi = exp(log(rate.adj)+log(SE.rate.adj)*1.96)) ]
    data[,c('lam.temp','var.temp') := NULL]
  }
  
  else {
    ie <- paste0('list(', obs, '=sum(',obs,'), ', pyrs, '=sum(',pyrs,'))') 
    l <- parse(text = ie)
    data <- data[, eval(l), by=print]
  }
  ia <- paste0('rate := ',obs,'/', pyrs)
  k <- parse(text = ia)
  data[, eval(k), by = print]
  eval.me3 <- paste('exp(1/',obs,')')
  eval.me3 <- parse(text = eval.me3)
  data[, SE.rate := eval(eval.me3)]
  data[, ':='(rate.lo = exp(log(rate)-log(SE.rate)*1.96),
              rate.hi = exp(log(rate)+log(SE.rate)*1.96)) ]
  return(data[])
}


