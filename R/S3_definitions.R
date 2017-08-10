

#' @export
print.sir <- function(x, subset = NULL, ...) {
  
  at <- attributes(x)$sir.meta
  PF <- parent.frame(1L)
  subset <- evalLogicalSubset(x, substitute(subset), enclos = PF)
  x <- x[subset, ]
  setDT(x)
  
  t1 <- paste0("SIR (adjusted by ", paste(at$adjust, collapse = ', '),')',
               ' with ', at$conf.level*100, '% ', 'confidence intervals (', at$conf.type,')')
  
  
  # cat
  t3 <- paste0(' Total sir: ', round(at$pooled.sir$sir,2),' (', 
               round(at$pooled.sir$sir.lo,2),'-', round(at$pooled.sir$sir.hi, 2),')\n',
               ' Total observed: ', at$pooled.sir$observed, '\n',
               ' Total expected: ', round(at$pooled.sir$expected,2), '\n',
               ' Total person-years: ', round(at$pooled.sir$pyrs))

  rv <- intersect(names(x), c('sir','sir.lo','sir.hi','observed','expected','pyrs'))
  if (length(rv)) {
    x[, (rv) := lapply(.SD, round, digits = 2L),  .SDcols = rv]
  }
  
  rv <- intersect(names(x), c('p_value'))
  if (length(rv)) {
    x[, (rv) := lapply(.SD, round, digits = 4),  .SDcols = rv]
  }

  
  if(is.null(at$lrt.test)) {
    d <- paste("Could not test", at$lrt.test.type)
  } else {
    if(at$lrt.test.type == 'homogeneity') {
      d <- paste("Test for homogeneity: p", p.round( c(at$lrt.test)))
    }
    if(at$lrt.test.type == 'trend') {
      d <- paste("Test for trend: p", p.round( c(at$lrt.test)))
    }
  }
  #b <- round(c(ta$total$sir, ta$total$sir.lo, ta$total$sir.hi), 2)
  # cat('\n',"Total observed", ta$total$observed, '\n',
  #     "Total expected:", ta$total$expected, '\n',
  #     "SIR:", paste0(b[1], ' (',b[2], '-',b[3],')'), '\n',
  #     "Person-years:", ta$total$pyrs, '\n',
  #     fill=TRUE)
  
  cat(t1, '\n')
  if(x[,.N] > 1) {
    cat(d, '\n')
  }
  cat(fill=TRUE)
  cat(t3, '\n', fill=TRUE)  
  
  print(data.table(x), ...)
  return(invisible())
}

#' @export 
`[.sir` <- function(x, ...) {
  y <- NextMethod()
  if (is.data.frame(y)) {
    setattr(y, "class", class(x))
    setattr(y, "sir.meta", attr(x, "sir.meta"))
  }
  y
}


#' @import grDevices
#' @export
print.sirspline <- function(x, ...) {
  if ( x$spline.dependent ) {
    if( any( !is.na(x$p.values))) {
      cat( 'global p-value:', p.round(x$p.values[1]),'\n' )
      cat( 'level p-value:', p.round(x$p.values[2]) , fill= TRUE)      
    } else {
      cat( 'No models compared.', fill= TRUE)
    }
    cat('---', '\n')
    cat('Colour codes:', '\n', fill=TRUE)
  } else {
    
    for(i in 1:length(x$p.values)) {
      cat( x$spline[i] ,': p ', p.round( x$p.values[[i]] ), '\n', sep = '')
    }
    cat(fill=TRUE)
    
  }
  # Print colour codes:
  cols <- unique(x$spline.est.A[,1])
  col.length <- length(cols)
  print( data.frame(levels = cols, colour = palette()[1:col.length]), include.rownames = FALSE)
  
  # Print p-values
  return(invisible())
}
    

#' Plot method for sir-object
#' 
#' Plot SIR estimates with error bars
#' 
#' @seealso \code{\link{sir}},  \code{\link{sirspline}}
#' 
#' @import graphics
#' 
#' @author Matti Rantanen
#' 
#' @param x an object returned by function \code{sir}
#' @param conf.int default TRUE draws confidence intervals
#' @param xlab overwrites default x-axis label
#' @param ylab overwrites default y-axis label
#' @param xlim x-axis minimum and maximum values
#' @param main optional plot title
#' @param abline logical; draws a grey line in SIR = 1
#' @param log logical; SIR is not in log scale by default
#' @param eps error bar vertical bar height (works only in 'model' or 'univariate')
#' @param left.margin adjust left marginal of the plot to fit long variable names
#' @param ... arguments passed on to plot(), segment and lines()
#' 
#' 
#' @details Plot SIR estimates and confidence intervals 
#' \itemize{
#'  \item univariate - plots SIR with univariate confidence intervals
#'  \item model - plots SIR with Poisson modelled confidence intervals
#' }
#' 
#' \strong{Customize}
#' Normal plot parameters can be passed to \code{plot}. These can be a vector when plotting error bars:
#' \itemize{
#'  \item \code{pch} - point type
#'  \item \code{lty} - line type
#'  \item \code{col} - line/point colour 
#'  \item \code{lwd} - point/line size
#' }
#'
#' \strong{Tips for plotting splines}
#' It's possible to use \code{plot} to first draw the 
#' confidence intervals using specific line type or colour and then plotting 
#' again the estimate using \code{lines(... , conf.int = FALSE)} with different 
#' settings. This works only when \code{plot.type} is 'splines'.
#' 
#' 
#' @examples 
#' \dontrun{
#' # Plot SIR estimates
#'# plot(sir.by.gender, col = c(4,2), log=FALSE, eps=0.2, lty=1, lwd=2, pch=19,  
#'#      main = 'SIR by gender', abline=TRUE)
#' }
#' @export

plot.sir <- function(x, conf.int = TRUE, ylab, xlab, xlim, main, 
                     eps=0.2, abline = TRUE, log = FALSE, left.margin, ...) {
  
  a <- data.table(x)
  at <- attributes(x)$sir.meta
  level_names <- at$print
  
  if(is.null(level_names)) {
    levels <- 'Crude'
    level_names <- levels
  }
  else {
    q <- paste0('paste(', paste(level_names, collapse=', '),', sep = ":")' )
    q <- parse(text = q)
    levels <- a[, eval(q)]
  }
  
  # predefined parameters
  if( missing(main) ){
    main <- NA
  }
  if( missing(xlab) ){
    xlab <- 'SIR'
  }
  if( missing(ylab) ){
    ylab <- NA
  }
  if( missing(xlim) ) {
    xlimit <- c(min(a$sir.lo[a$sir.lo <Inf]), max(a$sir.hi[a$sir.hi <Inf]))
    xlimit <- xlimit + diff(xlimit)*c(-.3,.3)
  } 
  else {
    xlimit <- xlim
  }
  
  # par options
  old.margin <- par("mar")
  new.margin <- old.margin
  if(missing(left.margin)) {
    new.margin[2] <- 4.1 + sqrt( max(nchar(as.character(level_names))) )*2
  } 
  else {
    new.margin[2] <- left.margin
  }
  par(mar = new.margin)
  
  # plot frame, estimates and CI (optional abline)
  logarithm <- ''
  if(log){
    logarithm <- 'x'
    if(xlimit[1]==0) xlimit[1] <- xlimit[1] + 0.01
  }
  y.axis.levels <- 1:length(levels)
  plot(c(xlimit), c(min(y.axis.levels)-0.5, max(y.axis.levels)+0.5), 
       type='n', yaxt = 'n', xlab=xlab, ylab=ylab, log=logarithm, main = main, ...)
  axis(side = 2, at = y.axis.levels, labels = levels, las=1)
  
  if(abline) {
    abline(v=1, col = 'darkgray')
  }
  
  points(a$sir, factor(y.axis.levels, labels=levels), ...) 
  if(conf.int) {
    segments(a$sir.lo, y.axis.levels , a$sir.hi, y.axis.levels, ...)
    segments(a$sir.lo, y.axis.levels - eps, a$sir.lo, y.axis.levels +eps, ... )
    segments(a$sir.hi, y.axis.levels - eps, a$sir.hi, y.axis.levels +eps, ... )
  }
  
  # reset margins
  par(mar = old.margin)
}

#' @title \code{plot} method for sirspline-object
#' 
#' @description Plot SIR splines using R base graphics.
#' 
#' 
#' @import graphics
#' 
#' @author Matti Rantanen
#' 
#' @param x an object returned by function sirspline
#' @param conf.int logical; default TRUE draws also the 95 confidence intervals
#' @param xlab overwrites default x-axis label; can be a vector if multiple splines fitted
#' @param ylab overwrites default y-axis label; can be a vector if multiple splines fitted
#' @param log logical; default FALSE. Should the y-axis be in log scale
#' @param abline logical; draws a reference line where SIR = 1
#' @param type select \code{type = 'n'} to plot only figure frames 
#' @param ... arguments passed on to plot()
#' 
#' @details
#' In \code{plot.sirspline} almost every graphical parameter are user
#' adjustable, such as \code{ylim}, \code{xlim}.
#' \code{plot.sirsplines} calls \code{lines.splines} to add lines.
#' 
#' The plot axis without lines can be plotted using option \code{type = 'n'}. 
#' On top of the frame it's then possible to add a \code{grid}, 
#' \code{abline} or text before plotting the lines (see: \code{sirspline}).
#' @export
#' @family sir functions
plot.sirspline <- function(x, conf.int=TRUE, abline = TRUE, log = FALSE, type, ylab, xlab,  ...) {

  #print(list(...))
  
  ## premilinary checks  
  if (is.null(x$spline.seq.A)) stop('No splines found.')
  
  ## prepare dimension and par
  plotdim <- as.numeric(c( !is.null( x$spline.seq.A ),
                           !is.null( x$spline.seq.B ),
                           !is.null( x$spline.seq.C ) ))
  if(sum(plotdim) > 1) {
    save_par <- par(no.readonly = TRUE)
    par(mfrow=c(1,sum(plotdim)))
    type <- 'l'
  }
  
  ## set labels
  if ( missing(xlab) ) {
    xlab <- x$spline
  }
  
  if ( missing(ylab) ) {
    ylab <- rep('SIR',sum(plotdim))
    if(log){
      ylab <- rep('log(SIR)', sum(plotdim))
    }
    if(x$spline.dependent & sum(plotdim) > 1) {
      ylab <- c(ylab[1], paste(ylab[2:sum(plotdim)], 'ratio'))
    }
  }
  else{
    if( length(ylab) < sum(plotdim))
      ylab <- rep(ylab, sum(plotdim))
    if(length(ylab) > sum(plotdim)) {
      warning('set ylabs in a vector length of num of plots (',sum(plotdim),')')
    }
  }
  
  ## set scale
  if(!is.logical(log)) stop('log should be a logical value.')
  log.bin <- ifelse(log, 'y', '')
  
  ## remove infinite values
  #rm_inf <- function(est){
  #  x[[est]][ is.finite(x[[est]][[2]]) & is.finite(x[[est]][[3]]) & is.finite(x[[est]][[4]]), ]
  #}
  
  spl <- c('spline.seq.A', 'spline.seq.B', 'spline.seq.C')[1:sum(plotdim)]
  est <- gsub("seq", "est", spl)
  
  for (i in 1:sum(plotdim)) {  # age, per, fot, 
    # empty plot
    max_x <- range(x[[spl[i]]])
    max_y <- range( x[[est[i]]][, 2:4] )
    plot(max_x, max_y, type = 'n', ylab = ylab[i], xlab = xlab[i], log = log.bin, ...) 
    if(abline) abline(h = 1)
    
    # plot lines
    if (missing(type) || type != 'n') {
      lines.sirspline(x, conf.int = conf.int, select.spline = i, ...)
    }
  }
  if(sum(plotdim) > 1) {
    par(save_par)
  }
  return(invisible())
}



#' @title lines method for sirspline-object
#' @description Plot SIR spline lines with R base graphics
#' 
#' 
#' @author Matti Rantanen
#' 
#' @param x an object returned by function sirspline
#' @param conf.int logical; default TRUE draws also the 95 confidence intervals
#' @param print.levels name(s) to be plotted. Default plots all levels.
#' @param select.spline select which spline variable (a number or a name) is plotted.
#' @param ... arguments passed on to lines()
#' 
#' @details  In \code{lines.sirspline} most of graphical parameters is user 
#' adjustable.
#' Desired spline variable can be selected with \code{select.spline} and only one
#' can be plotted at a time. The spline variable can include 
#' several levels, e.g. gender (these are the levels of \code{print}
#' from \code{sirspline}). All levels are printed by default, but a
#' specific level can be selected using argument
#' \code{print.levels}. Printing the levels separately enables  e.g. to
#' give different colours for each level.
#' 
#' @family sir functions
#' 
#' @import graphics
#' @export
lines.sirspline <- function(x, conf.int = TRUE, print.levels = NA, select.spline, ... ){
  ## input: sirspline object, with only one spline var (spline.est.A)
  ## input: print levels can be > 1.
  
  ## subset splines
  if( length(x$spline) > 1 ) {
    if ( missing(select.spline) ) {
      stop(paste('select what spline to plot in select.spline:', paste(x$spline, collapse = ', ')))
    }
    else {
      if(is.numeric(select.spline)) {
        k <- select.spline
      }
      else {
        k <- which(x$spline == select.spline)
      }
      if(length(k) == 0 | length(x$spline) < k) stop('select.spline name/number is incorrect')
    }
  } 
  else {
    k <- 1
  }
  
  spl <- c('spline.seq.A', 'spline.seq.B', 'spline.seq.C')[k]
  est <- gsub("seq", "est", spl)
  
  ## remove infinite values
  # x[[h]] <- rm_inf(est=h)
  
  # get print levels
  if(missing(print.levels)) {
    print.levels <- NA
  }
  pl <- unique(x$spline.est.A[,1])
  if(any( is.null(print.levels), is.na(print.levels))) {
    print.levels <- pl
  }
  pl <- pl[ pl %in% print.levels]
  
  ## get conf.int
  if( !is.logical(conf.int) ) stop('conf.int is not logical')
  n <- c(2,4)[c(!conf.int, conf.int)]
  
  
  ## draw lines
  for( l in pl ){
    # loop through print.levels
    index <- which(x$spline.est.A$i == l)
    
    for(m in 2:n) {
      # loop through estiamte and confidence intervals
      lines(x = x[[spl]], y = x[[est]][index, m], ...)
    }
  }
}

#' @title Print an rate object
#' @author Matti Rantanen
#' @description Print method function for \code{rate} objects; see
#' \code{\link{rate}}.
#' @param x an \code{rate} object
#' @param subset a logical condition to subset results table by
#' before printing; use this to limit to a certain stratum. E.g.
#' \code{subset = sex == "female"}
#' @param ... arguments for data.tables print method, e.g. row.names = FALSE suppresses row numbers.
#' @export
print.rate <- function(x, subset = NULL, ...) {
  
  ra <- attributes(x)$rate.meta
  PF <- parent.frame(1L)
  TF <- environment()
  subset <- evalLogicalSubset(x, substitute(subset), enclos = PF)
  x <- x[subset, ]
  
  # pre texts:
  cat('\n')
  if(!is.null(ra$adjust)){
    if(is.character(ra$weights)) {
      a <- paste(ra$weights, collapse = ',')
    }
    if(all(is.numeric(ra$weights))) {
      a <- length(ra$weights)
    }
    if(is.list(ra$weights)) {
      a <- sapply(ra$weights, length)
    }
    
    b <- paste(ra$adjust,a, collapse = ', ', sep = '; ')
    cat('Adjusted rates (', b,') ', sep = '')
  }
  else{
    cat('Crude rates ')
  }
  cat('and', '95%', 'confidence intervals:', fill=TRUE)
  cat('\n')
  # table itself

  
  setDT(x)
  print(x, ...)
}


#' @title plot method for rate object
#' @description Plot rate estimates with confidence intervals lines using R base graphics
#' @author Matti Rantanen
#' 
#' @param x a rate object (see \code{\link{rate}})
#' @param conf.int logical; default TRUE draws the confidence intervals
#' @param eps is the height of the ending of the error bars
#' @param left.margin set a custom left margin for long variable names. Function
#' tries to do it by default.
#' @param xlim change the x-axis location
#' @param ... arguments passed on to graphical functions points and segment 
#' (e.g. \code{col}, \code{lwd}, \code{pch} and \code{cex})
#' 
#' @details This is limited explanatory tool but most graphical 
#' parameters are user adjustable. 
#' 
#' @import graphics
#' @export
plot.rate <- function(x, conf.int = TRUE, eps = 0.2, left.margin, xlim, ...) {
  
  ra <- attributes(x)$rate.meta
  varcol <- ra$print
  
  if(is.null(varcol)) {
    lvl.name <- 'Crude'
  }
  else {
    pp <- paste0('paste(', paste(varcol, collapse=','),',sep = ":")')
    q <- parse(text=pp)
    lvl.name <- x[,eval(q)]
  }
  lvls <- 1:length(lvl.name)
  
  # WHICH RATE:
  if('rate.adj' %in% names(x)) {
    r <- x$rate.adj
    hi <- x$rate.adj.hi
    lo <- x$rate.adj.lo
  }
  else {
    r <- x$rate
    hi <- x$rate.hi
    lo <- x$rate.lo
  }
  # X-AXIS LIMITs
  if(missing(xlim)) {
    t <- range(na.omit(c(lo , r, hi)))
    t0 <- (t[2]-t[1])/4
    xlimit <- c(pmax(t[1]-t0, 0), t[2] + t0)
  } 
  else {
    xlimit <- xlim
  }
  
  # MARGINS  
  if(missing(left.margin)) {
   old.margin <- new.margin <- par("mar")
   new.margin[2] <- 4.1 + sqrt( max(nchar(as.character(lvl.name))) )*2
  }
  else {
   new.margin[2] <- left.margin
  }
  par(mar = new.margin)
  
  plot(c(xlimit), c(min(lvls)-0.5, max(lvls)+0.5), type='n', yaxt = 'n', ylab = '', xlab='')
  axis(side = 2, at = lvls, labels = lvl.name, las = 1)
  points(r, lvls, ...)
  
  if(conf.int) {
    segments(lo, lvls, hi, lvls, ...)
    segments(lo, lvls - eps, lo, lvls + eps, ...)
    segments(hi, lvls - eps, hi, lvls + eps, ...)
  }
  par(mar = old.margin) # revert margins
}


#' @export 
print.yrs <- function(x, ...) {
  print(as.numeric(x))
}


#' @export 
`[.yrs` <- function(x, ...) {
  yl <- attr(x, "year.length")
  structure(NextMethod(), year.length = yl, class = c("yrs", "numeric"))
}


#' @export 
`[.aggre` <- function(x, ...) {
  xa <- attributes(x)
  y <- NextMethod()
  if (is.data.frame(y)) {
    setattr(y, "class", xa$class)
    setattr(y, "aggre.meta", xa$aggre.meta)
    setattr(y, "breaks", xa$breaks)
  }
  y
}

#' @export 
subset.aggre <- function(x, ...) {
  y <- NextMethod()
  if (is.data.frame(y)) {
    setattr(y, "class", class(x))
    setattr(y, "aggre.meta", attr(x, "aggre.meta"))
    setattr(y, "breaks", attr(x, "breaks"))
  }
  y
}

preface_survtab.print <- function(x) {
  surv.int <- NULL ## APPEASE R CMD CHECK
  at <- attributes(x)$survtab.meta
  arg <- at$arguments
  
  cat("\n")
  cat("Call: \n", oneWhitespace(deparse(at$call)), "\n")
  cat("\n")
  cat("Type arguments: \n surv.type:", as.character(arg$surv.type), 
      "--- surv.method:", as.character(arg$surv.method))
  if (as.character(arg$surv.type) == "surv.rel")
    cat(" --- relsurv.method:", as.character(arg$relsurv.method))
  cat("\n \n")
  cat("Confidence interval arguments: \n level:", 
      as.character(arg$conf.level*100), "%")
  cat(" --- transformation:",
      as.character(arg$conf.type))
  cat("\n \n")
  cat("Totals:") 
  totCat <- paste0("\n person-time:", round(sum(x$pyrs)))
  if (arg$surv.method == "lifetable") {
    totCat <- paste0("\n at-risk at T=0: ", round(sum(x[surv.int == 1L]$n)))
  }
    
  cat(totCat)
  cat(" --- events:", sum(x$d))
  cat("\n \n")
  if (length(at$print.vars) > 0L) {
    cat("Stratified by:", paste0("'", at$print.vars, "'", collapse = ", "))
    if (length(at$adjust.vars) > 0L) cat(" --- ")
  }
  if (length(at$adjust.vars) > 0L) {
    cat("Adjusted by:", paste0("'", at$adjust.vars, "'", collapse = ", "))
  }
  cat("\n")
  invisible()
}


#' @title Print an aggre Object
#' @author Joonas Miettinen
#' @description Print method function for \code{aggre} objects; see
#' \code{\link{as.aggre}} and \code{\link{aggre}}.
#' @param x an \code{aggre} object
#' @param subset a logical condition to subset results table by
#' before printing; use this to limit to a certain stratum. E.g.
#' \code{subset = sex == "male"}
#' @param ... arguments passed to \code{print.data.table}; try e.g.
#' \code{top = 2} for numbers of rows in head and tail printed 
#' if the table is large, 
#' \code{nrow = 100} for number of rows to print, etc.
#' @export
print.aggre <- function(x, subset = NULL, ...) {
  
  PF <- parent.frame(1L)
  TF <- environment()
  sa <- attributes(x)$aggre.meta
  
  subset <- evalLogicalSubset(x, substitute(subset), enclos = PF)
  x <- x[subset, ]
  setDT(x)
  
  print(x, ...)
  
}

#' @title Summarize an aggre Object
#' @author Joonas Miettinen
#' @description \code{summary} method function for \code{aggre} objects; see
#' \code{\link{as.aggre}} and \code{\link{aggre}}.
#' @param object an \code{aggre} object
#' @param by list of columns to summarize by - e.g. \code{list(V1, V2)}
#' where \code{V1} and \code{V2} are columns in the data.
#' @param subset a logical condition to subset results table by
#' before summarizing; use this to limit to a certain stratum. E.g.
#' \code{subset = sex == "male"}
#' @param ... unused
#' @export
#' @family aggregation functions
summary.aggre <- function(object, by = NULL, subset = NULL, ...) {
  
  PF <- parent.frame(1L)
  TF <- environment()
  x <- object
  sa <- attributes(x)$aggre.meta
  
  subset <- evalLogicalSubset(x, substitute(subset), enclos = PF)
  x <- x[subset, ]
  setDT(x)
  
  bys <- substitute(by)
  bye <- evalPopArg(x, bys, enclos = environment(), types = c("list", "NULL"))
  
  vals <- sa$values
  vals <- intersect(names(x), vals)
  if (!length(vals)) {
    cat("No originally created value columns appear to be left in data.")
  }
  r <- x[, lapply(.SD, sum), by = eval(bye), .SDcols = vals]
  r
}

#' @title Print a survtab Object
#' @author Joonas Miettinen
#' @description Print method function for \code{survtab} objects; see
#' \code{\link{survtab_ag}}.
#' @param x a \code{survtab} object
#' @param subset a logical condition to subset results table by
#' before printing; use this to limit to a certain stratum. E.g.
#' \code{subset = sex == "male"}
#' @param ... arguments passed to \code{print.data.table}; try e.g.
#' \code{top = 2} for numbers of rows in head and tail printed 
#' if the table is large, 
#' \code{nrow = 100} for number of rows to print, etc.
#' @export
#' @family survtab functions
print.survtab <- function(x, subset = NULL, ...) {
  
  Tstart <- Tstop <- NULL ## APPEASE R CMD CHECK
  
  PF <- parent.frame(1L)
  TF <- environment()
  sa <- attributes(x)$survtab.meta
  
  subset <- evalLogicalSubset(x, substitute(subset), enclos = PF)
  x <- x[subset, ]
  
  preface_survtab.print(x)
  
  setDT(x)
  
  if (nrow(x) == 0L) {
    print(x)
    return(invisible())
  }
  
  pv <- as.character(sa$print.vars)
  if (length(pv) == 0L) pv <- NULL
  
  magicMedian <- function(x) {
    if (length(x) %% 2L == 0L) median(x[-1L], na.rm = TRUE) else
      median(x, na.rm = TRUE)
  }
  
  ## to avoid e.g. 'factor(V1, 1:2)' going bonkers
  pv_orig <- pv
  if (length(pv) > 0L) {
    pv <- makeTempVarName(x, pre = paste0("print_", 1:length(pv)))
    setnames(x, pv_orig, pv)
  }
  
  medmax <- x[, list(Tstop = c(magicMedian(c(min(Tstart),Tstop)), max(Tstop))), keyby = eval(pv)]
  
  setkeyv(medmax, c(pv, "Tstop"))
  setkeyv(x, c(pv, "Tstop"))
  x <- x[medmax]
  
  rv <- intersect(names(x), c(sa$est.vars, sa$CI.vars, sa$misc.vars))
  if (length(rv)) {
    x[, (rv) := lapply(.SD, round, digits = 4L),  .SDcols = rv]
  }
  
  sv <- intersect(names(x), sa$SE.vars)
  if (length(sv > 0L)) {
    x[, c(sv) := lapply(.SD, signif, digits = 4L), .SDcols = sv]
  }
  
  
  setcolsnull(x, keep = c(pv, "Tstop", sa$surv.vars), colorder = TRUE)
  if (length(pv)) setnames(x, pv, pv_orig)
  print(data.table(x), ...)
  invisible()
}

#' @title Summarize a survtab Object
#' @author Joonas Miettinen
#' @description Summary method function for \code{survtab} objects; see
#' \code{\link{survtab_ag}}. Returns estimates at given time points
#' or all time points if \code{t} and \code{q} are both \code{NULL}.
#' @param object a \code{survtab} object
#' @param t a vector of times at which time points (actually intervals that
#' contain t) to print summary table of survival function estimates by strata;
#' values not existing in any interval cause rows containing only \code{NAs} to
#' be returned. 
#' @param q a named \code{list} of quantiles to include in returned data set,
#' where names must match to estimates in \code{object};
#' returns intervals where the quantiles are reached first;
#' e.g. \code{list(surv.obs = 0.5)} finds the interval where \code{surv.obs}
#' is 0.45 and 0.55 at the beginning and end of the interval, respectively;
#' returns rows with \code{NA} values for quantiles not reached in estimates
#' (e.g. if \code{q = list(surv.obs = 0.5)} but lowest estimate is 0.6);
#' see Examples.
#' @param subset a logical condition to subset results table by
#' before printing; use this to limit to a certain stratum. E.g.
#' \code{subset = sex == "male"}
#' @param ... unused; required for congruence with other \code{summary} methods
#' 
#' @details
#' Note that this function returns the intervals and NOT the time points
#' corresponding to quantiles / estimates corresponding to time points.
#' If you want precise estimates at time points that are not interval breaks,
#' add the time points as breaks and re-estimate the survival time function.
#' In interval-based estimation, the estimates denote e.g. probability of 
#' dying \emph{during} the interval, so time points within the intervals
#' are not usually considered at all. See e.g. Seppa, Dyba, and Hakulinen 
#' (2015).
#' 
#' @references
#' Seppa K., Dyba T. and Hakulinen T.: Cancer Survival, 
#' Reference Module in Biomedical Sciences. Elsevier. 08-Jan-2015 
#' doi: 10.1016/B978-0-12-801238-3.02745-8.
#' 
#' @examples 
#' 
#' library(Epi)
#' library(survival)
#' 
#' ## NOTE: recommended to use factor status variable
#' x <- Lexis(entry = list(FUT = 0, AGE = dg_age, CAL = get.yrs(dg_date)), 
#'            exit = list(CAL = get.yrs(ex_date)), 
#'            data = sire[sire$dg_date < sire$ex_date, ],
#'            exit.status = factor(status, levels = 0:2, 
#'            labels = c("alive", "canD", "othD")), 
#'            merge = TRUE)
#' ## pretend some are male
#' set.seed(1L)
#' x$sex <- rbinom(nrow(x), 1, 0.5)
#' ## observed survival
#' st <- survtab(Surv(time = FUT, event = lex.Xst) ~ sex, data = x, 
#'                   surv.type = "cif.obs",
#'                   breaks = list(FUT = seq(0, 5, 1/12)))
#' 
#' ## estimates at full years of follow-up
#' summary(st, t = 1:5)
#' 
#' ## interval estimate closest to 75th percentile, i.e.
#' ## first interval where surv.obs < 0.75 at end
#' ## (just switch 0.75 to 0.5 for median survival, etc.)
#' summary(st, q = list(surv.obs = 0.75))
#' ## multiple quantiles
#' summary(st, q = list(surv.obs = c(0.75, 0.90), CIF_canD = 0.20))
#' 
#' ## if you want all estimates in a new data.frame, you can also simply do
#' 
#' x <- as.data.frame(st)
#' 
#' @export
#' @family survtab functions
summary.survtab <- function(object, t = NULL, subset = NULL, q = NULL, ...) {
  
  PF <- parent.frame(1L)
  at <- copy(attr(object, "survtab.meta"))
  subr <- copy(at$surv.breaks)
  
  if (!is.null(t) && !is.null(q)) {
    stop("Only supply either t or q.")
  } 
  
  sb <- substitute(subset)
  subset <- evalLogicalSubset(object, sb, enclos = PF)
  x <- object[subset, ]
  
  ## to avoid e.g. 'factor(V1, 1:2)' going bonkers
  pv_orig <- pv <- at$print.vars
  if (length(pv) > 0L) {
    pv <- makeTempVarName(x, pre = paste0("print_", 1:length(pv)))
    setnames(x, pv_orig, pv)
  }
  
  setDT(x)
  
  ## quantile detection --------------------------------------------------------
  if (!is.null(q)) {
    bn <- setdiff(names(q), at$est.vars)
    if (length(bn) > 0L) {
      stop("No survival time function estimates named ",
           paste0("'", bn, "'", collapse = ", "), 
           " found in supplied survtab object. Available ",
           "survival time function estimates: ", 
           paste0("'", at$est.vars, "'", collapse = ", "))
    }
    
    lapply(q, function(x) {
      if (min(x <= 0L) || max(x >= 1L)) {
        stop("Quantiles must be expressed as numbers between 0 and 1, ",
             "e.g. surv.obs = 0.5.")
      }
    })
    
    
    m <- x[, .SD[1, ], keyby = eval(pv)][, c(pv, "Tstop"), with = FALSE]
    setDF(m)
    
    rollVars <- makeTempVarName(x, pre = names(q))
    x[, c(rollVars) := lapply(.SD, copy), .SDcols = names(q)]
    
    m <- lapply(seq_along(q), function(i) {
      m <- merge(m, q[[i]])
      setnames(m, "y", rollVars[i])
      if (length(pv)) setorderv(m, pv)
      m[, c(pv, rollVars[i]), drop = FALSE]
    })
    names(m) <- names(q)
    
    l <- vector("list", length(q))
    names(l) <- names(q)
    for (k in names(q)) {
      
      l[[k]] <- setDT(x[m[[k]], on = names(m[[k]]), roll = 1L])
      
    }
    l <- rbindlist(l)
    set(l, j = rollVars, value = NULL)
    if (length(pv)) setkeyv(l, pv)
    x <- l
  }
  
  ## time point detection ------------------------------------------------------
  
  if (!is.null(t)) {
    
    tcutv <- makeTempVarName(x, pre = "cut_time_")
    
    set(x, j = tcutv, value = cut(x$Tstop, breaks = subr, right = T, 
                                  include.lowest = F))
    cutt <- cut(t, breaks = subr, right = T, include.lowest = F)
    
    l <- list(cutt)
    names(l) <- tcutv
    if (length(pv)) {
      pvdt <- setDF(unique(x, by = pv))[, pv, drop = FALSE]
      l <- setDT(merge(pvdt, as.data.frame(l)))
      setkeyv(l, pv)
    }
    
    x <- x[l, on = c(pv, tcutv)]
    
    set(x, j = tcutv, value = NULL)
    if (length(pv)) setkeyv(x, pv)
  }
  
  
  ## final touches -------------------------------------------------------------
  if (length(pv) > 0L) setnames(x, pv, pv_orig)
  
  if (!return_DT()) setDFpe(x)
  
  x
}

#' @export 
`[.survtab` <- function(x, ...) {
  y <- NextMethod()
  if (is.data.frame(y)) {
    setattr(y, "class", class(x))
    setattr(y, "survtab.meta", attr(x, "survtab.meta"))
  }
  y
}

#' @export 
subset.survtab <- function(x, ...) {
  y <- NextMethod()
  if (is.data.frame(y)) {
    setattr(y, "class", class(x))
    setattr(y, "survtab.meta", attr(x, "survtab.meta"))
  }
  y
}


#' @export 
`[.survmean` <- function(x, ...) {
  y <- NextMethod()
  if (is.data.frame(y)) {
    setattr(y, "class", class(x))
    setattr(y, "survmean.mean", attr(x, "survmean.mean"))
  }
  y
}


#' @export 
subset.survmean <- function(x, ...) {
  y <- NextMethod()
  if (is.data.frame(y)) {
    setattr(y, "class", class(x))
    setattr(y, "survmean.mean", attr(x, "survmean.mean"))
  }
  y
}

#' @export 
`[.rate` <- function(x, ...) {
  y <- NextMethod()
  if (is.data.frame(y)) {
    setattr(y, "class", class(x))
    setattr(y, "rate.meta", attr(x, "rate.meta"))
  }
  y
}

#' @export 
subset.rate <- function(x, ...) {
  y <- NextMethod()
  if (is.data.frame(y)) {
    setattr(y, "class", class(x))
    setattr(y, "rate.meta", attr(x, "rate.meta"))
  }
  y
}





prep_plot_survtab <- function(x, 
                              y = NULL, 
                              subset = NULL, 
                              conf.int = TRUE, 
                              enclos = parent.frame(1L), 
                              ...) {
  
  ## subsetting ----------------------------------------------------------------
  subset <- evalLogicalSubset(data = x, substiset = substitute(subset), 
                              enclos = environment())
  
  attrs <- attributes(x)
  
  if (!inherits(x, "survtab")) stop("x is not a survtab object")
  if (is.null(attrs$survtab.meta)) {
    stop("Missing meta information (attributes) in survtab object; ",
         "have you tampered with it after estimation?")
  }
  strata.vars <- attrs$survtab.meta$print.vars
  x <- copy(x)
  setDT(x)
  x <- x[subset, ]
  
  ## detect survival variables in data -----------------------------------------
  surv_vars <- c("surv.obs","CIF.rel","CIF_","r.e2","r.pp")
  wh <- NULL
  for (k in surv_vars) {
    wh <- c(wh, which(substr(names(x), 1, nchar(k)) == k))
  }
  surv_vars <- names(x)[wh]
  surv_vars <- surv_vars[!substr(surv_vars, nchar(surv_vars)-1, nchar(surv_vars)) %in% c("hi","lo")]
  if (length(surv_vars) == 0) {
    stop("x does not appear to have any survival variables; ",
         "did you tamper with it after estimation?")
  }
  
  
  ## getting y -----------------------------------------------------------------
  if (!is.null(y)) {
    if (!is.character(y)) {
      stop("please supply y as a character string indicating ",
           "the name of a variable in x")
    }
    if (length(y) > 1) stop("y must be of length 1 or NULL")
    if (!all_names_present(x, y, stops = FALSE)) {
      stop("Given survival variable in argument 'y' ",
           "not present in survtab object ('", y, "')")
    }
  } else {
    y <- surv_vars[length(surv_vars)]
    if (length(surv_vars) > 1L) message("y was NULL; chose ", y, " automatically")
  }
  rm(surv_vars)
  
  if (substr(y, 1, 3) == "CIF" && conf.int) {
    stop("No confidence intervals currently supported for CIFs. ",
         "Hopefully they will be added in a future version; ",
         "meanwhile use conf.int = FALSE when plotting CIFs.")
  }
  
  
  ## confidence intervals ------------------------------------------------------
  y.lo <- y.hi <- y.ci <- NULL
  if (conf.int) {
    
    y.lo <- paste0(y, ".lo")
    y.hi <- paste0(y, ".hi")
    y.ci <- c(y.lo, y.hi)
    
    badCIvars <- setdiff(y.ci, names(x))
    if (sum(length(badCIvars))) {
      stop("conf.int = TRUE, but missing confidence interval ",
           "variables in data for y = '", y, "' (could not detect ",
           "variables named", paste0("'", badCIvars, "'", collapse = ", ") ,")")
    }
    
  } 
  
  list(x = x, y = y, y.ci = y.ci, y.lo = y.lo, y.hi = y.hi, 
       strata = strata.vars, attrs = attrs)
  
}





#' \code{plot} method for survtab objects
#' 
#' Plotting for \code{survtab} objects
#' 
#' @import graphics
#' 
#' @author Joonas Miettinen
#' 
#' @param x a \code{survtab} output object
#' @param y survival a character vector of a variable names to plot;
#' e.g. \code{y = "r.e2"}
#' @param subset a logical condition; \code{obj} is subset accordingly 
#' before plotting; use this for limiting to specific strata, 
#' e.g. \code{subset = sex == "male"}
#' @param conf.int logical; if \code{TRUE}, also plots any confidence intervals
#' present in \code{obj} for variables in \code{y}
#' @param col line colour; one value for each stratum; will be recycled
#' @param lty line type; one value for each stratum; will be recycled
#' @param ylab label for Y-axis
#' @param xlab label for X-axis
#' @param ... additional arguments passed on to \code{plot} and 
#' \code{lines.survtab}; e.g. \code{ylim} can be defined this way
#' @examples 
#' data(sire)
#' data(sibr)
#' si <- rbind(sire, sibr)
#' si$period <- cut(si$dg_date, as.Date(c("1993-01-01", "2004-01-01", "2013-01-01")), right = FALSE)
#' si$cancer <- c(rep("rectal", nrow(sire)), rep("breast", nrow(sibr)))
#' x <- lexpand(si, birth = bi_date, entry = dg_date, exit = ex_date, 
#'              status = status %in% 1:2, 
#'              fot = 0:5, aggre = list(cancer, period, fot))
#' st <- survtab_ag(fot ~ cancer + period, data = x, 
#'                  surv.method = "lifetable", surv.type = "surv.obs")
#' 
#' plot(st, "surv.obs", subset = cancer == "breast", ylim = c(0.5, 1), col = "blue")
#' lines(st, "surv.obs", subset = cancer == "rectal", col = "red")
#' 
#' ## or
#' plot(st, "surv.obs", col = c(2,2,4,4), lty = c(1, 2, 1, 2))
#' @export
#' @family survtab functions
plot.survtab <- function(x, y = NULL, subset=NULL, conf.int=TRUE, col=NULL,lty=NULL, ylab = NULL, xlab = NULL, ...) {
  
  Tstop <- delta <- NULL ## APPEASE R CMD CHECK
  ## prep ----------------------------------------------------------------------
  PF <- parent.frame(1L)
  subset <- substitute(subset)
  subset <- evalLogicalSubset(data = x, subset, enclos = PF)
  
  l <- prep_plot_survtab(x = x, y = y, subset = subset, 
                         conf.int = conf.int, enclos = PF)
  x <- l$x
  y <- l$y
  y.ci <- l$y.ci
  y.lo <- l$y.lo
  y.hi <- l$y.hi
  
  ## figure out limits, etc. to pass to plot() ---------------------------------
  
  min_y <- do.call("min", c(mget(c(y, y.lo), as.environment(x)), na.rm = TRUE))
  min_y <- max(min_y, 0)
  max_y <- max(x[[y]], na.rm=TRUE)
  
  if (substr(y, 1, 3) == "CIF") {
    min_y <- 0.0
  } else {
    max_y <- max(1.0, max_y)
  }
  
  max_x <- max(x[, Tstop])
  min_x <- min(x[, Tstop-delta])
  
  if (is.null(ylab)) {
    ylab <- "Observed survival"
    if (substr(y[1], 1,4) %in% c("r.e2", "r.pp")) ylab <- "Net survival"
    if (substr(y[1], 1,4) == "CIF_") ylab <- "Absolute risk"
    if (substr(y[1], 1,6) == "CIF.rel") ylab <- "Absolute risk"
  }
  if (is.null(xlab)) xlab <- "Time from entry"
 
  ## attributes insurance to pass to lines.survtab
  setattr(x, "survtab.meta", l$attrs$survtab.meta)
  setattr(x, "class", c("survtab", "data.table", "data.frame"))
  
  ## plotting ------------------------------------------------------------------
  plot(I(c(min_y,max_y))~I(c(min_x,max_x)), data=x, type="n", 
       xlab = xlab, ylab = ylab, ...)
  
  
  lines.survtab(x, subset = NULL, y = y, conf.int=conf.int,
                col=col, lty=lty, ...)
  
 
}


#' \code{lines} method for survtab objects
#' 
#' Plot \code{lines} from a \code{survtab} object
#' 
#' @import graphics
#' 
#' @author Joonas Miettinen
#' 
#' @param x a \code{survtab} output object
#' @param y a variable to plot; a quoted name of a variable
#' in \code{x}; e.g. \code{y = "surv.obs"};
#' if \code{NULL}, picks last survival variable column in order in \code{x}
#' @param subset a logical condition; \code{obj} is subset accordingly 
#' before plotting; use this for limiting to specific strata, 
#' e.g. \code{subset = sex == "male"}
#' @param conf.int logical; if \code{TRUE}, also plots any confidence intervals
#' present in \code{obj} for variables in \code{y}
#' @param col line colour passed to \code{matlines}
#' @param lty line type passed to \code{matlines}
#' @param ... additional arguments passed on to to a \code{matlines} call;
#' e.g. \code{lwd} can be defined this way
#' @examples 
#' data(sire)
#' data(sibr)
#' si <- rbind(sire, sibr)
#' si$period <- cut(si$dg_date, as.Date(c("1993-01-01", "2004-01-01", "2013-01-01")), right = FALSE)
#' si$cancer <- c(rep("rectal", nrow(sire)), rep("breast", nrow(sibr)))
#' x <- lexpand(si, birth = bi_date, entry = dg_date, exit = ex_date, 
#'              status = status %in% 1:2, 
#'              fot = 0:5, aggre = list(cancer, period, fot))
#' st <- survtab_ag(fot ~ cancer + period, data = x, 
#'                  surv.method = "lifetable", surv.type = "surv.obs")
#' 
#' plot(st, "surv.obs", subset = cancer == "breast", ylim = c(0.5, 1), col = "blue")
#' lines(st, "surv.obs", subset = cancer == "rectal", col = "red")
#' 
#' ## or
#' plot(st, "surv.obs", col = c(2,2,4,4), lty = c(1, 2, 1, 2))
#' @export
#' @family survtab functions
lines.survtab <- function(x, y = NULL, subset = NULL, 
                          conf.int = TRUE, col=NULL, lty=NULL, ...) {
  Tstop <- NULL ## APPEASE R CMD CHECK
  ## prep ----------------------------------------------------------------------
  PF <- parent.frame(1L)
  global_breaks <- attr(x, "survtab.meta")$surv.breaks
  
  subset <- substitute(subset)
  subset <- evalLogicalSubset(data = x, subset, enclos = PF)
  
  
  l <- prep_plot_survtab(x = x, y = y, subset = subset, 
                         conf.int = conf.int, enclos = environment())
  x <- l$x
  y <- l$y
  y.ci <- l$y.ci
  y.lo <- l$y.lo
  y.hi <- l$y.hi
  strata <- l$strata ## character vector of var names
  
  
  ## impute first values (time = 0, surv = 1 / cif = 0) ------------------------
  
  is_CIF <- if (substr(y, 1, 3) == "CIF") TRUE else FALSE
  setkeyv(x, c(strata, "Tstop"))
  first <- x[1, ]
  if (length(strata)) first <- unique(x, by = strata)
  first[, c(y) := ifelse(is_CIF, 0, 1)]
  first$Tstop <- min(global_breaks)
  
  if (length(y.ci) > 0) first[, (y.ci) := get(y) ]
  x <- rbindlist(list(first, x[, ]), use.names = TRUE)
  setkeyv(x, c(strata, "Tstop"))
  
  ## plotting ------------------------------------------------------------------
  
  if (is.null(lty)) {
    lty <- list(c(1,2,2))
    if (!length(y.ci)) lty <- list(1)
  }
  
  lines_by(x = "Tstop", y = c(y, y.ci), 
           strata.vars = strata, 
           data = x, col = col, lty = lty, ...)
  
  
}



lines_by <- function(x, y, strata.vars = NULL, data, col, lty, ...) {
  ## INTENTION: plots lines separately by strata,
  ## which may have different colours / linetypes.
  ## @param x a variable to plot y by; a character string
  ## @param y a character vector of variables to plot by x;
  ## e.g. the estimate and confidence interval variables
  ## @param strata.vars a character string vector; variables
  ## to add lines by, which may have different colours etc for identification
  ## @param data a data.frame where x, y, and strata.vars are found
  ## @param col a vector of colors passed to lines(); if vector length 1,
  ## used for each level of strata. If vector length > 1, 
  ## has to match to total number of strata. If list, must match
  ## to number of strata by length and contain elements of length
  ## length(y).
  ## @param see col; line type passed to lines().
  ## @param ... other arguments passed on to lines().
  
  TF <- environment()
  PF <- parent.frame(1L)
  
  stopifnot(is.data.frame(data))
  stopifnot(is.character(x) && length(x) == 1L)
  stopifnot(is.character(y) && length(y) > 0L)
  stopifnot(is.character(strata.vars) || is.null(strata.vars))
  all_names_present(data, c(x,y,strata.vars))
  
  d <- mget(c(strata.vars, y, x), envir = as.environment(data))
  setDT(d)
  setkeyv(d, c(strata.vars, x))
  
  ## create list of datas
  l <- list(d)
  inter <- 1L
  if (length(strata.vars)) {
    inter <- do.call(interaction, d[, strata.vars, with = FALSE])
    l <- vector("list", uniqueN(inter))
    l <- split(d, f = inter, drop = TRUE)
  }
  
  l <- lapply(l, function(tab) {
    setDT(tab)
    setcolsnull(tab, keep = c(x, y))
    tab
  })
  
  
  ## figure out colours and ltys
  for (objname in c("col", "lty")) {
    obj <- TF[[objname]]
    
    if (missing(obj) || !length(obj)) obj <- 1
    if (!length(obj) %in% c(1, length(l))) {
      stop("Argument ", objname, " is not of length 1 or ",
           "of length equal to total number of strata (",
           length(l), ").")
    }
    
    ol <- unlist(lapply(obj, length))
    if (length(y) > 1 && is.list(obj) && !all(ol %in% c(1, length(y)))) {
      stop("Argument y is of length > 1, and you passed ",
           objname, " as a list of values, but at least one element is not ",
           "of length 1 or length(y).")
    }
    
    ## NOTE: rep works for vector and list just the same
    if (length(obj) == 1) obj <- rep(obj, length(l))
    obj <- as.list(obj)
    
    assign(x = objname, value = obj)
  }
  
  lapply(seq_along(l), function(i) {
    
    tab <- l[[i]]
    cols <- col[[i]]
    ltys <- lty[[i]]
    
    matlines(x = tab[[x]], y = tab[, y, with = FALSE], 
             col = cols, lty = ltys, ...)
    
  })
  
  invisible(NULL)
}




#' @title Graphically Inspect Curves Used in Mean Survival Computation
#' @description Plots the observed (with extrapolation) and expected survival
#' curves for all strata in an object created by \code{\link{survmean}}
#' @author Joonas Miettinen
#' @param x a \code{survmean} object
#' @param ... arguments passed (ultimately) to \code{matlines}; you
#' may, therefore, supply e.g. \code{xlab} through this, though arguments
#' such as \code{lty} and \code{col} will not work
#' @details 
#' 
#' For examples see \code{\link{survmean}}. This function is intended only
#' for graphically inspecting that the observed survival curves with extrapolation
#' and the expected survival curves have been sensibly computed in \code{survmean}.
#' 
#' If you want finer control over the plotted curves, extract the curves from
#' the \code{survmean} output using 
#' 
#' \code{attr(x, "curves")}
#' 
#' where \code{x} is a \code{survmean} object.
#' @export
#' @family survmean functions
plot.survmean <- function(x, ...) {
  at <- attr(x, "survmean.meta")
  curves <- at$curves
  if (is.null(curves)) {
    stop("no curves information in x; sometimes lost if x ",
         "altered after using survmean")
  }
  
  by.vars <- at$tprint
  by.vars <- c(by.vars, at$tadjust)
  by.vars <- intersect(by.vars, names(curves))
  if (!length(by.vars)) by.vars <- NULL
  
  plot(curves$surv ~ curves$Tstop, type="n",
       xlab = "Time from entry", ylab = "Survival")
  lines.survmean(x, ...)
  
  subr <- at$breaks[[at$survScale]]
  abline(v = max(subr), lty=2, col="grey")
  
  if (length(by.vars)) {
    ## add legend denoting colors
    Stratum <- curves[, unique(interaction(.SD)), .SDcols = eval(by.vars)]
    legend(x = "topright", legend = Stratum, col = seq_along(Stratum), lty = 1)
  }
  
}

#' @title Graphically Inspect Curves Used in Mean Survival Computation
#' @description Plots the observed (with extrapolation) and expected survival
#' curves for all strata in an object created by \code{\link{survmean}}
#' @author Joonas Miettinen
#' @param x a \code{survmean} object
#' @param ... arguments passed (ultimately) to \code{matlines}; you
#' may, therefore, supply e.g. \code{lwd} through this, though arguments
#' such as \code{lty} and \code{col} will not work
#' @details 
#' 
#' This function is intended to be a workhorse for \code{\link{plot.survmean}}.
#' If you want finer control over the plotted curves, extract the curves from
#' the \code{survmean} output using 
#' 
#' \code{attr(x, "curves")}
#' 
#' where \code{x} is a \code{survmean} object.
#' @export
#' @family survmean functions
lines.survmean <- function(x, ...) {
  at <- copy(attr(x, "survmean.meta"))
  curves <- at$curves
  if (is.null(curves)) stop("no curves information in x; usually lost if x altered after using survmean")
  
  by.vars <- at$tprint
  by.vars <- c(by.vars, at$tadjust)
  by.vars <- c("survmean_type", by.vars)
  by.vars <- intersect(by.vars, names(curves))
  if (!length(by.vars)) by.vars <- NULL
  
  curves <- data.table(curves)
  setkeyv(curves, c(by.vars, "Tstop"))
  
  type_levs <- length(levels(interaction(curves[, c(by.vars), with=FALSE])))/2L
  other_levs <- 1L
  if (length(by.vars) > 1) {
    other_levs <- length(levels(interaction(curves[, setdiff(by.vars, "survmean_type"), with=FALSE])))
  }
  
  curves <- cast_simple(curves, columns = by.vars, rows = "Tstop", values = "surv")
  matlines(x=curves$Tstop, y=curves[, setdiff(names(curves), "Tstop"), with=FALSE],  
           lty = rep(1:2, each=type_levs), col = 1:other_levs, ...)
}







#' @export
getCall.survtab <- function(x, ...) {
  attributes(x)$survtab.meta$call
}


#' @export
formula.survtab <- function(x, ...) {
  attr(x, "survtab.meta")$arguments$formula
}





#' @export
getCall.survmean <- function(x, ...) {
  attributes(x)$survmean.meta$call
}


#' @export
formula.survmean <- function(x, ...) {
  attr(x, "survmean.meta")$formula
}



