
#' @title Print method for \code{sir} objects
#' @author Matti Rantanen, Joonas Miettinen
#' @description Prints the results of the \code{sir} function
#' @param x a \code{sir} object
#' @param ... unused
#' @export
print.sir <- function(x, ...) {
  
  cat("SIR Standardized by: ", x[['adjusted']] , fill=TRUE)
  
  cat('\n',"Total observed:", data.frame(x[[1]])[,'observed'], '\n',
      "Total expected:", data.frame(x[[1]])[,'expected'], '\n',
      #"Total SIR:", data.frame(x[[1]])[,'sir'], '\n',
      "Total person-years:", data.frame(x[[1]])[,'pyrs'], '\n',
      fill=TRUE)
  
  if ( is.null(x$model) ) {
    cat("Univariate SIR/SMR:", '\n')
    print( x$univariate[] )
  } else {
    cat("Poisson modelled SIR:", '\n')
    print( x$model[] )
  }
  cat(fill=TRUE)
  if (is.null( x[['lrt.test']] )) {
    cat("Couldn't test homogeneity.",'\n')
  } 
  else if(x$test.type == 'homogeneity') {
    cat("Test for homogeneity p", p.round( c(x$lrt.test)), '\n' )
  }
  else if(x$test.type == 'trend') {
    cat("Test for trend p", p.round( c(x$lrt.test)), '\n' )
  }
  
  return(invisible())
}

#' @title Print method for \code{sirspline} objects
#' @author Matti Rantanen, Joonas Miettinen
#' @description Prints the results of the \code{sirspline} function
#' @param x a \code{sir} object
#' @param ... unused
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
#' @param plot.type select 'model'(=default), 'univariate'
#' @param conf.int default TRUE draws confidence intervals
#' @param xlab overwrites default x-axis label
#' @param ylab overwrites default y-axis label
#' @param xlim x-axis minimum and maximum values
#' @param main optional plot title
#' @param abline logical; draws a gray line in SIR = 1
#' @param lang language:  'fi'(default) or 'en'
#' @param log logical; SIR is not in log scale by default
#' @param eps error bar vertical bar height (works only in 'model' or 'univariate')
#' @param left.margin adjust left marginal of the plot to fit long variablenames
#' @param ... arguments passed on to plot()
#' 
#' 
#' @details Plot SIR estimates and confidence intervals 
#' \itemize{
#'  \item univariate - plots SIR with univariates confidence intervals
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
#' \strong{Tips for plottin splines}
#' It's possible to use \code{plot} to first draw the 
#' confidence intervals using specific line type or colour and then plotting 
#' againg the estimate using \code{lines(... , conf.int = FALSE)} with different 
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
plot.sir <- function(x, plot.type = 'model', 
                     conf.int = TRUE, ylab, xlab, xlim, main, 
                     eps=0.2, abline = TRUE, lang = 'fi', log = FALSE, left.margin, ...) {
  
  if (plot.type %in% c('model','univariate')) {
    if ( !is.null(x[[3]]) & plot.type == 'model' ) {
      pick <- 3
    }
    else {
      pick <- 2
    }
    a <- data.frame( x[[pick]] )
    
    # variable levels / y-axis
    variable.columns <- which(names(a) == 'observed') - 1
    if( variable.columns > 0) {
      levels.org <- a[, 1:variable.columns]
      if( variable.columns == 1 ) {
        levels <- levels.org
      }
      if(variable.columns > 1) {
        levels <- levels.org[,1]
        for(i in 2:variable.columns){
          levels <- paste(levels, levels.org[,i], sep = ':')
        }  
      }
    }
    else {
      levels.org <- 1
      levels <- 'Crude'
      if(lang=='en') {
        levels <- 'Total'
      }
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
      xlimit <- c(0, max( a[['X97.5..']][a[['X97.5..']]<Inf] ) )
    } 
    else {
      xlimit <- xlim
    }
    
    # par options
    op <- par(no.readonly = TRUE)
    
    if(missing(left.margin)) {
      new.margin <- par("mar")
      new.margin[2] <- 4.1 + sqrt( max(nchar(as.character(levels))) )*2
    } 
    else {
      new.margin[2] <- left.margin
    }
    par(mar = new.margin)
    
    # plot frame, estimates and CI (optional abline)
    logarithm <- ''
    if(log){
      logarithm <- 'x'
      xlimit[1] <- xlimit[1] + 1
    }
    y.axis.levels <- c(1:length(levels))
    plot(c(xlimit), c(min(y.axis.levels)-0.5, max(y.axis.levels)+0.5), 
         type='n', yaxt = 'n', xlab=xlab, ylab=ylab, log=logarithm, main = main, ...)
    axis(side = 2, at = y.axis.levels, labels = levels, las=1)
    
    if(abline) {
      abline(v=1, col = 'darkgray')
    }
    
    points(a$sir, factor(y.axis.levels, labels=levels), ...) 
    if(conf.int) {
      segments(a[['X2.5..']], y.axis.levels , a[['X97.5..']], y.axis.levels, ...)
      segments(a[['X2.5..']], y.axis.levels - eps, a[['X2.5..']], y.axis.levels +eps, ... )
      segments(a[['X97.5..']], y.axis.levels - eps, a[['X97.5..']], y.axis.levels +eps, ... )    
    }
    
    # reset margins
    par(op)
    
  }
  else {
    message('Select plot.type: "model" or "univarite"')
  }
}

#' @title \code{plot} method for sirspline-object
#' 
#' @description Plot SIR splines using R base graphics.
#' 
#' @seealso \code{\link{sir}},  \code{\link{sirspline}}, \code{\link{lines.sirspline}}
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
#' In \code{plot.sirspline} almost every graphical parameter is user
#' adjustable, such as \code{ylim}, \code{xlim}.
#' \code{plot.sirsplines} calls \code{lines.splines} to add lines.
#' 
#' The plot axis without lines can be plotted using option \code{type = 'n'}. 
#' On top of the frame it's then possible to add a \code{grid}, 
#' \code{abline} or text before plotting the lines (see: \code{sirspline}).
#' @export
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
#' @description Plot SIR spline lines wtih R base graphics
#' 
#' 
#' @author Matti Rantanen
#' 
#' @param x an object returned by function sirspline
#' @param conf.int logical; default TRUE draws also the 95 confidence intervals
#' @param print.levels name(s) to be plottet. Default plots all levels.
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
#' \code{print.levels}. Printing the levels seperately enables  e.g. to
#' give different colours for each level.
#' 
#' @seealso \code{\link{sir}},  \code{\link{sirspline}}, \code{\link{plot.sirspline}}
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

print.yrs <- function(x, ...) {
  # NextMethod() ## this still prints attributes
  print(as.numeric(x))
}

## yrs objects, from get.yrs
`[.yrs` <- function(x, ...) {
  yl <- attr(x, "year.length")
  structure(NextMethod(), year.length = yl, class = c("yrs", "numeric"))
}

## subsetting for aggre objects that retains attributes
`[.aggre` <- function(x, ...) {
  y <- NextMethod()
  if (is.data.frame(y)) {
    setattr(y, "class", class(x))
    setattr(y, "aggreVars", attr(x, "aggreVars"))
    setattr(y, "breaks", attr(x, "breaks"))
  }
  y
}

subset.aggre <- function(x, ...) {
  y <- NextMethod()
  if (is.data.frame(y)) {
    setattr(y, "class", class(x))
    setattr(y, "aggreVars", attr(x, "aggreVars"))
    setattr(y, "breaks", attr(x, "breaks"))
  }
  y
}

preface_survtab.print <- function(x) {
  at <- attributes(x)$survtab.meta
  arg <- at$arguments
  
  cat("\n")
  cat("Call: \n", oneWhitespace(deparse(at$call)), "\n")
  cat("\n")
  cat("Type arguments: \nsurv.type:", as.character(arg$surv.type), 
      "--- surv.method:", as.character(arg$surv.method))
  if (as.character(arg$surv.type) == "surv.rel")
    cat(" --- relsurv.method:", as.character(arg$relsurv.method))
  cat("\n \n")
  cat("Confidence interval arguments: \n level:", 
      as.character(arg$conf.level*100), "%")
  cat(" --- transformation:",
      as.character(arg$conf.type))
  cat("\n \n")
  cat("Totals: \n person-time:", round(sum(x$pyrs)), "--- events:", sum(x$d))
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

#' @title Print a survtab Object
#' @author Joonas Miettinen
#' @description Print method function for \code{survtab} objects; see
#' \code{\link{survtab_ag}}.
#' @param x a \code{survtab} object
#' @param subset a logical condition to subset results table by
#' before printing; use this to limit to a certain stratum. E.g.
#' \code{subset = sex == "male"}
#' @param ... arguments passed to \code{print.data.table}; try e.g.
#' \code{top = 2} for numbers of rows in head and tail, 
#' \code{nrow = 100} for number of rows to print, etc.
#' @export
print.survtab <- function(x, subset = NULL, ...) {
  
  PF <- parent.frame(1L)
  TF <- environment()
  sa <- attributes(x)$survtab.meta
  
  subset <- evalLogicalSubset(x, substitute(subset), enclos = PF)
  x <- x[subset, ]
  
  preface_survtab.print(x)
  
  setDT(x)
  pv <- as.character(sa$print.vars)
  if (length(pv) == 0L) pv <- NULL
  
  magicMedian <- function(x) {
    if (length(x) %% 2L == 0L) median(x[-1L], na.rm = TRUE) else
      median(x, na.rm = TRUE)
  }
  
  medmax <- x[, list(Tstop = c(magicMedian(c(min(Tstart),Tstop)), max(Tstop))), keyby = eval(pv)]
  
  setkeyv(medmax, c(pv, "Tstop"))
  setkeyv(x, c(pv, "Tstop"))
  x <- x[medmax]
  
  x[, c(sa$est.vars, sa$CI.vars, sa$misc.vars) := lapply(.SD, function(x) round(x, 4L)), 
       .SDcols = c(sa$est.vars, sa$CI.vars, sa$misc.vars)]
  
  if (length(sa$SE.vars) > 0L) x[, c(sa$SE.vars) := lapply(.SD, function(x) signif(x, 4L)), .SDcols = c(sa$SE.vars)]
  
  # x[, c(sa$value.vars) := lapply(.SD, function(x) round(x, 2L)), .SDcols = c(sa$value.vars)]  
  
  setcolsnull(x, keep = c(pv, "Tstop", sa$surv.vars), colorder = TRUE)
  
  print(data.table(x), ...)
  invisible()
}

#' @title Summarize a survtab Object
#' @author Joonas Miettinen
#' @description Summary method function for \code{survtab} objects; see
#' \code{\link{survtab_ag}}. Returns estimates at given time points
#' or all time points.
#' @param object a \code{survtab} object
#' @param t a vector of times at which time points to print
#' summary table of survival function estimates by strata;
#' will automatically use values in breaks used to split data
#' for aggregation closest to these values, so e.g. supplyig 
#' \code{t = c(2.5, 5.1)}
#' with data that was split by the breaks \code{seq(0, 5, 1/12)}
#' causes the times \code{c(2.5, 5.0)} to be used. 
#' Since the estimates at the time points closest to \code{t} are selected,
#' values of \code{t} are compared with column \code{Tstop} in the data.
#' If \code{NULL}, prints all rows.
#' @param subset a logical condition to subset results table by
#' before printing; use this to limit to a certain stratum. E.g.
#' \code{subset = sex == "male"}
#' @param ... unused; required for congruence with other \code{summary} methods
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
#' 
#' ## observed survival
#' st <- survtab_lex(Surv(time = FUT, event = lex.Xst) ~ 1, data = x, 
#'                   surv.type = "surv.obs",
#'                   breaks = list(FUT = seq(0, 5, 1/12)))
#' 
#' ## estimates at full years of follow-up
#' summary(st, t = 1:5)
#' 
#' ## interval estimate closest to 75th percentile (75 % survival;
#' ## just switch 0.75 to 0.5 for median survival, etc.)
#' summary(st, subset = surv.obs == surv.obs[which.min(abs(surv.obs - 0.75))])
#' 
#' ## if you want all estimates in a new data.frame, you can also simply do
#' 
#' x <- as.data.frame(st)
#' 
#' @export
summary.survtab <- function(object, t = NULL, subset = NULL, ...) {
  
  PF <- parent.frame(1L)
  
  subset <- evalLogicalSubset(object, substitute(subset), enclos = PF)
  x <- object[subset, ]
  
  preface_survtab.print(x)
  
  at <- attr(x, "survtab.meta")
  
  if (is.null(t)) t <- unique(x$Tstop-x$delta) else {
    
    wh <- sapply(t, function(x_) which.min(abs(unique(x$Tstop) - x_)))
    wh <- sort(unique(wh))
    t <- unique(x$Tstop)[wh]
    
  }
  
  setDT(x)
  setkeyv(x, c(at$print.vars, "Tstop"))
  x <- x[Tstop %in% t]
  
  if (!getOption("popEpi.datatable")) setDFpe(x)
  
  x
}

## subsetting for survtab objects that retains attributes
`[.survtab` <- function(x, ...) {
  y <- NextMethod()
  if (is.data.frame(y)) {
    setattr(y, "class", class(x))
    setattr(y, "survtab.meta", attr(x, "survtab.meta"))
  }
  y
}

subset.survtab <- function(x, ...) {
  y <- NextMethod()
  if (is.data.frame(y)) {
    setattr(y, "class", class(x))
    setattr(y, "survtab.meta", attr(x, "survtab.meta"))
  }
  y
}

## subsetting for meansurv objects that retains attributes
`[.survmean` <- function(x, ...) {
  y <- NextMethod()
  if (is.data.frame(y)) {
    setattr(y, "class", class(x))
    setattr(y, "surv.breaks", attr(x, "surv.breaks"))
    setattr(y, "by.vars", attr(x, "by.vars"))
    setattr(y, "curves", attr(x, "curves"))
  }
  y
}

subset.survmean <- function(x, ...) {
  y <- NextMethod()
  if (is.data.frame(y)) {
    setattr(y, "class", class(x))
    setattr(y, "surv.breaks", attr(x, "surv.breaks"))
    setattr(y, "by.vars", attr(x, "by.vars"))
    setattr(y, "curves", attr(x, "curves"))
  }
  y
}


prep_plot_survtab <- function(x, y = NULL, subset = NULL, conf.int = TRUE, enclos = parent.frame(1L), ...) {
  
  ## subsetting ----------------------------------------------------------------
  subset <- evalLogicalSubset(data = x, substiset = substitute(subset), enclos = environment())
  
  attrs <- attributes(x)
  
  if (!inherits(x, "survtab")) stop("x is not a survtab object")
  if (is.null(attrs$survtab.meta)) stop("Missing meta information (attributes) in survtab object; have you tampered with it after estimation?")
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
  if (length(surv_vars) == 0) stop("x does not appear to have any survival variables; did you tamper with it after estimation?")
  
  
  ## getting y -----------------------------------------------------------------
  if (!is.null(y)) {
    if (!is.character(y)) stop("please supply y as a character string indicating the name of a variable in x")
    if (length(y) > 1) stop("y must be of length 1 or NULL")
    if (!all_names_present(x, y, stops = FALSE)) stop("Given survival variable in argument 'y' not present in survtab object ('", y, "')")
  } else {
    y <- surv_vars[length(surv_vars)]
    if (length(surv_vars) > 1L) message("y was NULL; chose ", y, " automatically")
  }
  rm(surv_vars)
  
  if (substr(y, 1, 3) == "CIF" && conf.int) stop("No confidence intervals currently supported for CIFs. Hopefully they will be added in a future version; meanwhile use conf.int = FALSE when plotting CIFs.")
  
  
  ## confidence intervals ------------------------------------------------------
  y.lo <- y.hi <- y.ci <- NULL
  if (conf.int) {
    
    y.lo <- paste0(y, ".lo")
    y.hi <- paste0(y, ".hi")
    y.ci <- c(y.lo, y.hi)
    
    badCIvars <- setdiff(y.ci, names(x))
    if (sum(length(badCIvars))) stop("conf.int = TRUE, but missing confidence interval variables in data for y = '", y, "' (could not detect variables named", paste0("'", badCIvars, "'", collapse = ", ") ,")")
    
  } 
  
  list(x = x, y = y, y.ci = y.ci, y.lo = y.lo, y.hi = y.hi, strata = strata.vars, attrs = attrs)
  
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
plot.survtab <- function(x, y = NULL, subset=NULL, conf.int=TRUE, col=NULL,lty=NULL, ylab = NULL, xlab = NULL, ...) {
  
  ## prep ----------------------------------------------------------------------
  PF <- parent.frame(1L)
  subset <- substitute(subset)
  subset <- evalLogicalSubset(data = x, subset, enclos = PF)
  
  l <- prep_plot_survtab(x = x, y = y, subset = subset, conf.int = conf.int, enclos = PF)
  x <- l$x
  y <- l$y
  y.ci <- l$y.ci
  y.lo <- l$y.lo
  y.hi <- l$y.hi
  
  ## figure out limits, etc. to pass to plot() ---------------------------------
  min_y <- min(x[, c(y,y.lo), with=FALSE], na.rm=TRUE)
  min_y <- max(min_y, 0)
  
  max_y <- max(x[, c(y,y.hi), with=FALSE], na.rm=TRUE) + 0.025
  if (substr(y[1], 1, 3) == "CIF") min_y <- 0L else max_y <- 1L
  
  max_x <- max(x[, Tstop])
  min_x <- min(x[, Tstop-delta])
  
  if (is.null(ylab)) {
    ylab <- "Observed survival"
    if (substr(y[1], 1,4) %in% c("r.e2", "r.pp")) ylab <- "Net survival"
    if (substr(y[1], 1,4) == "CIF_") ylab <- "Absolute risk"
    if (substr(y[1], 1,6) == "CIF.rel") ylab <- "Absolute risk"
  }
  if (is.null(xlab)) xlab <- "Years from entry"
 
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
lines.survtab <- function(x, y = NULL, subset = NULL, conf.int = TRUE, col=NULL, lty=NULL, ...) {
  
  ## prep ----------------------------------------------------------------------
  PF <- parent.frame(1L)
  subset <- substitute(subset)
  subset <- evalLogicalSubset(data = x, subset, enclos = PF)
  
  l <- prep_plot_survtab(x = x, y = y, subset = subset, conf.int = conf.int, enclos = environment())
  x <- l$x
  y <- l$y
  y.ci <- l$y.ci
  y.lo <- l$y.lo
  y.hi <- l$y.hi
  strata <- l$strata ## character vector of var names
  
  
  ## need to determine total number of strata; also used in casting ------------
  ## note: if no strata, create a dummy repeating 1L as strata
  tmpBy <- makeTempVarName(x)
  if (length(strata) == 0L) {
    strata <- tmpBy
    x[, c(tmpBy) := 1L]
  }
  x[, c(tmpBy) := interaction(mget(rev(strata)))]
  x[, c(tmpBy) := factor(get(tmpBy), labels = 1:uniqueN(get(tmpBy)))]
  x[, c(tmpBy) := robust_values(get(tmpBy))]
  
  Nstrata <- x[, uniqueN(get(tmpBy))]
  
  ## color and line type matching to strata ------------------------------------
  if (is.null(lty)) {
    lty <- if (conf.int) c(1,2,2) else 1
  } else {
    lty <- if(conf.int) rep(lty, each = 3) else lty
  }
  if (is.null(col)) {
    col <- if(conf.int) rep(1, 3) else 1
  } else {
    col <- if(conf.int) rep(col, each = 3) else col 
  }
  
  ## impute first values (time = 0, surv = 1 / cif = 0) ------------------------
  
  is_CIF <- if (substr(y, 1, 3) == "CIF") TRUE else FALSE
  first <- x[!duplicated(get(tmpBy)), ]
  first[, c(y) := ifelse(is_CIF, 0, 1)]
  first[, Tstop := 0]
  
  if (length(y.ci) > 0) first[, (y.ci) := get(y) ]
  x <- rbindlist(list(first, x[, ]), use.names = TRUE)
  setkeyv(x, c(tmpBy, "surv.int"))
  
  ## cast to accommodate strata ------------------------------------------------
  x <- cast_simple(x, columns = tmpBy, rows = "Tstop", 
                   values = c(y, y.ci))
  estVars <- names(x)[1:Nstrata+1]
  strata <- lapply(rep(y, Nstrata), gsub, replacement = "", x = estVars)
  strata <- unique(unlist(strata))
  newOrder <- NULL
  
  ## reorder to match col, lty, etc. -------------------------------------------
  for (k in strata) {
    newOrder <- c(newOrder, names(x)[grep(k, names(x))])
  }
  setcolorder(x, c("Tstop", newOrder))
  setDF(x)
  
  ## plotting ------------------------------------------------------------------
  graphics::matlines(x = x$Tstop, 
                     y = x[, setdiff(names(x), "Tstop")], 
                     col=col, lty=lty, ...)
  
  
}











