#### S3 definitions
setOldClass("pe")
setOldClass(c("pe", 'data.table'))
setOldClass(c("pe", 'list'))


as.pe <- function(x) {
  if (is.null(x)) {return(data.table())}
  UseMethod("as.pe")
}


setAs("data.table", "pe", function(from) {
  as.pe(from)
})

setAs("list", "pe", function(from) {
  as.pe(from)
})




#' @title Add class \code{'pe'} to a \code{Lexis} object.
#' @author Joonas Miettinen
#' @description Coerces an R \code{Lexis} object to a \code{Lexis} object 
#' with the class
#' \code{pe}.
#' @param x a Lexis object
#' @S3method as.pe Lexis
as.pe.Lexis <- function(x) {
  if (!"Lexis" %in% class(x)) stop("not a Lexis object")
  pe <- copy(x) 
  
  cl <- class(x)
  wh <- which(cl == "Lexis")
  cl <- c(cl[1:(wh-1)], "pe", "Lexis",  cl[wh:length(cl)])
  
  setattr(pe, "class", cl)
  
  return(pe[])
}


#' @title Add class \code{'pe'} to a \code{data.table} object.
#' @author Joonas Miettinen
#' @description Coerces a \code{data.table} object to a \code{data.table} 
#' with the class
#' \code{pe}.
#' @param x a data.table
#' @S3method as.pe data.table
as.pe.data.table <- function(x) {
  pe <- copy(x) 
  
  cl <- class(x)
  wh <- which(cl == "data.table")
  cl <- c(cl[1:(wh-1)], "pe","data.table",  cl[wh:length(cl)])
  
  setattr(pe, "class", cl)
  
  return(pe[])
}


#' @title Add class \code{'pe'} to a \code{list} object.
#' @author Joonas Miettinen
#' @description Coerces an R \code{list} object to a \code{list} with the class
#' \code{pe}.
#' @param x a list
#' @S3method as.pe list
as.pe.list <- function(x) {
  pe <- copy(x) 
  
  cl <- class(x)
  wh <- which(cl == "list")
  cl <- c(cl[1:(wh-1)], "pe","list",  cl[wh:length(cl)])
  
  setattr(pe, "class", cl)
  
  return(pe)
}



#' @title Plot methods for popEpi objects
#' @author Matti Rantanen, Joonas Miettinen
#' @description Plots the results of a popEpi function
#' @param x a popEpi object
#' @param plot.type 'obs' or 'pyrs'
#' @param ... additional arguments passed on to plot.Lexis 
#' if no other printing method found
#' @S3method plot pe
#' @export plot.pe
#' @import Epi
#' @import graphics
plot.pe <- function(x, plot.type, ...) {
  plot.Lexis(x, ...)
  
}

#' @title Print method for \code{sir} objects
#' @author Matti Rantanen, Joonas Miettinen
#' @description Prints the results of the \code{sir} function
#' @export print.sir
#' @param x a \code{sir} object
#' @param ... unused
#' @S3method print sir
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
#' @export print.sirspline
#' @param x a \code{sir} object
#' @param ... unused
#' @S3method print sirspline
#' @import grDevices
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
#' @S3method plot sir
#' @export plot.sir
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
#' @S3method plot sirspline
#' @export plot.sirspline
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
#' 

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
#' @S3method lines sirspline
#' @export lines.sirspline
#' @import graphics


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


#' \code{plot} method for survtab objects
#' 
#' Plotting for \code{survtab} objects
#' 
#' @S3method plot survtab
#' @export plot.survtab
#' @import graphics
#' 
#' @author Joonas Miettinen
#' 
#' @param x a \code{survtab} output object
#' @param y survival a character vector of a variable names to plot;
#' e.g. \code{"r.e2"}
#' @param subset a logical condition; \code{obj} is subset accordingly 
#' before plotting
#' @param conf.int logical; if \code{TRUE}, adds any confidence intervals
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
#'              status = status %in% 1:2, pophaz = popmort,
#'              fot = 0:5)
#' st <- survtab(x, by.vars = c("cancer", "period"), 
#'               event.values = 1L, surv.method = "lifetable")
#' 
#' plot(st, "r.e2", subset = cancer == "breast", ylim = c(0.5, 1), col = "blue")
#' lines(st, "r.e2", subset = cancer == "rectal", col = "red")
#' 
#' ## or
#' plot(st, "r.e2", col = c(2,2,4,4), lty = c(1, 2, 1, 2))
plot.survtab <- function(x, y = NULL, subset=NULL, conf.int=NULL, col=NULL,lty=NULL, ylab = NULL, xlab = NULL, ...) {
  
  ## take copy preserving attributes (unlike  <- data.table())
  x <- copy(x)
  if (!inherits(x, "survtab")) stop("x is not a survtab object or it has been modified (e.g. subset) after survtab ")
  setDT(x)
  
  by.vars <- attr(x, "by.vars")
  
  surv_vars <- c("surv.obs","CIF.rel","CIF_","r.e2","r.pp")
  wh <- NULL
  for (k in surv_vars) {
    wh <- c(wh, which(substr(names(x), 1, nchar(k)) == k))
  }
  surv_vars <- names(x)[wh]; rm(wh)
  surv_vars <- surv_vars[!substr(surv_vars, nchar(surv_vars)-1, nchar(surv_vars)) %in% c("hi","lo")]
  if (length(surv_vars) == 0) stop("x does not appear to have any survival variables; is it an untouched output from survtab?")
  
  
  ## getting y -----------------------------------------------------------------
  if (!is.null(y)) {
    if (!is.character(y)) stop("please supply y as a character string indicating the name of a variable in x")
    if (length(y) > 1) stop("y must be of length 1 or NULL")
    all_names_present(x, y)
  } else {
    y <- surv_vars[length(surv_vars)]
    message("y was NULL; chose ", y, " automatically")
  }
  rm(surv_vars)
  
  ## subsetting ----------------------------------------------------------------
  subset <- substitute(subset)
  subset <- evalLogicalSubset(data = x, substiset = subset)
  
  ## confidence intervals ------------------------------------------------------
  y.lo <- paste0(y, ".lo")
  y.hi <- paste0(y, ".hi")
  y.ci <- c(y.lo, y.hi)
  y.ci <- intersect(y.ci, names(x))
  ley <- length(c(y.ci))
  
  if (is.null(conf.int)) {
    conf.int <- conf.int <- ifelse(ley %in% 1:2, TRUE, FALSE)
  }
  if (!conf.int) y.ci <- NULL
  
  ## plotting ------------------------------------------------------------------
  ## y is a data.table with survival variables as columns
  min_y <- min(x[subset, c(y,y.ci), with=FALSE], na.rm=TRUE)
  min_y <- max(min_y, 0)
  max_y <- max(x[subset, c(y,y.ci), with=FALSE], na.rm=TRUE)
  
  max_x <- max(x[subset, Tstop])
  
  if (is.null(ylab)) {
    ylab <- "Observed survival"
    if (substr(y[1], 1,4) %in% c("r.e2", "r.pp")) ylab <- "Net survival"
    if (substr(y[1], 1,4) == "CIF_") ylab <- "Absolute risk"
    if (substr(y[1], 1,6) == "CIF.rel") ylab <- "Absolute risk"
  }
  if (is.null(xlab)) xlab <- "Years from entry"
 
  
  plot(I(c(min_y,max_y))~I(c(0,max_x)), data=x, type="n", 
       xlab = xlab, ylab = ylab, ...)
  
  lines.survtab(x, subset = subset, y = y, conf.int=conf.int,
                col=col, lty=lty, ...)
  
 
}


#' \code{lines} method for survtab objects
#' 
#' Plot \code{lines} from a \code{survtab} object
#' 
#' @S3method lines survtab
#' @export lines.survtab
#' @import graphics
#' 
#' @author Joonas Miettinen
#' 
#' @param x a \code{survtab} output object
#' @param y a variable to plot; a quoted name of a variable
#' in \code{x}; e.g. \code{"surv.obs"};
#' if \code{NULL}, picks last survival variable column in order in \code{x}
#' @param subset a logical condition; \code{x} is subset accordingly 
#' before plotting
#' @param conf.int logical; if \code{TRUE}, plots confidence regions as well
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
#'              status = status %in% 1:2, pophaz = popmort,
#'              fot = 0:5)
#' st <- survtab(x, by.vars = c("cancer", "period"), 
#'               event.values = 1L, surv.method = "lifetable")
#' 
#' plot(st, "r.e2", subset = cancer == "breast", ylim = c(0.5, 1), col = "blue")
#' lines(st, "r.e2", subset = cancer == "rectal", col = "red")
#' 
#' ## or
#' plot(st, "r.e2", col = c(2,2,4,4))

lines.survtab <- function(x, y = NULL, subset = NULL, 
                          conf.int = TRUE, 
                          col=NULL, lty=NULL, ...) {
  ## take copy preserving attributes (unlike data.table())
  x <- copy(x)
  setDT(x)
  
  by.vars <- attr(x, "by.vars")
  if (!is.null(by.vars)) {
    all_names_present(x, by.vars)
  } else {
    by.vars <- makeTempVarName(x, pre = "by_")
    x[, (by.vars) := 1L]
    all_names_present(x, by.vars)
  }
  
  surv_vars <- c("surv.obs","CIF.rel","CIF_","r.e2","r.pp")
  wh <- NULL
  for (k in surv_vars) {
    wh <- c(wh, which(substr(names(x), 1, nchar(k)) == k))
  }
  surv_vars <- names(x)[wh]; rm(wh)
  surv_vars <- surv_vars[!substr(surv_vars, nchar(surv_vars)-1, nchar(surv_vars)) %in% c("hi","lo")]
  if (length(surv_vars) == 0) stop("x does not appear to have any survival variables; is it an untouched output from survtab?")
  
  
  ## subsetting ----------------------------------------------------------------
  subset <- substitute(subset)
  subset <- evalLogicalSubset(data = x, substiset = subset)
  
  
  ## getting y -----------------------------------------------------------------  
  if (!is.null(y)) {
    if (!is.character(y)) stop("please supply y as a character string indicating the name of a variable in x")
    if (length(y) > 1) stop("y must be of length 1 or NULL")
    all_names_present(x, y)
  } else {
    y <- surv_vars[length(surv_vars)]
    message("y was NULL; chose ", y, " automatically")
  }
  
  
  ## confidence intervals ------------------------------------------------------
  is_CIF <- FALSE
  if (substr(y,1,3) == "CIF") is_CIF <- TRUE
  varname <- copy(y)
  
  y.lo <- paste0(y, ".lo")
  y.hi <- paste0(y, ".hi")
  y.ci <- c(y.lo, y.hi)
  y.ci <- intersect(y.ci, names(x))
  ley <- length(c(y.ci))
  
  if (is.null(conf.int)) {
    conf.int <- ifelse(ley %in% 1:2, TRUE, FALSE)
  }
  if (!conf.int) y.ci <- NULL
  
  ## need to determine total number of strata; also used in casting
  ## note: if no by.vars, created a dummy repeating 1L as by.vars
  tmpBy <- makeTempVarName(x)
  x[, (tmpBy) := interaction(mget(rev(by.vars)))]
  x[, (tmpBy) := factor(get(tmpBy), labels = 1:uniqueN(get(tmpBy)))]
  x[, (tmpBy) := robust_values(get(tmpBy))]
  
  Nstrata <- x[subset, uniqueN(get(tmpBy))]
  
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
  
  ## cast to accommodate by.vars
  
  ## impute first values (time = 0, surv = 1 / cif = 0)
  first <- x[subset & !duplicated(get(tmpBy)), ]
  first[, c(varname) := ifelse(is_CIF, 0, 1)]
  first[, Tstop := 0]
  
  
  if (length(y.ci) > 0) first[, (y.ci) := get(varname) ]
  x <- rbindlist(list(first, x[subset, ]), use.names = TRUE)
  setkeyv(x, c(tmpBy, "surv.int"))
  
  x <- cast_simple(x, columns = tmpBy, rows = "Tstop", 
                   values = c(y, y.ci))
  estVars <- names(x)[1:Nstrata+1]
  strata <- lapply(rep(y, Nstrata), gsub, replacement = "", x = estVars)
  strata <- unique(unlist(strata))
  newOrder <- NULL
  
  
  for (k in strata) {
    newOrder <- c(newOrder, names(x)[grep(k, names(x))])
  }
  setcolorder(x, c("Tstop", newOrder))
  setDF(x)
  
  ## plotting
  graphics::matlines(x = x$Tstop, 
                     y = x[, setdiff(names(x), "Tstop")], 
                     col=col, lty=lty, ...)
  
  
}











