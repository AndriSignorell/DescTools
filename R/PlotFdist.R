
#' Frequency Distribution Plot 
#' 
#' This function was developed to create a univariate graphical representation 
#' of the frequency distribution of a numerical vector. 
#' It combines a histogram, a density
#' curve, a boxplot and the empirical cumulative distribution function (ecdf)
#' in one single plot. A rug as well as a model distribution curve (e.g. a
#' normal curve) can optionally be superposed. This results in a dense and
#' informative picture of the facts. Still the function remains flexible as all
#' possible arguments can be passed to the single components (\code{hist},
#' \code{boxplot} etc.) as a list (see examples). 
#' 
#' Performance has been significantly improved, but if \code{x} is growing
#' large (n > 1e7) the function will take its time to complete. Especially the
#' density curve and the ecdf, but as well as the boxplot (due to the chosen
#' alpha channel) will take their time to calculate and plot.\cr In such cases
#' consider taking a sample, i.e. \code{ PlotFdist(x[sample(length(x),
#' size=5000)])}, the big picture of the distribution won't usually change
#' much. .
#' 
#' @param x the numerical variable, whose distribution is to be plotted. 
#' @param main main title of the plot. 
#' @param xlab label of the x-axis, defaults to \code{""}. (The name of the
#' variable is typically placed in the main title and would be redundant here.)
#' @param xlim range of the x-axis, defaults to a pretty \code{range(x, na.rm =
#' TRUE)}. 
#' @param args.hist list of additional arguments to be passed to the histogram
#' \code{hist()}.  The defaults chosen when setting \code{args.hist = NULL} are
#' more or less the same as in \code{\link{hist}}. The argument \code{type}
#' defines, whether a histogram (\code{"hist"}) or a plot with \code{type =
#' "h"} (for 'histogram' like vertical lines for \code{mass} representation)
#' should be used.  The arguments for a "h-plot"" will be \code{col},
#' \code{lwd}, \code{pch.col}, \code{pch}, \code{pch.bg} for the line and for
#' an optional point character on top.  The default type used will be chosen on
#' the structure of \code{x}. If \code{x} is an integer with up to 12 unique
#' values there will be a "h-plot" and else a histogram! 
#' 
#' @param args.rug list of additional arguments to be passed to the function
#' \code{rug()}.  Use \code{args.rug = NA} if no rug should be added. This is
#' the default. Use \code{args.rug = NULL} to add rug with reasonable default
#' values.  
#' @param args.dens list of additional arguments to be passed to
#' \code{density}.  Use \code{args.dens = NA} if no density curve should be
#' drawn. The defaults are taken from \code{\link{density}}. 
#' @param args.curve list of additional arguments to be passed to
#' \code{\link{curve}}.  This argument allows to add a fitted distribution
#' curve to the histogram. By default no curve will be added (\code{args.curve
#' = NA}). If the argument is set to \code{NULL}, a normal curve with
#' \code{mean(x)} and \code{sd(x)} will be drawn. See examples for more
#' details. 
#' @param args.boxplot list of additional arguments to be passed to the boxplot
#' \code{boxplot()}.  The defaults are pretty much the same as in
#' \code{\link{boxplot}}.  The two additional arguments \code{pch.mean}
#' (default \code{23}) and \code{col.meanci} (default \code{"grey80"}) control,
#' if the mean is displayed within the boxplot. Setting those arguments to
#' \code{NA} will prevent them from being displayed. 

#' @param args.ecdf list of additional arguments to be passed to \code{ecdf()}.
#' Use \code{args.ecdf = NA} if no empirical cumulation function should be
#' included in the plot.  The defaults are taken from \code{\link{plot.ecdf}}.

#' @param args.curve.ecdf list of additional arguments to be passed to
#' \code{\link{curve}}.  This argument allows to add a fitted distribution
#' curve to the cumulative distribution function. By default no curve will be
#' added (\code{args.curve.ecdf = NA}). If the argument is set to \code{NULL},
#' a normal curve with \code{mean(x)} and \code{sd(x)} will be drawn. See
#' examples for more details. 

#' @param heights heights of the plotparts, defaults to \code{c(2,0.5,1.4)} for
#' the histogram, the boxplot and the empirical cumulative distribution
#' function, resp. to \code{c(2,1.5)} for a histogram and a boxplot only. 
#' 
#' @param pdist distances of the plotparts, defaults to \code{c(0, 0)}, say
#' there will be no distance between the histogram, the boxplot and the
#' ecdf-plot. This can be useful for instance in case that the x-axis has to be
#' added to the histogram. 
#' 
#' @param na.rm logical, should \code{NA}s be omitted? Histogram and boxplot
#' could do without this option, but the density-function refuses to plot with
#' missings. Defaults to \code{FALSE}. 
#' 
#' @param cex.axis character extension factor for the axes.
#' @param cex.main character extension factor for the main title. Must be set
#' in dependence of the plot parts in order to get a harmonic view.
#' 
#' @param mar A numerical vector of the form \code{c(bottom, left, top, right)}
#' which gives the number of lines of outer margin to be specified on the four
#' sides of the plot. The default is \code{c(0, 0, 3, 0)}.
#' 
#' @param las numeric in \code{c(0,1,2,3)}; the orientation of axis labels. See
#' \code{\link{par}}.
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{hist}}, \code{\link{boxplot}}, \code{\link{ecdf}},
#' \code{\link{density}}, \code{\link{rug}}, \code{\link{layout}} 
#' @keywords hplot
#' 
#' @examples
#' 
#' PlotFdist(x=d.pizza$delivery_min, na.rm=TRUE)
#' 
#' # define additional arguments for hist, dens and boxplot
#' # do not display the mean and its CI on the boxplot
#' PlotFdist(d.pizza$delivery_min, args.hist=list(breaks=50),
#'   args.dens=list(col="olivedrab4"), na.rm=TRUE,
#'   args.boxplot=list(col="olivedrab2", pch.mean=NA, col.meanci=NA))
#' 
#' 
#' # do a "h"-plot instead of a histogram for integers
#' x <- sample(runif(10), 100, replace = TRUE)
#' PlotFdist(x, args.hist=list(type="mass"))
#' 
#' pp <- rpois(n = 100, lambda = 3)
#' PlotFdist(pp, args.hist = list(type="mass", pch=21, col=DescTools::horange,
#'           cex.pch=2.5, col.pch=DescTools::hred, lwd=3, bg.pch="white"),
#'           args.boxplot = NULL, args.ecdf = NA, main="Probability mass function")
#' 
#' # special arguments for hist, density and ecdf
#' PlotFdist(x=faithful$eruptions,
#'           args.hist=list(breaks=20), args.dens=list(bw=.1),
#'           args.ecdf=list(cex=1.2, pch=16, lwd=1), args.rug=TRUE)
#' 
#' # no density curve, no ecdf but add rug instead, make boxplot a bit higher
#' PlotFdist(x=d.pizza$delivery_min, na.rm=TRUE, args.dens=NA, args.ecdf=NA,
#'   args.hist=list(xaxt="s"),  # display x-axis on the histogram
#'   args.rug=TRUE, heights=c(3, 2.5), pdist=2.5, main="Delivery time")
#' 
#' # alpha channel on rug is cool, but takes its time for being drawn...
#' PlotFdist(x=d.pizza$temperature, args.rug=list(col=SetAlpha("black", 0.1)), na.rm=TRUE)
#' 
#' # plot a normal density curve, but no boxplot nor ecdf
#' x <- rnorm(1000)
#' PlotFdist(x, args.curve = NULL, args.boxplot=NA, args.ecdf=NA)
#' 
#' # compare with a t-distribution
#' PlotFdist(x, args.curve = list(expr="dt(x, df=2)", col="darkgreen"),
#'           args.boxplot=NA, args.ecdf=NA)
#' legend(x="topright", legend=c("kernel density", "t-distribution (df=2)"),
#'        fill=c(getOption("col1", DescTools::hred), "darkgreen"), xpd=NA)
#' 
#' # add a gamma distribution curve to both, histogram and ecdf
#' ozone <- airquality$Ozone; m <- mean(ozone, na.rm = TRUE); v <- var(ozone, na.rm = TRUE)
#' PlotFdist(ozone, args.hist = list(breaks=15),
#'   args.curve = list(expr="dgamma(x, shape = m^2/v, scale = v/m)", col=DescTools::hecru),
#'   args.curve.ecdf = list(expr="pgamma(x, shape = m^2/v, scale = v/m)", col=DescTools::hecru),
#'   na.rm = TRUE, main = "Airquality - Ozone")
#' 
#' legend(x="topright", xpd=NA,
#'        legend=c(expression(plain("gamma:  ") * Gamma * " " * bgroup("(", k * " = " *
#'            over(bar(x)^2, s^2) * " , " * theta * plain(" = ") * over(s^2, bar(x)), ")") ),
#'                 "kernel density"),
#'        fill=c(DescTools::hecru, getOption("col1", DescTools::hred)), text.width = 0.25)
#' 




PlotFdist <- function (x, main = deparse(substitute(x)), xlab = ""
                       , xlim = NULL
                       # , do.hist =NULL # !(all(IsWhole(x,na.rm=TRUE)) & length(unique(na.omit(x))) < 13)
                       # do.hist overrides args.hist, add.dens and rug
                       , args.hist = NULL          # list( breaks = "Sturges", ...)
                       , args.rug = NA             # list( ticksize = 0.03, side = 1, ...), pass NA if no rug
                       , args.dens = NULL          # list( bw = "nrd0", col="#9A0941FF", lwd=2, ...), NA for no dens
                       , args.curve = NA           # list( ...), NA for no dcurve
                       , args.boxplot = NULL       # list( pars=list(boxwex=0.5), ...), NA for no boxplot
                       , args.ecdf = NULL          # list( col="#8296C4FF", ...), NA for no ecdf
                       , args.curve.ecdf = NA      # list( ...), NA for no dcurve
                       , heights = NULL            # heights (hist, boxplot, ecdf) used by layout
                       , pdist = NULL              # distances of the plots, default = 0
                       , na.rm = FALSE, cex.axis = NULL, cex.main = NULL, mar = NULL, las=1) {
  
  
  
  .PlotMass <- function(x = x, xlab = "", ylab = "",
                        xaxt = ifelse(add.boxplot || add.ecdf, "n", "s"), xlim = xlim, ylim = NULL, main = NA, las = 1,
                        yaxt="n", col=1, lwd=3, pch=NA, col.pch=1, cex.pch=1, bg.pch=0, cex.axis=cex.axis, ...)   {
    
    pp <- prop.table(table(x))
    
    if(is.null(ylim))
      ylim <- c(0, max(pretty(pp)))
    
    plot(pp, type = "h", lwd=lwd, col=col,
         xlab = "", ylab = "", cex.axis=cex.axis, xlim=xlim, ylim=ylim,
         xaxt = xaxt, main = NA, frame.plot = FALSE,
         las = las, panel.first = {
           abline(h = axTicks(2), col = "grey", lty = "dotted")
           abline(h = 0, col = "black")
         })
    
    if(!identical(pch, NA))
      points(pp, type="p", pch=pch, col=col.pch, bg=bg.pch, cex=cex.pch)
    
  }
  
  
  
  # Plot function to display the distribution of a cardinal variable
  # combines a histogram with a density curve, a boxplot and an ecdf
  # rug can be added by using add.rug = TRUE
  
  # default colors are Helsana CI-colors
  
  # dev question: should dots be passed somewhere??
  
  # usr <- par(no.readonly=TRUE)
  # on.exit({
  #   graphics::par(usr)               # reset par on exit  
  #   graphics::layout(matrix(1))      # reset layout on exit    
  # }, add=TRUE)
  
  
  .withGraphicsState({
    
    if(!is.null(cex.axis)) par(cex.axis=cex.axis)
    if(!is.null(cex.main)) par(cex.axis=cex.main)
    
    opt <- DescToolsOptions(stamp=NULL)
    
    add.boxplot <- !identical(args.boxplot, NA)
    add.rug <- !identical(args.rug, NA)
    add.dens <- !identical(args.dens, NA)
    add.ecdf <- !identical(args.ecdf, NA)
    add.dcurve <- !identical(args.curve, NA)
    add.pcurve <- !identical(args.curve.ecdf, NA)
    
    # preset heights
    if(is.null(heights)){
      if(add.boxplot) {
        if(add.ecdf) heights <- c(1.8, 0.5, 1.6)
        else heights <- c(2, 1.4)
      } else {
        if(add.ecdf) heights <- c(2, 1.4)
      }
    }
    
    if(is.null(pdist)) {
      if(add.boxplot) pdist <- c(0, 0)
      else pdist <- c(0, 1)
    }
    
    # layout changes par settings arbitrarily, especially cex in the first case
    # so store here and reset
    ppp <- par()[grep("cex", names(par()))]
    if (add.ecdf && add.boxplot) {
      layout(matrix(c(1, 2, 3), nrow = 3, byrow = TRUE), heights = heights, TRUE)
      # if(is.null(cex.axis)) cex.axis <- 1.3
      # if(is.null(cex.main)) cex.main <- 1.7
    } else {
      if((add.ecdf || add.boxplot)) {
        layout(matrix(c(1, 2), nrow = 2, byrow = TRUE), heights = heights[1:2], TRUE)
        #      if(is.null(cex.axis)) cex.axis <- 0.9
        # } else {
        #   if(is.null(cex.axis)) cex.axis <- 0.95
      }
    }
    par(ppp)  # reset unwanted layout changes
    
    # plot histogram, change margin if no main title
    par(mar = c(ifelse(add.boxplot || add.ecdf, 0, 5.1), 4.1, 2.1, 2.1))
    
    if(!is.null(mar)) {
      par(oma=mar)
    } else {
      if(!is.na(main)) { par(oma=c(0,0,2,0)) }
    }
    
    # wait for omitting NAs until all arguments are evaluated, e.g. main...
    if(na.rm) x <- x[!is.na(x)]
    
    
    if(!is.null(args.hist[["panel.last"]])) {
      panel.last <- args.hist[["panel.last"]]
      args.hist[["panel.last"]] <- NULL
      
    } else {
      panel.last <- NULL
    }
    
    if(is.null(args.hist$type)){
      do.hist <- !(isTRUE(all.equal(x, round(x), tol = sqrt(.Machine$double.eps))) && length(unique(x)) < 13)
    } else {
      do.hist <- (args.hist$type == "hist")
      args.hist$type <- NULL
    }
    
    # handle open list of arguments: args.legend in barplot is implemented this way...
    # we need histogram anyway to define xlim
    args.hist1 <- list(x = x, xlab = "", ylab = "", freq = FALSE,
                       xaxt = ifelse(add.boxplot || add.ecdf, "n", "s"), xlim = xlim, ylim = NULL, main = NA, las = 1,
                       col = "white", border = "grey70", yaxt="n")
    if (!is.null(args.hist)) {
      args.hist1[names(args.hist)] <- args.hist
    }
    
    
    x.hist <- DoCall("hist", c(args.hist1[names(args.hist1) %in%
                                            c("x", "breaks", "include.lowest", "right", "nclass")], plot = FALSE))
    x.hist$xname <- deparse(substitute(x))
    if (is.null(xlim))    args.hist1$xlim <- range(pretty(x.hist$breaks))
    args.histplot <- args.hist1[!names(args.hist1) %in% c("x", "breaks", "include.lowest", "right", "nclass")]
    
    
    if (do.hist) {
      # calculate max ylim for density curve, provided there should be one...
      # what's the maximal value in density or in histogramm$densities?
      
      # plot density
      if (add.dens) {
        # preset default values
        args.dens1 <- list(x = x, bw = (if(length(x) > 1000){"nrd0"} else {"SJ"}),
                           col = Pal()[2], lwd = 2, lty = "solid")
        if (!is.null(args.dens)) {
          args.dens1[names(args.dens)] <- args.dens
        }
        
        # x.dens <- DoCall("density", args.dens1[-match(c("col",
        #                                                  "lwd", "lty"), names(args.dens1))])
        #
        # # overwrite the ylim if there's a larger density-curve
        # args.histplot[["ylim"]] <- range(pretty(c(0, max(c(x.dens$y, x.hist$density)))))
        
        x.dens <- try( DoCall("density", 
                              args.dens1[-match(c("col", "lwd", "lty"), names(args.dens1))])
                       , silent=TRUE)
        
        if(inherits(x.dens, "try-error")) {
          warning(gettextf("density curve could not be added\n%s", x.dens))
          add.dens <- FALSE
          
        } else {
          # overwrite the ylim if there's a larger density-curve
          # but only if the user has not set an ylim value by himself, 
          # ... we should not disobey or overrun user instructions 
          if(is.null(args.histplot[["ylim"]]))
            args.histplot[["ylim"]] <- range(pretty(c(0, max(c(x.dens$y, x.hist$density)))))
          
        }
        
      }
      
      # plot histogram
      DoCall("plot", append(list(x.hist), args.histplot))
      
      # draw axis
      ticks <- axTicks(2)
      n <- max(floor(log(ticks, base = 10)))    # highest power of ten
      if(abs(n)>2) {
        lab <- Format(ticks * 10^(-n), digits=max(Ndec(as.character(zapsmall(ticks*10^(-n))))))
        axis(side=2, at=ticks, labels=lab, las=las, cex.axis=par("cex.axis"))
        
        text(x=par("usr")[1], y=par("usr")[4], bquote(~~~x~10^.(n)), xpd=NA, 
             pos = 3, cex=par("cex.axis") * 0.8)
        
      } else {
        axis(side=2, cex.axis=par("cex.axis"), las=las)
        
      }
      
      if(!is.null(panel.last)){
        eval(parse(text=panel.last))
      }
      
      if (add.dens) {
        lines(x.dens, col = args.dens1$col, lwd = args.dens1$lwd, lty = args.dens1$lty)
      }
      
      
      # plot special distribution curve
      if (add.dcurve) {
        # preset default values
        args.curve1 <- list(expr = parse(text = gettextf("dnorm(x, %s, %s)", mean(x), sd(x))),
                            add = TRUE,
                            n = 500, col = Pal()[3], lwd = 2, lty = "solid")
        if (!is.null(args.curve)) {
          args.curve1[names(args.curve)] <- args.curve
        }
        
        if (is.character(args.curve1$expr)) args.curve1$expr <- parse(text=args.curve1$expr)
        
        # do.call("curve", args.curve1)
        # this throws an error heere:
        # Error in eval(expr, envir, enclos) : could not find function "expr"
        # so we roll back to do.call
        do.call("curve", args.curve1)
        
      }
      
      
      if (add.rug) {
        args.rug1 <- list(x = x, col = "grey")
        if (!is.null(args.rug)) {
          args.rug1[names(args.rug)] <- args.rug
        }
        DoCall("rug", args.rug1)
      }
      
      
    } else {
      # do not draw a histogram, but a line bar chart
      # PlotMass
      args.hist1 <- list(x = x, xlab = "", ylab = "", xlim = xlim,
                         xaxt = ifelse(add.boxplot || add.ecdf, "n", "s"), 
                         ylim = NULL, main = NA, las = 1,
                         yaxt="n", col=1, lwd=3, pch=NA, col.pch=1, 
                         cex.pch=2, bg.pch=0, cex.axis=cex.axis)
      if (is.null(xlim))    args.hist1$xlim <- range(pretty(x.hist$breaks))
      
      if (!is.null(args.hist)) {
        args.hist1[names(args.hist)] <- args.hist
        if(is.null(args.hist$col.pch))   # use the same color for pch as for the line, when not defined
          args.hist1$col.pch <- args.hist1$col
      }
      
      DoCall(.PlotMass, args.hist1)
      
      
      # plot(prop.table(table(x)), type = "h", xlab = "", ylab = "",
      #      xaxt = "n", xlim = args.hist1$xlim, main = NA,
      #      frame.plot = FALSE, las = 1, cex.axis = cex.axis, panel.first = {
      #        abline(h = axTicks(2), col = "grey", lty = "dotted")
      #        abline(h = 0, col = "black")
      #      })
    }
    
    # boxplot
    if(add.boxplot){
      par(mar = c(ifelse(add.ecdf, 0, 5.1), 4.1, pdist[1], 2.1))
      args.boxplot1 <- list(x = x, frame.plot = FALSE, main = NA, boxwex = 1,
                            horizontal = TRUE, ylim = args.hist1$xlim, col="grey95",
                            at = 1, xaxt = ifelse(add.ecdf, "n", "s"),
                            outcex = 1.3, outcol = rgb(0,0,0,0.5), cex.axis=cex.axis,
                            pch.mean=3, col.meanci="grey85")
      if (!is.null(args.boxplot)) {
        args.boxplot1[names(args.boxplot)] <- args.boxplot
      }
      plot(1, type="n", xlim=args.hist1$xlim, ylim=c(0,1)+.5, xlab="", ylab="", axes=FALSE)
      grid(ny=NA)
      if(length(x)>1){
        ci <- MeanCI(x, na.rm=TRUE)
        rect(xleft = ci[2], ybottom = 0.62, xright = ci[3], ytop = 1.35,
             col=args.boxplot1$col.meanci, border=NA)
      } else {
        ci <- mean(x)
      }
      args.boxplot1$add = TRUE
      DoCall("boxplot", args.boxplot1)
      points(x=ci[1], y=1, cex=1.5, col="grey65", pch=args.boxplot1$pch.mean, bg="white")
      
    }
    
    # plot ecdf
    if (add.ecdf) {
      par(mar = c(5.1, 4.1, pdist[2], 2.1))
      #     args.ecdf1 <- list(x = x, frame.plot = FALSE, main = NA,
      #                        xlim = args.hist1$xlim, col = getOption("col1", hblue), lwd = 2,
      #                        xlab = xlab, yaxt = "n", ylab = "", verticals = TRUE,
      #                        do.points = FALSE, cex.axis = cex.axis)
      
      # 13.1.2018 Andri:
      # if there are many datapoints (n > 1e5) well distributed over the x range, a histogram is significantly
      # faster, than plot.ecdf, which will break down in performance
      # however, if there are only few unique values, the histogram will not be correct and might result in
      # gross deviations.
      # example: PlotECDF(rep(-40, 2001), breaks = 1000)
      
      # we provisionally use the number of classes length(x.hist$mids) as proxy for good distribution
      # not sure, how robust this is...
      
      args.ecdf1 <- list(x = x, main = NA, 
                         breaks={if(length(x)>1000 & length(x.hist$mids) > 10) 1000 else NULL}, 
                         ylim=c(0,1),
                         xlim = args.hist1$xlim, col = Pal()[1], lwd = 2,
                         xlab = "", ylab = "", 
                         frame.plot = FALSE, cex.axis=cex.axis)
      if (!is.null(args.ecdf)) {
        args.ecdf1[names(args.ecdf)] <- args.ecdf
      }
      
      DoCall("PlotECDF", args.ecdf1)
      
      # plot special distribution ecdf curve
      if (add.pcurve) {
        # preset default values
        args.curve.ecdf1 <- list(expr = parse(text = gettextf("pnorm(x, %s, %s)", mean(x), sd(x))),
                                 add = TRUE,
                                 n = 500, col = Pal()[3], lwd = 2, lty = "solid")
        if (!is.null(args.curve.ecdf)) {
          args.curve.ecdf1[names(args.curve.ecdf)] <- args.curve.ecdf
        }
        
        if (is.character(args.curve.ecdf1$expr))
          args.curve.ecdf1$expr <- parse(text=args.curve.ecdf1$expr)
        
        # do.call("curve", args.curve1)
        # this throws an error here:
        # Error in eval(expr, envir, enclos) : could not find function "expr"
        # so we roll back to do.call
        do.call("curve", args.curve.ecdf1)
        
      }
      
    }
    
    if(!is.na(main)) {
      title(main=main, outer = TRUE)
    }
    
    if(!identical(xlab, "")) {
      title(xlab=xlab)
    }
    
    DescToolsOptions(opt)
    
    if(!is.null(DescToolsOptions("stamp")))
      Stamp()
  
    # close .withGraphicsState
    })
  
}

