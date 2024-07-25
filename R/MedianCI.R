
#' Confidence Interval for the Median
#' 
#' Calculate the confidence interval for the median.
#' 
#' The \code{"exact"} method is the way SAS is said to calculate the confidence
#' interval. This is also implemented in \code{\link{SignTest}}. The boot
#' confidence interval type is calculated by means of \code{\link[boot]{boot.ci}}
#' with default type \code{"perc"}.\cr Use \code{\link{sapply}},
#' resp.\code{\link{apply}}, to get the confidence intervals from a data.frame
#' or from a matrix.
#' 
#' @param x a (non-empty) numeric vector of data values.
#' 
#' @param conf.level confidence level of the interval
#' 
#' @param sides a character string specifying the side of the confidence
#' interval, must be one of \code{"two.sided"} (default), \code{"left"} or
#' \code{"right"}. You can specify just the initial letter. \code{"left"} would
#' be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.
#' 
#' @param na.rm logical. Should missing values be removed? Defaults to
#' \code{FALSE}.
#' 
#' @param method defining the type of interval that should be calculated (one
#' out of \code{"exact"}, \code{"boot"}). Default is \code{"exact"}. See
#' Details.
#' 
#' @param \dots the dots are passed on to \code{\link[boot]{boot.ci}}. In particular,
#' the type of bootstrap confidence interval can be defined via this. The
#' defaults are \code{R=999} and \code{type="perc"}.
#' 
#' @return a numeric vector with 3 elements: \item{median}{median}
#' \item{lwr.ci}{lower bound of the confidence interval} \item{upr.ci}{upper
#' bound of the confidence interval}
#' 
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{wilcox.test}}, \code{\link{MeanCI}},
#' \code{\link{median}}, \code{\link{HodgesLehmann}}
#' @keywords univar
#' @examples
#' 
#' MedianCI(d.pizza$price, na.rm=TRUE)
#' MedianCI(d.pizza$price, conf.level=0.99, na.rm=TRUE)
#' 
#' t(round(sapply(d.pizza[,c("delivery_min","temperature","price")], MedianCI, na.rm=TRUE), 3))
#' 
#' MedianCI(d.pizza$price, na.rm=TRUE, method="exact")
#' MedianCI(d.pizza$price, na.rm=TRUE, method="boot")
#' 
#' 
#' x <- runif(100)
#' 
#' set.seed(448)
#' MedianCI(x, method="boot")
#' 
#' # ... the same as
#' set.seed(448)
#' MedianCI(x, method="boot", type="bca")
#' 
#' MedianCI(x, method="boot", type="basic")
#' MedianCI(x, method="boot", type="perc")
#' MedianCI(x, method="boot", type="norm", R=499)
#' # not supported:
#' MedianCI(x, method="boot", type="stud")
#' 
#' MedianCI(x, method="boot", sides="right")
#' 
#' 


# Confidence intervall for the median


MedianCI <- function(x, 
                     conf.level=0.95, sides = c("two.sided","left","right"), 
                     method=c("exact","boot"),
                     na.rm=FALSE, ...) {
  
  if(na.rm) x <- na.omit(x)
  
  MedianCI_Binom <- function( x, conf.level = 0.95,
                              sides = c("two.sided", "left", "right"), na.rm = FALSE ){
    
    # http://www.stat.umn.edu/geyer/old03/5102/notes/rank.pdf
    # http://de.scribd.com/doc/75941305/Confidence-Interval-for-Median-Based-on-Sign-Test
    if(na.rm) x <- na.omit(x)
    n <- length(x)
    switch( match.arg(sides)
            , "two.sided" = {
              k <- qbinom(p = (1 - conf.level) / 2, size=n, prob=0.5, lower.tail=TRUE)
              ci <- sort(x)[c(k, n - k + 1)]
              attr(ci, "conf.level") <- 1 - 2 * pbinom(k-1, size=n, prob=0.5)
            }
            , "left" = {
              k <- qbinom(p = (1 - conf.level), size=n, prob=0.5, lower.tail=TRUE)
              ci <- c(sort(x)[k], Inf)
              attr(ci, "conf.level") <- 1 - pbinom(k-1, size=n, prob=0.5)
            }
            , "right" = {
              k <- qbinom(p = conf.level, size=n, prob=0.5, lower.tail=TRUE)
              ci <- c(-Inf, sort(x)[k])
              attr(ci, "conf.level") <- pbinom(k, size=n, prob=0.5)
            }
    )
    # confints for small samples can be outside the observed range e.g. n < 6
    if(identical(StripAttr(ci), NA_real_)) {
      ci <- c(-Inf, Inf)
      attr(ci, "conf.level") <- 1
    }  
    return(ci)
  }
  
  MedianCI_Boot <- function(x, conf.level=0.95, sides = c("two.sided", "left", "right"), 
                            na.rm=FALSE, ...){
    
    if(sides!="two.sided")
      conf.level <- 1 - 2*(1-conf.level)
    
    R <- DescTools::InDots(..., arg="R", default=999)
    boot.med <- boot::boot(x, function(x, d) {
      median(x[d], na.rm=na.rm)
      # standard error for the median required for studentized bci type:
      # not implemented here, as not suitable for this case.
      # sqrt(pi/2) * MeanSE(x[d])
      # mad(x[d], na.rm=na.rm) / sqrt(length(na.omit(x[d])))
      
    }, R=R)
    
    dots <- list(...)
    if(is.null(dots[["type"]]))
      dots$type <- "perc"
    
    if(dots$type %nin% c("norm","basic","perc","bca")){
      warning(gettextf("bootstrap type '%s' is not supported", dots$type))
      return( c(NA, NA))
    }
    
    dots$boot.out <- boot.med
    dots$conf <- conf.level
    
    res <- do.call(boot::boot.ci, dots)
    
    if(dots$type == "norm")
      # uses different structure for results
      res <- res[[4]][c(2,3)]
    else
      res <- res[[4]][c(4,5)]
    
    return(res)
  }
  
  
  
  sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
  
  # if(sides!="two.sided")
  #   conf.level <- 1 - 2*(1-conf.level)
  
  # alte Version, ziemlich grosse Unterschiede zu wilcox.test:
  # Bosch: Formelsammlung Statistik (bei Markus Naepflin), S. 95
  # x <- sort(x)
  # return( c(
  # x[ qbinom(alpha/2,length(x),0.5) ], ### lower limit
  # x[ qbinom(1-alpha/2,length(x),0.5) ] ### upper limit
  # ) )
  
  method <- match.arg(arg=method, choices=c("exact","boot"))
  
  switch( method
          , "exact" = { # this is the SAS-way to do it
            # https://stat.ethz.ch/pipermail/r-help/2003-September/039636.html
            r <- MedianCI_Binom(x, conf.level = conf.level, sides=sides)
          }
          , "boot" = {
            r <- MedianCI_Boot(x, conf.level = conf.level, sides=sides, ...)
          } )
  
  med <- median(x, na.rm=na.rm)
  if(is.na(med)) {   # do not report a CI if the median is not defined...
    res <- rep(NA, 3)
    
  } else {
    res <- c(median=med, r)
    # report the conf.level which can deviate from the required one
    if(method=="exact")  attr(res, "conf.level") <-  attr(r, "conf.level")
  }
  names(res) <- c("median","lwr.ci","upr.ci")
  
  if(sides=="left")
    res[3] <- Inf
  else if(sides=="right")
    res[2] <- -Inf
  
  return( res )
  
}

