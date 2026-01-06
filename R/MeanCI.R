
#' Confidence Intervals for the Mean
#' 
#' Collection of several approaches to determine confidence intervals for the
#' mean. Both, the classical way and bootstrap intervals are implemented for
#' both, normal and trimmed means.
#' 
#' The confidence intervals for the trimmed means use winsorized variances as
#' described in the references.
#' 
#' @param x a (non-empty) numeric vector of data values. 
#' @param conf.level confidence level of the interval. 
#' @param sides a character string specifying the side of the confidence
#' interval, must be one of \code{"two.sided"} (default), \code{"left"} or
#' \code{"right"}. \code{"left"} would be analogue to a hypothesis of
#' \code{"greater"} in a \code{t.test}. You can specify just the initial
#' letter.
#' @param method A vector of character strings representing the type of
#' intervals required. The value should be any subset of the values
#' \code{"classic"}, \code{"boot"}.  See \code{\link[boot]{boot.ci}}. 
#' @param sd the standard deviation of x. If provided it's interpreted as sd of
#' the population and the normal quantiles will be used for constructing the
#' confidence intervals. If left to \code{NULL} (default) the sample
#' \code{sd(x)} will be calculated and used in combination with the
#' t-distribution.
#' @param trim the fraction (0 to 0.5) of observations to be trimmed from each
#' end of \code{x} before the mean is computed. Values of \code{trim} outside
#' that range are taken as the nearest endpoint. 
#' @param na.rm a logical value indicating whether \code{NA} values should be
#' stripped before the computation proceeds. Defaults to FALSE. 
#' 
#' @param ... further arguments are passed to the \code{\link[boot]{boot}} function.
#' Supported arguments are \code{type} (\code{"norm"}, \code{"basic"},
#' \code{"stud"}, \code{"perc"}, \code{"bca"}), \code{parallel} and the number
#' of bootstrap replicates \code{R}. If not defined those will be set to their
#' defaults, being \code{"basic"} for \code{type}, option
#' \code{"boot.parallel"} (and if that is not set, \code{"no"}) for
#' \code{parallel} and \code{999} for \code{R}.

#' @return a numeric vector with 3 elements: \item{est}{estimator, say the calculated mean}
#' \item{lci}{lower bound of the confidence interval} \item{uci}{upper
#' bound of the confidence interval}

#' @author Andri Signorell <andri@@signorell.net>
#' 
#' @seealso \code{\link{Mean}}, \code{\link{t.test}}, \code{\link{MeanDiffCI}},
#' \code{\link{MedianCI}}, \code{\link{VarCI}}, \code{\link{MeanCIn}}

#' @references Wilcox, R. R., Keselman H. J. (2003) Modern robust data analysis
#' methods: measures of central tendency \emph{Psychol Methods}, 8(3):254-74
#' 
#' Wilcox, R. R. (2005) \emph{Introduction to robust estimation and hypothesis
#' testing} Elsevier Academic Press
#' @keywords univar

#' @examples
#' 
#' x <- d.pizza$price[1:20]
#' 
#' MeanCI(x, na.rm=TRUE)
#' MeanCI(x, conf.level=0.99, na.rm=TRUE)
#' 
#' MeanCI(x, sides="left")
#' # same as:
#' t.test(x, alternative="greater")
#' 
#' MeanCI(x, sd=25, na.rm=TRUE)
#' 
#' # the different types of bootstrap confints
#' MeanCI(x, method="boot", type="norm", na.rm=TRUE)
#' MeanCI(x, trim=0.1, method="boot", type="norm", na.rm=TRUE)
#' MeanCI(x, trim=0.1, method="boot", type="basic", na.rm=TRUE)
#' MeanCI(x, trim=0.1, method="boot", type="stud", na.rm=TRUE)
#' MeanCI(x, trim=0.1, method="boot", type="perc", na.rm=TRUE)
#' MeanCI(x, trim=0.1, method="boot", type="bca", na.rm=TRUE)
#' 
#' MeanCI(x, trim=0.1, method="boot", type="bca", R=1999, na.rm=TRUE)
#' 
#' # Getting the MeanCI for more than 1 column
#' round(t(sapply(d.pizza[, 1:4], MeanCI, na.rm=TRUE)), 3)
#' 



MeanCI <- function (x,  
                    conf.level = 0.95, sides = c("two.sided","left","right"), 
                    method = c("classic", "boot"),
                    sd = NULL, trim = 0,
                    na.rm = FALSE, ...) {
  
  if (na.rm) x <- na.omit(x)
  
  sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
  
  if(sides!="two.sided")
    conf.level <- 1 - 2*(1-conf.level)
  
  winvar <- function(x, trim) {
    n <- length(x)
    # calculate the winsorized variance of x
    trn <- floor(trim * n) + 1
    
    # new 17.2.2015:
    minval <- sort(x, partial = trn)[trn]
    maxval <- sort(x, partial = max((n - trn + 1), 1))[max((n - trn + 1), 1)]
    winvar <- var(DescTools::Winsorize(x, val = c(minval, maxval)))
    
    # This was an overkill, we need only the n-thest value here:
    # winvar <- var(Winsorize(x, minval=max(Small(x, trn)), maxval=min(Large(x, trn))))
    #
    # degrees of freedom
    DF <- n - 2*(trn-1) - 1
    return(c(var=winvar, DF=DF))
  }
  
  method <- match.arg(method, c("classic", "boot"))
  if(method == "classic"){
    if(trim != 0) {
      # see: http://dornsife.usc.edu/assets/sites/239/docs/Rallfun-v27.txt
      #      http://www.psychology.mcmaster.ca/bennett/boot09/rt2.pdf
      
      wvar <- winvar(x, trim)
      # the standard error
      se <- sqrt(wvar["var"]) / ((1 - 2*trim) * sqrt(length(x)))
      
      res <- mean(x, trim = trim) + c(0, -1, 1) * qt(1-(1-conf.level)/2, wvar["DF"]) * se
      
    } else {
      if(is.null(sd)) {
        a <- qt(p = (1 - conf.level)/2, df = length(x) - 1) * sd(x)/sqrt(length(x))
      } else {
        a <- qnorm(p = (1 - conf.level)/2) * sd/sqrt(length(x))
      }
      res <- c(mean(x), mean(x) + a, mean(x) - a)
    }
    
  } else {
    
    # see: http://www.psychology.mcmaster.ca/bennett/boot09/percentileT.pdf
    # this might contain an erroneous calculation of boot variance...
    
    btype <- InDots(..., arg="type", default="basic")
    
    # we need separate functions for trimmed means and normal means
    if(trim != 0) {
      boot.fun <- boot(x,
                       function(x, i){
                         # this is according to the example in boot.ci
                         m <- mean(x[i], na.rm = FALSE, trim = trim)
                         n <- length(i)
                         v <- winvar(x, trim)/((1-2*trim)*sqrt(length(x)))^2
                         c(m, v)
                       },
                       R=InDots(..., arg="R", default=999),
                       parallel=InDots(..., arg="parallel", default="no"))
      
    } else {
      boot.fun <- boot(x,
                       function(x, i){
                         # this is according to the example in boot.ci
                         m <- mean(x[i], na.rm = FALSE)
                         n <- length(i)
                         v <- (n-1) * var(x[i]) / n^2
                         # v <- (sd(x[i]) / sqrt(n))^2  # following Bennet
                         c(m, v)
                         # IMPORTANT: boot.ci requires the estimated VARIANCE of the statistic
                         # pop sd estimated from bootstrapped sample
                       },
                       R=InDots(..., arg="R", default=999),
                       parallel=InDots(..., arg="parallel", default="no"))
    }
    ci <- boot.ci(boot.fun, conf=conf.level, type=btype)
    
    if(btype == "norm"){
      res <- c(mean=boot.fun$t0[1], lwr.ci=ci[[4]][2], upr.ci=ci[[4]][3])
    } else {
      res <- c(mean=boot.fun$t0[1], lwr.ci=ci[[4]][4], upr.ci=ci[[4]][5])
    }
  }
  
  if(sides=="left")
    res[3] <- Inf
  else if(sides=="right")
    res[2] <- -Inf
  
  names(res) <- c("est", "lci", "uci")
  return(res)
  
}



