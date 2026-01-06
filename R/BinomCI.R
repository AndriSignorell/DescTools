

# internal functions

.wilson <- function(x, n, alpha) {
  
  p.hat <- x/n
  q.hat <- 1 - p.hat
  z <- qnorm(1-alpha/2)
  z2 <- z^2
  
  term1 <- (x + z2/2) / (n + z2)
  term2 <- z * sqrt(n) / (n + z2) * 
    sqrt(p.hat * q.hat + z2 / (4 * n))
  
  return(c(
    lci = max(0, term1 - term2),
    uci = min(1, term1 + term2)
  ))
  
}


.wilson_cc <- function(x, n, alpha) {
  
  p.hat <- x/n
  q.hat <- 1 - p.hat
  z <- qnorm(1 - alpha/2)
  z2 <- z^2
  
  lci <- ( 2 * x + z2 - 1 - z * sqrt(z2 - 2 - 1/n + 
              4 * p.hat * (n * q.hat + 1))) / (2 * (n + z2))
  
  uci <- ( 2 * x + z2 + 1 + z * sqrt(z^2 + 2 - 1/n + 
              4 * p.hat * (n * q.hat - 1))) / (2 * (n + z2))
  
  return( c(
    lci = max(0, ifelse(p.hat == 0, 0, lci)),
    uci = min(1, ifelse(p.hat == 1, 1, uci)))
  )
  
}


.wilson_mod <- function(x, n, alpha) {
  
  p.hat <- x/n
  q.hat <- 1 - p.hat
  z <- qnorm(1 - alpha/2)
  z2 <- z^2
  
  term1 <- (x + z2/2) / (n + z2)
  term2 <- z * sqrt(n) / (n + z2) * sqrt(p.hat * q.hat + z2 / (4 * n))
  
  if((n <= 50 & x %in% c(1, 2)) | (n >= 51 & x %in% c(1:3)))
    lci <- 0.5 * qchisq(alpha, 2 * x)/n
  else
    lci <-  max(0, term1 - term2)
  
  if((n <= 50 & x %in% c(n-1, n-2)) | (n >= 51 & x %in% c(n-(1:3))))
    uci <- 1 - 0.5 * qchisq(alpha, 2 * (n - x))/n
  else
    uci <- min(1, term1 + term2)
  
  return( c( lci = lci, uci = uci) )
  
}



.agresti_coull <- function(x, n, alpha)  {

  z <- qnorm(1-alpha/2)
  
  n.tilde <- n + z^2
  p.tilde <- .nonStdEst(x, n, z)
  q.tilde <- 1 - p.tilde
  
  term2 <- z * sqrt(p.tilde * q.tilde) / sqrt(n.tilde)
  
  return( c(
    lci = max(0, p.tilde - term2),
    uci = min(1, p.tilde + term2))
    )
  
}



.wald <- function(x, n, alpha, corr=FALSE){
  
  p.hat <- x/n
  
  # margin of error
  ME <- qnorm(1 - alpha/2) * sqrt(p.hat * (1 - p.hat) / n)
  
  # continuity correction
  if(corr)
    ME <- ME + 1/(2*n)
  
  lci <- max(0, p.hat - ME)
  uci <- min(1, p.hat + ME)
  
  return(c(lci=lci, uci=uci))
}


.jeffreys <- function(x, n, alpha){
  
  return(c( 
    lci = if(x == 0) 0 
            else qbeta(alpha/2, x + 0.5, n - x + 0.5),
    uci = if(x == n) 1 
            else qbeta(1-alpha/2, x + 0.5, n - x + 0.5)
  ))
  
}


.jeffreys_mod <- function(x, n, alpha)  {
  
  return(c(
    lci =
      if (x == n) { 
        (alpha/2)^(1/n) 
      } else if (x <= 1) {
        0
      } else {
        qbeta(alpha/2, x + 0.5, n - x + 0.5)
      },
    
    uci =
      if (x == 0) {
        1 - (alpha/2)^(1/n)
      } else if (x >= n - 1) {
        1
      } else {
        qbeta(1 - alpha/2, x + 0.5, n - x + 0.5)
      }
  ))
  
}


.clopper_pearson <- function(x, n, alpha){
  return(c(
    lci = if (x == 0) 0 else qbeta(alpha/2, x, n - x + 1),
    uci = if (x == n) 1 else qbeta(1 - alpha/2, x + 1, n - x)
  ))
}


.arcsine <- function(x, n, alpha) {
  
  p.tilde <- (x + 0.375)/(n + 0.75)
  ME <- 0.5 * qnorm(1-alpha/2) / sqrt(n)
  
  res <- c(
    lci = sin(asin(sqrt(p.tilde)) - ME)^2,
    uci = sin(asin(sqrt(p.tilde)) + ME)^2
  ) 
  attr(res, "p.tilde") <- p.tilde
  
  return(res)  
  
}


.logit <- function(x, n, alpha){
  
  SetNames(
    
    LogitInv(log(x/(n-x)) - 
             c(1,-1) * qnorm(1-alpha/2) * sqrt(n/(x*(n-x)))),
    
    names=c("lci", "uci"))
}


.witting <- function(x, n, alpha) {
  
  # here the uniform random number is by design
  # define set.seed() before calling the function, if you want
  # reproducible results
  x.tilde <- x + runif(1, min = 0, max = 1)
  
  pbinom.abscont <- function(q, size, prob){
    v <- trunc(q)
    return( pbinom(v-1, size = size, prob = prob) +
               (q - v) * dbinom(v, size = size, prob = prob))
  }
  
  qbinom.abscont <- function(p, size, x){
    
    fun <- function(prob, size, x, p){
      pbinom.abscont(x, size, prob) - p
    }
    uniroot(fun, interval = c(0, 1), size = size, x = x, p = p)$root
  }
  
  res <- c(
    lci = qbinom.abscont(1-alpha, size = n, x = x.tilde),
    uci = qbinom.abscont(alpha, size = n, x = x.tilde)
  )
  attr(res, "p.tilde") <- x.tilde / n
  
  return(res)
  
}


.pratt <- function(x, n, alpha) {
  
  if(x==0) {
    lci <- 0
    uci <- 1 - alpha^(1/n)
  } else if(x==1) {
    lci <- 1 - (1 - alpha/2)^(1/n)
    uci <- 1 - (alpha/2)^(1/n)
  } else if(x==(n-1)) {
    lci <- (alpha/2)^(1/n)
    uci <- (1 - alpha/2)^(1/n)
  } else if(x==n) {
    lci <- alpha^(1/n)
    uci <- 1
    
  } else {
    z <- qnorm(1 - alpha/2)
    
    A <- ((x+1) / (n-x))^2
    B <- 81*(x+1)*(n-x)-9*n-8
    C <- (0-3)*z*sqrt(9*(x+1)*(n-x)*(9*n+5-z^2)+n+1)
    D <- 81*(x+1)^2-9*(x+1)*(2+z^2)+1
    E <- 1+A*((B+C)/D)^3
    uci <- 1/E
    
    A <- (x / (n-x-1))^2
    B <- 81*x*(n-x-1)-9*n-8
    C <- 3*z*sqrt(9*x*(n-x-1)*(9*n+5-z^2)+n+1)
    D <- 81*x^2-9*x*(2+z^2)+1
    E <- 1+A*((B+C)/D)^3
    lci <- 1/E
  }
  
  return(c(lci = lci, uci = uci))
  
}


.midp <- function(x, n, alpha){
  
  # Functions to find root of for the lower and higher bounds of the CI
  low <- function(x, n, p) {
    0.5 * dbinom(x, size=n, prob=p) + 
      pbinom(x, size=n, prob=p, lower.tail=FALSE) - alpha/2
  }
  
  upr <- function(x, n, p) {
    0.5 * dbinom(x, size=n, prob=p) + 
      pbinom(x - 1, size=n, prob=p) - alpha/2
  }
  
  # pick lci = 0 when x = 0 and uci = 1 when x = n
  lci <- 0
  uci <- 1
  p.hat <- x/n
  
  # Calculate CI by finding roots of the funcs
  if (x!=0) {
    lci <- uniroot(low, interval=c(0, p.hat), x=x, n=n)$root
  } 
  if (x!=n) {
    uci  <- uniroot(upr, interval=c(p.hat, 1), x=x, n=n)$root
  }
  
  return(c(lci = lci, uci = uci))
  
}


.blaker <- function(x, n, alpha) {
  
  acceptbin <- function (x, n, p) {
    
    p1 <- 1 - pbinom(x - 1, n, p)
    p2 <- pbinom(x, n, p)
    
    a1 <- p1 + pbinom(qbinom(p1, n, p) - 1, n, p)
    a2 <- p2 + 1 - pbinom(qbinom(1 - p2, n, p), n, p)
    
    return(min(a1, a2))
  }
  
  tol <- .Machine$double.eps^0.5
  
  lci <- 0
  uci <- 1
  
  if (x != 0) {
    lci <- qbeta(alpha/2, x, n - x + 1)
    while ( acceptbin(x, n, lci + tol) < alpha ) 
      lci = lci + tol
  }
  
  if (x != n) {
    uci <- qbeta(1 - alpha/2, x + 1, n - x)
    while (acceptbin(x, n, uci - tol) < alpha) 
      uci <- uci - tol
  }

  return(c(lci = lci, uci = uci))
  
}


.lik <- function(x, n, alpha) {
  
  p.hat <- x/n
  lci <- 0
  uci <- 1
  z <- qnorm(1 - alpha * 0.5)
  
  # preset tolerance, should we offer function argument?
  tol <- .Machine$double.eps^0.5
  
  BinDev <- function(y, x, mu, wt, bound = 0, 
                     tol = .Machine$double.eps^0.5, ...) {
    
    # returns the binomial deviance for y, x, wt
    ll.y <- ifelse(y %in% c(0, 1), 0, dbinom(x, wt, y, log=TRUE))
    ll.mu <- ifelse(mu %in% c(0, 1), 0, dbinom(x, wt, mu, log=TRUE))
    res <- ifelse(abs(y - mu) < tol, 0, 
                  sign(y - mu) * sqrt(-2 * (ll.y - ll.mu)))
    return(res - bound)
  }
  
  if(x != 0 && tol < p.hat) {
    lci <- if(BinDev(tol, x, p.hat, n, -z, tol) <= 0) {
      uniroot(f = BinDev, 
              interval = c(tol, if(p.hat < tol || p.hat == 1) 1 - tol else p.hat), 
              bound = -z, x = x, mu = p.hat, wt = n)$root }
  }
  
  if(x != n && p.hat < (1-tol)) {
    uci <- if(BinDev(y = 1 - tol, x = x, mu = ifelse(p.hat > 1 - tol, tol, p.hat), 
                          wt = n, bound = z, tol = tol) < 0) {
      
      lci <- if(BinDev(tol, x, if(p.hat < tol || p.hat == 1) 1 - tol else p.hat, n, -z, tol) <= 0) {
        uniroot(f = BinDev, interval = c(tol, p.hat),
                bound = -z, x = x, mu = p.hat, wt = n)$root  }
      
    } else {
      
      uniroot(f = BinDev, interval = c(if(p.hat > 1 - tol) tol else p.hat, 1 - tol),
              bound = z, x = x, mu = p.hat, wt = n)$root     }
  }
  
  return(c(lci = lci, uci = uci))
  
}



.nonStdEst <- function(x, n, alpha){
  z2 <- qnorm(1-alpha/2)^2
  # p.tilde
  return( (x + z2/2) / (n + z2)) 
}




##' Confidence Intervals for Binomial Proportions
##' 
##' Compute confidence intervals for binomial proportions according to a number
##' of the most common proposed methods.
##' 
##' All arguments are being recycled.
##' 
##' The \bold{Wald } interval is obtained by inverting the acceptance region of
##' the Wald large-sample normal test.
##' 
##' The \bold{Wald with continuity correction } interval is obtained by adding
##' the term 1/(2*n) to the Wald interval.
##' 
##' The \bold{Wilson} interval, which here is the default method, was
##' introduced by Wilson (1927) and is the inversion of the CLT approximation
##' to the family of equal tail tests of p = p0.  The Wilson interval is
##' recommended by Agresti and Coull (1998) as well as by Brown et al (2001).
##' It is also returned as \code{conf.int} from the function
##' \code{\link{prop.test}} with the \code{correct} option set to \code{FALSE}.
##' 
##' The \bold{Wilson cc} interval is a modification of the Wilson interval
##' adding a continuity correction term. This is returned as \code{conf.int}
##' from the function \code{\link{prop.test}} with the \code{correct} option
##' set to \code{TRUE}.
##' 
##' The \bold{modified Wilson} interval is a modification of the Wilson
##' interval for x close to 0 or n as proposed by Brown et al (2001).
##' 
##' The \bold{Agresti-Coull} interval was proposed by Agresti and Coull (1998)
##' and is a slight modification of the Wilson interval. The Agresti-Coull
##' intervals are never shorter than the Wilson intervals; cf. Brown et al
##' (2001). The internally used point estimator p-tilde is returned as
##' attribute.
##' 
##' The \bold{Jeffreys} interval is an implementation of the equal-tailed
##' Jeffreys prior interval as given in Brown et al (2001).
##' 
##' The \bold{modified Jeffreys} interval is a modification of the Jeffreys
##' interval for \code{x == 0 | x == 1} and \code{x == n-1 | x == n} as
##' proposed by Brown et al (2001).
##' 
##' The \bold{Clopper-Pearson} interval is based on quantiles of corresponding
##' beta distributions. This is sometimes also called exact interval.
##' 
##' The \bold{arcsine} interval is based on the variance stabilizing
##' distribution for the binomial distribution.
##' 
##' The \bold{logit} interval is obtained by inverting the Wald type interval
##' for the log odds.
##' 
##' The \bold{Witting} interval (cf. Beispiel 2.106 in Witting (1985)) uses
##' randomization to obtain uniformly optimal lower and upper confidence bounds
##' (cf. Satz 2.105 in Witting (1985)) for binomial proportions.
##' Repeated calls will yield slightly different interval bounds unless the
##' random number generator is fixed.
##' 
##' The \bold{Pratt} interval is obtained by extremely accurate normal
##' approximation. (Pratt 1968)
##' 
##' The \bold{Mid-p} approach is used to reduce the conservatism of the
##' Clopper-Pearson, which is known to be very pronounced. The method midp
##' accumulates the tail areas.  The lower bound \eqn{p_l} is found as the
##' solution to the equation \deqn{\frac{1}{2} f(x;n,p_l) + (1-F(x;m,p_l)) =
##' \frac{\alpha}{2}} where \eqn{f(x;n,p)} denotes the probability mass
##' function (pmf) and \eqn{F(x;n,p)} the (cumulative) distribution function of
##' the binomial distribution with size \eqn{n} and proportion \eqn{p}
##' evaluated at \eqn{x}.  The upper bound \eqn{p_u} is found as the solution
##' to the equation \deqn{\frac{1}{2} f(x;n,p_u) + F(x-1;m,p_u) =
##' \frac{\alpha}{2}} In case x=0 then the lower bound is zero and in case x=n
##' then the upper bound is 1.
##' 
##' The \bold{Likelihood-based} approach is said to be theoretically appealing.
##' Confidence intervals are based on profiling the binomial deviance in the
##' neighbourhood of the MLE.
##' 
##' For the \bold{Blaker} method refer to Blaker (2000).
##' 
##' For more details we refer to Brown et al (2001) as well as Witting (1985).
##' 
##' Some approaches for the confidence intervals are capable of violating the
##' \code{[0, 1]} boundaries and potentially yield negative results or values beyond
##' 1.  These would be truncated such as not to exceed the valid range of \code{[0,
##' 1]}.
##' 
##' So now, which interval should we use? The Wald interval often has
##' inadequate coverage, particularly for small n and values of p close to 0 or
##' 1.  Conversely, the Clopper-Pearson Exact method is very conservative and
##' tends to produce wider intervals than necessary. Brown et al.  recommends
##' the Wilson or Jeffreys methods for small n and Agresti-Coull, Wilson, or
##' Jeffreys, for larger n as providing more reliable coverage . than the
##' alternatives.
##' 
##' For the methods \code{"wilson"}, \code{"wilsoncc"}, \code{"modified
##' wilson"}, \code{"agresti-coull"}, \code{"witting"} and \code{"arcsine"} 
##' the internally used point estimator for the proportion 
##' value can be returned (by
##' setting \code{std_est = FALSE}). The point estimate typically is slightly
##' shifted towards 0.5 compared to the standard estimator.  See the literature
##' for the more details.
##' 
##' @param x number of successes.
##' @param n number of trials.
##' @param conf.level confidence level, defaults to 0.95.
##' @param sides a character string specifying the side of the confidence
##' interval, must be one of \code{"two.sided"} (default), \code{"left"} or
##' \code{"right"}. You can specify just the initial letter. \code{"left"}
##' would be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.
##' @param method character string specifing which method to use; this can be
##' one out of: \code{"wald"}, \code{"wilson"} (default), \code{"wilsoncc"},
##' \code{"agresti-coull"}, \code{"jeffreys"}, \code{"modified wilson"},
##' \code{"modified jeffreys"}, \code{"clopper-pearson"}, \code{"arcsine"},
##' \code{"logit"}, \code{"witting"}, \code{"pratt"}, \code{"midp"},
##' \code{"lik"} and \code{"blaker"}.  Abbreviation of method is accepted. See
##' details.
##' @param std_est logical, specifying if the standard point estimator for the
##' proportion value \code{x/n} should be returned (\code{TRUE}, default) or
##' the method-specific internally used alternative point estimate
##' (\code{FALSE}).
##' @return A named vector with 3 elements \code{est, lci, uci}
##' for estimate, lower and upper confidence interval.
##' 
##' For more than one argument each, a 3-column matrix is returned.
##' @note The base of this function once was \code{binomCI()} from the
##' \pkg{SLmisc} package. In the meantime, the code has been updated on several
##' occasions and it has undergone numerous extensions and bug fixes.
##' @author Matthias Kohl <Matthias.Kohl@@stamats.de>, Rand R. Wilcox (Pratt's
##' method), Michael Hoehle <hoehle@@math.su.se> (Mid-p), Ralph Scherer
##' <shearer.ra76@@gmail.com> (Blaker), Andri Signorell <andri@@signorell.net>
##' (interface issues and all the rest)
##' @seealso \code{\link[stats]{binom.test}}, \code{\link[Hmisc]{binconf}},
##' \code{\link{MultinomCI}}, \code{\link{BinomDiffCI}},
##' \code{\link{BinomRatioCI}}
##' @references Agresti A. and Coull B.A. (1998) Approximate is better than
##' "exact" for interval estimation of binomial proportions.  \emph{American
##' Statistician}, \bold{52}, pp. 119-126.
##' 
##' Brown L.D., Cai T.T. and Dasgupta A. (2001) Interval estimation for a
##' binomial proportion \emph{Statistical Science}, \bold{16}(2), pp. 101-133.
##' 
##' Witting H. (1985) \emph{Mathematische Statistik I}. Stuttgart: Teubner.
##' 
##' Pratt J. W. (1968) A normal approximation for binomial, F, Beta, and other
##' common, related tail probabilities \emph{Journal of the American
##' Statistical Association}, 63, 1457- 1483.
##' 
##' Wilcox, R. R. (2005) \emph{Introduction to robust estimation and hypothesis
##' testing}. Elsevier Academic Press
##' 
##' Newcombe, R. G. (1998) Two-sided confidence intervals for the single
##' proportion: comparison of seven methods, \emph{Statistics in Medicine},
##' 17:857-872 https://pubmed.ncbi.nlm.nih.gov/16206245/
##' 
##' Blaker, H. (2000) Confidence curves and improved exact confidence intervals
##' for discrete distributions, \emph{Canadian Journal of Statistics} 28 (4),
##' 783-798
##' @keywords univar
##' @examples
##' 
##' BinomCI(x=37, n=43, 
##'         method=eval(formals(BinomCI)$method))   # return all methods
##' 
##' prop.test(x=37, n=43, correct=FALSE) # same as method wilson
##' prop.test(x=37, n=43, correct=TRUE)  # same as method wilsoncc
##' 
##' 
##' # the confidence interval computed by binom.test
##' #   corresponds to the Clopper-Pearson interval
##' BinomCI(x=42, n=43, method="clopper-pearson")
##' binom.test(x=42, n=43)$conf.int
##' 
##' 
##' # all arguments are being recycled:
##' BinomCI(x=c(42, 35, 23, 22), n=43, method="wilson")
##' BinomCI(x=c(42, 35, 23, 22), n=c(50, 60, 70, 80), method="jeffreys")
##' 
##' # example Table I in Newcombe (1998)
##' meths <- c("wald", "waldcc", "wilson", "wilsoncc",
##'            "clopper-pearson","midp", "lik")
##' round(cbind(
##'   BinomCI(81, 263, m=meths)[, -1],
##'   BinomCI(15, 148, m=meths)[, -1],
##'   BinomCI(0, 20, m=meths)[, -1],
##'   BinomCI(1, 29, m=meths)[, -1]), 4)
##' 
##' 
##' # returning p.tilde for agresti-coull ci
##' BinomCI(x=81, n=263, meth="agresti-coull", std_est = c(TRUE, FALSE))



#' @rdname BinomCI
#' @export
BinomCI <- function(x, n, 
                    conf.level = 0.95, sides = c("two.sided","left","right"),
                    method = c("wilson", "wilsoncc", "modified wilson",
                      "agresti-coull", 
                      "jeffreys", "modified jeffreys",
                      "lik", "blaker",
                      "clopper-pearson", "midp",
                      "waldcc", "wald",
                      "logit", "arcsine",
                      "witting", "pratt" ), 
                    std_est=TRUE) {


  iBinomCI <- function(x, n, conf.level, sides = c("two.sided","left","right"),
                       method = c("wilson", "wilsoncc", "modified wilson",
                                         "agresti-coull", 
                                         "jeffreys", "modified jeffreys",
                                         "lik", "blaker",
                                         "clopper-pearson", "midp",
                                         "waldcc", "wald",
                                         "logit", "arcsine",
                                         "witting", "pratt" ), std_est) {
    
    if(length(x) != 1) stop("'x' has to be of length 1 (number of successes)")
    if(length(n) != 1) stop("'n' has to be of length 1 (number of trials)")
    if(length(conf.level) != 1)  stop("'conf.level' has to be of length 1 (confidence level)")
    if(conf.level < 0.5 | conf.level > 1)  stop("'conf.level' has to be in [0.5, 1]")
    
    
    method <- match.arg(arg=method, 
                        choices=c("wilson", "wilsoncc", "modified wilson",
                                   "agresti-coull", "jeffreys", "modified jeffreys",
                                   "lik", "blaker", "clopper-pearson", "midp",
                                   "waldcc", "wald", "logit", "arcsine",
                                   "witting", "pratt" ))
    
    sides <- match.arg(sides, choices = c("two.sided","left","right"), 
                       several.ok = FALSE)
    
    alpha <- 1 - conf.level
    
    if(sides!="two.sided")
      conf.level <- 1 - 2*alpha

    CI <- switch( method
            , "wald" =              { .wald(x, n, alpha) }
            , "waldcc" =            { .wald(x, n, alpha, corr=TRUE) }
            , "jeffreys" =          { .jeffreys(x, n, alpha) }
            , "modified jeffreys" = { .jeffreys_mod(x, n, alpha) }
            , "clopper-pearson" =   { .clopper_pearson(x, n, alpha) }
            , "arcsine" =           { .arcsine(x, n, alpha) }
            , "logit" =             { .logit(x, n, alpha) }
            , "witting" =           { .witting(x, n, alpha) }
            , "agresti-coull" =     { .agresti_coull(x, n, alpha) }
            , "pratt" =             { .pratt(x, n, alpha) }
            , "wilson" =            { .wilson(x, n, alpha) }
            , "wilsoncc" =          { .wilson_cc(x, n, alpha) }
            , "modified wilson" =   { .wilson_mod(x, n, alpha) }
            , "midp" =              { .midp(x, n, alpha) }
            , "blaker" =            { .blaker(x, n, alpha) }
            , "lik" =               { .lik(x, n, alpha) }
    )

    
    # this is the default estimator used by the most (but not all) methods
    est <- x/n
    
    if(!std_est){
      
      if(method %in% 
              c("agresti-coull", "wilson", "wilsoncc", "modified wilson"))
        est <- .nonStdEst(x, n, alpha)
      
      else if(method %in% c("arcsine", "witting"))
        est <- attr(CI, "p.tilde")
    }
    
        
    # dot not return ci bounds outside [0,1]
    ci <- c( est    = est, 
             lci = max(0, CI["lci"]), 
             uci = min(1, CI["uci"]) )
    
    if(sides=="left")
      ci[3] <- 1
    else if(sides=="right")
      ci[2] <- 0
    
    return(ci)
    
  }
  
  # set defaults when user does not provide argument
  # (we can't match.arg, when several options should be possible)
  if(missing(sides)) sides <- "two.sided"
  if(missing(method)) method <- "wilson"
  
  # handle vectors
  # which parameter has the highest dimension
  lst <- list(x=x, n=n, conf.level=conf.level, sides=sides, 
              method=method, std_est=std_est)
  
  maxdim <- max(unlist(lapply(lst, length)))
  
  # recycle all params to maxdim
  lgp <- lapply( lst, rep, length.out=maxdim )
  # # increase conf.level for one sided intervals
  # lgp$conf.level[lgp.sides!="two.sided"] <- 1 - 2*(1-lgp$conf.level[lgp.sides!="two.sided"])
  
  # get rownames
  lgn <- DescTools::Recycle(x=if(is.null(names(x))) paste("x", seq_along(x), sep=".") else names(x),
                            n=if(is.null(names(n))) paste("n", seq_along(n), sep=".") else names(n),
                            conf.level=conf.level, sides=sides, method=method, std_est=std_est)
  
  
  xn <- apply(as.data.frame(lgn[sapply(lgn, function(x) length(unique(x)) != 1)]), 1, paste, collapse=":")
  
  res <- t(sapply(1:maxdim, function(i) iBinomCI(x=lgp$x[i], n=lgp$n[i],
                                                 conf.level=lgp$conf.level[i],
                                                 sides=lgp$sides[i],
                                                 method=lgp$method[i], 
                                                 std_est=lgp$std_est[i])))
  colnames(res)[1] <- c("est")
  rownames(res) <- xn
  
  return(res)
  
}




BinomCIn <- function(p=0.5, width, interval=c(1, 1e5), conf.level=0.95, sides="two.sided", method="wilson") {
  uniroot(f = function(n) diff(BinomCI(x=p*n, n=n, conf.level=conf.level, 
                                       sides=sides, method=method)[-1]) - width, 
          interval = interval)$root
}


