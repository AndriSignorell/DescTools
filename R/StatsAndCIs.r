
# Project:	DescTools
# Chapter:  Statistical functions and confidence intervals
#
# Purpose:  Tools for descriptive statistics, the missing link...
#	          Univariat, pairwise bivariate, groupwise und multivariate
#
# Author:   Andri Signorell
# Version:	0.99.x
#


# some aliases



NormWeights <- function(x, weights, na.rm=FALSE, zero.rm=FALSE, normwt=FALSE) {
  

  # Idea Henrik Bengtsson
  # we remove values with zero (and negative) weight. 
  # This would:
  #  1) take care of the case when all weights are zero,
  #  2) it will most likely speed up the sorting.
  
  if (na.rm){
    
    if(zero.rm)
      # remove zeros
      keep <- x[!is.na(x)] & weights[!is.na(weights) & (weights>0)]
    else
      keep <- x[!is.na(x)] & weights[!is.na(weights)]
    
    x <- x[keep]
    weights <- weights[keep]
  } 
  
  if(any(is.na(x)) | (!is.null(weights) & any(is.na(weights))))
    return(NA_real_)
  
  n <- length(x)
  
  if (length(weights) != n) 
    stop("length of 'weights' must equal the number of rows in 'x'")
  
  # x and weights have length=0
  if(length(x)==0)
    return(list(x = x, weights = x, wsum = NaN))
  
  if (any(weights< 0) || (s <- sum(weights)) == 0) 
    stop("weights must be non-negative and not all zero")
  

  # we could normalize the weights to sum up to 1
  if (normwt) 
    weights <- weights * n/s
  
  return(list(x=x, weights=as.double(weights), wsum=s))
  
}




Mean <- function (x, ...)
  UseMethod("Mean")


Mean.Freq <- function(x, breaks, ...)  {
  sum(head(MoveAvg(breaks, order=2, align="left"), -1) * x$perc)
}



Mean.default <- function (x, weights = NULL, trim = 0, na.rm = FALSE, ...) {

  if(is.null(weights)) {
    # use mean here instead of mean.default in order to be able to handle
    # mean.Date, mean.POSIXct etc.
    mean(x, trim, na.rm, ...)

  } else {
    if(trim!=0)
      warning("trim can't be set together with weights, we fall back to trim=0!")

    # # verbatim from stats:::weighted.mean.default
    # 
    # if (length(weights) != length(x))
    #   stop("'x' and 'w' must have the same length")
    # weights <- as.double(weights)
    # if (na.rm) {
    #   i <- !is.na(x)
    #   weights <- weights[i]
    #   x <- x[i]
    # }
    # sum((x * weights)[weights != 0])/sum(weights)
    
    # use a standard treatment for weights
    z <- NormWeights(x, weights, na.rm=na.rm, zero.rm=TRUE)
    
    # we get no 0-weights back here...
    sum(z$x * z$weights) / z$wsum
    
  }

}




# Average absolute deviation from the mean
MeanAD <- function (x, weights=NULL, center = Mean, na.rm = FALSE) {
  
  # MeanAD_w(x=0:6, w=c(21,46,54,40,24,10,5))
  
  if (na.rm) 
    x <- na.omit(x)
  
  
  if (is.function(center)) {
    fct <- center
    center <- "fct"
    if(is.null(weights))
      center <- gettextf("%s(x)", center)
    else
      center <- gettextf("%s(x, weights=weights)", center)
    center <- eval(parse(text = center))
  }
  
  if(!is.null(weights)) {
    z <- NormWeights(x, weights, na.rm=na.rm, zero.rm=TRUE)
    res <- sum(abs(z$x - center) * z$weights) / z$wsum
    
  } else {
    # Calculates the mean absolute deviation from the sample mean.
    res <- mean(abs(x - center))
  }
  
  return(res)
  
}  




MAD <- function(x, weights = NULL, center = Median, constant = 1.4826, na.rm = FALSE, 
                low = FALSE, high = FALSE) {
  
  
  if (is.function(center)) {
    fct <- center
    center <- "fct"
    if(is.null(weights))
      center <- gettextf("%s(x)", center)
    else
      center <- gettextf("%s(x, weights=weights)", center)
    center <- eval(parse(text = center))
  }
  
  if(!is.null(weights)) {
    z <- NormWeights(x, weights, na.rm=na.rm)
    
    res <- constant *  Median(abs(z$x - center), weights = z$weights)
    
  } else {
    # fall back to mad(), if there are no weights
    res <- mad(x, center = center, constant = constant, na.rm = na.rm, low=low, high=high)
    
  }
  
  return(res)
  
}



MADCI <- function(x, y = NULL, two.samp.diff = TRUE, gld.est = "TM", 
                  conf.level = 0.95, sides = c("two.sided","left","right"), 
                  na.rm = FALSE, ...) {

  if (na.rm) x <- na.omit(x)
  
  sides <- match.arg(sides, choices = c("two.sided","left","right"), 
                     several.ok = FALSE)
  
  if(sides!="two.sided")
    conf.level <- 1 - 2*(1-conf.level)

  asv.mad <- function(x, method = "TM"){
    lambda <- fit.fkml(x, method = method)$lambda
    m  <- median(x)
    mad.x <- mad(x)
    fFinv <- dgl(c(m - mad.x, m + mad.x, m), lambda1 = lambda)
    FFinv <- pgl(c(m - mad.x, m + mad.x), lambda1 = lambda)
    A <- fFinv[1] + fFinv[2]
    C <- fFinv[1] - fFinv[2]
    B <- C^2 + 4*C*fFinv[3]*(1 - FFinv[2] - FFinv[1])
    
    (1/(4 * A^2))*(1 + B/fFinv[3]^2)
    
  } 
  
  alpha <- 1 - conf.level
  z <- qnorm(1 - alpha/2)
  
  est <- mad.x <- mad(x)
  
  n.x <- length(x)
  asv.x <- asv.mad(x, method = gld.est)
  
  if(is.null(y)){
    ci <- mad.x + c(-z, z) * sqrt(asv.x / n.x)
    
  } else{
    y <- y[!is.na(y)]
    mad.y <- mad(y)
    n.y <- length(y)
    
    asv.y <- asv.mad(y, method = gld.est)
    
    if(two.samp.diff){
      est <- mad.x - mad.y
      ci <- est + c(-z, z)*sqrt(asv.x/n.x + asv.y/n.y)
    } else{
      est <- (mad.x/mad.y)^2
      log.est <- log(est)
      var.est <- 4 * est * ((1/mad.y^2)*asv.x/n.x + (est/mad.y^2)*asv.y/n.y)
      Var.log.est <- (1 / est^2) * var.est
      
      ci <- exp(log.est + c(-z, z) * sqrt(Var.log.est))
    }
  }
  
  res <- c(est, ci)
  
  names(res) <- c("mad","lwr.ci","upr.ci")
  
  if(sides=="left")
    res[3] <- Inf
  else if(sides=="right")
    res[2] <- -Inf
  
  return( res )
  
}



# from stats
SD <- function (x, weights = NULL, na.rm = FALSE, ...)
  sqrt(Var(if (is.vector(x) || is.factor(x)) x else as.double(x),
           weights=weights, na.rm = na.rm, ...))


Var <- function (x, ...)
  UseMethod("Var")



Var.default <- function (x, weights = NULL, na.rm = FALSE, method = c("unbiased",  "ML"), ...) {

  if(is.null(weights)) {
    res <- var(x=x, na.rm=na.rm)
    
  } else {
    z <- NormWeights(x, weights, na.rm=na.rm)

    if (match.arg(method) == "ML")
      return(as.numeric(stats::cov.wt(cbind(z$x), z$weights, method = "ML")$cov))
    
    xbar <- sum(z$weights * x) / z$wsum
    
    res <- sum(z$weights * ((z$x - xbar)^2))/(z$wsum - 1)
    
  }
  
  return(res)
  
}


Var.Freq <- function(x, breaks, ...)  {
  n <- sum(x$freq)
  mu <- sum(head(MoveAvg(breaks, order=2, align="left"), -1) * x$perc)
  s2 <- (sum(head(MoveAvg(breaks, order=2, align="left"), -1)^2 * x$freq) - n*mu^2) / (n-1)

  return(s2)
}




Cov <- cov
Cor <- cor


# Length(x)
# Table(x)
# Log(x)
# Abs(x)
#
# "abs", "sign", "sqrt", "ceiling", "floor", "trunc", "cummax", "cummin", "cumprod", "cumsum",
# "log", "log10", "log2", "log1p", "acos", "acosh", "asin", "asinh", "atan", "atanh",
# "exp", "expm1", "cos", "cosh", "cospi", "sin", "sinh", "sinpi", "tan", "tanh",
# "tanpi", "gamma", "lgamma", "digamma", "trigamma"


Range <- function(x, trim=NULL, robust=FALSE, na.rm = FALSE, ...){

  RobRange <- function(x, trim = NULL, fac = 3, na.rm = FALSE) {

    if(is.null(trim))
      trim <- 0.2
    # author: Werner Stahel
    # from:   regr.r

    if(na.rm) x <- na.omit(x)

    ldat <- x[is.finite(x)]
    if (is.character(ldat)|length(ldat) == 0) stop("invalid data")
    trim <- c(trim, 0.2)[1]
    if (!is.finite(trim)) trim <- 0.2
    lmn <- mean(ldat, trim=trim)
    lds <- sort(abs(ldat - lmn))
    ln <- ceiling((1 - trim) * length(ldat))
    if (ln < 3) {
      warning("Not enough valid data. returning ordinary range")
      lsd <- Inf
    } else {
      lsd <- fac * sum(lds[1:ln] / (ln-1))
      if (lsd == 0) {
        warning("Robust range has width 0. returning ordinary range")
        lsd <- Inf }
    }
    bounds <- c(max(lmn - lsd, min(ldat)), min(lmn + lsd, max(ldat)))

    res <- diff(bounds)
    attr(res, "bounds") <- bounds

    return(res)

  }


  if(robust)
    RobRange(x=x, trim=trim, na.rm=na.rm, ...)

  else {
    if(is.null(trim))
      trim <- 0

    rng <- range(Trim(x, trim=trim, na.rm=na.rm), na.rm=na.rm)
    res <- diff(rng)
    attr(res, "bounds") <- rng

    res

  }

}



# ------------------------------------------
# Authors: Andreas Alfons and Matthias Templ
#          Vienna University of Technology
# ------------------------------------------

#' Weighted median
#'
#' Compute the weighted median (Eurostat definition).
#'
#' The implementation strictly follows the Eurostat definition.
#'
#' @param x a numeric vector.
#' @param weights an optional numeric vector giving the sample weights.
#' @param sorted a logical indicating whether the observations in \code{x} are
#' already sorted.
#' @param na.rm a logical indicating whether missing values in \code{x} should
#' be omitted.
#' @return The weighted median of values in \code{x} is returned.
#'
#' @author Andreas Alfons and Matthias Templ
#'
#' @seealso \code{\link{arpt}}, \code{\link{incMedian}},
#' \code{\link{weightedQuantile}}
#'
#' @references Working group on Statistics on Income and Living Conditions
#' (2004) Common cross-sectional EU indicators based on EU-SILC; the gender pay
#' gap.  \emph{EU-SILC 131-rev/04}, Eurostat.
#'
#' @keywords survey
#'
#' @examples
#' data(eusilc)
#' weightedMedian(eusilc$eqIncome, eusilc$rb050)
#'
#' @export



Median <- function(x, ...)
  UseMethod("Median")


Median.default <- function(x, weights = NULL, na.rm = FALSE, ...) {
  Quantile(x, weights, probs=0.5, na.rm=na.rm)

}


# ordered interface for the median
Median.factor <- function(x, na.rm = FALSE, ...) {

  # Answered by Hong Ooi on 2011-10-28T00:37:08-04:00
  # http://www.rqna.net/qna/nuiukm-idiomatic-method-of-finding-the-median-of-an-ordinal-in-r.html

  # return NA, if x is not ordered
  # clearme: why not median.ordered?
  if(!is.ordered(x)) return(NA)

  if(na.rm) x <- na.omit(x)
  if(any(is.na(x))) return(NA)

  levs <- levels(x)
  m <- median(as.integer(x), na.rm = na.rm)
  if(floor(m) != m)
  {
    warning("Median is between two values; using the first one")
    m <- floor(m)
  }
  ordered(m, labels = levs, levels = seq_along(levs))
}





Median.Freq <- function(x, breaks, ...)  {

  mi <- min(which(x$cumperc > 0.5))
  breaks[mi] + (tail(x$cumfreq, 1)/2 - x[mi-1, "cumfreq"]) /
    x[mi, "freq"] * diff(breaks[c(mi, mi+1)])

}




# ------------------------------------------
# Authors: Andreas Alfons and Matthias Templ
#          Vienna University of Technology
# ------------------------------------------

#' Weighted quantiles
#'
#' Compute weighted quantiles (Eurostat definition).
#'
#' The implementation strictly follows the Eurostat definition.
#'
#' @param x a numeric vector.
#' @param weights an optional numeric vector giving the sample weights.
#' @param probs numeric vector of probabilities with values in \eqn{[0,1]}.
#' @param sorted a logical indicating whether the observations in \code{x} are
#' already sorted.
#' @param na.rm a logical indicating whether missing values in \code{x} should
#' be omitted.
#'
#' @return A numeric vector containing the weighted quantiles of values in
#' \code{x} at probabilities \code{probs} is returned.  Unlike
#' \code{\link[stats]{quantile}}, this returns an unnamed vector.
#'
#' @author Andreas Alfons and Matthias Templ
#'
#' @seealso \code{\link{incQuintile}}, \code{\link{weightedMedian}}
#'
#' @references Working group on Statistics on Income and Living Conditions
#' (2004) Common cross-sectional EU indicators based on EU-SILC; the gender pay
#' gap.  \emph{EU-SILC 131-rev/04}, Eurostat.
#'
#' @keywords survey
#'
#' @examples
#' data(eusilc)
#' weightedQuantile(eusilc$eqIncome, eusilc$rb050)
#'
#' @export


Quantile <- function(x, weights = NULL, probs = seq(0, 1, 0.25),
                             na.rm = FALSE, names = TRUE, type = 7) {

  sorted <- FALSE

  # initializations
  if (!is.numeric(x)) stop("'x' must be a numeric vector")
  
  n <- length(x)
  
  if (n == 0 || (!isTRUE(na.rm) && any(is.na(x)))) {
    # zero length or missing values
    return(rep.int(NA, length(probs)))
  }
  if (!is.null(weights)) {
    if (!is.numeric(weights)) stop("'weights' must be a numeric vector")
    else if (length(weights) != n) {
      stop("'weights' must have the same length as 'x'")
    } else if (!all(is.finite(weights))) stop("missing or infinite weights")
    if (any(weights < 0)) warning("negative weights")
    
    if (!is.numeric(probs) || all(is.na(probs)) ||
        isTRUE(any(probs < 0 | probs > 1))) {
      stop("'probs' must be a numeric vector with values in [0,1]")
      
    }
    
    if (all(weights == 0)) { # all zero weights
      warning("all weights equal to zero")
      return(rep.int(0, length(probs)))
    }
  }
  
  # remove NAs (if requested)
  if(isTRUE(na.rm)){
    indices <- !is.na(x)
    x <- x[indices]
    n <- length(x)
    if(!is.null(weights)) weights <- weights[indices]
  }
  # sort values and weights (if requested)
  if(!isTRUE(sorted)) {
    #        order <- order(x, na.last=NA)  ## too slow
    order <- order(x)
    x <- x[order]
    weights <- weights[order]  # also works if 'weights' is NULL
  }
  # some preparations
  if(is.null(weights)) rw <- (1:n)/n
  else rw <- cumsum(weights)/sum(weights)
  
  # obtain quantiles
  q <- sapply(probs,
              function(p) {
                if (p == 0) return(x[1])
                else if (p == 1) return(x[n])
                select <- min(which(rw >= p))
                if(rw[select] == p) mean(x[select:(select+1)])
                else x[select]
              })
  
  # return(unname(q))
  # why unname? change to named.. 14.10.2020
  return(q)
  
}





IQRw <- function (x, weights = NULL, na.rm = FALSE, type = 7) {
  
  diff(Quantile(x, weights=weights, probs=c(0.25, 0.75), na.rm=na.rm, type=type))
  
}



## stats: functions (RobRange, Hmean, Gmean, Aad, HuberM etc.) ====


CorPart <- function(m, x, y)  {

  cl <- match.call()

  if(dim(m)[1] != dim(m)[2]) {
    n.obs <- dim(m)[1]
    m <- cor(m, use="pairwise")
  }
  if(!is.matrix(m)) m <- as.matrix(m)

  # first reorder the matrix to select the right variables
  nm <- dim(m)[1]
  t.mat <- matrix(0, ncol=nm, nrow=nm)
  xy <- c(x,y)
  numx <- length(x)
  numy <- length(y)
  nxy <- numx+numy

  for (i in 1:nxy) {
    t.mat[i, xy[i]] <- 1
  }

  reorder <- t.mat %*% m %*% t(t.mat)
  reorder[abs(reorder) > 1] <- NA    # this allows us to use the matrix operations to reorder and pick

  X <- reorder[1:numx, 1:numx]
  Y <- reorder[1:numx, (numx+1):nxy]

  phi <- reorder[(numx+1):nxy,(numx+1):nxy]
  phi.inv <- solve(phi)

  X.resid <- X - Y %*% phi.inv %*% t(Y)
  sd <- diag(sqrt(1/diag(X.resid)))
  X.resid <- sd %*% X.resid %*% sd

  colnames(X.resid) <- rownames(X.resid) <- colnames(m)[x]

  return(X.resid)

}


FisherZ <- function(rho)  {0.5*log((1+rho)/(1-rho)) }   #converts r to z

FisherZInv <- function(z) {(exp(2*z)-1)/(1+exp(2*z)) }   #converts back again


CorCI <- function(rho, n, conf.level = 0.95, alternative = c("two.sided","less","greater")) {


  if (n < 3L)
    stop("not enough finite observations")

  if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level)
                               || conf.level < 0 || conf.level > 1))
    stop("'conf.level' must be a single number between 0 and 1")

  alternative <- match.arg(alternative)

  # correct rho == 1 with rho == almost 1 in order to return ci = c(1, 1)
  # which is a sensible value for the confidence interval
  if(identical(rho, 1)) 
    ci <- c(1, 1)
  
  else {
    z <- FisherZ(rho)
    sigma <- 1/sqrt(n - 3)
  
    ci <- switch(alternative,
                 less = c(-Inf, z + sigma * qnorm(conf.level)),
                 greater = c(z - sigma * qnorm(conf.level), Inf),
                 two.sided = z + c(-1, 1) * sigma * qnorm((1 + conf.level)/2))
    ci <- FisherZInv(ci)
  }

  return(c(cor = rho, lwr.ci = ci[1], upr.ci = ci[2]))
  
}




CorPolychor <- function (x, y, ML=FALSE, control=list(), std.err=FALSE, maxcor=.9999){

  # last modified 21 Oct 08 by J. Fox

  binBvn <- function(rho, row.cuts, col.cuts, bins=4){
    # last modified 29 Mar 07 by J. Fox

    row.cuts <- if (missing(row.cuts)) c(-Inf, 1:(bins - 1)/bins, Inf) else  c(-Inf, row.cuts, Inf)
    col.cuts <- if (missing(col.cuts)) c(-Inf, 1:(bins - 1)/bins, Inf) else  c(-Inf, col.cuts, Inf)
    r <- length(row.cuts) - 1
    c <- length(col.cuts) - 1
    P <- matrix(0, r, c)
    R <- matrix(c(1, rho, rho, 1), 2, 2)
    for (i in 1:r){
      for (j in 1:c){
        P[i,j] <- pmvnorm(lower=c(row.cuts[i], col.cuts[j]),
                          upper=c(row.cuts[i+1], col.cuts[j+1]),
                          corr=R)
      }
    }
    P
  }


  f <- function(pars) {
    if (length(pars) == 1){
      rho <- pars
      if (abs(rho) > maxcor) rho <- sign(rho)*maxcor
      row.cuts <- rc
      col.cuts <- cc
    }
    else {
      rho <- pars[1]
      if (abs(rho) > maxcor) rho <- sign(rho)*maxcor
      row.cuts <- pars[2:r]
      col.cuts <- pars[(r+1):(r+c-1)]
    }
    P <- binBvn(rho, row.cuts, col.cuts)
    - sum(tab * log(P))
  }

  tab <- if (missing(y)) x else table(x, y)
  zerorows <- apply(tab, 1, function(x) all(x == 0))
  zerocols <- apply(tab, 2, function(x) all(x == 0))
  zr <- sum(zerorows)
  if (0 < zr) warning(paste(zr, " row", suffix <- if(zr == 1) "" else "s",
                            " with zero marginal", suffix," removed", sep=""))
  zc <- sum(zerocols)
  if (0 < zc) warning(paste(zc, " column", suffix <- if(zc == 1) "" else "s",
                            " with zero marginal", suffix, " removed", sep=""))
  tab <- tab[!zerorows, ,drop=FALSE]
  tab <- tab[, !zerocols, drop=FALSE]
  r <- nrow(tab)
  c <- ncol(tab)
  if (r < 2) {
    warning("the table has fewer than 2 rows")
    return(NA)
  }
  if (c < 2) {
    warning("the table has fewer than 2 columns")
    return(NA)
  }
  n <- sum(tab)
  rc <- qnorm(cumsum(rowSums(tab))/n)[-r]
  cc <- qnorm(cumsum(colSums(tab))/n)[-c]
  if (ML) {
    result <- optim(c(optimise(f, interval=c(-1, 1))$minimum, rc, cc), f,
                    control=control, hessian=std.err)
    if (result$par[1] > 1){
      result$par[1] <- 1
      warning("inadmissible correlation set to 1")
    }
    else if (result$par[1] < -1){
      result$par[1] <- -1
      warning("inadmissible correlation set to -1")
    }
    if (std.err) {
      chisq <- 2*(result$value + sum(tab * log((tab + 1e-6)/n)))
      df <- length(tab) - r - c
      result <- list(type="polychoric",
                     rho=result$par[1],
                     row.cuts=result$par[2:r],
                     col.cuts=result$par[(r+1):(r+c-1)],
                     var=solve(result$hessian),
                     n=n,
                     chisq=chisq,
                     df=df,
                     ML=TRUE)
      class(result) <- "polycor"
      return(result)
    }
    else return(as.vector(result$par[1]))
  }
  else if (std.err){
    result <- optim(0, f, control=control, hessian=TRUE, method="BFGS")
    if (result$par > 1){
      result$par <- 1
      warning("inadmissible correlation set to 1")
    }
    else if (result$par < -1){
      result$par <- -1
      warning("inadmissible correlation set to -1")
    }
    chisq <- 2*(result$value + sum(tab *log((tab + 1e-6)/n)))
    df <- length(tab) - r - c
    result <- list(type="polychoric",
                   rho=result$par,
                   var=1/result$hessian,
                   n=n,
                   chisq=chisq,
                   df=df,
                   ML=FALSE)
    class(result) <- "CorPolychor"
    return(result)
  }
  else optimise(f, interval=c(-1, 1))$minimum
}




print.CorPolychor <- function(x, digits = max(3, getOption("digits") - 3), ...){

  # last modified 24 June 04 by J. Fox

  if (x$type == "polychoric"){
    se <- sqrt(diag(x$var))
    se.rho <- se[1]
    est <- if (x$ML) "ML est." else "2-step est."
    cat("\nPolychoric Correlation, ", est, " = ", signif(x$rho, digits),
        " (", signif(se.rho, digits), ")", sep="")
    if (x$df > 0)
      cat("\nTest of bivariate normality: Chisquare = ",
          signif(x$chisq, digits), ", df = ", x$df, ", p = ",
          signif(pchisq(x$chisq, x$df, lower.tail=FALSE), digits), "\n", sep="")
    else cat("\n")
    r <- length(x$row.cuts)
    c <- length(x$col.cuts)
    if (r == 0) return(invisible(x))
    row.cuts.se <- se[2:(r+1)]
    col.cuts.se <- se[(r+2):(r+c+1)]
    rowThresh <- signif(cbind(x$row.cuts, row.cuts.se), digits)
    if (r > 1) cat("\n  Row Thresholds\n")
    else cat("\n  Row Threshold\n")
    colnames(rowThresh) <- c("Threshold", "Std.Err.")
    rownames(rowThresh) <- if (r > 1) 1:r else " "
    print(rowThresh)
    colThresh <- signif(cbind(x$col.cuts, col.cuts.se), digits)
    if (c > 1) cat("\n\n  Column Thresholds\n")
    else cat("\n\n  Column Threshold\n")
    colnames(colThresh) <- c("Threshold", "Std.Err.")
    rownames(colThresh) <- if (c > 1) 1:c else " "
    print(colThresh)
  }
  else if (x$type == "polyserial"){
    se <- sqrt(diag(x$var))
    se.rho <- se[1]
    est <- if (x$ML) "ML est." else "2-step est."
    cat("\nPolyserial Correlation, ", est, " = ", signif(x$rho, digits),
        " (", signif(se.rho, digits), ")", sep="")
    cat("\nTest of bivariate normality: Chisquare = ", signif(x$chisq, digits),
        ", df = ", x$df, ", p = ", signif(pchisq(x$chisq, x$df, lower.tail=FALSE), digits),
        "\n\n", sep="")
    if (length(se) == 1) return(invisible(x))
    cuts.se <- se[-1]
    thresh <- signif(rbind(x$cuts, cuts.se), digits)
    colnames(thresh) <- 1:length(x$cuts)
    rownames(thresh) <- c("Threshold", "Std.Err.")
    print(thresh)
  }
  else print(unclass(x))
  invisible(x)
}




FindCorr <- function(x, cutoff = .90, verbose = FALSE) {

  # Author: Max Kuhn
  # source library(caret)

  varnum <- dim(x)[1]

  if(!isTRUE(all.equal(x, t(x)))) stop("correlation matrix is not symmetric")
  if(varnum ==1) stop("only one variable given")

  x <- abs(x)

  # re-ordered columns based on max absolute correlation
  originalOrder <- 1:varnum

  averageCorr <- function(x) mean(x, na.rm = TRUE)
  tmp <- x
  diag(tmp) <- NA

  maxAbsCorOrder <- order(apply(tmp, 2, averageCorr), decreasing = TRUE)
  x <- x[maxAbsCorOrder, maxAbsCorOrder]
  newOrder <- originalOrder[maxAbsCorOrder]

  deletecol <- 0

  for(i in 1L:(varnum-1))
  {
    for(j in (i+1):varnum)
    {
      if(!any(i == deletecol)  & !any(j == deletecol))
      {
        if(verbose)
          cat("Considering row\t", newOrder[i],
              "column\t", newOrder[j],
              "value\t", round(x[i,j], 3), "\n")
        if(abs(x[i,j]) > cutoff)
        {
          if(mean(x[i, -i]) > mean(x[-j, j]))
          {
            deletecol <- unique(c(deletecol, i))
            if(verbose) cat("  Flagging column\t", newOrder[i], "\n")
          } else {
            deletecol <- unique(c(deletecol, j))
            if(verbose) cat("  Flagging column\t", newOrder[j], "\n")
          }
        }
      }
    }
  }
  deletecol <- deletecol[deletecol != 0]
  newOrder[deletecol]
}






# Alternative:
# From roc bioconductor
# Vince Carey (stvjc@channing.harvard.edu)

# trapezint <- function (x, y, a, b){
#
#   if (length(x) != length(y))
#     stop("length x must equal length y")
#   y <- y[x >= a & x <= b]
#   x <- x[x >= a & x <= b]
#   if (length(unique(x)) < 2)
#     return(NA)
#   ya <- approx(x, y, a, ties = max, rule = 2)$y
#   yb <- approx(x, y, b, ties = max, rule = 2)$y
#   x <- c(a, x, b)
#   y <- c(ya, y, yb)
#   h <- diff(x)
#   lx <- length(x)
#   0.5 * sum(h * (y[-1] + y[-lx]))
# }


# AUC <- function(x, y, from=min(x, na.rm=TRUE), to = max(x, na.rm=TRUE), 
#                 method=c("trapezoid", "step", "spline", "linear"), 
#                 absolutearea = FALSE, subdivisions = 100, na.rm = FALSE, ...) {
# 
#   # calculates Area unter the curve
#   # example:
#   #   AUC( x=c(1,2,3,5), y=c(0,1,1,2))
#   #   AUC( x=c(2,3,4,5), y=c(0,1,1,2))
# 
#   if(na.rm) {
#     idx <- na.omit(cbind(x,y))
#     x <- x[idx]
#     y <- y[idx]
#   }
# 
#   if (length(x) != length(y))
#     stop("length x must equal length y")
# 
#   idx <- order(x)
#   x <- x[idx]
#   y <- y[idx]
# 
#   switch( match.arg( arg=method, choices=c("trapezoid","step","spline","linear") )
#           , "trapezoid" = { a <- sum((apply( cbind(y[-length(y)], y[-1]), 1, mean))*(x[-1] - x[-length(x)])) }
#           , "step" = { a <- sum( y[-length(y)] * (x[-1] - x[-length(x)])) }
#           , "linear" = {
#                 a <- MESS_auc(x, y, from = from , to = to, type="linear", 
#                                    absolutearea=absolutearea, subdivisions=subdivisions, ...)
#                        }
#           , "spline" = { 
#                 a <- MESS_auc(x, y, from = from , to = to, type="spline", 
#                      absolutearea=absolutearea, subdivisions=subdivisions, ...)
#             # a <- integrate(splinefun(x, y, method="natural"), lower=min(x), upper=max(x))$value 
#               }
#   )
#   return(a)
# }
# 
# 
# 
# MESS_auc <- function(x, y, from = min(x, na.rm=TRUE), to = max(x, na.rm=TRUE), type=c("linear", "spline"), 
#                 absolutearea=FALSE, subdivisions =100, ...) {
#   
#   type <- match.arg(type)
#   
#   # Sanity checks
#   stopifnot(length(x) == length(y))
#   stopifnot(!is.na(from))
#   
#   if (length(unique(x)) < 2)
#     return(NA)
#   
#   if (type=="linear") {
#     ## Default option
#     if (absolutearea==FALSE) {
#       values <- approx(x, y, xout = sort(unique(c(from, to, x[x > from & x < to]))), ...)
#       res <- 0.5 * sum(diff(values$x) * (values$y[-1] + values$y[-length(values$y)]))
#     } else { ## Absolute areas
#       ## This is done by adding artificial dummy points on the x axis
#       o <- order(x)
#       ox <- x[o]
#       oy <- y[o]
#       
#       idx <- which(diff(oy >= 0)!=0)
#       newx <- c(x, x[idx] - oy[idx]*(x[idx+1]-x[idx]) / (y[idx+1]-y[idx]))
#       newy <- c(y, rep(0, length(idx)))
#       values <- approx(newx, newy, xout = sort(unique(c(from, to, newx[newx > from & newx < to]))), ...)
#       res <- 0.5 * sum(diff(values$x) * (abs(values$y[-1]) + abs(values$y[-length(values$y)])))
#     }
#     
#   } else { ## If it is not a linear approximation
#     if (absolutearea)
#       myfunction <- function(z) { abs(splinefun(x, y, method="natural")(z)) }
#     
#     else
#       myfunction <- splinefun(x, y, method="natural")
#     
#     res <- integrate(myfunction, lower=from, upper=to, subdivisions=subdivisions)$value
#     
#   }
#   
#   res
#   
# }
# 


AUC <- function(x, y, from = min(x, na.rm=TRUE), to = max(x, na.rm=TRUE), 
                method=c("trapezoid", "step", "spline"), absolutearea = FALSE, 
                subdivisions = 100,  na.rm = FALSE, ...)  {
  
  
  # calculates Area unter the curve
  # example:
  #   AUC( x=c(1,2,3,5), y=c(0,1,1,2))
  #   AUC( x=c(2,3,4,5), y=c(0,1,1,2))
  
  if(na.rm) {
    idx <- na.omit(cbind(x,y))
    x <- x[idx]
    y <- y[idx]
  }
  
  if (length(x) != length(y))
    stop("length x must equal length y")
  
  if (length(x) < 2)
    return(NA)
  
  o <- order(x)
  x <- x[o]
  y <- y[o]

  ox <- x[o]
  oy <- y[o]
  
  method <- match.arg(method)
  
  if (method=="trapezoid") {
    
    # easy and short
    # , "trapezoid" = { a <- sum((apply( cbind(y[-length(y)], y[-1]), 1, mean))*(x[-1] - x[-length(x)])) }
    
    ## Default option
    if (!absolutearea) {
      values <- approx(x, y, xout = sort(unique(c(from, to, x[x > from & x < to]))), ...)
      res <- 0.5 * sum(diff(values$x) * (values$y[-1] + values$y[-length(values$y)]))
      
    } else { ## Absolute areas
      
      idx <- which(diff(oy >= 0)!=0)
      newx <- c(x, x[idx] - oy[idx]*(x[idx+1]-x[idx]) / (y[idx+1]-y[idx]))
      newy <- c(y, rep(0, length(idx)))
      values <- approx(newx, newy, xout = sort(unique(c(from, to, newx[newx > from & newx < to]))), ...)
      
      res <- 0.5 * sum(diff(values$x) * (abs(values$y[-1]) + abs(values$y[-length(values$y)])))
      
    }
    
  } else if (method=="step") {
    
    # easy and short
    # , "step" = { a <- sum( y[-length(y)] * (x[-1] - x[-length(x)])) }
    
    ## Default option
    if (!absolutearea) {
      values <- approx(x, y, xout = sort(unique(c(from, to, x[x > from & x < to]))), ...)
      
      res <- sum(diff(values$x) * values$y[-length(values$y)])
      # res <- sum( y[-length(y)] * (x[-1] - x[-length(x)])) 
      
    } else { ## Absolute areas
      
      idx <- which(diff(oy >= 0)!=0)
      newx <- c(x, x[idx] - oy[idx]*(x[idx+1]-x[idx]) / (y[idx+1]-y[idx]))
      newy <- c(y, rep(0, length(idx)))
      values <- approx(newx, newy, xout = sort(unique(c(from, to, newx[newx > from & newx < to]))), ...)
      
      res <- sum(diff(values$x) * abs(values$y[-length(values$y)]))
      
    }
    
  } else if (method=="spline") { 
    
    if (absolutearea)
      myfunction <- function(z) { abs(splinefun(x, y, method="natural")(z)) }
    else
      myfunction <- splinefun(x, y, method="natural")
    
    res <- integrate(myfunction, lower=from, upper=to, subdivisions=subdivisions)$value
    
  }
  
  return(res)
  
}





# library(microbenchmark)
# 
# baseMode <- function(x, narm = FALSE) {
#   if (narm) x <- x[!is.na(x)]
#   ux <- unique(x)
#   ux[which.max(table(match(x, ux)))]
# }
# x <- round(rnorm(1e7) *100, 4)
# microbenchmark(Mode(x), baseMode(x), DescTools:::fastMode(x), times = 15, unit = "relative")
# 


# mode value, the most frequent element
Mode <- function(x, na.rm=FALSE) {
  
  # // Source
  # // https://stackoverflow.com/questions/55212746/rcpp-fast-statistical-mode-function-with-vector-input-of-any-type
  # // Author: Ralf Stubner, Joseph Wood
  
  if(!is.atomic(x) | is.matrix(x)) stop("Mode supports only atomic vectors. Use sapply(*, Mode) instead.")
  
  if (na.rm) 
    x <- x[!is.na(x)]
  
  if (anyNA(x)) 
    # there are NAs, so no mode exist nor frequency
    return(structure(NA_real_, freq = NA_integer_))
  
  if(length(x) == 1L)
    # only one value in x, x is the mode
    return(structure(x, freq = 1L)) 
  
  # we don't have NAs so far, either there were then we've already stopped
  # or they've been stripped above
  res <- fastModeX(x, narm=FALSE)
  
  if(length(res)== 0L & attr(res, "freq")==1L)
    return(structure(NA_real_, freq = 1L))
  
  else
    # order results kills the attribute
    return(structure(res[order(res)], freq = attr(res, "freq")))

}



Gmean <- function (x, method = c("classic", "boot"),
                   conf.level = NA, sides = c("two.sided","left","right"),
                   na.rm = FALSE, ...) {

  # see also: http://www.stata.com/manuals13/rameans.pdf

  if(na.rm) x <- na.omit(x)
  is.na(x) <- x < 0

  if(any(x==0)){
    if(is.na(conf.level))
      0

    else
      c(0, NA, NA)

  } else {

    if(is.na(conf.level))
      exp(mean(log(x)))

    else
      exp(MeanCI(x=log(x), method = method,
                 conf.level = conf.level, sides = sides, ...))
  }

}


Gsd <- function (x, na.rm = FALSE) {

  if(na.rm) x <- na.omit(x)
  is.na(x) <- x <= 0

  exp(sd(log(x)))
}



Hmean <- function(x, method = c("classic", "boot"),
                  conf.level = NA, sides = c("two.sided","left","right"),
                  na.rm = FALSE, ...) {

  # see also for alternative ci
  # https://www.unistat.com/guide/confidence-intervals/
  
  is.na(x) <- x <= 0

  if(is.na(conf.level))
    res <- 1 / mean(1/x, na.rm = na.rm)

  else {
  #   res <- (1 / MeanCI(x = 1/x, method = method,
  #                      conf.level = conf.level, sides = sides, na.rm=na.rm, ...))
  #
  #   if(!is.na(conf.level)){
  #     res[2:3] <- c(min(res[2:3]), max(res[2:3]))
  #     if(res[2] < 0)
  #       res[c(2,3)] <- NA
  #   }
  #

    sides <- match.arg(sides, choices = c("two.sided", "left",
                                          "right"), several.ok = FALSE)
    if (sides != "two.sided")
      conf.level <- 1 - 2 * (1 - conf.level)

    res <- (1/(mci <- MeanCI(x = 1/x, method = method, conf.level = conf.level,
                     sides = "two.sided", na.rm = na.rm, ...)))[c(1, 3, 2)]
    
    # check if lower ci < 0, if so return NA, as CI not defined see Stata definition
    if( mci[2] <= 0) 
      res[2:3] <- NA
    
    names(res) <- names(res)[c(1,3,2)]

    if (sides == "left")
      res[3] <- Inf
    else if (sides == "right")
      # it's not clear, if we should not set this to 0
      res[2] <- NA

  }

  return(res)

}



TukeyBiweight <- function(x, const=9, na.rm = FALSE, conf.level = NA, ci.type = "bca", R=1000, ...) {

  if(na.rm) x <- na.omit(x)
  if(anyNA(x)) return(NA)

  if(is.na(conf.level)){
    #  .Call("tbrm", as.double(x[!is.na(x)]), const)
    res <- .Call("tbrm", PACKAGE="DescTools", as.double(x), const)

  } else {


    # adjusted bootstrap percentile (BCa) interval
    boot.tbw <- boot(x, function(x, d) .Call("tbrm", PACKAGE="DescTools", as.double(x[d]), const), R=R, ...)
    ci <- boot.ci(boot.tbw, conf=conf.level, type=ci.type)
    res <- c(tbw=boot.tbw$t0, lwr.ci=ci[[4]][4], upr.ci=ci[[4]][5])
  }

  return(res)

}



## Originally from  /u/ftp/NDK/Source-NDK-9/R/rg2-fkt.R :
.tauHuber <- function(x, mu, k=1.345, s = mad(x), resid = (x - mu)/s) {
  ## Purpose: Korrekturfaktor Tau fuer die Varianz von Huber-M-Schaetzern
  ## -------------------------------------------------------------------------
  ## Arguments: x = Daten mu = Lokations-Punkt k = Parameter der Huber Psi-Funktion
  ## -------------------------------------------------------------------------
  ## Author: Rene Locher Update: R. Frisullo 23.4.02;  M.Maechler (as.log(); s, resid)
  inr <- abs(resid) <= k
  psi  <- ifelse(inr, resid, sign(resid)*k)                # psi (x)
  psiP <- as.logical(inr)# = ifelse(abs(resid) <= k, 1, 0) # psi'(x)
  length(x) * sum(psi^2) / sum(psiP)^2
}

.wgt.himedian <- function(x, weights = rep(1,n)) {

  # Purpose: weighted hiMedian of x
  # Author: Martin Maechler, Date: 14 Mar 2002
  n <- length(x <- as.double(x))
  stopifnot(storage.mode(weights) %in% c("integer", "double"))
  if(n != length(weights))
    stop("'weights' must have same length as 'x'")
  # if(is.integer(weights)) message("using integer weights")
  # Original
  # .C(if(is.integer(weights)) "wgt_himed_i" else "wgt_himed",
  #    x, n, weights,
  #    res = double(1))$res

  if(is.integer(weights))
    .C("wgt_himed_i",
     x, n, weights,
     res = double(1))$res
  else
    .C("wgt_himed",
       x, n, weights,
       res = double(1))$res

}


##  A modified "safe" (and more general) Huber estimator:
.huberM <-
  function(x, k = 1.345, weights = NULL,
           tol = 1e-06,
           mu = if(is.null(weights)) median(x) else .wgt.himedian(x, weights),
           s = if(is.null(weights)) mad(x, center=mu)
           else .wgt.himedian(abs(x - mu), weights),
           se = FALSE,
           warn0scale = getOption("verbose"))
  {
    ## Author: Martin Maechler, Date: 6 Jan 2003, ff

    ## implicit 'na.rm = TRUE':
    if(any(i <- is.na(x))) {
      x <- x[!i]
      if(!is.null(weights)) weights <- weights[!i]
    }
    n <- length(x)
    sum.w <-
      if(!is.null(weights)) {
        stopifnot(is.numeric(weights), weights >= 0, length(weights) == n)
        sum(weights)
      } else n
    it <- 0L
    NA. <- NA_real_
    if(sum.w == 0) # e.g 'x' was all NA
      return(list(mu = NA., s = NA., it = it, se = NA.)) # instead of error

    if(se && !is.null(weights))
      stop("Std.error computation not yet available for the case of 'weights'")
    if (s <= 0) {
      if(s < 0) stop("negative scale 's'")
      if(warn0scale && n > 1)
        warning("scale 's' is zero -- returning initial 'mu'")
    }
    else {
      wsum <- if(is.null(weights)) sum else function(u) sum(u * weights)
      repeat {
        it <- it + 1L
        y <- pmin(pmax(mu - k * s, x), mu + k * s)
        mu1 <- wsum(y) / sum.w
        if (abs(mu - mu1) < tol * s)
          break
        mu <- mu1
      }
    }
    list(mu = mu, s = s, it = it,
         SE = if(se) s * sqrt(.tauHuber(x, mu=mu, s=s, k=k) / n) else NA.)
  }


HuberM <- function(x, k = 1.345, mu = median(x), s = mad(x, center=mu),
                   na.rm = FALSE, conf.level = NA, ci.type = c("wald", "boot"), ...){

  # new interface to HuberM, making it less complex
  # refer to robustbase::huberM if more control is required

  if(na.rm) x <- na.omit(x)
  if(anyNA(x)) return(NA)


  if(is.na(conf.level)){
    res <- .huberM(x=x, k=k, mu=mu, s=s, warn0scale=TRUE)$mu

    return(res)

  } else {

    switch(match.arg(ci.type)
           ,"wald"={
             res <- .huberM(x=x, k=k, mu=mu, s=s, se=TRUE, warn0scale=TRUE)
             # Solution: (12.6.06) - Robuste Regression (Rg-2d) - Musterloeungen zu Serie 1
             # r.loc$mu + c(-1,1)*qt(0.975,8)*sqrt(t.tau/length(d.ertrag))*r.loc$s
             #
             # Ruckstuhl's Loesung:
             # (Sleep.HM$mu + c(-1,1)*qt(0.975, length(Sleep)-1) *
             #              sqrt(f.tau(Sleep, Sleep.HM$mu)) * Sleep.HM$s/sqrt(length(Sleep)))

             #             ci <- qnorm(1-(1-conf.level)/2) * res$SE
             ci <- qt(1-(1-conf.level)/2, length(x)-1) *
               sqrt(.tauHuber(x, res$mu, k=k)) * res$s/sqrt(length(x))
             res <- c(hm=res$mu, lwr.ci=res$mu - ci, upr.ci=res$mu + ci)
           }
           ,"boot" ={
             R <- InDots(..., arg="R", default=1000)
             bci.type <- InDots(..., arg="type", default="perc")

             boot.hm <- boot(x, function(x, d){
               hm <- .huberM(x=x[d], k=k, mu=mu, s=s, se=TRUE)
               return(c(hm$mu, hm$s^2))
             }, R=R)
             ci <- boot.ci(boot.hm, conf=conf.level, ...)

             if(ci.type =="norm") {
               lwr.ci <- ci[[4]][2]
               upr.ci <- ci[[4]][3]
             } else {
               lwr.ci <- ci[[4]][4]
               upr.ci <- ci[[4]][5]
             }

             res <- c(hm=boot.hm$t0[1], lwr.ci=lwr.ci, upr.ci=upr.ci)

           }
    )
    return(res)

  }

}




# old version, replace 13.5.2015
#
# #  A modified "safe" (and more general) Huber estimator:
# HuberM <- function(x, k = 1.5, weights = NULL, tol = 1e-06,
# 	     mu = if(is.null(weights)) median(x) else wgt.himedian(x, weights),
# 	     s = if(is.null(weights)) mad(x, center=mu) else wgt.himedian(abs(x - mu), weights),
# 	     se = FALSE, warn0scale = getOption("verbose"), na.rm = FALSE, stats = FALSE) {
#
#     # Author: Martin Maechler, Date: 6 Jan 2003, ff
#
#     # Originally from  /u/ftp/NDK/Source-NDK-9/R/rg2-fkt.R :
#     tauHuber <- function(x, mu, k=1.5, s = mad(x), resid = (x - mu)/s) {
#       # Purpose: Korrekturfaktor Tau fuer die Varianz von Huber-M-Schaetzern
#       # ******************************************************************************
#       # Arguments: x = Daten mu = Lokations-Punkt k = Parameter der Huber Psi-Funktion
#       # ******************************************************************************
#       # Author: Rene Locher Update: R. Frisullo 23.4.02;  M.Maechler (as.log(); s, resid)
#       inr <- abs(resid) <= k
#       psi  <- ifelse(inr, resid, sign(resid)*k)                 #### psi (x)
#       psiP <- as.logical(inr) # = ifelse(abs(resid) <= k, 1, 0) #### psi'(x)
#       length(x) * sum(psi^2) / sum(psiP)^2
#     }
#
#     wgt.himedian <- function(x, weights = rep(1,n)) {
#
#         # Purpose: weighted hiMedian of x
#         # Author: Martin Maechler, Date: 14 Mar 2002
#         n <- length(x <- as.double(x))
#         stopifnot(storage.mode(weights) %in% c("integer", "double"))
#         if(n != length(weights))
#       stop("'weights' must have same length as 'x'")
#         # if(is.integer(weights)) message("using integer weights")
#         .C(if(is.integer(weights)) "wgt_himed_i" else "wgt_himed",
#            x, n, weights,
#            res = double(1))$res
#     }
#
#
#     # Andri: introduce na.rm
#     # old: implicit 'na.rm = TRUE'
#     if(na.rm) {
#         i <- is.na(x)
#         x <- x[!i]
#         if(!is.null(weights)) weights <- weights[!i]
#     } else {
#       if(anyNA(x)) return(NA)
#     }
#
#
#     n <- length(x)
#     sum.w <-
#         if(!is.null(weights)) {
#             stopifnot(is.numeric(weights), weights >= 0, length(weights) == n)
#             sum(weights)
#         } else n
#     it <- 0L
#     NA. <- NA_real_
#     if(sum.w == 0) # e.g 'x' was all NA
# 	return(list(mu = NA., s = NA., it = it, se = NA.)) # instead of error
#
#     if(se && !is.null(weights))
# 	stop("Std.error computation not yet available for the case of 'weights'")
#
#     if (s <= 0) {
#         if(s < 0) stop("negative scale 's'")
#         if(warn0scale && n > 1)
#             warning("scale 's' is zero -- returning initial 'mu'")
#     }
#     else {
#         wsum <- if(is.null(weights)) sum else function(u) sum(u * weights)
#
# 	repeat {
# 	    it <- it + 1L
#             y <- pmin(pmax(mu - k * s, x), mu + k * s)
# 	    mu1 <- wsum(y) / sum.w
# 	    if (abs(mu - mu1) < tol * s)
# 		break
# 	    mu <- mu1
# 	}
#     }
#
#   if(stats)
#     res <- list(mu = mu, s = s, it = it,
#              SE = if(se) s * sqrt(tauHuber(x, mu=mu, s=s, k=k) / n) else NA.)
#   else
#     res <- mu
#
#   return(res)
#
# }



HodgesLehmann <- function(x, y = NULL, conf.level = NA, na.rm = FALSE) {

  #   Werner Stahel's version:
  #
  #   f.HodgesLehmann <- function(data)
  #   {
  #     ## Purpose:   Hodges-Lehmann estimate and confidence interval
  #     ## -------------------------------------------------------------------------
  #     ## Arguments:
  #     ## Remark: function changed so that CI covers >= 95%, before it was too
  #     ##         small (9/22/04)
  #     ## -------------------------------------------------------------------------
  #     ## Author: Werner Stahel, Date: 12 Aug 2002, 14:13
  #     ## Update: Beat Jaggi, Date: 22 Sept 2004
  #     .cexact <-
  #       # c(NA,NA,NA,NA,NA,21,26,33,40,47,56,65,74,84,95,107,119,131,144,158)
  #       c(NA,NA,NA,NA,NA,22,27,34,41,48,57,66,75,85,96,108,120,132,145,159)
  #     .d <- na.omit(data)
  #     .n <- length(.d)
  #     .wa <- sort(c(outer(.d,.d,"+")/2)[outer(1:.n,1:.n,"<=")])
  #     .c <- if (.n<=length(.cexact)) .n*(.n+1)/2+1-.cexact[.n] else
  #       floor(.n*(.n+1)/4-1.96*sqrt(.n*(.n+1)*(2*.n+1)/24))
  #     .r <- c(median(.wa), .wa[c(.c,.n*(.n+1)/2+1-.c)])
  #     names(.r) <- c("estimate","lower","upper")
  #     .r
  #   }


  # inspired by package ICSNP, function hl.loc

  if(na.rm) {
    if(is.null(y))
      x <- na.omit(x)
    else {
      ok <- complete.cases(x, y)
      x <- x[ok]
      y <- y[ok]
    }
  }

  if(anyNA(x) || (!is.null(y) && anyNA(y)))
    if(is.na(conf.level))
      return(NA)
  else
    return(c(est=NA,  lwr.ci=NA, upr.ci=NA))


  res <- wilcox.test(x,  y, conf.int = TRUE, conf.level = Coalesce(conf.level, 0.8))

  if(is.na(conf.level)){
    result <-  res$estimate
    names(result) <- NULL
  } else {
    result <- c(est=res$estimate,  lwr.ci=res$conf.int[1], upr.ci=res$conf.int[2])
    names(result)[1] <- "est"
  }

  return(result)

}



Skew <- function (x, weights=NULL, na.rm = FALSE, method = 3, conf.level = NA, ci.type = "bca", R=1000, ...) {

  # C part for the expensive (x - mean(x))^2 etc. is a kind of 14 times faster
  #   > x <- rchisq(100000000, df=2)
  #   > system.time(Skew(x))
  #   user  system elapsed
  #   6.32    0.30    6.62
  #   > system.time(Skew2(x))
  #   user  system elapsed
  #   0.47    0.00    0.47


  i.skew <- function(x, weights=NULL, method = 3) {

    # method 1: older textbooks
    if(!is.null(weights)){
      # use a standard treatment for weights
      z <- NormWeights(x, weights, na.rm=na.rm, zero.rm=TRUE)
      r.skew <- .Call("rskeww", as.numeric(z$x), as.numeric(Mean(z$x, weights = z$weights)), as.numeric(z$weights), PACKAGE="DescTools")
      n <- z$wsum
      
    } else {
      if (na.rm) x <- na.omit(x)
      r.skew <- .Call("rskew", as.numeric(x), as.numeric(mean(x)), PACKAGE="DescTools")
      n <- length(x)
      
    }

    se <- sqrt((6*(n-2))/((n+1)*(n+3)))

    if (method == 2) {
      # method 2: SAS/SPSS
      r.skew <- r.skew * n^0.5 * (n - 1)^0.5/(n - 2)
      se <- se * sqrt(n*(n-1))/(n-2)
    }
    else if (method == 3) {
      # method 3: MINITAB/BDMP
      r.skew <- r.skew * ((n - 1)/n)^(3/2)
      se <- se * ((n - 1)/n)^(3/2)
    }
    return(c(r.skew, se^2))
  }


  if(is.na(conf.level)){
    res <- i.skew(x, weights=weights, method=method)[1]

  } else {

    if(ci.type == "classic") {
      res <- i.skew(x, weights=weights, method=method)
      res <- c(skewness=res[1],
               lwr.ci=qnorm((1-conf.level)/2) * sqrt(res[2]),
               upr.ci=qnorm(1-(1-conf.level)/2) * sqrt(res[2]))

    } else {
      # Problematic standard errors and confidence intervals for skewness and kurtosis.
      # Wright DB, Herrington JA. (2011) recommend only bootstrap intervals
      # adjusted bootstrap percentile (BCa) interval
      boot.skew <- boot(x, function(x, d) i.skew(x[d], weights=weights, method=method), R=R, ...)
      ci <- boot.ci(boot.skew, conf=conf.level, type=ci.type)
      if(ci.type =="norm") {
        lwr.ci <- ci[[4]][2]
        upr.ci <- ci[[4]][3]
      } else {
        lwr.ci <- ci[[4]][4]
        upr.ci <- ci[[4]][5]
      }
      
      res <- c(skew=boot.skew$t0[1], lwr.ci=lwr.ci, upr.ci=upr.ci)
    }
  }

  return(res)

}




Kurt <- function (x, weights=NULL, na.rm = FALSE, method = 3, conf.level = NA, 
                  ci.type = "bca", R=1000, ...) {

  i.kurt <- function(x, weights=NULL, na.rm = FALSE, method = 3) {

    # method 1: older textbooks
    if(!is.null(weights)){
      # use a standard treatment for weights
      z <- NormWeights(x, weights, na.rm=na.rm, zero.rm=TRUE)
      r.kurt <- .Call("rkurtw", as.numeric(z$x), as.numeric(Mean(z$x, weights = z$weights)), as.numeric(z$weights), PACKAGE="DescTools")
      n <- z$wsum
      
    } else {
      if (na.rm) x <- na.omit(x)
      r.kurt <- .Call("rkurt", as.numeric(x), as.numeric(mean(x)), PACKAGE="DescTools")
      n <- length(x)
      
    }
    
    se <- sqrt((24*n*(n-2)*(n-3))/((n+1)^2*(n+3)*(n+5)))

    if (method == 2) {
      # method 2: SAS/SPSS
      r.kurt <- ((r.kurt + 3) * (n + 1)/(n - 1) - 3) * (n - 1)^2/(n - 2)/(n - 3)
      se <- se * (((n-1)*(n+1))/((n-2)*(n-3)))
    }
    else if (method == 3) {
      # method 3: MINITAB/BDMP
      r.kurt <- (r.kurt + 3) * (1 - 1/n)^2 - 3
      se <- se * ((n-1)/n)^2
    }
    return(c(r.kurt, se^2))
  }

  if(is.na(conf.level)){
    res <- i.kurt(x, weights=weights, na.rm=na.rm, method=method)[1]

  } else {
    if(ci.type == "classic") {
      res <- i.kurt(x, weights=weights, method=method)
      res <- c(kurtosis=res[1],
               lwr.ci=qnorm((1-conf.level)/2) * sqrt(res[2]),
               upr.ci=qnorm(1-(1-conf.level)/2) * sqrt(res[2]))

    } else {

      # Problematic standard errors and confidence intervals for skewness and kurtosis.
      # Wright DB, Herrington JA. (2011) recommend only bootstrap intervals
      # adjusted bootstrap percentile (BCa) interval
      boot.kurt <- boot(x, function(x, d) i.kurt(x[d], weights=weights, na.rm=na.rm, method=method), R=R, ...)
      ci <- boot.ci(boot.kurt, conf=conf.level, type=ci.type)

      if(ci.type =="norm") {
        lwr.ci <- ci[[4]][2]
        upr.ci <- ci[[4]][3]
      } else {
        lwr.ci <- ci[[4]][4]
        upr.ci <- ci[[4]][5]
      }

      res <- c(kurt=boot.kurt$t0[1], lwr.ci=lwr.ci, upr.ci=upr.ci)
    }
  }

  return(res)

}



Outlier <- function(x, method=c("boxplot", "hampel"), value=TRUE, na.rm=FALSE){

  switch(match.arg(arg = method, choices = c("boxplot", "hampel")),
         #         boxplot =  { x[x %)(% (quantile(x, c(0.25,0.75), na.rm=na.rm) + c(-1,1) * 1.5*IQR(x,na.rm=na.rm))] }
         boxplot =  {
           # old, replaced by v. 0.99.26
           # res <- boxplot(x, plot = FALSE)$out

             qq <- quantile(as.numeric(x), c(0.25, 0.75), na.rm = na.rm, names = FALSE)
             iqr <- diff(qq)
             id <- x < (qq[1] - 1.5 * iqr) | x > (qq[2] + 1.5 * iqr)

           },
         
         hampel = {
           # hampel considers values outside of median ± 3*(median absolute deviation) to be outliers
           id <- x %][% (median(x, na.rm=na.rm) + 3 * c(-1, 1) * mad(x, na.rm=na.rm))
         }
  )

  if(value)
    res <- x[id]
  else
    res <- which(id)
  
  res <- res[!is.na(res)]
  
  return(res)

}



LOF <- function(data,k) {

  # source: library(dprep)

  # A function that finds the local outlier factor (Breunig,2000) of
  # the matrix "data" with k neighbors
  # Adapted by Caroline Rodriguez and Edgar Acuna, may 2004

  knneigh.vect <-
    function(x,data,k)
    {
      #Function that returns the distance from a vector "x" to
      #its k-nearest-neighbors in the matrix "data"

      temp=as.matrix(data)
      numrow=dim(data)[1]
      dimnames(temp)=NULL

      #subtract rowvector x from each row of data
      difference<- scale(temp, x, FALSE)

      #square and add all differences and then take the square root
      dtemp <- drop(difference^2 %*% rep(1, ncol(data)))
      dtemp=sqrt(dtemp)

      #order the distances
      order.dist <- order(dtemp)
      nndist=dtemp[order.dist]

      #find distance to k-nearest neighbor
      #uses k+1 since first distance in vector is a 0
      knndist=nndist[k+1]

      #find neighborhood
      #eliminate first row of zeros from neighborhood
      neighborhood=drop(nndist[nndist<=knndist])
      neighborhood=neighborhood[-1]
      numneigh=length(neighborhood)

      #find indexes of each neighbor in the neighborhood
      index.neigh=order.dist[1:numneigh+1]

      # this will become the index of the distance to first neighbor
      num1=length(index.neigh)+3

      # this will become the index of the distance to last neighbor
      num2=length(index.neigh)+numneigh+2

      #form a vector
      neigh.dist=c(num1,num2,index.neigh,neighborhood)

      return(neigh.dist)
    }



  dist.to.knn <-
    function(dataset,neighbors)
    {

      #function returns an object in which each column contains
      #the indices of the first k neighbors followed by the
      #distances to each of these neighbors

      numrow=dim(dataset)[1]

      #applies a function to find distance to k nearest neighbors
      #within "dataset" for each row of the matrix "dataset"

      knndist=rep(0,0)


      for (i in 1:numrow)
      {
        #find obervations that make up the k-distance neighborhood for dataset[i,]
        neighdist=knneigh.vect(dataset[i,],dataset,neighbors)

        #adjust the length of neighdist or knndist as needed to form matrix of neighbors
        #and their distances
        if (i==2)
        {
          if (length(knndist)<length(neighdist))
          {
            z=length(neighdist)-length(knndist)
            zeros=rep(0,z)
            knndist=c(knndist,zeros)
          }
          else if (length(knndist)>length(neighdist))
          {
            z=length(knndist)-length(neighdist)
            zeros=rep(0,z)
            neighdist=c(neighdist,zeros)
          }
        }
        else
        {
          if (i!=1)
          {
            if (dim(knndist)[1]<length(neighdist))
            {
              z=(length(neighdist)-dim(knndist)[1])
              zeros=rep(0,z*dim(knndist)[2])
              zeros=matrix(zeros,z,dim(knndist)[2])
              knndist=rbind(knndist,zeros)
            }
            else if (dim(knndist)[1]>length(neighdist))
            {
              z=(dim(knndist)[1]-length(neighdist))
              zeros=rep(0,z)
              neighdist=c(neighdist,zeros)
            }
          }
        }
        knndist=cbind(knndist,neighdist)
      }

      return(knndist)
    }


  reachability <-
    function(distdata,k)
    {
      #function that calculates the local reachability density
      #of Breuing(2000) for each observation in a matrix, using
      #a matrix (distdata) of k nearest neighbors computed by the function dist.to.knn2

      p=dim(distdata)[2]
      lrd=rep(0,p)

      for (i in 1:p)
      {
        j=seq(3,3+(distdata[2,i]-distdata[1,i]))
        # compare the k-distance from each observation to its kth neighbor
        # to the actual distance between each observation and its neighbors
        numneigh=distdata[2,i]-distdata[1,i]+1
        temp=rbind(diag(distdata[distdata[2,distdata[j,i]],distdata[j,i]]),distdata[j+numneigh,i])

        #calculate reachability
        reach=1/(sum(apply(temp,2,max))/numneigh)
        lrd[i]=reach
      }
      lrd
    }


  data=as.matrix(data)

  #find k nearest neighbors and their distance from each observation
  #in data
  distdata=dist.to.knn(data,k)
  p=dim(distdata)[2]

  #calculate the local reachability density for each observation in data
  lrddata=reachability(distdata,k)

  lof=rep(0,p)

  #computer the local outlier factor of each observation in data
  for ( i in 1:p)
  {
    nneigh=distdata[2,i]-distdata[1,i]+1
    j=seq(0,(nneigh-1))
    local.factor=sum(lrddata[distdata[3+j,i]]/lrddata[i])/nneigh
    lof[i]=local.factor
  }

  #return lof, a vector with the local outlier factor of each observation
  lof
}




BootCI <- function(x, y=NULL, FUN, ..., bci.method = c("norm", "basic", "stud", "perc", "bca"),
                   conf.level = 0.95, sides = c("two.sided", "left", "right"), R = 999) {

  dots <- as.list(substitute(list( ... ))) [-1]
  bci.method <- match.arg(bci.method)

  sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
  if(sides!="two.sided")
    conf.level <- 1 - 2*(1-conf.level)

  if(is.null(y))
    boot.fun <- boot(x, function(x, d) do.call(FUN, append(list(x[d]), dots)), R = R)
  else
    boot.fun <- boot(x, function(x, d) do.call(FUN, append(list(x[d], y[d]), dots)), R = R)

  ci <- boot.ci(boot.fun, conf=conf.level, type=bci.method)

  if (bci.method == "norm") {
    res <- c(est = boot.fun$t0, lwr.ci = ci[[4]][2],
             upr.ci = ci[[4]][3])

  } else {
    res <- c(est = boot.fun$t0, lwr.ci = ci[[4]][4],
             upr.ci = ci[[4]][5])
  }

  if(sides=="left")
    res[3] <- Inf
  else if(sides=="right")
    res[2] <- -Inf

  names(res)[1] <- deparse(substitute(FUN))
  return(res)

}





# Confidence Intervals for Binomial Proportions
BinomCI <- function(x, n, conf.level = 0.95, sides = c("two.sided","left","right"),
                    method = c("wilson", "wald", "waldcc", "agresti-coull", "jeffreys", "modified wilson", "wilsoncc",
                                "modified jeffreys", "clopper-pearson", "arcsine", "logit", "witting", "pratt", "midp", "lik", "blaker"), 
                    rand = 123, tol=1e-05) {

  if(missing(method)) method <- "wilson"
  if(missing(sides)) sides <- "two.sided"

  iBinomCI <- function(x, n, conf.level = 0.95, sides = c("two.sided","left","right"),
                       method = c("wilson", "wilsoncc", "wald", "waldcc","agresti-coull", "jeffreys", "modified wilson",
                       "modified jeffreys", "clopper-pearson", "arcsine", "logit", "witting", "pratt", "midp", "lik", "blaker"), 
                       rand = 123, tol=1e-05) {

    if(length(x) != 1) stop("'x' has to be of length 1 (number of successes)")
    if(length(n) != 1) stop("'n' has to be of length 1 (number of trials)")
    if(length(conf.level) != 1)  stop("'conf.level' has to be of length 1 (confidence level)")
    if(conf.level < 0.5 | conf.level > 1)  stop("'conf.level' has to be in [0.5, 1]")

    sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
    if(sides!="two.sided")
      conf.level <- 1 - 2*(1-conf.level)

    alpha <- 1 - conf.level
    kappa <- qnorm(1-alpha/2)
    p.hat <- x/n
    q.hat <- 1 - p.hat

    # this is the default estimator used by the most (but not all) methods
    est <- p.hat
    
    switch( match.arg(arg=method, choices=c("wilson", "wald", "waldcc", "wilsoncc","agresti-coull", "jeffreys", "modified wilson",
                                            "modified jeffreys", "clopper-pearson", "arcsine", "logit", "witting","pratt", "midp", "lik", "blaker"))
            , "wald" = {
              term2 <- kappa*sqrt(p.hat*q.hat)/sqrt(n)
              CI.lower <- max(0, p.hat - term2)
              CI.upper <- min(1, p.hat + term2)
            }
            , "waldcc" = {
              term2 <- kappa*sqrt(p.hat*q.hat)/sqrt(n)
              # continuity correction
              term2 <- term2 + 1/(2*n)
              CI.lower <- max(0, p.hat - term2)
              CI.upper <- min(1, p.hat + term2)
            }
            , "wilson" = {
              term1 <- (x + kappa^2/2)/(n + kappa^2)
              term2 <- kappa*sqrt(n)/(n + kappa^2)*sqrt(p.hat*q.hat + kappa^2/(4*n))
              CI.lower <-  max(0, term1 - term2)
              CI.upper <- min(1, term1 + term2)
            }
            , "wilsoncc" = {
              lci <- ( 2*x+kappa**2 -1 - kappa*sqrt(kappa**2 -
                            2- 1/n + 4*p.hat*(n*q.hat+1))) / (2*(n+kappa**2))
              uci <- ( 2*x+kappa**2 +1 + kappa*sqrt(kappa**2 +
                            2- 1/n + 4*p.hat*(n*q.hat-1))) / (2*(n+kappa**2))
              
              CI.lower <- max(0, ifelse(p.hat==0, 0, lci))
              CI.upper <- min(1, ifelse(p.hat==1, 1, uci))
              
            }
            , "agresti-coull" = {
              x.tilde <- x + kappa^2/2
              n.tilde <- n + kappa^2
              p.tilde <- x.tilde/n.tilde
              q.tilde <- 1 - p.tilde
              # non standard estimator!!
              est <- p.tilde
              term2 <- kappa*sqrt(p.tilde*q.tilde)/sqrt(n.tilde)
              CI.lower <- max(0, p.tilde - term2)
              CI.upper <- min(1, p.tilde + term2)
            }
            , "jeffreys" = {
              if(x == 0)
                CI.lower <- 0
              else
                CI.lower <- qbeta(alpha/2, x+0.5, n-x+0.5)
              if(x == n)
                CI.upper <- 1
              else
                CI.upper <- qbeta(1-alpha/2, x+0.5, n-x+0.5)
            }
            , "modified wilson" = {
              term1 <- (x + kappa^2/2)/(n + kappa^2)
              term2 <- kappa*sqrt(n)/(n + kappa^2)*sqrt(p.hat*q.hat + kappa^2/(4*n))
              ## comment by Andre Gillibert, 19.6.2017:
              ## old:
              ## if((n <= 50 & x %in% c(1, 2)) | (n >= 51 & n <= 100 & x %in% c(1:3)))
              ## new:
              if((n <= 50 & x %in% c(1, 2)) | (n >= 51 &            x %in% c(1:3)))
                CI.lower <- 0.5*qchisq(alpha, 2*x)/n
              else
                CI.lower <-  max(0, term1 - term2)

              ## if((n <= 50 & x %in% c(n-1, n-2)) | (n >= 51 & n <= 100 & x %in% c(n-(1:3))))
              if((n <= 50 & x %in% c(n-1, n-2)) | (n >= 51 &            x %in% c(n-(1:3))))
                CI.upper <- 1 - 0.5*qchisq(alpha, 2*(n-x))/n
              else
                CI.upper <- min(1, term1 + term2)
            }
            , "modified jeffreys" = {
              if(x == n)
                CI.lower <- (alpha/2)^(1/n)
              else {
                if(x <= 1)
                  CI.lower <- 0
                else
                  CI.lower <- qbeta(alpha/2, x+0.5, n-x+0.5)
              }
              if(x == 0)
                CI.upper <- 1 - (alpha/2)^(1/n)
              else{
                if(x >= n-1)
                  CI.upper <- 1
                else
                  CI.upper <- qbeta(1-alpha/2, x+0.5, n-x+0.5)
              }
            }
            , "clopper-pearson" = {
              CI.lower <- qbeta(alpha/2, x, n-x+1)
              CI.upper <- qbeta(1-alpha/2, x+1, n-x)
            }
            , "arcsine" = {
              p.tilde <- (x + 0.375)/(n + 0.75)
              # non standard estimator
              est <- p.tilde
              CI.lower <- sin(asin(sqrt(p.tilde)) - 0.5*kappa/sqrt(n))^2
              CI.upper <- sin(asin(sqrt(p.tilde)) + 0.5*kappa/sqrt(n))^2
            }
            , "logit" = {
              lambda.hat <- log(x/(n-x))
              V.hat <- n/(x*(n-x))
              lambda.lower <- lambda.hat - kappa*sqrt(V.hat)
              lambda.upper <- lambda.hat + kappa*sqrt(V.hat)
              CI.lower <- exp(lambda.lower)/(1 + exp(lambda.lower))
              CI.upper <- exp(lambda.upper)/(1 + exp(lambda.upper))
            }
            , "witting" = {
              set.seed(rand)
              x.tilde <- x + runif(1, min = 0, max = 1)
              pbinom.abscont <- function(q, size, prob){
                v <- trunc(q)
                term1 <- pbinom(v-1, size = size, prob = prob)
                term2 <- (q - v)*dbinom(v, size = size, prob = prob)
                return(term1 + term2)
              }
              qbinom.abscont <- function(p, size, x){
                fun <- function(prob, size, x, p){
                  pbinom.abscont(x, size, prob) - p
                }
                uniroot(fun, interval = c(0, 1), size = size, x = x, p = p)$root
              }
              CI.lower <- qbinom.abscont(1-alpha, size = n, x = x.tilde)
              CI.upper <- qbinom.abscont(alpha, size = n, x = x.tilde)
            }

            , "pratt" = {

              if(x==0) {
                CI.lower <- 0
                CI.upper <- 1-alpha^(1/n)
              } else if(x==1) {
                CI.lower <- 1-(1-alpha/2)^(1/n)
                CI.upper <- 1-(alpha/2)^(1/n)
              } else if(x==(n-1)) {
                CI.lower <- (alpha/2)^(1/n)
                CI.upper <- (1-alpha/2)^(1/n)
              } else if(x==n) {
                CI.lower <- alpha^(1/n)
                CI.upper <- 1
              } else {
                z <- qnorm(1 - alpha/2)

                A <- ((x+1) / (n-x))^2
                B <- 81*(x+1)*(n-x)-9*n-8
                C <- (0-3)*z*sqrt(9*(x+1)*(n-x)*(9*n+5-z^2)+n+1)
                D <- 81*(x+1)^2-9*(x+1)*(2+z^2)+1
                E <- 1+A*((B+C)/D)^3
                CI.upper <- 1/E

                A <- (x / (n-x-1))^2
                B <- 81*x*(n-x-1)-9*n-8
                C <- 3*z*sqrt(9*x*(n-x-1)*(9*n+5-z^2)+n+1)
                D <- 81*x^2-9*x*(2+z^2)+1
                E <- 1+A*((B+C)/D)^3
                CI.lower <- 1/E
              }
            }
            , "midp" = {
              
                #Functions to find root of for the lower and higher bounds of the CI
                #These are helper functions.
                f.low <- function(pi, x, n) {
                  1/2*dbinom(x, size=n, prob=pi) + 
                    pbinom(x, size=n, prob=pi, lower.tail=FALSE) - (1-conf.level)/2
                }
                f.up <- function(pi, x, n) {
                  1/2*dbinom(x, size=n, prob=pi) + 
                    pbinom(x-1, size=n, prob=pi) - (1-conf.level)/2
                }
                
                #One takes pi_low = 0 when x=0 and pi_up=1 when x=n
                CI.lower <- 0
                CI.upper <- 1
                
                #Calculate CI by finding roots of the f funcs
                if (x!=0) {
                  CI.lower <- uniroot(f.low, interval=c(0, p.hat), x=x, n=n)$root
                } 
                if (x!=n) {
                  CI.upper  <- uniroot(f.up, interval=c(p.hat, 1), x=x, n=n)$root
                }
                

            }
            , "lik" = {
              
              CI.lower <- 0
              CI.upper <- 1
              z <- qnorm(1 - alpha * 0.5)
              # preset tolerance, should we offer function argument?
              tol = .Machine$double.eps^0.5
              
              BinDev <- function(y, x, mu, wt, bound = 0, 
                                 tol = .Machine$double.eps^0.5, ...) {
                
                # returns the binomial deviance for y, x, wt
                ll.y <- ifelse(y %in% c(0, 1), 0, dbinom(x, wt, y, log=TRUE))
                ll.mu <- ifelse(mu %in% c(0, 1), 0, dbinom(x, wt, mu, log=TRUE))
                res <- ifelse(abs(y - mu) < tol, 0, 
                              sign(y - mu) * sqrt(-2 * (ll.y - ll.mu)))
                return(res - bound)
              }

              if(x!=0 && tol<p.hat) {
                CI.lower <- if(BinDev(tol, x, p.hat, n, -z, tol) <= 0) {
                  uniroot(f = BinDev, interval = c(tol, if(p.hat < tol || p.hat == 1) 1 - tol else p.hat), 
                          bound = -z, x = x, mu = p.hat, wt = n)$root }
              }

              if(x!=n && p.hat<(1-tol)) {
                CI.upper <- if(BinDev(y = 1 - tol, x = x, mu = ifelse(p.hat > 1 - tol, tol, p.hat), 
                                      wt = n, bound = z, tol = tol) < 0) {
                  CI.lower <- if(BinDev(tol, x, if(p.hat < tol || p.hat == 1) 1 - tol else p.hat, n, -z, tol) <= 0) {
                    uniroot(f = BinDev, interval = c(tol, p.hat),
                            bound = -z, x = x, mu = p.hat, wt = n)$root  }
                } else {
                  uniroot(f = BinDev, interval = c(if(p.hat > 1 - tol) tol else p.hat, 1 - tol),
                          bound = z, x = x, mu = p.hat, wt = n)$root     }
              }
            }
            , "blaker" ={
              
              acceptbin <- function (x, n, p) {
                
                p1 <- 1 - pbinom(x - 1, n, p)
                p2 <- pbinom(x, n, p)
                
                a1 <- p1 + pbinom(qbinom(p1, n, p) - 1, n, p)
                a2 <- p2 + 1 - pbinom(qbinom(1 - p2, n, p), n, p)
                
                return(min(a1, a2))
              }
              
              CI.lower <- 0
              CI.upper <- 1
              
              if (x != 0) {
                CI.lower <- qbeta((1 - conf.level)/2, x, n - x + 1)
                while (acceptbin(x, n, CI.lower + tol) < (1 - conf.level)) 
                  CI.lower = CI.lower + tol
              }
              
              if (x != n) {
                CI.upper <- qbeta(1 - (1 - conf.level)/2, x + 1, n - x)
                while (acceptbin(x, n, CI.upper - tol) < (1 - conf.level)) 
                  CI.upper <- CI.upper - tol
              }
              
            }
    )

    # dot not return ci bounds outside [0,1]
    ci <- c( est=est, lwr.ci=max(0, CI.lower), upr.ci=min(1, CI.upper) )

    if(sides=="left")
      ci[3] <- 1
    else if(sides=="right")
      ci[2] <- 0

    return(ci)

  }


  # handle vectors
  # which parameter has the highest dimension
  lst <- list(x=x, n=n, conf.level=conf.level, sides=sides, 
              method=method, rand=rand)
  maxdim <- max(unlist(lapply(lst, length)))
  # recycle all params to maxdim
  lgp <- lapply( lst, rep, length.out=maxdim )
  # # increase conf.level for one sided intervals
  # lgp$conf.level[lgp.sides!="two.sided"] <- 1 - 2*(1-lgp$conf.level[lgp.sides!="two.sided"])

  # get rownames
  lgn <- Recycle(x=if(is.null(names(x))) paste("x", seq_along(x), sep=".") else names(x),
                 n=if(is.null(names(n))) paste("n", seq_along(n), sep=".") else names(n),
                 conf.level=conf.level, sides=sides, method=method)



  xn <- apply(as.data.frame(lgn[sapply(lgn, function(x) length(unique(x)) != 1)]), 1, paste, collapse=":")


  res <- t(sapply(1:maxdim, function(i) iBinomCI(x=lgp$x[i], n=lgp$n[i],
                                                 conf.level=lgp$conf.level[i],
                                                 sides=lgp$sides[i],
                                                 method=lgp$method[i], rand=lgp$rand[i])))
  colnames(res)[1] <- c("est")
  rownames(res) <- xn

  return(res)

}




BinomCIn <- function(p=0.5, width, interval=c(1, 1e5), conf.level=0.95, sides="two.sided", method="wilson") {
  uniroot(f = function(n) diff(BinomCI(x=p*n, n=n, conf.level=conf.level, 
                                       sides=sides, method=method)[, -1]) - width, 
          interval = interval)$root
}





BinomDiffCI <- function(x1, n1, x2, n2, conf.level = 0.95, sides = c("two.sided","left","right"),
                        method=c("ac", "wald", "waldcc", "score", "scorecc", "mn",
                                 "mee", "blj", "ha", "hal", "jp")) {


  if(missing(sides))    sides <- match.arg(sides)
  if(missing(method))   method <- match.arg(method)
  

  iBinomDiffCI <- function(x1, n1, x2, n2, conf.level, sides, method) {
    
    #   .Wald #1
    #   .Wald (Corrected) #2
    #   .Exact
    #   .Exact (FM Score)
    #   .Newcombe Score #10
    #   .Newcombe Score (Corrected) #11
    #   .Farrington-Manning
    #   .Hauck-Anderson
    # http://www.jiangtanghu.com/blog/2012/09/23/statistical-notes-5-confidence-intervals-for-difference-between-independent-binomial-proportions-using-sas/
    #  Interval estimation for the difference between independent proportions: comparison of eleven methods.

    # https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf
    # http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.633.9380&rep=rep1&type=pdf
    
    # Newcombe (1998) (free):
    # http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.408.7354&rep=rep1&type=pdf
    
    
    # no need to check args here, they're already ok...
    # method <- match.arg(arg = method,
    #                     choices = c("wald", "waldcc", "ac", "score", "scorecc", "mn",
    #                                 "mee", "blj", "ha", "hal", "jp"))
    # 
    # sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
    
    
    if(sides!="two.sided")
      conf.level <- 1 - 2*(1-conf.level)

    alpha <- 1 - conf.level
    kappa <- qnorm(1 - alpha/2)

    p1.hat <- x1/n1
    p2.hat <- x2/n2
    est <- p1.hat - p2.hat

    switch(method,
           "wald" = {
             vd <- p1.hat*(1-p1.hat)/n1 + p2.hat*(1-p2.hat)/n2
             term2 <- kappa * sqrt(vd)

             CI.lower <- max(-1, est - term2)
             CI.upper <- min(1, est + term2)
           },

           "waldcc" = {
             vd <- p1.hat*(1-p1.hat)/n1 + p2.hat*(1-p2.hat)/n2

             term2 <- kappa * sqrt(vd)
             term2 <- term2 + 0.5 * (1/n1+1/n2)

             CI.lower <- max(-1, est - term2)
             CI.upper <- min(1, est + term2)
           },
           "ac" = {   # Agresti-Caffo

             n1 <- n1+2
             n2 <- n2+2
             x1  <- x1+1
             x2  <- x2+1

             p1.hat <- x1/n1
             p2.hat <- x2/n2
             est1 <-  p1.hat - p2.hat

             vd <- p1.hat*(1-p1.hat)/n1 + p2.hat*(1-p2.hat)/n2

             term2 <- kappa * sqrt(vd)

             CI.lower <- max(-1, est1 - term2)
             CI.upper <- min(1, est1 + term2)
           } ,
           "exact" = {   # exact

             CI.lower <- NA
             CI.upper <- NA
           },
           "score" = {   # Newcombe

             w1 <- BinomCI(x=x1, n=n1, conf.level=conf.level, method="wilson")
             w2 <- BinomCI(x=x2, n=n2, conf.level=conf.level, method="wilson")
             l1 <- w1[2]
             u1 <- w1[3]
             l2 <- w2[2]
             u2 <- w2[3]

             CI.lower <- est - kappa * sqrt(l1*(1-l1)/n1 + u2*(1-u2)/n2)
             CI.upper <- est + kappa * sqrt(u1*(1-u1)/n1 + l2*(1-l2)/n2)
           },
           "scorecc" = {   # Newcombe

             w1 <- BinomCI(x=x1, n=n1, conf.level=conf.level, method="wilsoncc")
             w2 <- BinomCI(x=x2, n=n2, conf.level=conf.level, method="wilsoncc")
             l1 <- w1[2]
             u1 <- w1[3]
             l2 <- w2[2]
             u2 <- w2[3]

             CI.lower <- max(-1, est - sqrt((p1.hat - l1)^2 + (u2-p2.hat)^2) )
             CI.upper <- min( 1, est + sqrt((u1-p1.hat)^2 + (p2.hat-l2)^2) )

           },
           "mee" = {   # Mee, also called Farrington-Mannig

             .score <- function (p1, n1, p2, n2, dif) {

               if(dif > 1) dif <- 1
               if(dif < -1) dif <- -1
               
               diff <- p1 - p2 - dif
               
               if (abs(diff) == 0) {
                 res <- 0
                 
               } else {
                 t <- n2/n1
                 a <- 1 + t
                 b <- -(1 + t + p1 + t * p2 + dif * (t + 2))
                 c <- dif * dif + dif * (2 * p1 + t + 1) + p1 + t * p2
                 d <- -p1 * dif * (1 + dif)
                 v <- (b/a/3)^3 - b * c/(6 * a * a) + d/a/2
                 # v might be very small, resulting in a value v/u^3 > |1| 
                 # causing a numeric error for acos(v/u^3)
                 # see:  x1=10, n1=10, x2=0, n1=10
                 if(abs(v) < .Machine$double.eps) v <- 0
                 s <- sqrt((b/a/3)^2 - c/a/3)
                 u <- ifelse(v>0, 1,-1) * s
                 w <- (3.141592654 + acos(v/u^3))/3
                 p1d <- 2 * u * cos(w) - b/a/3
                 p2d <- p1d - dif
                 n <- n1 + n2
                 res <- (p1d * (1 - p1d)/n1 + p2d * (1 - p2d)/n2)
                 # res <- max(0, res)   # might result in a value slightly negative
                    
               }
               
               return(sqrt(res))
             }

             pval <- function(delta){
               z <- (est - delta)/.score(p1.hat, n1, p2.hat, n2, delta)
               2 * min(pnorm(z), 1-pnorm(z))
             }

             CI.lower <- max(-1, uniroot(
               function(delta) pval(delta) - alpha, 
               interval = c(-1+1e-6, est-1e-6)
             )$root)

             CI.upper <- min(1, uniroot(
               function(delta) pval(delta) - alpha, 
               interval = c(est + 1e-6, 1-1e-6)
             )$root)

           },

           "blj" = {  # brown-li-jeffrey
             p1.dash <- (x1 + 0.5) / (n1+1)
             p2.dash <- (x2 + 0.5) / (n2+1)
             vd <- p1.dash*(1-p1.dash)/n1 + p2.dash*(1-p2.dash)/n2
             term2 <- kappa * sqrt(vd)
             est.dash <- p1.dash - p2.dash

             CI.lower <- max(-1, est.dash  - term2)
             CI.upper <- min(1, est.dash + term2)

           },
           "ha" = {   # Hauck-Anderson

             term2 <- 1/(2*min(n1,n2)) + kappa * sqrt(p1.hat*(1-p1.hat)/(n1-1) +
                                                        p2.hat*(1-p2.hat)/(n2-1))

             CI.lower <- max(-1, est - term2)
             CI.upper <- min( 1, est + term2 )
           },
           "mn" = {  # Miettinen-Nurminen

             .conf <- function(x1, n1, x2, n2, z, lower=FALSE){

               p1 <- x1/n1
               p2 <- x2/n2
               p.hat <- p1 - p2
               dp <- 1 + ifelse(lower, 1, -1) * p.hat
               i <- 1

               while (i <= 50) {
                 dp <- 0.5 * dp
                 y <- p.hat + ifelse(lower, -1, 1) * dp
                 score <- .score(p1, n1, p2, n2, y)
                 if (score < z) {
                   p.hat <- y
                 }
                 if ((dp < 0.0000001) || (abs(z - score) < 0.000001))
                   break()
                 else
                   i <- i + 1
               }
               return(y)
             }

             .score <- function (p1, n1, p2, n2, dif) {

               diff <- p1 - p2 - dif
               if (abs(diff) == 0) {
                 res <- 0
               }
               else {
                 t <- n2/n1
                 a <- 1 + t
                 b <- -(1 + t + p1 + t * p2 + dif * (t + 2))
                 c <- dif * dif + dif * (2 * p1 + t + 1) + p1 + t * p2
                 d <- -p1 * dif * (1 + dif)
                 v <- (b/a/3)^3 - b * c/(6 * a * a) + d/a/2
                 s <- sqrt((b/a/3)^2 - c/a/3)
                 u <- ifelse(v>0, 1,-1) * s
                 w <- (3.141592654 + acos(v/u^3))/3
                 p1d <- 2 * u * cos(w) - b/a/3
                 p2d <- p1d - dif
                 n <- n1 + n2
                 var <- (p1d * (1 - p1d)/n1 + p2d * (1 - p2d)/n2) * n/(n - 1)
                 res <- diff^2/var
               }
               return(res)
             }

             z = qchisq(conf.level, 1)

             CI.lower <- max(-1, .conf(x1, n1, x2, n2, z, TRUE))
             CI.upper <- min(1, .conf(x1, n1, x2, n2, z, FALSE))

           },
           "beal" = {
             
             # experimental code only...
             # http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.633.9380&rep=rep1&type=pdf

             a <- p1.hat + p2.hat
             b <- p1.hat - p2.hat
             u <- ((1/n1) + (1/n2)) / 4
             v <- ((1/n1) - (1/n2)) / 4
             V <- u*((2-a)*a - b^2) + 2*v*(1-a)*b
             z <- qchisq(p=1-alpha/2, df = 1)
             A <- sqrt(z*(V + z*u^2*(2-a)*a + z*v^2*(1-a)^2))
             B <- (b + z*v*(1-a)) / (1+z*u)

             CI.lower <- max(-1, B - A / (1 + z*u))
             CI.upper <- min(1, B + A / (1 + z*u))

           },
           "hal" = {  # haldane 
             
             psi <- (p1.hat + p2.hat) / 2
             u <- (1/n1 + 1/n2) / 4
             v <- (1/n1 - 1/n2) / 4
             
             z <- kappa
             
             theta <- ((p1.hat - p2.hat) + z^2 * v* (1-2*psi)) / (1+z^2*u)
             w <- z / (1+z^2*u) * sqrt(u * (4*psi*(1-psi) - (p1.hat - p2.hat)^2) + 
                                         2*v*(1-2*psi) *(p1.hat - p2.hat) + 
                                         4*z^2*u^2*(1-psi)*psi + z^2*v^2*(1-2*psi)^2)
             c(theta + w, theta - w)
             CI.lower <- max(-1, theta - w)
             CI.upper <- min(1, theta + w)
             
           },
           "jp" = {   # jeffery perks
             
             # same as haldane but with other psi
             psi <- 0.5 * ((x1 + 0.5) / (n1 + 1) + (x2 + 0.5) / (n2 + 1) )
             
             u <- (1/n1 + 1/n2) / 4
             v <- (1/n1 - 1/n2) / 4
             
             z <- kappa
             
             theta <- ((p1.hat - p2.hat) + z^2 * v* (1-2*psi)) / (1+z^2*u)
             w <- z / (1+z^2*u) * sqrt(u * (4*psi*(1-psi) - (p1.hat - p2.hat)^2) + 
                                         2*v*(1-2*psi) *(p1.hat - p2.hat) + 
                                         4*z^2*u^2*(1-psi)*psi + z^2*v^2*(1-2*psi)^2)
             c(theta + w, theta - w)
             CI.lower <- max(-1, theta - w)
             CI.upper <- min(1, theta + w)
             
           },
           
    )

    ci <- c(est = est, lwr.ci = min(CI.lower, CI.upper), upr.ci = max(CI.lower, CI.upper))

    if(sides=="left")
      ci[3] <- 1
    else if(sides=="right")
      ci[2] <- -1

    return(ci)

  }

  
  method <- match.arg(arg=method, several.ok = TRUE)
  sides <- match.arg(arg=sides, several.ok = TRUE)
  
  # Recycle arguments
  lst <- Recycle(x1=x1, n1=n1, x2=x2, n2=n2, conf.level=conf.level, sides=sides, method=method)

  res <- t(sapply(1:attr(lst, "maxdim"),
                  function(i) iBinomDiffCI(x1=lst$x1[i], n1=lst$n1[i], x2=lst$x2[i], n2=lst$n2[i],
                                           conf.level=lst$conf.level[i],
                                           sides=lst$sides[i],
                                           method=lst$method[i])))

  # get rownames
  lgn <- Recycle(x1=if(is.null(names(x1))) paste("x1", seq_along(x1), sep=".") else names(x1),
                 n1=if(is.null(names(n1))) paste("n1", seq_along(n1), sep=".") else names(n1),
                 x2=if(is.null(names(x2))) paste("x2", seq_along(x2), sep=".") else names(x2),
                 n2=if(is.null(names(n2))) paste("n2", seq_along(n2), sep=".") else names(n2),
                 conf.level=conf.level, sides=sides, method=method)
  xn <- apply(as.data.frame(lgn[sapply(lgn, function(x) length(unique(x)) != 1)]), 1, paste, collapse=":")

  rownames(res) <- xn
  return(res)

}


BinomRatioCI_old <- function(x1, n1, x2, n2, conf.level = 0.95, method = "katz.log", bonf = FALSE,
                         tol = .Machine$double.eps^0.25, R = 1000, r = length(x1)) {
  
  # source: asbio::ci.prat by Ken Aho <kenaho1 at gmail.com>
  
  conf <- conf.level
  
  x <- x1; m <- n1; y <- x2; n <- n2
  
  indices <- c("adj.log","bailey","boot","katz.log","koopman","noether","sinh-1")
  method <- match.arg(method, indices)
  
  
  if(any(c(length(m),length(y),length(n))!= length(x))) stop("x1, n1, x2, and n2 vectors must have equal length")
  
  alpha <- 1 - conf
  oconf <- conf
  conf <- ifelse(bonf == FALSE, conf, 1 - alpha/r)
  z.star <- qnorm(1 - (1 - conf)/2)
  x2 <- qchisq(conf, 1)
  
  ci.prat1 <- function(x, m, y, n, conf = 0.95, method = "katz.log", bonf = FALSE){
    if((x > m)|(y > n)) stop("Use correct parameterization for x1, x2, n1, and n2")
    
    #-------------------------- Adj-log ------------------------------#
    
    if(method == "adj.log"){
      if((x == m & y == n)){
        rat <- (x/m)/(y/n); x <- m - 0.5; y <- n - 0.5; nrat <- ((x+0.5)/(m+0.5))/((y+0.5)/(n+0.5)); varhat <- (1/(x+0.5)) - (1/(m+0.5)) + (1/(y+0.5)) - (1/(n+0.5))
        CIL <- nrat * exp(-1 * z.star * sqrt(varhat))
        CIU <- nrat * exp(z.star * sqrt(varhat))
      } else if(x == 0 & y == 0){CIL = 0; CIU = Inf; rat = 0; varhat <- (1/(x+0.5)) - (1/(m+0.5)) + (1/(y+0.5)) - (1/(n+0.5))
      }else{
        rat <- (x/m)/(y/n); nrat <- ((x+0.5)/(m+0.5))/((y+0.5)/(n+0.5)); varhat <- (1/(x+0.5)) - (1/(m+0.5)) + (1/(y+0.5)) - (1/(n+0.5))
        CIL <- nrat * exp(-1 * z.star * sqrt(varhat))
        CIU <- nrat * exp(z.star * sqrt(varhat))}
      CI <- c(rat, CIL, CIU)
    }
    
    #-------------------------------Bailey-----------------------------#
    
    if(method == "bailey"){
      rat <- (x/m)/(y/n)
      varhat <- ifelse((x == m) & (y == n),(1/(m-0.5)) - (1/(m)) + (1/(n-0.5)) - (1/(n)),(1/(x)) - (1/(m)) + (1/(y)) - (1/(n)))
      
      p.hat1 <- x/m; p.hat2 <- y/n;
      q.hat1 <- 1 - p.hat1; q.hat2 <- 1 - p.hat2
      
      if(x == 0 | y == 0){
        xn <- ifelse(x == 0, 0.5, x)
        yn <- ifelse(y == 0, 0.5, y)
        nrat <- (xn/m)/(yn/n)
        p.hat1 <- xn/m; p.hat2 <- yn/n;
        q.hat1 <- 1 - p.hat1; q.hat2 <- 1 - p.hat2
        if(xn == m | yn == n){
          xn <- ifelse(xn == m, m - 0.5, xn)
          yn <- ifelse(yn == n, n - 0.5, yn)
          nrat <- (xn/m)/(yn/n)
          p.hat1 <- xn/m; p.hat2 <- yn/n;
          q.hat1 <- 1 - p.hat1; q.hat2 <- 1 - p.hat2
        }
      }
      
      if(x == 0 | y == 0){
        if(x == 0 & y == 0){
          rat <- Inf
          CIL <- 0
          CIU <- Inf
        }
        if(x == 0 & y != 0){
          CIL <- 0
          CIU <- nrat * ((1+ z.star * sqrt((q.hat1/xn) + (q.hat2/yn) - (z.star^2 * q.hat1 * q.hat2)/(9 * xn * yn))/3)/((1 - (z.star^2 * q.hat2)/(9 * yn))))^3
        }
        if(y == 0 & x != 0){
          CIU = Inf
          CIL <- nrat * ((1- z.star * sqrt((q.hat1/xn) + (q.hat2/yn) - (z.star^2 * q.hat1 * q.hat2)/(9 * xn * yn))/3)/((1 - (z.star^2 * q.hat2)/(9 * yn))))^3
        }
      }else if(x == m | y == n){
        xn <- ifelse(x == m, m - 0.5, x)
        yn <- ifelse(y == n, n - 0.5, y)
        nrat <- (xn/m)/(yn/n)
        p.hat1 <- xn/m; p.hat2 <- yn/n;
        q.hat1 <- 1 - p.hat1; q.hat2 <- 1 - p.hat2
        CIL <- nrat * ((1- z.star * sqrt((q.hat1/xn) + (q.hat2/yn) - (z.star^2 * q.hat1 * q.hat2)/(9 * xn * yn))/3)/((1 - (z.star^2 * q.hat2)/(9 * yn))))^3
        CIU <- nrat * ((1+ z.star * sqrt((q.hat1/xn) + (q.hat2/yn) - (z.star^2 * q.hat1 * q.hat2)/(9 * xn * yn))/3)/((1 - (z.star^2 * q.hat2)/(9 * yn))))^3
      }else{
        CIL <- rat * ((1- z.star * sqrt((q.hat1/x) + (q.hat2/y) - (z.star^2 * q.hat1 * q.hat2)/(9 * x * y))/3)/((1 - (z.star^2 * q.hat2)/(9 * y))))^3
        CIU <- rat * ((1+ z.star * sqrt((q.hat1/x) + (q.hat2/y) - (z.star^2 * q.hat1 * q.hat2)/(9 * x * y))/3)/((1 - (z.star^2 * q.hat2)/(9 * y))))^3
      }
      CI <- c(rat, CIL, CIU)
    }
    
    #-------------------------- Boot ------------------------------#
    
    if(method == "boot"){
      rat <- (x/m)/(y/n)
      if((x == 0 & y == 0)|(x == 0 & y != 0)|(x != 0 & y == 0)){
        if(x == 0 & y == 0) {CIL <- 0;  CIU <- Inf; rat = 0; varhat = NA}
        if(x == 0 & y != 0) {CIL <- 0;  rat <- (x/m)/(y/n); x <- 0.5; nrat <- (x/m)/(y/n)
        varhat <- (1/x) - (1/m) + (1/y) - (1/n)
        CIU <- nrat * exp(z.star * sqrt(varhat))}
        if(x != 0 & y == 0) {CIU <- Inf;  rat <- (x/m)/(y/n); y <- 0.5; nrat <- (x/m)/(y/n)
        varhat <- (1/x) - (1/m) + (1/y) - (1/n)
        CIL <- nrat * exp(-1 * z.star * sqrt(varhat))}
      } else{
        num.data <- c(rep(1, x), rep(0, m - x))
        den.data <- c(rep(1, y), rep(0, n - y))
        nd <- matrix(ncol = R, nrow = m)
        dd <- matrix(ncol = R, nrow = n)
        brat <- 1:R
        for(i in 1L:R){
          nd[,i] <- sample(num.data, m, replace = TRUE)
          dd[,i] <- sample(den.data, n, replace = TRUE)
          brat[i] <- (sum(nd[,i])/m)/(sum(dd[,i])/n)
        }
        alpha <- 1 - conf
        CIU <- quantile(brat, 1 - alpha/2, na.rm = TRUE)
        CIL <- quantile(brat, alpha/2, na.rm = TRUE)
        varhat <- var(brat)
      }
      CI <- c(rat, CIL, CIU)
    }
    
    #-------------------------- Katz-log ------------------------------#
    
    if(method == "katz.log"){
      if((x == 0 & y == 0)|(x == 0 & y != 0)|(x != 0 & y == 0)|(x == m & y == n)){
        if(x == 0 & y == 0) {CIL <- 0;  CIU <- Inf; rat = 0; varhat = NA}
        if(x == 0 & y != 0) {CIL <- 0;  rat <- (x/m)/(y/n); x <- 0.5; nrat <- (x/m)/(y/n)
        varhat <- (1/x) - (1/m) + (1/y) - (1/n)
        CIU <- nrat * exp(z.star * sqrt(varhat))}
        if(x != 0 & y == 0) {CIU <- Inf;  rat <- (x/m)/(y/n); y <- 0.5; nrat <- (x/m)/(y/n)
        varhat <- (1/x) - (1/m) + (1/y) - (1/n)
        CIL <- nrat * exp(-1 * z.star * sqrt(varhat))}
        if(x == m & y == n) {
          rat <- (x/m)/(y/n); x <- m - 0.5; y <- n - 0.5; nrat <- (x/m)/(y/n); varhat <- (1/x) - (1/m) + (1/y) - (1/n); CIL <- nrat * exp(-1 * z.star * sqrt(varhat))
          x <- m - 0.5; y <- n - 0.5; nrat <- (x/m)/(y/n); varhat <- (1/x) - (1/m) + (1/y) - (1/n); CIU <- nrat * exp(z.star * sqrt(varhat))
        }
      } else
      {rat <- (x/m)/(y/n); varhat <- (1/x) - (1/m) + (1/y) - (1/n)
      CIL <- rat * exp(-1 * z.star * sqrt(varhat))
      CIU <- rat * exp(z.star * sqrt(varhat))}
      CI <- c(rat, CIL, CIU)
    }
    
    #-------------------------- Koopman ------------------------------#
    
    if(method == "koopman"){
      
      if(x == 0 & y == 0) {CIL <- 0;  CIU <- Inf; rat = 0; varhat = NA
      } else {
        
        a1 = n * (n * (n + m) * x + m * (n + x) * (z.star^2))
        a2 = -n * (n * m * (y + x) + 2 * (n + m) * y *
                     x + m * (n + y + 2 * x) * (z.star^2))
        a3 = 2 * n * m * y * (y + x) + (n + m) * (y^2) *
          x + n * m * (y + x) * (z.star^2)
        a4 = -m * (y^2) * (y + x)
        b1 = a2/a1; b2 = a3/a1; b3 = a4/a1
        c1 = b2 - (b1^2)/3;  c2 = b3 - b1 * b2/3 + 2 * (b1^3)/27
        ceta = suppressWarnings(acos(sqrt(27) * c2/(2 * c1 * sqrt(-c1))))
        t1 <- suppressWarnings(-2 * sqrt(-c1/3) * cos(pi/3 - ceta/3))
        t2 <- suppressWarnings(-2 * sqrt(-c1/3) * cos(pi/3 + ceta/3))
        t3 <- suppressWarnings(2 * sqrt(-c1/3) * cos(ceta/3))
        p01 = t1 - b1/3; p02 = t2 - b1/3; p03 = t3 - b1/3
        p0sum = p01 + p02 + p03; p0up = min(p01, p02, p03); p0low = p0sum - p0up - max(p01, p02, p03)
        
        
        U <- function(a){
          p.hatf <- function(a){
            (a * (m + y) + x + n - ((a * (m + y) + x + n)^2 - 4 * a * (m + n) * (x + y))^0.5)/(2 * (m + n))
          }
          p.hat <- p.hatf(a)
          (((x - m * p.hat)^2)/(m * p.hat * (1 - p.hat)))*(1 + (m * (a - p.hat))/(n * (1 - p.hat))) - x2
        }
        
        rat <- (x/m)/(y/n); nrat <- (x/m)/(y/n); varhat <- (1/x) - (1/m) + (1/y) - (1/n)
        if((x == 0) & (y != 0)) {nrat <- ((x + 0.5)/m)/(y/n); varhat <- (1/(x + 0.5)) - (1/m) + (1/y) - (1/n)}
        if((y == 0) & (x != 0)) {nrat <- (x/m)/((y + 0.5)/n); varhat <- (1/x) - (1/m) + (1/(y + 0.5)) - (1/n)}
        if((y == n) & (x == m)) {nrat <- 1; varhat <- (1/(m - 0.5)) - (1/m) + 1/(n - 0.5) - (1/n)}
        
        La <- nrat * exp(-1 * z.star * sqrt(varhat)) * 1/4
        Lb <- nrat
        Ha <- nrat
        Hb <- nrat * exp(z.star * sqrt(varhat)) * 4
        
        #----------------------------------------------------------------------------#
        
        if((x != 0) & (y == 0)) {
          if(x == m){
            CIL = (1 - (m - x) * (1 - p0low)/(y + m - (n + m) * p0low))/p0low
            CIU <- Inf
          }
          else{
            CIL <- uniroot(U, c(La, Lb), tol=tol)$root
            CIU <- Inf
          }
        }
        
        #------------------------------------------------------------#
        
        if((x == 0) & (y != n)) {
          CIU <- uniroot(U, c(Ha, Hb), tol=tol)$root
          CIL <- 0
        }
        
        #------------------------------------------------------------#
        
        if(((x == m)|(y == n)) & (y != 0)){
          
          
          if((x == m)&(y == n)){
            U.0 <- function(a){if(a <= 1) {m * (1 - a)/a - x2}
              else{(n * (a - 1)) - x2}
            }
            CIL <- uniroot(U.0, c(La, rat), tol = tol)$root
            CIU <- uniroot(U.0, c(rat, Hb), tol = tol)$root
          }
          
          #------------------------------------------------------------#
          
          if((x == m) & (y != n)){
            
            phat1 = x/m; phat2 = y/n
            phihat = phat2/phat1
            phiu = 1.1 * phihat
            r = 0
            while (r >= -z.star) {
              a = (m + n) * phiu
              b = -((x + n) * phiu + y + m)
              c = x + y
              p1hat = (-b - sqrt(b^2 - 4 * a * c))/(2 * a)
              p2hat = p1hat * phiu
              q2hat = 1 - p2hat
              var = (m * n * p2hat)/(n * (phiu - p2hat) +
                                       m * q2hat)
              r = ((y - n * p2hat)/q2hat)/sqrt(var)
              phiu1 = phiu
              phiu = 1.0001 * phiu1
            }
            CIU = (1 - (m - x) * (1 - p0up)/(y + m - (n + m) * p0up))/p0up
            CIL = 1/phiu1
          }
          
          #------------------------------------------------------------#
          
          if((y == n) & (x != m)){
            p.hat2 = y/n; p.hat1 = x/m; phihat = p.hat1/p.hat2
            phil = 0.95 * phihat; r = 0
            if(x != 0){
              while(r <= z.star) {
                a = (n + m) * phil
                b = -((y + m) * phil + x + n)
                c = y + x
                p1hat = (-b - sqrt(b^2 - 4 * a * c))/(2 * a)
                p2hat = p1hat * phil
                q2hat = 1 - p2hat
                var = (n * m * p2hat)/(m * (phil - p2hat) +
                                         n * q2hat)
                r = ((x - m * p2hat)/q2hat)/sqrt(var)
                CIL = phil
                phil = CIL/1.0001
              }
            }
            
            phiu = 1.1 * phihat
            
            if(x == 0){CIL = 0; phiu <- ifelse(n < 100, 0.01, 0.001)}
            
            r = 0
            while(r >= -z.star) {
              a = (n + m) * phiu
              b = -((y + m) * phiu + x  + n)
              c = y + x
              p1hat = (-b - sqrt(b^2 - 4 * a * c))/(2 * a)
              p2hat = p1hat * phiu
              q2hat = 1 - p2hat
              var = (n * m * p2hat)/(m * (phiu - p2hat) +
                                       n * q2hat)
              r = ((x  - m * p2hat)/q2hat)/sqrt(var)
              phiu1 = phiu
              phiu = 1.0001 * phiu1
            }
            CIU <- phiu1
          }
        } else if((y != n) & (x != m) & (x != 0) & (y != 0)){
          CIL <- uniroot(U, c(La, Lb), tol=tol)$root
          CIU <- uniroot(U, c(Ha, Hb), tol=tol)$root
        }
      }
      CI <- c(rat, CIL, CIU)
    }
    
    #-------------------------- Noether ------------------------------#
    
    if(method == "noether"){
      if((x == 0 & y == 0)|(x == 0 & y != 0)|(x != 0 & y == 0)|(x == m & y == n)){
        if(x == 0 & y == 0) {CIL <- 0;  CIU <- Inf; rat = 0; se.hat <- NA; varhat = NA}
        if(x == 0 & y != 0) {rat <- (x/m)/(y/n); CIL <- 0;  x <- 0.5
        nrat <- (x/m)/(y/n); se.hat <- nrat * sqrt((1/x) - (1/m) + (1/y) - (1/n))
        CIU <- nrat + z.star * se.hat}
        if(x != 0 & y == 0) {rat <- Inf; CIU <- Inf;  y <- 0.5
        nrat <- (x/m)/(y/n); se.hat <- nrat * sqrt((1/x) - (1/m) + (1/y) - (1/n))
        CIL <- nrat - z.star * se.hat}
        if(x == m & y == n) {
          rat <- (x/m)/(y/n); x <- m - 0.5; y <- n - 0.5; nrat <- (x/m)/(y/n); se.hat <- nrat * sqrt((1/x) - (1/m) + (1/y) - (1/n))
          CIU <- nrat + z.star * se.hat
          CIL <- nrat - z.star * se.hat
        }
      } else
      {
        rat <- (x/m)/(y/n)
        se.hat <- rat * sqrt((1/x) - (1/m) + (1/y) - (1/n))
        CIL <- rat - z.star * se.hat
        CIU <- rat + z.star * se.hat
      }
      varhat <- ifelse(is.na(se.hat), NA, se.hat^2)
      CI <- c(rat, max(0,CIL), CIU)
    }
    
    #------------------------- sinh-1 -----------------------------#
    
    if(method == "sinh-1"){
      
      if((x == 0 & y == 0)|(x == 0 & y != 0)|(x != 0 & y == 0)|(x == m & y == n)){
        if(x == 0 & y == 0) {CIL <- 0;  CIU <- Inf; rat = 0; varhat = NA}
        if(x == 0 & y != 0) {rat <- (x/m)/(y/n); CIL <- 0;  x <- z.star
        nrat <- (x/m)/(y/n); varhat <- 2 * asinh((z.star/2)*sqrt(1/x + 1/y - 1/m - 1/n))
        CIU <- exp(log(nrat) + varhat)}
        if(x != 0 & y == 0) {rat = Inf; CIU <- Inf;  y <- z.star
        nrat <- (x/m)/(y/n); varhat <- 2 * asinh((z.star/2)*sqrt(1/x + 1/y - 1/m - 1/n))
        CIL <- exp(log(nrat) - varhat)}
        if(x == m & y == n) {
          rat <- (x/m)/(y/n); x <- m - 0.5; y <- n - 0.5; nrat <- (x/m)/(y/n); varhat <- 2 * asinh((z.star/2)*sqrt(1/x + 1/y - 1/m - 1/n))
          CIL <- exp(log(nrat) - varhat)
          CIU <- exp(log(nrat) + varhat)
        }
      } else
      {rat <- (x/m)/(y/n); varhat <- 2 * asinh((z.star/2)*sqrt(1/x + 1/y - 1/m - 1/n))
      CIL <- exp(log(rat) - varhat)
      CIU <- exp(log(rat) + varhat)
      }
      CI <- c(rat, CIL, CIU)
    }
    
    #------------------------Results ------------------------------#
    
    res <- list(CI = CI, varhat = varhat)
    res
  }
  
  CI <- matrix(ncol = 3, nrow = length(x1))
  vh <- rep(NA, length(x1))
  
  for(i in 1L : length(x1)){
    temp <- ci.prat1(x = x[i], m = m[i], y = y[i], n = n[i], conf = conf, method = method, bonf = bonf)
    CI[i,] <- temp$CI
    vh[i] <- temp$varhat
  }
  
  CI <- data.frame(CI)
  if(length(x1) == 1) row.names(CI) <- ""
  head <- paste(paste(as.character(oconf * 100),"%",sep=""), c("Confidence interval for ratio of binomial proportions"))
  if(method == "adj.log")head <- paste(head,"(method=adj-log)")
  if(method == "bailey")head <- paste(head,"(method=Bailey)")
  if(method == "boot")head <- paste(head,"(method=percentile bootstrap)")
  if(method == "katz.log")head <- paste(head,"(method=Katz-log)")
  if(method == "koopman")head <- paste(head,"(method=Koopman)")
  if(method == "noether")head <- paste(head,"(method=Noether)")
  if(method == "sinh")head <- paste(head,"(method=sinh^-1)")
  
  if(bonf == TRUE)head <- paste(head, "\n Bonferroni simultaneous intervals, r = ", bquote(.(r)),
                                "\n Marginal confidence = ", bquote(.(conf)), "\n", sep = "")
  
  ends <- c("Estimate", paste(as.character(c((1-oconf)/2, 1-((1-oconf)/2))*100), "%", sep=""))
  # res <- list(varhat = vh, ci = CI, ends = ends, head = head)
  # class(res) <- "ci"
  res <- data.matrix(CI)
  dimnames(res) <- list(NULL, c("est","lwr.ci","upr.ci"))
  
  res
}




BinomRatioCI <- function(x1, n1, x2, n2, conf.level = 0.95, sides = c("two.sided","left","right"),
                          method =c("katz.log","adj.log","bailey","koopman","noether","sinh-1","boot"),
                          tol = .Machine$double.eps^0.25, R = 1000) {
  
  # source: asbio::ci.prat by Ken Aho <kenaho1 at gmail.com>
  

  iBinomRatioCI <- function(x, m, y, n, conf, sides, method) {
    
    if((x > m)|(y > n)) stop("Use correct parameterization for x1, x2, n1, and n2")

    method <- match.arg(method, c("katz.log","adj.log","bailey","koopman","noether","sinh-1","boot"))
    
    if(sides!="two.sided")
      conf <- 1 - 2*(1-conf)
    
    alpha <- 1 - conf

    z.star <- qnorm(1 - (1 - conf)/2)
    x2 <- qchisq(conf, 1)
    
    #-------------------------- Adj-log ------------------------------#
    
    if(method == "adj.log"){
      if((x == m & y == n)){
        rat <- (x/m)/(y/n) 
        x <- m - 0.5
        y <- n - 0.5
        nrat <- ((x+0.5)/(m+0.5))/((y+0.5)/(n+0.5))
        varhat <- (1/(x+0.5)) - (1/(m+0.5)) + (1/(y+0.5)) - (1/(n+0.5))
        
        CIL <- nrat * exp(-1 * z.star * sqrt(varhat))
        CIU <- nrat * exp(z.star * sqrt(varhat))
        
      } else if(x == 0 & y == 0){
        
        CIL = 0 
        CIU = Inf 
        rat = 0 
        varhat <- (1/(x+0.5)) - (1/(m+0.5)) + (1/(y+0.5)) - (1/(n+0.5))
        
      } else {
        
        rat <- (x/m)/(y/n) 
        nrat <- ((x+0.5)/(m+0.5))/((y+0.5)/(n+0.5))
        varhat <- (1/(x+0.5)) - (1/(m+0.5)) + (1/(y+0.5)) - (1/(n+0.5))
        CIL <- nrat * exp(-1 * z.star * sqrt(varhat))
        CIU <- nrat * exp(z.star * sqrt(varhat))
      }
      
      CI <- c(rat, CIL, CIU)
      
    }
    
    #-------------------------------Bailey-----------------------------#
    
    if(method == "bailey"){
      rat <- (x/m)/(y/n)
      varhat <- ifelse((x == m) & (y == n),(1/(m-0.5)) - (1/(m)) + (1/(n-0.5)) - (1/(n)),(1/(x)) - (1/(m)) + (1/(y)) - (1/(n)))
      
      p.hat1 <- x/m; p.hat2 <- y/n;
      q.hat1 <- 1 - p.hat1; q.hat2 <- 1 - p.hat2
      
      if(x == 0 | y == 0){
        xn <- ifelse(x == 0, 0.5, x)
        yn <- ifelse(y == 0, 0.5, y)
        nrat <- (xn/m)/(yn/n)
        p.hat1 <- xn/m; p.hat2 <- yn/n;
        q.hat1 <- 1 - p.hat1; q.hat2 <- 1 - p.hat2
        if(xn == m | yn == n){
          xn <- ifelse(xn == m, m - 0.5, xn)
          yn <- ifelse(yn == n, n - 0.5, yn)
          nrat <- (xn/m)/(yn/n)
          p.hat1 <- xn/m; p.hat2 <- yn/n;
          q.hat1 <- 1 - p.hat1; q.hat2 <- 1 - p.hat2
        }
      }
      
      if(x == 0 | y == 0){
        if(x == 0 & y == 0){
          rat <- Inf
          CIL <- 0
          CIU <- Inf
        }
        if(x == 0 & y != 0){
          CIL <- 0
          CIU <- nrat * ((1+ z.star * sqrt((q.hat1/xn) + (q.hat2/yn) - (z.star^2 * q.hat1 * q.hat2)/(9 * xn * yn))/3)/((1 - (z.star^2 * q.hat2)/(9 * yn))))^3
        }
        if(y == 0 & x != 0){
          CIU = Inf
          CIL <- nrat * ((1- z.star * sqrt((q.hat1/xn) + (q.hat2/yn) - (z.star^2 * q.hat1 * q.hat2)/(9 * xn * yn))/3)/((1 - (z.star^2 * q.hat2)/(9 * yn))))^3
        }
      }else if(x == m | y == n){
        xn <- ifelse(x == m, m - 0.5, x)
        yn <- ifelse(y == n, n - 0.5, y)
        nrat <- (xn/m)/(yn/n)
        p.hat1 <- xn/m; p.hat2 <- yn/n;
        q.hat1 <- 1 - p.hat1; q.hat2 <- 1 - p.hat2
        CIL <- nrat * ((1- z.star * sqrt((q.hat1/xn) + (q.hat2/yn) - (z.star^2 * q.hat1 * q.hat2)/(9 * xn * yn))/3)/((1 - (z.star^2 * q.hat2)/(9 * yn))))^3
        CIU <- nrat * ((1+ z.star * sqrt((q.hat1/xn) + (q.hat2/yn) - (z.star^2 * q.hat1 * q.hat2)/(9 * xn * yn))/3)/((1 - (z.star^2 * q.hat2)/(9 * yn))))^3
      }else{
        CIL <- rat * ((1- z.star * sqrt((q.hat1/x) + (q.hat2/y) - (z.star^2 * q.hat1 * q.hat2)/(9 * x * y))/3)/((1 - (z.star^2 * q.hat2)/(9 * y))))^3
        CIU <- rat * ((1+ z.star * sqrt((q.hat1/x) + (q.hat2/y) - (z.star^2 * q.hat1 * q.hat2)/(9 * x * y))/3)/((1 - (z.star^2 * q.hat2)/(9 * y))))^3
      }
      CI <- c(rat, CIL, CIU)
    }
    
    #-------------------------- Boot ------------------------------#
    
    if(method == "boot"){
      rat <- (x/m)/(y/n)
      if((x == 0 & y == 0)|(x == 0 & y != 0)|(x != 0 & y == 0)){
        if(x == 0 & y == 0) {CIL <- 0;  CIU <- Inf; rat = 0; varhat = NA}
        if(x == 0 & y != 0) {CIL <- 0;  rat <- (x/m)/(y/n); x <- 0.5; nrat <- (x/m)/(y/n)
        varhat <- (1/x) - (1/m) + (1/y) - (1/n)
        CIU <- nrat * exp(z.star * sqrt(varhat))}
        if(x != 0 & y == 0) {CIU <- Inf;  rat <- (x/m)/(y/n); y <- 0.5; nrat <- (x/m)/(y/n)
        varhat <- (1/x) - (1/m) + (1/y) - (1/n)
        CIL <- nrat * exp(-1 * z.star * sqrt(varhat))}
      } else{
        num.data <- c(rep(1, x), rep(0, m - x))
        den.data <- c(rep(1, y), rep(0, n - y))
        nd <- matrix(ncol = R, nrow = m)
        dd <- matrix(ncol = R, nrow = n)
        brat <- 1:R
        for(i in 1L:R){
          nd[,i] <- sample(num.data, m, replace = TRUE)
          dd[,i] <- sample(den.data, n, replace = TRUE)
          brat[i] <- (sum(nd[,i])/m)/(sum(dd[,i])/n)
        }
        alpha <- 1 - conf
        CIU <- quantile(brat, 1 - alpha/2, na.rm = TRUE)
        CIL <- quantile(brat, alpha/2, na.rm = TRUE)
        varhat <- var(brat)
      }
      CI <- c(rat, CIL, CIU)
    }
    
    #-------------------------- Katz-log ------------------------------#
    
    if(method == "katz.log"){
      if((x == 0 & y == 0)|(x == 0 & y != 0)|(x != 0 & y == 0)|(x == m & y == n)){
        if(x == 0 & y == 0) {CIL <- 0;  CIU <- Inf; rat = 0; varhat = NA}
        if(x == 0 & y != 0) {CIL <- 0;  rat <- (x/m)/(y/n); x <- 0.5; nrat <- (x/m)/(y/n)
        varhat <- (1/x) - (1/m) + (1/y) - (1/n)
        CIU <- nrat * exp(z.star * sqrt(varhat))}
        if(x != 0 & y == 0) {CIU <- Inf;  rat <- (x/m)/(y/n); y <- 0.5; nrat <- (x/m)/(y/n)
        varhat <- (1/x) - (1/m) + (1/y) - (1/n)
        CIL <- nrat * exp(-1 * z.star * sqrt(varhat))}
        if(x == m & y == n) {
          rat <- (x/m)/(y/n); x <- m - 0.5; y <- n - 0.5; nrat <- (x/m)/(y/n); varhat <- (1/x) - (1/m) + (1/y) - (1/n); CIL <- nrat * exp(-1 * z.star * sqrt(varhat))
          x <- m - 0.5; y <- n - 0.5; nrat <- (x/m)/(y/n); varhat <- (1/x) - (1/m) + (1/y) - (1/n); CIU <- nrat * exp(z.star * sqrt(varhat))
        }
      } else
      {rat <- (x/m)/(y/n); varhat <- (1/x) - (1/m) + (1/y) - (1/n)
      CIL <- rat * exp(-1 * z.star * sqrt(varhat))
      CIU <- rat * exp(z.star * sqrt(varhat))}
      CI <- c(rat, CIL, CIU)
    }
    
    #-------------------------- Koopman ------------------------------#
    
    if(method == "koopman"){
      
      if(x == 0 & y == 0) {CIL <- 0;  CIU <- Inf; rat = 0; varhat = NA
      } else {
        
        a1 = n * (n * (n + m) * x + m * (n + x) * (z.star^2))
        a2 = -n * (n * m * (y + x) + 2 * (n + m) * y *
                     x + m * (n + y + 2 * x) * (z.star^2))
        a3 = 2 * n * m * y * (y + x) + (n + m) * (y^2) *
          x + n * m * (y + x) * (z.star^2)
        a4 = -m * (y^2) * (y + x)
        b1 = a2/a1; b2 = a3/a1; b3 = a4/a1
        c1 = b2 - (b1^2)/3;  c2 = b3 - b1 * b2/3 + 2 * (b1^3)/27
        ceta = suppressWarnings(acos(sqrt(27) * c2/(2 * c1 * sqrt(-c1))))
        t1 <- suppressWarnings(-2 * sqrt(-c1/3) * cos(pi/3 - ceta/3))
        t2 <- suppressWarnings(-2 * sqrt(-c1/3) * cos(pi/3 + ceta/3))
        t3 <- suppressWarnings(2 * sqrt(-c1/3) * cos(ceta/3))
        p01 = t1 - b1/3; p02 = t2 - b1/3; p03 = t3 - b1/3
        p0sum = p01 + p02 + p03; p0up = min(p01, p02, p03); p0low = p0sum - p0up - max(p01, p02, p03)
        
        
        U <- function(a){
          p.hatf <- function(a){
            (a * (m + y) + x + n - ((a * (m + y) + x + n)^2 - 4 * a * (m + n) * (x + y))^0.5)/(2 * (m + n))
          }
          p.hat <- p.hatf(a)
          (((x - m * p.hat)^2)/(m * p.hat * (1 - p.hat)))*(1 + (m * (a - p.hat))/(n * (1 - p.hat))) - x2
        }
        
        rat <- (x/m)/(y/n); nrat <- (x/m)/(y/n); varhat <- (1/x) - (1/m) + (1/y) - (1/n)
        if((x == 0) & (y != 0)) {nrat <- ((x + 0.5)/m)/(y/n); varhat <- (1/(x + 0.5)) - (1/m) + (1/y) - (1/n)}
        if((y == 0) & (x != 0)) {nrat <- (x/m)/((y + 0.5)/n); varhat <- (1/x) - (1/m) + (1/(y + 0.5)) - (1/n)}
        if((y == n) & (x == m)) {nrat <- 1; varhat <- (1/(m - 0.5)) - (1/m) + 1/(n - 0.5) - (1/n)}
        
        La <- nrat * exp(-1 * z.star * sqrt(varhat)) * 1/4
        Lb <- nrat
        Ha <- nrat
        Hb <- nrat * exp(z.star * sqrt(varhat)) * 4
        
        #----------------------------------------------------------------------------#
        
        if((x != 0) & (y == 0)) {
          if(x == m){
            CIL = (1 - (m - x) * (1 - p0low)/(y + m - (n + m) * p0low))/p0low
            CIU <- Inf
          }
          else{
            CIL <- uniroot(U, c(La, Lb), tol=tol)$root
            CIU <- Inf
          }
        }
        
        #------------------------------------------------------------#
        
        if((x == 0) & (y != n)) {
          CIU <- uniroot(U, c(Ha, Hb), tol=tol)$root
          CIL <- 0
        }
        
        #------------------------------------------------------------#
        
        if(((x == m)|(y == n)) & (y != 0)){
          
          
          if((x == m)&(y == n)){
            U.0 <- function(a){if(a <= 1) {m * (1 - a)/a - x2}
              else{(n * (a - 1)) - x2}
            }
            CIL <- uniroot(U.0, c(La, rat), tol = tol)$root
            CIU <- uniroot(U.0, c(rat, Hb), tol = tol)$root
          }
          
          #------------------------------------------------------------#
          
          if((x == m) & (y != n)){
            
            phat1 = x/m; phat2 = y/n
            phihat = phat2/phat1
            phiu = 1.1 * phihat
            r = 0
            while (r >= -z.star) {
              a = (m + n) * phiu
              b = -((x + n) * phiu + y + m)
              c = x + y
              p1hat = (-b - sqrt(b^2 - 4 * a * c))/(2 * a)
              p2hat = p1hat * phiu
              q2hat = 1 - p2hat
              var = (m * n * p2hat)/(n * (phiu - p2hat) +
                                       m * q2hat)
              r = ((y - n * p2hat)/q2hat)/sqrt(var)
              phiu1 = phiu
              phiu = 1.0001 * phiu1
            }
            CIU = (1 - (m - x) * (1 - p0up)/(y + m - (n + m) * p0up))/p0up
            CIL = 1/phiu1
          }
          
          #------------------------------------------------------------#
          
          if((y == n) & (x != m)){
            p.hat2 = y/n; p.hat1 = x/m; phihat = p.hat1/p.hat2
            phil = 0.95 * phihat; r = 0
            if(x != 0){
              while(r <= z.star) {
                a = (n + m) * phil
                b = -((y + m) * phil + x + n)
                c = y + x
                p1hat = (-b - sqrt(b^2 - 4 * a * c))/(2 * a)
                p2hat = p1hat * phil
                q2hat = 1 - p2hat
                var = (n * m * p2hat)/(m * (phil - p2hat) +
                                         n * q2hat)
                r = ((x - m * p2hat)/q2hat)/sqrt(var)
                CIL = phil
                phil = CIL/1.0001
              }
            }
            
            phiu = 1.1 * phihat
            
            if(x == 0){CIL = 0; phiu <- ifelse(n < 100, 0.01, 0.001)}
            
            r = 0
            while(r >= -z.star) {
              a = (n + m) * phiu
              b = -((y + m) * phiu + x  + n)
              c = y + x
              p1hat = (-b - sqrt(b^2 - 4 * a * c))/(2 * a)
              p2hat = p1hat * phiu
              q2hat = 1 - p2hat
              var = (n * m * p2hat)/(m * (phiu - p2hat) +
                                       n * q2hat)
              r = ((x  - m * p2hat)/q2hat)/sqrt(var)
              phiu1 = phiu
              phiu = 1.0001 * phiu1
            }
            CIU <- phiu1
          }
        } else if((y != n) & (x != m) & (x != 0) & (y != 0)){
          CIL <- uniroot(U, c(La, Lb), tol=tol)$root
          CIU <- uniroot(U, c(Ha, Hb), tol=tol)$root
        }
      }
      CI <- c(rat, CIL, CIU)
    }
    
    #-------------------------- Noether ------------------------------#
    
    if(method == "noether"){
      if((x == 0 & y == 0)|(x == 0 & y != 0)|(x != 0 & y == 0)|(x == m & y == n)){
        if(x == 0 & y == 0) {CIL <- 0;  CIU <- Inf; rat = 0; se.hat <- NA; varhat = NA}
        if(x == 0 & y != 0) {rat <- (x/m)/(y/n); CIL <- 0;  x <- 0.5
        nrat <- (x/m)/(y/n); se.hat <- nrat * sqrt((1/x) - (1/m) + (1/y) - (1/n))
        CIU <- nrat + z.star * se.hat}
        if(x != 0 & y == 0) {rat <- Inf; CIU <- Inf;  y <- 0.5
        nrat <- (x/m)/(y/n); se.hat <- nrat * sqrt((1/x) - (1/m) + (1/y) - (1/n))
        CIL <- nrat - z.star * se.hat}
        if(x == m & y == n) {
          rat <- (x/m)/(y/n); x <- m - 0.5; y <- n - 0.5; nrat <- (x/m)/(y/n); se.hat <- nrat * sqrt((1/x) - (1/m) + (1/y) - (1/n))
          CIU <- nrat + z.star * se.hat
          CIL <- nrat - z.star * se.hat
        }
      } else
      {
        rat <- (x/m)/(y/n)
        se.hat <- rat * sqrt((1/x) - (1/m) + (1/y) - (1/n))
        CIL <- rat - z.star * se.hat
        CIU <- rat + z.star * se.hat
      }
      varhat <- ifelse(is.na(se.hat), NA, se.hat^2)
      CI <- c(rat, max(0,CIL), CIU)
    }
    
    #------------------------- sinh-1 -----------------------------#
    
    if(method == "sinh-1"){
      
      if((x == 0 & y == 0)|(x == 0 & y != 0)|(x != 0 & y == 0)|(x == m & y == n)){
        if(x == 0 & y == 0) {CIL <- 0;  CIU <- Inf; rat = 0; varhat = NA}
        if(x == 0 & y != 0) {rat <- (x/m)/(y/n); CIL <- 0;  x <- z.star
        nrat <- (x/m)/(y/n); varhat <- 2 * asinh((z.star/2)*sqrt(1/x + 1/y - 1/m - 1/n))
        CIU <- exp(log(nrat) + varhat)}
        if(x != 0 & y == 0) {rat = Inf; CIU <- Inf;  y <- z.star
        nrat <- (x/m)/(y/n); varhat <- 2 * asinh((z.star/2)*sqrt(1/x + 1/y - 1/m - 1/n))
        CIL <- exp(log(nrat) - varhat)}
        if(x == m & y == n) {
          rat <- (x/m)/(y/n); x <- m - 0.5; y <- n - 0.5; nrat <- (x/m)/(y/n); varhat <- 2 * asinh((z.star/2)*sqrt(1/x + 1/y - 1/m - 1/n))
          CIL <- exp(log(nrat) - varhat)
          CIU <- exp(log(nrat) + varhat)
        }
      } else
      {rat <- (x/m)/(y/n); varhat <- 2 * asinh((z.star/2)*sqrt(1/x + 1/y - 1/m - 1/n))
      CIL <- exp(log(rat) - varhat)
      CIU <- exp(log(rat) + varhat)
      }
      CI <- c(rat, CIL, CIU)
    }
    
    #------------------------Results ------------------------------#
    
    # res <- list(CI = CI, varhat = varhat)
    CI
    
  }
  
  
  
  if(missing(sides))    sides <- match.arg(sides)
  if(missing(method))   method <- match.arg(method)

  # Recycle arguments
  lst <- Recycle(x1=x1, n1=n1, x2=x2, n2=n2, conf.level=conf.level, sides=sides, method=method)
  
  # iBinomRatioCI <- function(x, m, y, n, conf, method){
    
  res <- t(sapply(1:attr(lst, "maxdim"),
                  function(i) iBinomRatioCI(x=lst$x1[i], m=lst$n1[i], y=lst$x2[i], n=lst$n2[i],
                                            conf=lst$conf.level[i],
                                            sides=lst$sides[i],
                                            method=lst$method[i])))
  
  # get rownames
  lgn <- Recycle(x1=if(is.null(names(x1))) paste("x1", seq_along(x1), sep=".") else names(x1),
                 n1=if(is.null(names(n1))) paste("n1", seq_along(n1), sep=".") else names(n1),
                 x2=if(is.null(names(x2))) paste("x2", seq_along(x2), sep=".") else names(x2),
                 n2=if(is.null(names(n2))) paste("n2", seq_along(n2), sep=".") else names(n2),
                 conf.level=conf.level, sides=sides, method=method)
  
  xn <- apply(as.data.frame(lgn[sapply(lgn, function(x) length(unique(x)) != 1)]), 1, paste, collapse=":")
  
  return(SetNames(res, 
                  rownames=xn, 
                  colnames=c("est", "lwr.ci", "upr.ci")))
  
}





MultinomCI <- function(x, conf.level = 0.95, sides = c("two.sided","left","right"),
                       method = c("sisonglaz", "cplus1", "goodman", "wald", "waldcc", "wilson")) {

  # Code mainly by:
  # Pablo J. Villacorta Iglesias <pjvi@decsai.ugr.es>\n
  # Department of Computer Science and Artificial Intelligence, University of Granada (Spain)

  .moments <- function(c, lambda){

    a <- lambda + c
    b <- lambda - c
    if(b < 0) b <- 0
    if(b > 0) den <- ppois(a, lambda) - ppois(b-1, lambda)
    if(b == 0) den <- ppois(a,lambda)

    mu <- mat.or.vec(4,1)
    mom <- mat.or.vec(5,1)
    for(r in 1:4){
      poisA <- 0
      poisB <- 0

      if((a-r) >=0){ poisA <- ppois(a,lambda)-ppois(a-r,lambda) }
      if((a-r) < 0){ poisA <- ppois(a,lambda) }
      if((b-r-1) >=0){ poisB <- ppois(b-1,lambda)-ppois(b-r-1,lambda) }
      if((b-r-1) < 0 && (b-1)>=0){ poisB <- ppois(b-1,lambda) }
      if((b-r-1) < 0 && (b-1) < 0){ poisB <- 0 }

      mu[r] <- (lambda^r)*(1-(poisA-poisB)/den)
    }
    mom[1] <- mu[1]
    mom[2] <- mu[2] + mu[1] - mu[1]^2
    mom[3] <- mu[3] + mu[2]*(3-3*mu[1]) + (mu[1]-3*mu[1]^2+2*mu[1]^3)
    mom[4] <- mu[4] + mu[3]*(6-4*mu[1]) + mu[2]*(7-12*mu[1]+6*mu[1]^2)+mu[1]-4*mu[1]^2+6*mu[1]^3-3*mu[1]^4
    mom[5] <- den

    return(mom)

  }

  .truncpoi <- function(c, x, n, k){

    m <- matrix(0, k, 5)

    for(i in 1L:k){
      lambda <- x[i]
      mom <- .moments(c, lambda)
      for(j in 1L:5L){ m[i,j] <- mom[j] }
    }
    for(i in 1L:k){ m[i, 4] <- m[i, 4] - 3 * m[i, 2]^2 }

    s <- colSums(m)
    s1 <- s[1]
    s2 <- s[2]
    s3 <- s[3]
    s4 <- s[4]

    probn <- 1/(ppois(n,n)-ppois(n-1,n))
    z <- (n-s1)/sqrt(s2)
    g1 <- s3/(s2^(3/2))
    g2 <- s4/(s2^2)
    poly <- 1 + g1*(z^3-3*z)/6 + g2*(z^4-6*z^2+3)/24
    + g1^2*(z^6-15*z^4 + 45*z^2-15)/72
    f <- poly*exp(-z^2/2)/(sqrt(2)*gamma(0.5))

    probx <- 1
    for(i in 1L:k){ probx <- probx * m[i,5]  }

    return(probn * probx * f / sqrt(s2))
  }


  n <- sum(x, na.rm=TRUE)
  k <- length(x)
  p <- x/n

  if (missing(method)) method <- "sisonglaz"
  if(missing(sides)) sides <- "two.sided"

  sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
  if(sides!="two.sided")
    conf.level <- 1 - 2*(1-conf.level)


  method <- match.arg(arg = method, choices = c("sisonglaz", "cplus1", "goodman", "wald", "waldcc", "wilson"))
  if(method == "goodman") {

    q.chi <- qchisq(conf.level, k - 1)
    lci <- (q.chi + 2*x - sqrt(q.chi*(q.chi + 4*x*(n-x)/n))) / (2*(n+q.chi))
    uci <- (q.chi + 2*x + sqrt(q.chi*(q.chi + 4*x*(n-x)/n))) / (2*(n+q.chi))

    res <- cbind(est=p, lwr.ci=pmax(0, lci), upr.ci=pmin(1, uci))

  } else if(method == "wald") {

    q.chi <- qchisq(conf.level, 1)
    lci <- p - sqrt(q.chi * p * (1 - p)/n)
    uci <- p + sqrt(q.chi * p * (1 - p)/n)

    res <- cbind(est=p, lwr.ci=pmax(0, lci), upr.ci=pmin(1, uci))

  } else if(method == "waldcc") {

    q.chi <- qchisq(conf.level, 1)
    lci <- p - sqrt(q.chi * p * (1 - p)/n) - 1/(2*n)
    uci <- p + sqrt(q.chi * p * (1 - p)/n) + 1/(2*n)

    res <- cbind(est=p, lwr.ci=pmax(0, lci), upr.ci=pmin(1, uci))

  } else if(method == "wilson") {

    q.chi <- qchisq(conf.level, 1)
    lci <- (q.chi + 2*x - sqrt(q.chi^2 + 4*x*q.chi * (1 - p))) / (2*(q.chi + n))
    uci <- (q.chi + 2*x + sqrt(q.chi^2 + 4*x*q.chi * (1 - p))) / (2*(q.chi + n))

    res <- cbind(est=p, lwr.ci=pmax(0, lci), upr.ci=pmin(1, uci))

  } else {  # sisonglaz, cplus1

    const <- 0
    pold <- 0

    for(cc in 1:n){
      poi <- .truncpoi(cc, x, n, k)
      if(poi > conf.level && pold < conf.level) {
        const <- cc
        break
      }
      pold <- poi
    }

    delta <- (conf.level - pold)/(poi - pold)
    const <- const - 1

    if(method == "sisonglaz") {
      res <- cbind(est = p, lwr.ci = pmax(0, p - const/n), upr.ci = pmin(1, p + const/n + 2*delta/n))

    } else if(method == "cplus1") {
      res <- cbind(est = p, lwr.ci = pmax(0, p - const/n - 1/n), upr.ci = pmin(1,p + const/n + 1/n))
    }
  }

  if(sides=="left")
    res[3] <- 1
  else if(sides=="right")
    res[2] <- 0

  return(res)
}



# Confidence Intervals for Poisson mean

PoissonCI <- function(x, n = 1, conf.level = 0.95, sides = c("two.sided","left","right"),
                      method = c("exact","score", "wald","byar")) {

  if(missing(method)) method <- "exact"
  if(missing(sides)) sides <- "two.sided"


  iPoissonCI <- function(x, n = 1, conf.level = 0.95, sides = c("two.sided","left","right"),
                         method = c("exact","score", "wald","byar")) {

    # ref:  http://www.ijmo.org/papers/189-S083.pdf but wacklig!!!
    # http://www.math.montana.edu/~rjboik/classes/502/ci.pdf
    # http://www.ine.pt/revstat/pdf/rs120203.pdf
    # http://www.pvamu.edu/include/Math/AAM/AAM%20Vol%206,%20Issue%201%20(June%202011)/06_%20Kibria_AAM_R308_BK_090110_Vol_6_Issue_1.pdf

    # see also:   pois.conf.int {epitools}

    sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
    if(sides!="two.sided")
      conf.level <- 1 - 2*(1-conf.level)


    if(missing(method)) method <- "score"

    if(length(conf.level) != 1)  stop("'conf.level' has to be of length 1 (confidence level)")
    if(conf.level < 0.5 | conf.level > 1)  stop("'conf.level' has to be in [0.5, 1]")

    alpha <- 1 - conf.level
    z <- qnorm(1-alpha/2)

    lambda <- x/n

    switch( match.arg(arg=method, choices=c("exact","score", "wald","byar"))
            , "exact" = {
              ci <- poisson.test(x, n, conf.level = conf.level)$conf.int
              lwr.ci <- ci[1]
              upr.ci <- ci[2]
            }
            , "score" = {
              term1 <- (x + z^2/2)/n
              term2 <- z * n^-0.5 * sqrt(x/n + z^2/(4*n))
              lwr.ci <- term1 - term2
              upr.ci <- term1 + term2
            }
            , "wald" = {
              term2 <- z*sqrt(lambda/n)
              lwr.ci <- lambda - term2
              upr.ci <- lambda + term2
            }
            , "byar" = {
              xcc <- x + 0.5
              zz  <- (z/3) * sqrt(1/xcc)
              lwr.ci <- (xcc * (1 - 1/(9 * xcc) - zz)^3)/n
              upr.ci <- (xcc * (1 - 1/(9 * xcc) + zz)^3)/n
            }

            # agresti-coull is the same as score
            #             , "agresti-coull" = {
            #               lwr.ci <- lambda + z^2/(2*n) - z*sqrt(lambda/n + z^2/(4*n^2))
            #               upr.ci <- lambda + z^2/(2*n) + z*sqrt(lambda/n + z^2/(4*n^2))
            #
            #             }
            # garwood is the same as exact, check that!!
            #             , "garwood" = {
            #               lwr.ci <- qchisq((1 - conf.level)/2, 2*x)/(2*n)
            #               upr.ci <- qchisq(1 - (1 - conf.level)/2, 2*(x + 1))/(2*n)
            #             }
    )

    ci <- c( est=lambda, lwr.ci=lwr.ci, upr.ci=upr.ci )

    if(sides=="left")
      ci[3] <- Inf
    else if(sides=="right")
      ci[2] <- -Inf

    return(ci)
  }

  # handle vectors
  # which parameter has the highest dimension
  lst <- list(x=x, n=n, conf.level=conf.level, sides=sides, method=method)
  maxdim <- max(unlist(lapply(lst, length)))
  # recycle all params to maxdim
  lgp <- lapply( lst, rep, length.out=maxdim )

  res <- sapply(1:maxdim, function(i) iPoissonCI(x=lgp$x[i], n=lgp$n[i],
                                                 conf.level=lgp$conf.level[i],
                                                 sides=lgp$sides[i],
                                                 method=lgp$method[i]))
  rownames(res)[1] <- c("est")

  lgn <- Recycle(x=if(is.null(names(x))) paste("x", seq_along(x), sep=".") else names(x),
                 n=if(is.null(names(n))) paste("n", seq_along(n), sep=".") else names(n),
                 conf.level=conf.level, sides=sides, method=method)
  xn <- apply(as.data.frame(lgn[sapply(lgn, function(x) length(unique(x)) != 1)]), 1, paste, collapse=":")

  colnames(res) <- xn

  return(t(res))

}




# Konfidenzintervall fuer den Median

MedianCI <- function(x, conf.level=0.95, sides = c("two.sided","left","right"), na.rm=FALSE, method=c("exact","boot"), R=999) {
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
  
  
  
  sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
  if(sides!="two.sided")
    conf.level <- 1 - 2*(1-conf.level)

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
            boot.med <- boot(x, function(x, d) median(x[d], na.rm=na.rm), R=R)
            r <- boot.ci(boot.med, conf=conf.level, type="basic")[[4]][4:5]
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





QuantileCI <- function(x, probs=seq(0, 1, .25), conf.level = 0.95, sides = c("two.sided", "left", "right"),
           na.rm = FALSE, method = c("exact", "boot"), R = 999) {
  
  .QuantileCI <- function(x, probs, conf.level = 0.95, sides = c("two.sided", "left", "right")) {
    # Near-symmetric distribution-free confidence interval for a quantile `q`.
  
    # https://stats.stackexchange.com/questions/99829/how-to-obtain-a-confidence-interval-for-a-percentile
    #
    # Search over a small range of upper and lower order statistics for the 
    # closest coverage to 1-alpha (but not less than it, if possible).
    
    alpha <- 1- conf.level
    
    n <- length(x)
    
    u <- qbinom(1-alpha/2, n, probs) + (-2:2) + 1
    l <- qbinom(alpha/2, n, probs) + (-2:2)
    u[u > n] <- Inf
    l[l < 0] <- -Inf
    coverage <- outer(l, u, function(a, b) pbinom(b-1, n, probs) - pbinom(a-1, n, probs))
    if (max(coverage) < 1-alpha) i <- which(coverage==max(coverage)) else
      i <- which(coverage == min(coverage[coverage >= 1-alpha]))
    # minimal difference
    i <- i[1]
    
    # order statistics and the actual coverage
    u <- rep(u, each=5)[i]
    l <- rep(l, 5)[i]
    
    # get the values  
    if(probs %nin% c(0,1))
      s <- sort(x, partial=c(u, l))
    else
      s <- sort(x)
    
    res <- c(lwr.ci=s[l], upr.ci=s[u])
    attr(res, "conf.level") <- coverage[i]
    
    if(sides=="left")
      res[3] <- Inf
    else if(sides=="right")
      res[2] <- -Inf
    
    return(res)
    
  }
  
  
  if (na.rm) x <- na.omit(x)
  if(anyNA(x))
    stop("missing values and NaN's not allowed if 'na.rm' is FALSE")
  
  sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
  if(sides!="two.sided")
    conf.level <- 1 - 2*(1-conf.level)

  method <- match.arg(arg=method, choices=c("exact","boot"))
  
  switch( method
          , "exact" = { # this is the SAS-way to do it
            r <- lapply(probs, function(p) .QuantileCI(x, probs=p, conf.level = conf.level, sides=sides))
            coverage <- sapply(r, function(z) attr(z, "conf.level"))
            r <- do.call(rbind, r)
            attr(r, "conf.level") <- coverage
          }
          , "boot" = {
            boot.med <- boot(x, function(x, d) quantile(x[d], probs=probs, na.rm=na.rm), R=R)
            r <- boot.ci(boot.med, conf=conf.level, type="basic")[[4]][4:5]
          } )
  
  qq <- quantile(x, probs=probs, na.rm=na.rm)
  
  if(length(probs)==1){
    res <- c(qq, r)
    names(res) <- c("est","lwr.ci","upr.ci")
    # report the conf.level which can deviate from the required one
    if(method=="exact")  attr(res, "conf.level") <-  attr(r, "conf.level")
    
  } else {
    res <- cbind(qq, r)
    colnames(res) <- c("est","lwr.ci","upr.ci")
    # report the conf.level which can deviate from the required one
    if(method=="exact")  
      # report coverages for all probs
      attr(res, "conf.level") <-  attr(r, "conf.level")
    
  }
  
  return( res )
  
}




# standard error of mean
MeanSE <- function(x, sd = NULL, na.rm = FALSE) {
  if(na.rm) x <- na.omit(x)
  if(is.null(sd)) s <- sd(x)
  s/sqrt(length(x))
}



MeanCI <- function (x, sd = NULL, trim = 0, method = c("classic", "boot"),
                    conf.level = 0.95, sides = c("two.sided","left","right"), na.rm = FALSE, ...) {

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
    winvar <- var(Winsorize(x, minval = minval, maxval = maxval))

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
      names(res) <- c("mean", "lwr.ci", "upr.ci")

    } else {
      if(is.null(sd)) {
        a <- qt(p = (1 - conf.level)/2, df = length(x) - 1) * sd(x)/sqrt(length(x))
      } else {
        a <- qnorm(p = (1 - conf.level)/2) * sd/sqrt(length(x))
      }
      res <- c(mean = mean(x), lwr.ci = mean(x) + a, upr.ci = mean(x) - a)
    }

  } else {

    # see: http://www.psychology.mcmaster.ca/bennett/boot09/percentileT.pdf
    # this might contain an erroneuous calculation of boot variance...

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

  return(res)
}



MeanCIn <- function(ci, sd, interval=c(2, 1e5), conf.level=0.95, norm=FALSE, 
                    tol = .Machine$double.eps^0.5) {
  
  width <- diff(ci)/2
  alpha <- (1-conf.level)/2
  
  if(norm)
    uniroot(f = function(n) sd/sqrt(n) * qnorm(p = 1-alpha) - width, 
            interval = interval, tol = tol)$root
  else
    uniroot(f = function(n) (qt(1-alpha, df=n-1) * sd / sqrt(n)) - width, 
            interval = interval, tol = tol)$root
}




# MeanCIn <- function(xm, sd, pop_sd=NULL, width, interval=c(1, 1e5), conf.level=0.95, sides="two.sided") {
# 
#   sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
#   if(sides!="two.sided")
#     conf.level <- 1 - 2*(1-conf.level)
# 
#   if(!is.null(sd))  
#     mci <- function(n) qt(c((1-conf.level)/2, 1-, df = n-1) *sd / sqrt(n)
#   
#   else if(!is.null(pop_sd))
#     mci <- function(n) qnorm((1-conf.level)/2) *sd / sqrt(n)
#   
#   else{
#     warning("Provide either sd or pop_sd")
#     return(NA)
#   }
#   
#   uniroot(f = function(n) diff(mci(n)) - width, 
#           interval = interval)$root
#   
#   if(sides=="left")
#     res[3] <- Inf
#   else if(sides=="right")
#     res[2] <- -Inf
#   
#   
# }



# CIn <- function(p=0.5, width, interval=c(1, 1e5), conf.level=0.95, sides="two.sided", method="wilson") {
#   uniroot(f = function(n) diff(BinomCI(x=p*n, n=n, conf.level=conf.level, 
#                                        sides=sides, method=method)[, -1]) - width, 
#           interval = interval)$root
# }



MeanDiffCI <- function(x, ...){
  UseMethod("MeanDiffCI")
}



MeanDiffCI.formula <- function (formula, data, subset, na.action, ...) {

  # this is from t.test.formula

  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- as.name("model.frame")
  m$... <- NULL
  mf <- eval(m, parent.frame())
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  g <- factor(mf[[-response]])
  if (nlevels(g) != 2L)
    stop("grouping factor must have exactly 2 levels")
  DATA <- setNames(split(mf[[response]], g), c("x", "y"))

  y <- DoCall("MeanDiffCI", c(DATA, list(...)))

  #   y$data.name <- DNAME
  #   if (length(y$estimate) == 2L)
  #     names(y$estimate) <- paste("mean in group", levels(g))
  y
}



MeanDiffCI.default <- function (x, y, method = c("classic", "norm","basic","stud","perc","bca"),
                                conf.level = 0.95, sides = c("two.sided","left","right"),
                                paired = FALSE, na.rm = FALSE, R=999, ...) {

  if (na.rm) {
    x <- na.omit(x)
    y <- na.omit(y)
  }

  sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
  if(sides!="two.sided")
    conf.level <- 1 - 2*(1-conf.level)

  method <- match.arg(method, c("classic", "norm","basic","stud","perc","bca"))
  if(method == "classic"){
    a <- t.test(x, y, conf.level = conf.level, paired = paired)
    if(paired)
      res <- c(meandiff = mean(x - y), lwr.ci = a$conf.int[1], upr.ci = a$conf.int[2])
    else
      res <- c(meandiff = mean(x) - mean(y), lwr.ci = a$conf.int[1], upr.ci = a$conf.int[2])

  } else {

    diff.means <- function(d, f){
      n <- nrow(d)
      gp1 <- 1:table(as.numeric(d[,2]))[1]
      m1 <- sum(d[gp1,1] * f[gp1])/sum(f[gp1])
      m2 <- sum(d[-gp1,1] * f[-gp1])/sum(f[-gp1])
      m1 - m2
    }

    m <- cbind(c(x,y), c(rep(1,length(x)), rep(2,length(y))))

    if(paired)
      boot.fun <- boot(x-y, function(d, i) mean(d[i]), R=R, stype="i")
    else
      boot.fun <- boot(m, diff.means, R=R, stype="f", strata = m[,2])

    ci <- boot.ci(boot.fun, conf=conf.level, type=method)
    if(method == "norm"){
      res <- c(meandiff=boot.fun$t0, lwr.ci=ci[[4]][2], upr.ci=ci[[4]][3])
    } else {
      res <- c(meandiff=boot.fun$t0, lwr.ci=ci[[4]][4], upr.ci=ci[[4]][5])
    }
  }

  if(sides=="left")
    res[3] <- Inf
  else if(sides=="right")
    res[2] <- -Inf

  return(res)
}


# CohenEffectSize <- function(x){

# (C) Antti Arppe 2007-2011
# E-mail: antti.arppe@helsinki.fi

# Cohen's Effect Size (1988)
# e0 <- matrix(,ctable.rows,ctable.cols)
# for(i in 1:ctable.rows)
# for(j in 1:ctable.cols)
# e0[i,j] <- sum.row[i]*sum.col[j]/N
# p0 <- e0/N
# p1 <- ctable/N
# effect.size <- sqrt(sum(((p1-p0)^2)/p0))
# noncentrality <- N*(effect.size^2)
# d.f=(ctable.rows-1)*(ctable.cols-1)
# beta <- pchisq(qchisq(alpha,df=d.f,lower.tail=FALSE),df=d.f,ncp=noncentrality)
# power <- 1-beta

# return(effect.size)
# }


CohenD <- function(x, y=NULL, pooled = TRUE, correct = FALSE, conf.level = NA, na.rm = FALSE) {

  if (na.rm) {
    x <- na.omit(x)
    if(!is.null(y)) y <- na.omit(y)
  }

  if(is.null(y)){   # one sample Cohen d
    d <- mean(x) / sd(x)
    n <- length(x)
    if(!is.na(conf.level)){
      # reference: Smithson Confidence Intervals pp. 36:
      ci <- .nctCI(d / sqrt(n), df = n-1, conf = conf.level)
      res <- c(d=d, lwr.ci=ci[1]/sqrt(n), upr.ci=ci[3]/sqrt(n))
    } else {
      res <- d
    }
  } else {

    meanx <- mean(x)
    meany <- mean(y)
    #     ssqx <- sum((x - meanx)^2)
    #     ssqy <- sum((y - meany)^2)
    nx <- length(x)
    ny <- length(y)

    DF <- nx + ny - 2
    d <- (meanx - meany)

    if(pooled){
      d <- d / sqrt(((nx - 1) * var(x) + (ny - 1) * var(y)) / DF)
    }else{
      d <- d / sd(c(x, y))
    }

    #  if(unbiased) d <- d * gamma(DF/2)/(sqrt(DF/2) * gamma((DF - 1)/2))

    if(correct){  # "Hedges's g"
      # Hedges, L. V. & Olkin, I. (1985). Statistical methods for meta-analysis. Orlando, FL: Academic Press.
      d <- d * (1 - 3 / ( 4 * (nx + ny) - 9))
    }

    if(!is.na(conf.level)) {
      # old:
      # The Handbook of Research Synthesis and Meta-Analysis (Cooper, Hedges, & Valentine, 2009)
      ## p 238
      # ci <- d + c(-1, 1) * sqrt(((nx+ny) / (nx*ny) + .5 * d^2 / DF) * ((nx + ny)/DF)) * qt((1 - conf.level) / 2, DF)

      # supposed to be better, Smithson's version:
      ci <- .nctCI(d / sqrt(nx*ny/(nx+ny)), df = DF, conf = conf.level)
      res <- c(d=d, lwr.ci=ci[1]/sqrt(nx*ny/(nx+ny)), upr.ci=ci[3]/sqrt(nx*ny/(nx+ny)))

      res <- c(d=d, lwr.ci=unname(ci[1]), upr.ci=unname(ci[2]))
    } else {
      res <- d
    }
  }

  ## Cohen, J. (1992). A power primer. Psychological Bulletin, 112, 155-159. Crow, E. L. (1991).
  attr(res, "magnitude") <- c("negligible","small","medium","large")[findInterval(abs(d), c(0.2, 0.5, 0.8)) + 1]

  return(res)

}



.nctCI <- function(tval.1, df, conf) {

  # Function for finding the upper and lower confidence limits for the noncentrality from noncentral t distributions.
  # Especially helpful when forming confidence intervals around the standardized effect size, Cohen's d.

  ###################################################################################################################
  # The following code was adapted from code written by Michael Smithson:
  # Australian National University, sometime around the early part of October, 2001
  # Adapted by Joe Rausch & Ken Kelley: University of Notre Dame, in January 2002.
  # Available at: JRausch@nd.edu & KKelley@nd.edu
  ###################################################################################################################


  # tval.1 is the observed t value, df is the degrees of freedom (group size need not be equal), and conf is simply 1 - alpha

  #         Result <- matrix(NA,1,4)
  tval <- abs(tval.1)


  ############################This part Finds the Lower bound for the confidence interval###########################
  ulim <- 1 - (1-conf)/2

  # This first part finds a lower value from which to start.
  lc <- c(-tval,tval/2,tval)
  while(pt(tval, df, lc[1])<ulim)    {
    lc <- c(lc[1]-tval,lc[1],lc[3])
  }

  # This next part finds the lower limit for the ncp.
  diff <- 1
  while(diff > .00000001)    {
    if(pt(tval, df, lc[2]) <ulim)
      lc <- c(lc[1],(lc[1]+lc[2])/2,lc[2])
    else lc <- c(lc[2],(lc[2]+lc[3])/2,lc[3])
    diff <- abs(pt(tval,df,lc[2]) - ulim)
    ucdf <- pt(tval,df,lc[2])
  }
  res.1 <- ifelse(tval.1 >= 0,lc[2],-lc[2])

  ############################This part Finds the Upper bound for the confidence interval###########################
  llim <- (1-conf)/2

  # This first part finds an upper value from which to start.
  uc <- c(tval,1.5*tval,2*tval)
  while(pt(tval,df,uc[3])>llim)   {
    uc <- c(uc[1],uc[3],uc[3]+tval)
  }

  # This next part finds the upper limit for the ncp.
  diff <- 1
  while(diff > .00000001)         {
    if(pt(tval,df,uc[2])<llim)
      uc <- c(uc[1],(uc[1]+uc[2])/2,uc[2])
    else uc <- c(uc[2],(uc[2]+uc[3])/2,uc[3])
    diff <- abs(pt(tval,df,uc[2]) - llim)
    lcdf <- pt(tval,df,uc[2])
  }
  res <- ifelse(tval.1 >= 0,uc[2],-uc[2])


  #################################This part Compiles the results into a matrix#####################################

  return(c(lwr.ci=min(res, res.1), lprob=ucdf, upr.ci=max(res, res.1), uprob=lcdf))

  #        Result[1,1] <- min(res,res.1)
  #         Result[1,2] <- ucdf
  #         Result[1,3] <- max(res,res.1)
  #         Result[1,4] <- lcdf
  # dimnames(Result) <- list("Values", c("Lower.Limit", "Prob.Low.Limit", "Upper.Limit", "Prob.Up.Limit"))
  #         Result
}



CoefVar <- function (x, ...) {
  UseMethod("CoefVar")
}



CoefVar.lm <- function (x, unbiased = FALSE, conf.level = NA, na.rm = FALSE, ...) {

  # source:  http://www.ats.ucla.edu/stat/mult_pkg/faq/general/coefficient_of_variation.htm

  # In the modeling setting, the CV is calculated as the ratio of the root mean squared error (RMSE)
  # to the mean of the dependent variable.

  # root mean squared error
  rmse <- sqrt(sum(x$residuals^2) / x$df.residual)
  res <- rmse / mean(x$model[[1]], na.rm=na.rm)

  # This is the same approach as in CoefVar.default, but it's not clear
  # if it is correct in the environment of a model
  n <- x$df.residual
  if (unbiased) {
    res <- res * ((1 - (1/(4 * (n - 1))) + (1/n) * res^2) +
                    (1/(2 * (n - 1)^2)))
  }
  if (!is.na(conf.level)) {
    ci <- .nctCI(sqrt(n)/res, df = n - 1, conf = conf.level)
    res <- c(est = res, low.ci = unname(sqrt(n)/ci["upr.ci"]),
             upr.ci = unname(sqrt(n)/ci["lwr.ci"]))
  }

  return(res)

}

# interface for lme???
# dependent variable in lme
# dv <- unname(nlme::getResponse(x))


CoefVar.default <- function (x, weights=NULL, unbiased = FALSE, conf.level = NA, na.rm = FALSE, ...) {

  if(is.null(weights)){
    if(na.rm) x <- na.omit(x)
    res <- SD(x) / Mean(x)
    n <- length(x)
    
  }
  else {
    res <- SD(x, weights = weights) / Mean(x, weights = weights)
    n <- sum(weights)
    
  }

  if(unbiased) {
    res <- res * ((1 - (1/(4*(n-1))) + (1/n) * res^2)+(1/(2*(n-1)^2)))
  }

  if(!is.na(conf.level)){
    ci <- .nctCI(sqrt(n)/res, df = n-1, conf = conf.level)
    res <- c(est=res, low.ci= unname(sqrt(n)/ci["upr.ci"]), upr.ci= unname(sqrt(n)/ci["lwr.ci"]))
  }

  return(res)

}

# aus agricolae: Variations Koeffizient aus aov objekt
#
# CoefVar.aov <- function(x){
#   return(sqrt(sum(x$residual^2) / x$df.residual) / mean(x$fitted.values))
# }


CoefVar.aov <- function (x, unbiased = FALSE, conf.level = NA, na.rm = FALSE, ...) {

  # source:  http://www.ats.ucla.edu/stat/mult_pkg/faq/general/coefficient_of_variation.htm

  # In the modeling setting, the CV is calculated as the ratio of the root mean squared error (RMSE)
  # to the mean of the dependent variable.

  # root mean squared error
  rmse <- sqrt(sum(x$residuals^2) / x$df.residual)
  res <- rmse / mean(x$model[[1]], na.rm=na.rm)

  # This is the same approach as in CoefVar.default, but it's not clear
  # if it is correct in the enviroment of a model
  n <- x$df.residual
  if (unbiased) {
    res <- res * ((1 - (1/(4 * (n - 1))) + (1/n) * res^2) +
                    (1/(2 * (n - 1)^2)))
  }
  if (!is.na(conf.level)) {
    ci <- .nctCI(sqrt(n)/res, df = n - 1, conf = conf.level)
    res <- c(est = res, low.ci = unname(sqrt(n)/ci["upr.ci"]),
             upr.ci = unname(sqrt(n)/ci["lwr.ci"]))
  }

  return(res)

}



VarCI <- function (x, method = c("classic", "bonett", "norm", "basic","stud","perc","bca"),
                   conf.level = 0.95, sides = c("two.sided","left","right"), na.rm = FALSE, R=999) {

  if (na.rm) x <- na.omit(x)
  method <- match.arg(method, c("classic","bonett", "norm","basic","stud","perc","bca"))

  sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
  if(sides!="two.sided")
    conf.level <- 1 - 2*(1-conf.level)

  if(method == "classic"){
    df <- length(x) - 1
    v <- var(x)
    res <- c (var = v, lwr.ci = df * v/qchisq((1 - conf.level)/2, df, lower.tail = FALSE)
              , upr.ci = df * v/qchisq((1 - conf.level)/2, df) )

  } else if(method=="bonett") {

    z <- qnorm(1-(1-conf.level)/2)
    n <- length(x)
    cc <- n/(n-z)
    v <- var(x)
    mtr <- mean(x, trim = 1/(2*(n-4)^0.5))
    m <- mean(x)
    gam4 <- n * sum((x-mtr)^4) / (sum((x-m)^2))^2
    se <- cc * sqrt((gam4 - (n-3)/n)/(n-1))
    lci <- exp(log(cc * v) - z*se)
    uci <- exp(log(cc * v) + z*se)

    res <- c(var=v, lwr.ci=lci, upr.ci=uci)

  } else {
    boot.fun <- boot(x, function(x, d) var(x[d], na.rm=na.rm), R=R)
    ci <- boot.ci(boot.fun, conf=conf.level, type=method)
    if(method == "norm"){
      res <- c(var=boot.fun$t0, lwr.ci=ci[[4]][2], upr.ci=ci[[4]][3])
    } else {
      res <- c(var=boot.fun$t0, lwr.ci=ci[[4]][4], upr.ci=ci[[4]][5])
    }
  }

  if(sides=="left")
    res[3] <- Inf
  else if(sides=="right")
    res[2] <- 0

  return(res)
}



## stats: Lorenz, Gini & ineq ====

Lc <- function(x, ...)
  UseMethod("Lc")


Lc.formula <- function(formula, data, subset, na.action, ...) {

  # this is taken basically from wilcox.test.formula

  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")

  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- as.name("model.frame")
  m$... <- NULL
  mf <- eval(m, parent.frame())
  #   mf$na.action <- substitute(na.action)
  #   DNAME <- paste(names(mf), collapse = " by ")
  #
  #   DATA <- list(table(mf))
  #   do.call("Lc", c(DATA, list(...)))
  drop <- TRUE
  #   mf <- model.frame(x, data)
  x <- split(x = mf[,1], f = mf[,2], drop=drop, ...)

  res <- lapply(x, FUN = "Lc", ...)
  class(res) <- "Lclist"

  return(res)

}


Lc.default <- function(x, n = rep(1, length(x)), na.rm = FALSE, ...) {

  xx <- x
  nn <- n

  g <- Gini(x, n, na.rm=na.rm)

  if(na.rm) x <- na.omit(x)
  if (any(is.na(x)) || any(x < 0)) return(NA_real_)

  k <- length(x)
  o <- order(x)
  x <- x[o]
  n <- n[o]
  x <- n*x
  p <- cumsum(n)/sum(n)
  L <- cumsum(x)/sum(x)
  p <- c(0,p)
  L <- c(0,L)
  L2 <- L * mean(x)
  Lc <- list(p, L, L2, g, xx, nn)
  names(Lc) <- c("p", "L", "L.general", "Gini", "x", "n")
  class(Lc) <- "Lc"

  # no plot anymore, we have plot(lc) and Desc(lc, plotit=TRUE)
  # if(plot) plot(Lc)
  Lc
}


plot.Lc <- function(x, general=FALSE, lwd=2, type="l", xlab="p", ylab="L(p)",
                    main="Lorenz curve", las=1, pch=NA, ...)  {
  if(!general)
    L <- x$L
  else
    L <- x$L.general
  plot(x$p, L, type=type, main=main, lwd=lwd, xlab=xlab, ylab=ylab, xaxs="i",
       yaxs="i", las=las, ...)

  abline(0, max(L))

  if(!is.na(pch)){
    opar <- par(xpd=TRUE)
    on.exit(par(opar))
    points(x$p, L, pch=pch, ...)
  }

}


lines.Lc <- function(x, general=FALSE, lwd=2, conf.level = NA, args.cband = NULL, ...) {

  # Lc.boot.ci <- function(x, conf.level=0.95, n=1000){
  #
  #   x <- rep(x$x, times=x$n)
  #   m <- matrix(sapply(1:n, function(i) sample(x, replace = TRUE)), nrow=length(x))
  #
  #   lst <- apply(m, 2, Lc)
  #   list(x=c(lst[[1]]$p, rev(lst[[1]]$p)),
  #        y=c(apply(do.call(rbind, lapply(lst, "[[", "L")), 2, quantile, probs=(1-conf.level)/2),
  #            rev(apply(do.call(rbind, lapply(lst, "[[", "L")), 2, quantile, probs=1-(1-conf.level)/2)))
  #        )
  # }
  #
  #
  if(!general)
    L <- x$L
  else
    L <- x$L.general


  if (!(is.na(conf.level) || identical(args.cband, NA)) ) {


    args.cband1 <- list(col = SetAlpha(DescToolsOptions("col")[1], 0.12), border = NA)
    if (!is.null(args.cband))
      args.cband1[names(args.cband)] <- args.cband

#    ci <- Lc.boot.ci(x, conf.level=conf.level) # Vertrauensband
    ci <- predict(object=x, conf.level=conf.level, general=general)
    do.call("DrawBand", c(args.cband1, list(x=c(ci$p, rev(ci$p))),
                                       list(y=c(ci$lci, rev(ci$uci)))))
  }


  lines(x$p, L, lwd=lwd, ...)

}


plot.Lclist <- function(x, col=1, lwd=2, lty=1, main = "Lorenz curve",
                        xlab="p", ylab="L(p)", ...){

  # Recycle arguments
  lgp <- Recycle(x=seq_along(x), col=col, lwd=lwd, lty=lty)

  plot(x[[1]], col=lgp$col[1], lwd=lgp$lwd[1], lty=lgp$lty[1], main=main, xlab=xlab, ylab=ylab, ...)
  for(i in 2L:length(x)){
    lines(x[[i]], col=lgp$col[i], lwd=lgp$lwd[i], lty=lgp$lty[i])
  }
}


predict.Lc <- function(object, newdata, conf.level=NA, general=FALSE, n=1000, ...){

  confint.Lc <- function(object, conf.level = 0.95, general=FALSE, n=1000, ...){

    x <- rep(object$x, times=object$n)
    m <- replicate(n = n, sample(x, replace = TRUE))

    lst <- apply(m, 2, Lc)

    list(x=lst[[1]]$p,
         lci=apply(do.call(rbind, lapply(lst, "[[", ifelse(general, "L.general", "L"))), 2, quantile, probs=(1-conf.level)/2),
         uci=apply(do.call(rbind, lapply(lst, "[[", ifelse(general, "L.general", "L"))), 2, quantile, probs=1-(1-conf.level)/2)
    )
  }

  if(!general)
    L <- object$L
  else
    L <- object$L.general


  if(missing(newdata)){
    newdata <- object$p
    res <- data.frame(p=object$p, L=L)
  } else {
    res <- do.call(data.frame, approx(x=object$p, y=L, xout=newdata))
    colnames(res) <- c("p", "L")
  }

  if(!identical(conf.level, NA)){

    ci <- confint.Lc(object, conf.level=conf.level, general=general, n=n)

    lci <- approx(x=ci$x, y=ci$lci, xout=newdata)
    uci <- approx(x=ci$x, y=ci$uci, xout=newdata)

    res <- data.frame(res, lci=lci$y, uci=uci$y)

  }

  res

}

# Original Zeileis:
# Gini <- function(x)
# {
#   n <- length(x)
#   x <- sort(x)
#   G <- sum(x * 1:n)
#   G <- 2*G/(n*sum(x))
#   G - 1 - (1/n)
# }

# other:
# http://rss.acs.unt.edu/Rdoc/library/reldist/html/gini.html
# http://finzi.psych.upenn.edu/R/library/dplR/html/gini.coef.html


Gini <- function(x, n = rep(1, length(x)), unbiased = TRUE, conf.level = NA, R = 1000, type = "bca", na.rm = FALSE) {

  # cast to numeric, as else sum(x * 1:n) might overflow for integers
  # http://stackoverflow.com/questions/39579029/integer-overflow-error-using-gini-function-of-package-desctools
  x <- as.numeric(x)

  x <- rep(x, n)    # same handling as Lc
  if(na.rm) x <- na.omit(x)
  if (any(is.na(x)) || any(x < 0)) return(NA_real_)

  i.gini <- function (x, unbiased = TRUE){
    n <- length(x)
    x <- sort(x)

    res <- 2 * sum(x * 1:n) / (n*sum(x)) - 1 - (1/n)
    if(unbiased) res <- n / (n - 1) * res

    # limit Gini to 0 here, if negative values appear, which is the case with
    # Gini( c(10,10,10))
    return( pmax(0, res))

    # other guy out there:
    #     N <- if (unbiased) n * (n - 1) else n * n
    #     dsum <- drop(crossprod(2 * 1:n - n - 1, x))
    #     dsum / (mean(x) * N)
    # is this slower, than above implementation??
  }

  if(is.na(conf.level)){
    res <- i.gini(x, unbiased = unbiased)

  } else {
    # adjusted bootstrap percentile (BCa) interval
    boot.gini <- boot(x, function(x, d) i.gini(x[d], unbiased = unbiased), R=R)
    ci <- boot.ci(boot.gini, conf=conf.level, type=type)
    res <- c(gini=boot.gini$t0, lwr.ci=ci[[4]][4], upr.ci=ci[[4]][5])
  }

  return(res)

}



GiniSimpson <- function(x, na.rm = FALSE) {


  # referenz:   Sachs, Angewandte Statistik, S. 57

  # example:
  # x <- as.table(c(69,17,7,62))
  # rownames(x) <- c("A","B","AB","0")
  # GiniSimpson(x)

  if(!is.factor(x)){
    warning("x is not a factor!")
    return(NA)
  }
  
  if(na.rm) x <- na.omit(x)

  ptab <- prop.table(table(x))
  return(sum(ptab*(1-ptab)))
  
}




HunterGaston <- function(x, na.rm = FALSE){

  # we must restrict to x as factors here to ensure we have all the levels
  # these are used in length(p)
    
  if(!is.factor(x)){
    warning("x is not a factor!")
    return(NA)
  }
  if(na.rm) x <- na.omit(x)
  
  p <- prop.table(table(x))
  sum(p*(1-p)) * length(p)/(length(p)-1)
  
}





Atkinson <- function(x, n = rep(1, length(x)), parameter = 0.5, na.rm = FALSE) {

  x <- rep(x, n)    # same handling as Lc and Gini
  if(na.rm) x <- na.omit(x)
  if (any(is.na(x)) || any(x < 0)) return(NA_real_)

  if(is.null(parameter)) parameter <- 0.5
  if(parameter==1)
    A <- 1 - (exp(mean(log(x)))/mean(x))
  else
  {
    x <- (x/mean(x))^(1-parameter)
    A <- 1 - mean(x)^(1/(1-parameter))
  }
  A
}

Herfindahl <- function(x, n = rep(1, length(x)), parameter=1, na.rm = FALSE) {

  x <- rep(x, n)    # same handling as Lc and Gini
  if(na.rm) x <- na.omit(x)
  if (any(is.na(x)) || any(x < 0)) return(NA_real_)

  if(is.null(parameter))
    m <- 1
  else
    m <- parameter
  Herf <- x/sum(x)
  Herf <- Herf^(m+1)
  Herf <- sum(Herf)^(1/m)
  Herf
}

Rosenbluth <- function(x, n = rep(1, length(x)), na.rm = FALSE) {

  x <- rep(x, n)    # same handling as Lc and Gini
  if(na.rm) x <- na.omit(x)
  if (any(is.na(x)) || any(x < 0)) return(NA_real_)

  n <- length(x)
  x <- sort(x)
  HT <- (n:1)*x
  HT <- 2*sum(HT/sum(x))
  HT <- 1/(HT-1)
  HT
}

###

## stats: assocs etc. ====


CutQ <- function(x, breaks=quantile(x, seq(0, 1, by=0.25), na.rm=TRUE), 
                 labels=NULL, na.rm = FALSE, ...){

  # old version:
  #  cut(x, breaks=probsile(x, breaks=probs, na.rm = na.rm), include.lowest=TRUE, labels=labels)

  # $Id: probscut.R 1431 2010-04-28 17:23:08Z ggrothendieck2 $
  # from gtools

  if(na.rm) x <- na.omit(x)

  if(length(breaks)==1 && IsWhole(breaks))
    breaks <- quantile(x, seq(0, 1, by = 1/breaks), na.rm = TRUE)
  
  if(is.null(labels)) labels <- gettextf("Q%s", 1:(length(breaks)-1))

  # probs <- quantile(x, probs)
  dups <- duplicated(breaks)
  if(any(dups)) {

    flag <- x %in% unique(breaks[dups])
    retval <- ifelse(flag, paste("[", as.character(x), "]", sep=''), NA)
    uniqs <- unique(breaks)

    # move cut points over a bit...
    reposition <- function(cut) {
      flag <- x>=cut
      if(sum(flag)==0)
        return(cut)
      else
        return(min(x[flag]))
    }

    newprobs <- sapply(uniqs, reposition)
    retval[!flag] <- as.character(cut(x[!flag], breaks=newprobs, include.lowest=TRUE,...))

    levs <- unique(retval[order(x)])        # ensure factor levels are
    # properly ordered
    retval <- factor(retval, levels=levs)

    ## determine open/closed interval ends
    mkpairs <- function(x) # make table of lower, upper
      sapply(x,
             function(y) if(length(y)==2) y[c(2,2)] else y[2:3]
      )
    pairs <- mkpairs(strsplit(levs, '[^0-9+\\.\\-]+'))
    rownames(pairs) <- c("lower.bound","upper.bound")
    colnames(pairs) <- levs

    closed.lower <- rep(FALSE, ncol(pairs)) # default lower is open
    closed.upper <- rep(TRUE, ncol(pairs))  # default upper is closed
    closed.lower[1] <- TRUE                 # lowest interval is always closed

    for(i in 2:ncol(pairs))                 # open lower interval if above singlet
      if(pairs[1,i]==pairs[1,i-1] && pairs[1,i]==pairs[2,i-1])
        closed.lower[i] <- FALSE

    for(i in 1:(ncol(pairs)-1))             # open upper interval if below singlet
      if(pairs[2,i]==pairs[1,i+1] && pairs[2,i]==pairs[2,i+1])
        closed.upper[i] <- FALSE

    levs <- ifelse(pairs[1,]==pairs[2,],
                   pairs[1,],
                   paste(ifelse(closed.lower,"[","("),
                         pairs[1,],
                         ",",
                         pairs[2,],
                         ifelse(closed.upper,"]",")"),
                         sep='')
    )
    levels(retval) <- levs

  } else
    retval <- cut( x, breaks, include.lowest=TRUE,  labels=labels, ... )

  return(retval)

}



# Phi-Koeff
Phi  <- function (x, y = NULL, ...) {
  if(!is.null(y)) x <- table(x, y, ...)
  # when computing phi, note that Yates' correction to chi-square must not be used.
  as.numeric( sqrt( suppressWarnings(chisq.test(x, correct=FALSE)$statistic) / sum(x) ) )

  # should we implement: ??
  # following http://technology.msb.edu/old/training/statistics/sas/books/stat/chap26/sect19.htm#idxfrq0371
  # (Liebetrau 1983)
  # this makes phi -1 < phi < 1 for 2x2 tables  (same for CramerV)
  # (prod(diag(x)) - prod(diag(Rev(x, 2)))) / sqrt(prod(colSums(x), rowSums(x)))

}



# Kontingenz-Koeffizient
ContCoef <- function(x, y = NULL, correct = FALSE, ...) {
  if(!is.null(y)) x <- table(x, y, ...)
  chisq <- suppressWarnings(chisq.test(x, correct = FALSE)$statistic)
  cc <- as.numeric( sqrt( chisq / ( chisq + sum(x)) ))
  if(correct) {  # Sakoda's adjusted Pearson's C
    k <- min(nrow(x),ncol(x))
    cc <- cc/sqrt((k-1)/k)
  }
  return(cc)
}


CramerV <- function(x, y = NULL, conf.level = NA,
                    method = c("ncchisq", "ncchisqadj", "fisher", "fisheradj"), 
                    correct=FALSE, ...){

  if(!is.null(y)) x <- table(x, y, ...)

  # CIs and power for the noncentral chi-sq noncentrality parameter (ncp):
  # The function lochi computes the lower CI limit and hichi computes the upper limit.
  # Both functions take 3 arguments: observed chi-sq, df, and confidence level.

  # author:   Michael Smithson
  # http://psychology3.anu.edu.au/people/smithson/details/CIstuff/Splusnonc.pdf

  # see also: MBESS::conf.limits.nc.chisq, Ken Kelly


  lochi <- function(chival, df, conf) {
    
    # we don't have lochi for chival==0 
    # optimize would report minval = maxval
    if(chival==0) return(NA)
    
    ulim <- 1 - (1-conf)/2
    #  This first part finds a lower value from which to start.
    lc <- c(.001, chival/2, chival)
    while(pchisq(chival, df, lc[1]) < ulim) {
      if(pchisq(chival, df) < ulim)
        return(c(0, pchisq(chival, df)))
      lc <- c(lc[1]/4, lc[1], lc[3])
    }
    #	This next part finds the lower limit for the ncp.
    diff <- 1
    while(diff > .00001) {
      if(pchisq(chival, df, lc[2]) < ulim)
        lc <- c(lc[1],(lc[1]+lc[2])/2, lc[2])
      else lc <- c(lc[2], (lc[2]+lc[3])/2, lc[3])
      diff <- abs(pchisq(chival, df, lc[2]) - ulim)
      ucdf <- pchisq(chival, df, lc[2])
    }
    c(lc[2], ucdf)
  }

  hichi <- function(chival,df,conf) {
    
    # we don't have hichi for chival==0 
    if(chival==0) return(NA)
    
    #	This first part finds upper and lower startinig values.
    uc <- c(chival, 2*chival, 3*chival)
    llim <- (1-conf)/2
    while(pchisq(chival, df, uc[1]) < llim) {
      uc <- c(uc[1]/4,uc[1],uc[3])
    }
    while(pchisq(chival,df,uc[3])>llim) {
      uc <- c(uc[1],uc[3],uc[3]+chival)
    }
    #	This next part finds the upper limit for the ncp.
    diff <- 1
    while(diff > .00001) {
      if(pchisq(chival, df, uc[2]) < llim)
        uc <- c(uc[1], (uc[1] + uc[2]) / 2, uc[2])
      else uc <- c(uc[2], (uc[2] + uc[3]) / 2, uc[3])
      diff <- abs(pchisq(chival, df, uc[2]) - llim)
      lcdf <- pchisq(chival, df, uc[2])
    }
    c(uc[2], lcdf)
  }

  # Remark Andri 18.12.2014:
  # lochi and hichi could be replaced with:
  #   optimize(function(x) abs(pchisq(chival, DF, x)  - (1-(1-conf.level)/2)), c(0, chival))
  #   optimize(function(x) abs(pchisq(chival, DF, x)  - (1-conf.level)/2), c(0, 3*chival))
  #
  # ... which would run ~ 25% faster and be more exact

  
  
  # what can go wrong while calculating chisq.stat?
  # we don't need test results here, so we suppress those warnings
  chisq.hat <- suppressWarnings(chisq.test(x, correct = FALSE)$statistic)
  df <- prod(dim(x)-1)
  n <- sum(x)
  
  if(correct){
  
    # Bergsma, W, A bias-correction for Cramer's V and Tschuprow's T
    # September 2013Journal of the Korean Statistical Society 42(3)
    # DOI: 10.1016/j.jkss.2012.10.002
    phi.hat <- chisq.hat / n
    v <- as.numeric(sqrt(max(0, phi.hat - df/(n-1)) / 
           (min(sapply(dim(x), function(i) i - 1 / (n-1) * (i-1)^2) - 1))))

  } else {
    v <- as.numeric(sqrt(chisq.hat/(n * (min(dim(x)) - 1))))
  }
  
  
  if (is.na(conf.level)) {
    res <- v

  } else {

    switch(match.arg(method),
      ncchisq={
            ci <- c(lochi(chisq.hat, df, conf.level)[1], hichi(chisq.hat, df, conf.level)[1])
            # corrected by michael smithson, 17.5.2014:
            #    ci <- unname(sqrt( (ci + df) / (sum(x) * (min(dim(x)) - 1)) ))
            ci <- unname(sqrt( (ci) / (n * (min(dim(x)) - 1)) ))
            },

      ncchisqadj={
        ci <- c(lochi(chisq.hat, df, conf.level)[1] + df, hichi(chisq.hat, df, conf.level)[1] + df)
        # corrected by michael smithson, 17.5.2014:
        #    ci <- unname(sqrt( (ci + df) / (sum(x) * (min(dim(x)) - 1)) ))
        ci <- unname(sqrt( (ci) / (n * (min(dim(x)) - 1)) ))
      },

      fisher={
              se <- 1 / sqrt(n-3) * qnorm(1-(1-conf.level)/2)
              ci <- tanh(atanh(v) + c(-se, se))
            },

      fisheradj={
                se <- 1 / sqrt(n-3) * qnorm(1-(1-conf.level)/2)
                # bias correction
                adj <- 0.5 * v / (n-1)
                ci <- tanh(atanh(v) + c(-se, se) + adj)

      })

    #    "Cram\u00E9r's association coefficient"
    res <- c("Cramer V"=v, lwr.ci=max(0, ci[1]), upr.ci=min(1, ci[2]))

  }

  return(res)
}



ncparamF <- function(type1, type2, nu1, nu2) {

  # author Ali Baharev <ali.baharev at gmail.com>
  
  # Returns the noncentrality parameter of the noncentral F distribution 
  # if probability of Type I and Type II error, degrees of freedom of the 
  # numerator and the denominator in the F test statistics are given.
  
  
  .C("fpow",  PACKAGE = "DescTools", as.double(type1), as.double(type2), 
     as.double(nu1), as.double(nu2), lambda=double(1))$lambda
}



YuleQ <- function(x, y = NULL, ...){

  if(!is.null(y)) x <- table(x, y, ...)

  # allow only 2x2 tables
  stopifnot(prod(dim(x)) == 4 || length(x) == 4)

  a <- x[1,1]
  b <- x[1,2]
  c <- x[2,1]
  d <- x[2,2]
  return((a*d- b*c)/(a*d + b*c))  #Yule Q

}


YuleY <- function(x, y = NULL, ...){

  if(!is.null(y)) x <- table(x, y, ...)

  # allow only 2x2 tables
  stopifnot(prod(dim(x)) == 4 || length(x) == 4)

  a <- x[1,1]
  b <- x[1,2]
  c <- x[2,1]
  d <- x[2,2]
  return((sqrt(a*d) - sqrt(b*c))/(sqrt(a*d)+sqrt(b*c))) # YuleY

}


TschuprowT <- function(x, y = NULL, correct = FALSE, ...){

  if(!is.null(y)) x <- table(x, y, ...)

  # Tschuprow, A. A. (1939) Principles of the Mathematical Theory of Correlation; translated by M. Kantorowitsch. W. Hodge & Co.
  # http://en.wikipedia.org/wiki/Tschuprow's_T
  # Hartung S. 451

  # what can go wrong while calculating chisq.stat?
  # we don't need test results here, so we suppress those warnings
  chisq.hat <- suppressWarnings(chisq.test(x, correct = FALSE)$statistic)
  n <- sum(x)
  df <- prod(dim(x)-1)
  
  if(correct) {
    # Bergsma, W, A bias-correction for Cramer's V and Tschuprow's T
    # September 2013Journal of the Korean Statistical Society 42(3)
    # DOI: 10.1016/j.jkss.2012.10.002
    # see also CramerV
    
    phi.hat <- chisq.hat / n
    as.numeric(sqrt(max(0, phi.hat - df/(n-1)) / 
                     (sqrt(prod(sapply(dim(x), function(i) i - 1 / (n-1) * (i-1)^2) - 1)))))
    
  } else {
    as.numeric( sqrt(chisq.hat/(n * sqrt(df))))
  }

}




# based on Kappa from library(vcd)
# author: David Meyer
# see also: kappa in library(psych)

CohenKappa <- function (x, y = NULL, weights = c("Unweighted", "Equal-Spacing", "Fleiss-Cohen"), conf.level = NA, ...) {

  if (is.character(weights)) weights <- match.arg(weights)

  if(!is.null(y)) {
    # we can not ensure a reliable weighted kappa for 2 factors with different levels
    # so refuse trying it... (unweighted is no problem)

    if( !identical(weights, "Unweighted")) stop("Vector interface for weighted Kappa is not supported. Provide confusion matrix.")

    # x and y must have the same levels in order to build a symmetric confusion matrix
    x <- factor(x)
    y <- factor(y)
    lvl <- unique(c(levels(x), levels(y)))
    x <- factor(x, levels=lvl)
    y <- factor(y, levels=lvl)
    x <- table(x, y, ...)

  } else {
    d <- dim(x)
    if (d[1L] != d[2L]) stop("x must be square matrix if provided as confusion matrix")
  }

  d <- diag(x)
  n <- sum(x)
  nc <- ncol(x)
  colFreqs <- colSums(x)/n
  rowFreqs <- rowSums(x)/n

  kappa <- function(po, pc) (po - pc)/(1 - pc)
  std <- function(po, pc, W = 1) sqrt(sum(W * W * po * (1 - po))/crossprod(1 - pc)/n)

  po <- sum(d)/n
  pc <- crossprod(colFreqs, rowFreqs)

  k <- as.vector(kappa(po, pc))
  s <- as.vector(std(po, pc))

  W <- if (is.matrix(weights))
    weights
  else if (weights == "Equal-Spacing")
    1 - abs(outer(1:nc, 1:nc, "-"))/(nc - 1)
  else # weightx == "Fleiss-Cohen"
    1 - (abs(outer(1:nc, 1:nc, "-"))/(nc - 1))^2

  pow <- sum(W * x)/n
  pcw <- sum(W * colFreqs %o% rowFreqs)

  kw <- as.vector(kappa(pow, pcw))
  sw <- as.vector(std(x/n, 1 - pcw, W))

  #   structure(list(Unweighted = c(value = k, ASE = s), Weighted = c(value = kw,
  #       ASE = sw), Weights = W), class = "Kappa")

  if (is.na(conf.level)) {
    if(identical(weights, "Unweighted"))
      res <- k
    else
      res <- kw
  } else {
    if(identical(weights, "Unweighted")) {
      ci <- k + c(1,-1) * qnorm((1-conf.level)/2) * s
      res <- c("kappa"=k, lwr.ci=ci[1], upr.ci=ci[2])
    } else {
      ci <- kw + c(1,-1) * qnorm((1-conf.level)/2) * sw
      res <- c("kappa"=kw, lwr.ci=ci[1], upr.ci=ci[2])
    }
  }
  return(res)

}

# KappaTest <- function(x, weights = c("Equal-Spacing", "Fleiss-Cohen"), conf.level = NA) {
# to do, idea is to implement a Kappa test for H0: kappa = 0 as in
# http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf, pp. 1687
#   print( "still to do...." )

# }


KappaM <- function(x, method = c("Fleiss", "Conger", "Light"), conf.level = NA) {

  # ratings <- as.matrix(na.omit(x))
  #
  # ns <- nrow(ratings)
  # nr <- ncol(ratings)
  #
  # # Build table
  # lev <- levels(as.factor(ratings))
  #
  # for (i in 1:ns) {
  #   frow <- factor(ratings[i,],levels=lev)
  #
  #   if (i==1)
  #     ttab <- as.numeric(table(frow))
  #   else
  #     ttab <- rbind(ttab, as.numeric(table(frow)))
  # }
  #
  # ttab <- matrix(ttab, nrow=ns)

  # we have not factors for matrices, but we need factors below...
  if(is.matrix(x))
    x <- as.data.frame(x)
  
  x <- na.omit(x)
  ns <- nrow(x)
  nr <- ncol(x)

  # find all levels in the data (data.frame)
  lev <- levels(factor(unlist(x)))
  # apply the same levels to all variables and switch to integer matrix
  xx <- do.call(cbind, lapply(x, factor, levels=lev))

  ttab <- apply(Abind(lapply(as.data.frame(xx), function(z) Dummy(z, method="full", levels=seq_along(lev))), along = 3),
                c(1,2), sum)

  agreeP <- sum((rowSums(ttab^2)-nr)/(nr*(nr-1))/ns)

  switch( match.arg(method, choices= c("Fleiss", "Conger", "Light"))
          , "Fleiss" = {
            chanceP <- sum(colSums(ttab)^2)/(ns*nr)^2
            value <- (agreeP - chanceP)/(1 - chanceP)

            pj <- colSums(ttab)/(ns*nr)
            qj <- 1-pj

            varkappa <- (2/(sum(pj*qj)^2*(ns*nr*(nr-1))))*(sum(pj*qj)^2-sum(pj*qj*(qj-pj)))
            SEkappa <- sqrt(varkappa)

            ci <- value + c(1,-1) * qnorm((1-conf.level)/2) * SEkappa
          }
          , "Conger" = {
            # for (i in 1:nr) {
            #   rcol <- factor(x[,i],levels=lev)
            #
            #   if (i==1)
            #     rtab <- as.numeric(table(rcol))
            #   else
            #     rtab <- rbind(rtab, as.numeric(table(rcol)))
            # }

            rtab <- apply(Abind(lapply(as.data.frame(t(xx)), function(z) Dummy(z, method="full", levels=seq_along(lev))), along = 3),
                          c(1,2), sum)

            rtab <- rtab/ns

            chanceP <- sum(colSums(ttab)^2)/(ns*nr)^2 - sum(apply(rtab, 2, var)*(nr-1)/nr)/(nr-1)
            value <- (agreeP - chanceP)/(1 - chanceP)

            # we have not SE for exact Kappa value
            ci <- c(NA, NA)

          }
          , "Light" = {
            m <- DescTools::PairApply(x, DescTools::CohenKappa, symmetric=TRUE)
            value <- mean(m[upper.tri(m)])

            levlen <- length(lev)
            for (nri in 1:(nr - 1)) for (nrj in (nri + 1):nr) {
              for (i in 1:levlen) for (j in 1:levlen) {
                if (i != j) {
                  r1i <- sum(x[, nri] == lev[i])
                  r2j <- sum(x[, nrj] == lev[j])
                  if (!exists("dis"))
                    dis <- r1i * r2j
                  else dis <- c(dis, r1i * r2j)
                }
              }
              if (!exists("disrater"))
                disrater <- sum(dis)
              else disrater <- c(disrater, sum(dis))
              rm(dis)
            }
            B <- length(disrater) * prod(disrater)
            chanceP <- 1 - B/ns^(choose(nr, 2) * 2)
            varkappa <- chanceP/(ns * (1 - chanceP))
            SEkappa <- sqrt(varkappa)

            ci <- value + c(1,-1) * qnorm((1-conf.level)/2) * SEkappa

          }
  )


  if (is.na(conf.level)) {
    res <- value
  } else {
    res <- c("kappa"=value, lwr.ci=ci[1], upr.ci=ci[2])
  }
  return(res)

}



Agree <- function(x, tolerance = 0, na.rm = FALSE) {

  x <- as.matrix(x)
  if(na.rm) x <- na.omit(x)

  if(anyNA(x)) return(NA)

  ns <- nrow(x)
  nr <- ncol(x)

  if (is.numeric(x)) {
    rangetab <- apply(x, 1, max) - apply(x, 1, min)
    coeff <-  sum(rangetab <= tolerance)/ns

  } else {
    rangetab <- as.numeric(sapply(apply(x, 1, table), length))
    coeff <- (sum(rangetab == 1)/ns)
    tolerance <- 0
  }

  rval <- coeff
  attr(rval, c("subjects")) <- ns
  attr(rval, c("raters")) <- nr

  return(rval)

}


# ICC(ratings)
# ICC_(ratings, type="ICC3", conf.level=0.95)
# ICC_(ratings, type="all", conf.level=0.95)

ICC <- function(x, type=c("all", "ICC1","ICC2","ICC3","ICC1k","ICC2k","ICC3k"), conf.level = NA, na.rm = FALSE) {

  ratings <- as.matrix(x)
  if(na.rm) ratings <- na.omit(ratings)

  ns <- nrow(ratings)
  nr <- ncol(ratings)

  x.s <- stack(data.frame(ratings))
  x.df <- data.frame(x.s, subs = rep(paste("S", 1:ns, sep = ""), nr))

  s.aov <- summary(aov(values ~ subs + ind, data=x.df))
  stats <- matrix(unlist(s.aov), ncol=3, byrow=TRUE)
  MSB <- stats[3,1]
  MSW <- (stats[2,2] + stats[2,3])/(stats[1,2] + stats[1,3])
  MSJ <- stats[3,2]
  MSE <- stats[3,3]

  ICC1 <- (MSB- MSW)/(MSB+ (nr-1)*MSW)
  ICC2 <- (MSB- MSE)/(MSB + (nr-1)*MSE + nr*(MSJ-MSE)/ns)
  ICC3 <- (MSB - MSE)/(MSB+ (nr-1)*MSE)
  ICC12 <- (MSB-MSW)/(MSB)
  ICC22 <- (MSB- MSE)/(MSB +(MSJ-MSE)/ns)
  ICC32 <- (MSB-MSE)/MSB

  #find the various F values from Shrout and Fleiss
  F11 <- MSB/MSW
  df11n <- ns-1
  df11d <- ns*(nr-1)
  p11 <- 1 - pf(F11, df11n, df11d)
  F21 <- MSB/MSE
  df21n <- ns-1
  df21d <- (ns-1)*(nr-1)
  p21 <- 1-pf(F21, df21n, df21d)
  F31 <- F21


  # results <- t(results)

  results <- data.frame(matrix(NA, ncol=8, nrow=6))
  colnames(results ) <- c("type", "est","F-val","df1","df2","p-val","lwr.ci","upr.ci")
  rownames(results) <- c("Single_raters_absolute","Single_random_raters","Single_fixed_raters", "Average_raters_absolute","Average_random_raters","Average_fixed_raters")

  results[,1] = c("ICC1","ICC2","ICC3","ICC1k","ICC2k","ICC3k")
  results[,2] = c(ICC1, ICC2, ICC3, ICC12, ICC22, ICC32)
  results[1,3] <- results[4,3] <- F11
  results[2,3] <- F21
  results[3,3] <- results[6,3] <- results[5,3] <- F31 <- F21
  results[5,3] <- F21
  results[1,4] <- results[4,4] <- df11n
  results[1,5] <- results[4,5] <- df11d
  results[1,6] <- results[4,6] <- p11
  results[2,4] <- results[3,4] <- results[5,4] <- results[6,4] <- df21n
  results[2,5] <- results[3,5] <- results[5,5] <- results[6,5] <- df21d
  results[2,6] <- results[5,6] <- results[3,6] <- results[6,6] <- p21

  #now find confidence limits
  #first, the easy ones
  alpha <- 1 - conf.level
  F1L <- F11 / qf(1-alpha/2, df11n, df11d)
  F1U <- F11 * qf(1-alpha/2, df11d, df11n)
  L1 <- (F1L-1) / (F1L + (nr - 1))
  U1 <- (F1U -1) / (F1U + nr - 1)
  F3L <- F31 / qf(1-alpha/2, df21n, df21d)
  F3U <- F31 * qf(1-alpha/2, df21d, df21n)
  results[1,7] <- L1
  results[1,8] <- U1
  results[3,7] <- (F3L-1)/(F3L+nr-1)
  results[3,8] <- (F3U-1)/(F3U+nr-1)
  results[4,7] <- 1- 1/F1L
  results[4,8] <- 1- 1/F1U
  results[6,7] <- 1- 1/F3L
  results[6,8] <- 1 - 1/F3U

  #the hard one is case 2
  Fj <- MSJ/MSE
  vn <- (nr-1)*(ns-1)* ( (nr*ICC2*Fj+ns*(1+(nr-1)*ICC2) - nr*ICC2))^2
  vd <- (ns-1)*nr^2 * ICC2^2 * Fj^2 + (ns *(1 + (nr-1)*ICC2) - nr*ICC2)^2
  v <- vn/vd
  F3U <- qf(1-alpha/2,ns-1,v)
  F3L <- qf(1-alpha/2,v,ns-1)

  L3 <- ns *(MSB- F3U*MSE)/(F3U*(nr * MSJ + (nr*ns-nr-ns) * MSE)+ ns*MSB)
  results[2, 7] <- L3
  U3 <- ns *(F3L * MSB - MSE)/(nr * MSJ + (nr * ns - nr - ns)*MSE + ns * F3L * MSB)
  results[2, 8] <- U3
  L3k <- L3 * nr/(1+ L3*(nr-1))
  U3k <- U3 * nr/(1+ U3*(nr-1))
  results[5, 7] <- L3k
  results[5, 8] <- U3k


  #clean up the output
  results[,2:8] <- results[,2:8]

  type <- match.arg(type, c("all", "ICC1","ICC2","ICC3","ICC1k","ICC2k","ICC3k"))

  switch(type
         , all={res <- list(results=results, summary=s.aov, stats=stats, MSW=MSW, ns=ns, nr=nr)
         class(res) <- "ICC"
         }
         , ICC1={idx <- 1}
         , ICC2={idx <- 2}
         , ICC3={idx <- 3}
         , ICC1k={idx <- 4}
         , ICC2k={idx <- 5}
         , ICC3k={idx <- 6}
  )

  if(type!="all"){
    if(is.na(conf.level)){
      res <- results[idx, c(2)][,]
    } else {
      res <- unlist(results[idx, c(2, 7:8)])
      names(res) <- c(type,"lwr.ci","upr.ci")
    }
  }

  return(res)

}


print.ICC <- function(x, digits = 3, ...){
  cat("\nIntraclass correlation coefficients \n")
  print(x$results, digits=digits)
  cat("\n Number of subjects =", x$ns, "    Number of raters =", x$nr, "\n")
}




# implementing Omega might be wise
# boostrap CI could be integrated in function instead on examples of help

CronbachAlpha <- function(x, conf.level = NA, cond = FALSE, na.rm = FALSE){

  i.CronbachAlpha <- function(x, conf.level = NA){
    nc <- ncol(x)
    colVars <- apply(x, 2, var)
    total   <- var(rowSums(x))
    res <- (total - sum(colVars)) / total * (nc/(nc-1))

    if (!is.na(conf.level)) {
      N <- length(x)
      ci <- 1 - (1-res) * qf( c(1-(1-conf.level)/2, (1-conf.level)/2), N-1, (nc-1)*(N-1))
      res <- c("Cronbach Alpha"=res, lwr.ci=ci[1], upr.ci=ci[2])
    }
    return(res)
  }


  x <- as.matrix(x)
  if(na.rm) x <- na.omit(x)

  res <- i.CronbachAlpha(x = x, conf.level = conf.level)

  if(cond) {
    condCronbachAlpha <- list()
    n <- ncol(x)
    if(n > 2) {     # can't calculate conditional with only 2 items
      for(i in 1:n){
        condCronbachAlpha[[i]] <- i.CronbachAlpha(x[,-i], conf.level = conf.level)
      }
      condCronbachAlpha <- data.frame(Item = 1:n, do.call("rbind", condCronbachAlpha))
      colnames(condCronbachAlpha)[2] <- "Cronbach Alpha"
    }
    res <- list(unconditional=res, condCronbachAlpha = condCronbachAlpha)
  }

  return(res)
}


KendallW <- function(x, correct=FALSE, test=FALSE, na.rm = FALSE) {

  # see also old Jim Lemon function kendall.w
  # other solution: library(irr);  kendall(ratings, correct = TRUE)
  # http://www.real-statistics.com/reliability/kendalls-w/


  dname <- deparse(substitute(x))

  ratings <- as.matrix(x)
  if(na.rm) ratings <- na.omit(ratings)

  ns <- nrow(ratings)
  nr <- ncol(ratings)

  #Without correction for ties
  if (!correct) {
    #Test for ties
    TIES = FALSE
    testties <- apply(ratings, 2, unique)
    if (!is.matrix(testties)) TIES=TRUE
    else { if (length(testties) < length(ratings)) TIES=TRUE }

    ratings.rank <- apply(ratings,2,rank)

    coeff.name <- "W"
    coeff <- (12*var(apply(ratings.rank,1,sum))*(ns-1))/(nr^2*(ns^3-ns))
  }
  else { #With correction for ties
    ratings <- as.matrix(na.omit(ratings))

    ns <- nrow(ratings)
    nr <- ncol(ratings)

    ratings.rank <- apply(ratings,2,rank)

    Tj <- 0
    for (i in 1:nr) {
      rater <- table(ratings.rank[,i])
      ties  <- rater[rater>1]
      l 	  <- as.numeric(ties)
      Tj	  <- Tj + sum(l^3-l)
    }

    coeff.name <- "Wt"
    coeff <- (12*var(apply(ratings.rank,1,sum))*(ns-1))/(nr^2*(ns^3-ns)-nr*Tj)
  }

  if(test){
    #test statistics
    Xvalue  <- nr*(ns-1)*coeff
    df1     <- ns-1
    names(df1) <- "df"
    p.value <- pchisq(Xvalue, df1, lower.tail = FALSE)
    method <- paste("Kendall's coefficient of concordance", coeff.name)
    alternative <- paste(coeff.name, "is greater 0")
    names(ns) <- "subjects"
    names(nr) <- "raters"
    names(Xvalue) <- "Kendall chi-squared"
    names(coeff) <- coeff.name
    rval <- list(#subjects = ns, raters = nr,
      estimate = coeff, parameter=c(df1, ns, nr),
      statistic = Xvalue, p.value = p.value,
      alternative = alternative, method = method, data.name = dname)

    class(rval) <- "htest"
  } else {
    rval <- coeff
  }

  if (!correct && TIES) warning("Coefficient may be incorrect due to ties")
  return(rval)
}



CCC <- function(x, y, ci = "z-transform", conf.level = 0.95, na.rm = FALSE){

  dat <- data.frame(x, y)

  if(na.rm) dat <- na.omit(dat)
  #   id <- complete.cases(dat)
  #   nmissing <- sum(!complete.cases(dat))
  #   dat <- dat[id,]


  N. <- 1 - ((1 - conf.level) / 2)
  zv <- qnorm(N., mean = 0, sd = 1)
  lower <- "lwr.ci"
  upper <- "upr.ci"

  k <- length(dat$y)
  yb <- mean(dat$y)
  sy2 <- var(dat$y) * (k - 1) / k
  sd1 <- sd(dat$y)

  xb <- mean(dat$x)
  sx2 <- var(dat$x) * (k - 1) / k
  sd2 <- sd(dat$x)

  r <- cor(dat$x, dat$y)
  sl <- r * sd1 / sd2

  sxy <- r * sqrt(sx2 * sy2)
  p <- 2 * sxy / (sx2 + sy2 + (yb - xb)^2)

  delta <- (dat$x - dat$y)
  rmean <- apply(dat, MARGIN = 1, FUN = mean)
  blalt <- data.frame(mean = rmean, delta)

  # Scale shift:
  v <- sd1 / sd2
  # Location shift relative to the scale:
  u <- (yb - xb) / ((sx2 * sy2)^0.25)
  # Variable C.b is a bias correction factor that measures how far the best-fit line deviates from a line at 45 degrees (a measure of accuracy). No deviation from the 45 degree line occurs when C.b = 1. See Lin (1989 page 258).
  # C.b <- (((v + 1) / (v + u^2)) / 2)^-1

  # The following taken from the Stata code for function "concord" (changed 290408):
  C.b <- p / r

  # Variance, test, and CI for asymptotic normal approximation (per Lin (March 2000) Biometrics 56:325-5):
  sep = sqrt(((1 - ((r)^2)) * (p)^2 * (1 - ((p)^2)) / (r)^2 + (2 * (p)^3 * (1 - p) * (u)^2 / r) - 0.5 * (p)^4 * (u)^4 / (r)^2 ) / (k - 2))
  ll = p - zv * sep
  ul = p + zv * sep

  # Statistic, variance, test, and CI for inverse hyperbolic tangent transform to improve asymptotic normality:
  t <- log((1 + p) / (1 - p)) / 2
  set = sep / (1 - ((p)^2))
  llt = t - zv * set
  ult = t + zv * set
  llt = (exp(2 * llt) - 1) / (exp(2 * llt) + 1)
  ult = (exp(2 * ult) - 1) / (exp(2 * ult) + 1)

  if(ci == "asymptotic"){
    rho.c <- as.data.frame(cbind(p, ll, ul))
    names(rho.c) <- c("est", lower, upper)
    rval <- list(rho.c = rho.c, s.shift = v, l.shift = u, C.b = C.b, blalt = blalt ) # , nmissing = nmissing)
  }

  else if(ci == "z-transform"){
    rho.c <- as.data.frame(cbind(p, llt, ult))
    names(rho.c) <- c("est", lower, upper)
    rval <- list(rho.c = rho.c, s.shift = v, l.shift = u, C.b = C.b, blalt = blalt) #, nmissing = nmissing)
  }
  return(rval)
}




KrippAlpha <- function (x, method = c("nominal", "ordinal", "interval", "ratio")) {

  method  <-  match.arg(method)

  coincidence.matrix <- function(x) {
    levx <- (levels(as.factor(x)))
    nval <- length(levx)
    cm <- matrix(rep(0, nval * nval), nrow = nval)
    dimx <- dim(x)
    vn <- function(datavec) sum(!is.na(datavec))
    if(any(is.na(x))) mc <- apply(x, 2, vn) - 1
    else mc <- rep(1, dimx[2])
    for(col in 1:dimx[2]) {
      for(i1 in 1:(dimx[1] - 1)) {
        for(i2 in (i1 + 1):dimx[1]) {
          if(!is.na(x[i1, col]) && !is.na(x[i2, col])) {
            index1 <- which(levx == x[i1, col])
            index2 <- which(levx == x[i2, col])
            cm[index1, index2] <- cm[index1,index2] + (1 + (index1 == index2))/mc[col]
            if(index1 != index2) cm[index2,index1] <- cm[index1,index2]
          }
        }
      }
    }
    nmv  <-  sum(apply(cm, 2, sum))
    return(structure(list(method="Krippendorff's alpha",
                          subjects=dimx[2], raters=dimx[1],irr.name="alpha",
                          value=NA,stat.name="nil",statistic=NULL,
                          cm=cm,data.values=levx,nmatchval=nmv,data.level=NA),
                     class = "irrlist"))
  }

  ka <- coincidence.matrix(x)
  ka$data.level <- method
  dimcm <- dim(ka$cm)
  utcm <- as.vector(ka$cm[upper.tri(ka$cm)])
  diagcm <- diag(ka$cm)
  occ <- sum(diagcm)
  nc <- apply(ka$cm,1,sum)
  ncnc <- sum(nc * (nc - 1))
  dv <- as.numeric(ka$data.values)
  diff2 <- rep(0,length(utcm))
  ncnk <- rep(0,length(utcm))
  ck <- 1

  if (dimcm[2]<2)
    ka$value <- 1.0
  else {
    for(k in 2:dimcm[2]) {
      for(c in 1:(k-1)) {
        ncnk[ck] <- nc[c] * nc[k]
        if(match(method[1],"nominal",0)) diff2[ck] <- 1
        if(match(method[1],"ordinal",0)) {
          diff2[ck] <- nc[c]/2
          if(k > (c+1))
            for(g in (c+1):(k-1)) diff2[ck] <- diff2[ck] + nc[g]
            diff2[ck] <- diff2[ck]+nc[k]/2
            diff2[ck] <- diff2[ck]^2
        }
        if(match(method[1],"interval",0)) diff2[ck] <- (dv[c]-dv[k])^2
        if(match(method[1],"ratio",0)) diff2[ck] <- (dv[c]-dv[k])^2/(dv[c]+dv[k])^2
        ck <- ck+1
      }
    }
    ka$value <- 1-(ka$nmatchval-1)*sum(utcm*diff2)/sum(ncnk*diff2)
  }
  return(ka)
}



Entropy <- function(x, y = NULL, base = 2, ...) {

  # x is either a table or a vector if y is defined

  if(!is.null(y)) { x <- table(x, y, ...) }
  x <- as.matrix(x)

  ptab <- x / sum(x)
  H <- - sum( ifelse(ptab > 0, ptab * log(ptab, base=base), 0) )
  return(H)

}


MutInf <- function(x, y = NULL, base = 2, ...){
  # ### Ref.:  http://en.wikipedia.org/wiki/Cluster_labeling

  if(!is.null(y)) { x <- table(x, y, ...) }
  x <- as.matrix(x)

  return(
    Entropy(rowSums(x), base=base) +
      Entropy(colSums(x), base=base) - Entropy(x, base=base)
  )

}



# Rao's Diversity from ade4 divc
# author:

DivCoef <- function(df, dis = NULL, scale = FALSE){
  # checking of user's data and initialization.
  if (!inherits(df, "data.frame")) stop("Non convenient df")
  if (any(df < 0)) stop("Negative value in df")
  if (!is.null(dis)) {
    if (!inherits(dis, "dist")) stop("Object of class 'dist' expected for distance")
    if (!IsEuclid(dis)) warning("Euclidean property is expected for distance")
    dis <- as.matrix(dis)
    if (nrow(df)!= nrow(dis)) stop("Non convenient df")
    dis <- as.dist(dis)
  }
  if (is.null(dis)) dis <- as.dist((matrix(1, nrow(df), nrow(df))
                                    - diag(rep(1, nrow(df)))) * sqrt(2))
  div <- as.data.frame(rep(0, ncol(df)))
  names(div) <- "diversity"
  rownames(div) <- names(df)
  for (i in 1:ncol(df)) {
    if(sum(df[, i]) < 1e-16) div[i, ] <- 0
    else div[i, ] <- (t(df[, i]) %*% (as.matrix(dis)^2) %*% df[, i]) / 2 / (sum(df[, i])^2)
  }
  if(scale == TRUE){
    divmax <- DivCoefMax(dis)$value
    div <- div / divmax
  }
  return(div)
}

IsEuclid <- function (distmat, plot = FALSE, print = FALSE, tol = 1e-07) {

  "bicenter.wt" <- function (X, row.wt = rep(1, nrow(X)), col.wt = rep(1, ncol(X))) {
    X <- as.matrix(X)
    n <- nrow(X)
    p <- ncol(X)
    if (length(row.wt) != n)
      stop("length of row.wt must equal the number of rows in x")
    if (any(row.wt < 0) || (sr <- sum(row.wt)) == 0)
      stop("weights must be non-negative and not all zero")
    row.wt <- row.wt/sr
    if (length(col.wt) != p)
      stop("length of col.wt must equal the number of columns in x")
    if (any(col.wt < 0) || (st <- sum(col.wt)) == 0)
      stop("weights must be non-negative and not all zero")
    col.wt <- col.wt/st
    row.mean <- apply(row.wt * X, 2, sum)
    col.mean <- apply(col.wt * t(X), 2, sum)
    col.mean <- col.mean - sum(row.mean * col.wt)
    X <- sweep(X, 2, row.mean)
    X <- t(sweep(t(X), 2, col.mean))
    return(X)
  }

  if (!inherits(distmat, "dist"))
    stop("Object of class 'dist' expected")
  if(any(distmat<tol))
    warning("Zero distance(s)")
  distmat <- as.matrix(distmat)
  n <- ncol(distmat)
  delta <- -0.5 * bicenter.wt(distmat * distmat)
  lambda <- eigen(delta, symmetric = TRUE, only.values = TRUE)$values
  w0 <- lambda[n]/lambda[1]
  if (plot)
    barplot(lambda)
  if (print)
    print(lambda)
  return((w0 > -tol))
}


DivCoefMax <- function(dis, epsilon = 1e-008, comment = FALSE) {

  # inititalisation
  if(!inherits(dis, "dist")) stop("Distance matrix expected")
  if(epsilon <= 0) stop("epsilon must be positive")
  if(!IsEuclid(dis)) stop("Euclidean property is expected for dis")
  D2 <- as.matrix(dis)^2 / 2
  n <- dim(D2)[1]
  result <- data.frame(matrix(0, n, 4))
  names(result) <- c("sim", "pro", "met", "num")
  relax <- 0    # determination de la valeur initiale x0
  x0 <- apply(D2, 1, sum) / sum(D2)
  result$sim <- x0    # ponderation simple
  objective0 <- t(x0) %*% D2 %*% x0
  if (comment == TRUE)
    print("evolution of the objective function:")
  xk <- x0    # grande boucle de test des conditions de Kuhn-Tucker
  repeat {
    # boucle de test de nullite du gradient projete
    repeat {
      maxi.temp <- t(xk) %*% D2 %*% xk
      if(comment == TRUE) print(as.character(maxi.temp))
      #calcul du gradient
      deltaf <- (-2 * D2 %*% xk)
      # determination des contraintes saturees
      sature <- (abs(xk) < epsilon)
      if(relax != 0) {
        sature[relax] <- FALSE
        relax <- 0
      }
      # construction du gradient projete
      yk <- ( - deltaf)
      yk[sature] <- 0
      yk[!(sature)] <- yk[!(sature)] - mean(yk[!(
        sature)])
      # test de la nullite du gradient projete
      if (max(abs(yk)) < epsilon) {
        break
      }
      # determination du pas le plus grand compatible avec les contraintes
      alpha.max <- as.vector(min( - xk[yk < 0] / yk[yk <
                                                      0]))
      alpha.opt <- as.vector( - (t(xk) %*% D2 %*% yk) / (
        t(yk) %*% D2 %*% yk))
      if ((alpha.opt > alpha.max) | (alpha.opt < 0)) {
        alpha <- alpha.max
      }
      else {
        alpha <- alpha.opt
      }
      if (abs(maxi.temp - t(xk + alpha * yk) %*% D2 %*% (
        xk + alpha * yk)) < epsilon) {
        break
      }
      xk <- xk + alpha * yk
    }
    # verification des conditions de KT
    if (prod(!sature) == 1) {
      if (comment == TRUE)
        print("KT")
      break
    }
    vectD2 <- D2 %*% xk
    u <- 2 * (mean(vectD2[!sature]) - vectD2[sature])
    if (min(u) >= 0) {
      if (comment == TRUE)
        print("KT")
      break
    }
    else {
      if (comment == TRUE)
        print("relaxation")
      satu <- (1:n)[sature]
      relax <- satu[u == min(u)]
      relax <-relax[1]
    }
  }
  if (comment == TRUE)
    print(list(objective.init = objective0, objective.final
               = maxi.temp))
  result$num <- as.vector(xk, mode = "numeric")
  result$num[result$num < epsilon] <- 0
  # ponderation numerique
  xk <- x0 / sqrt(sum(x0 * x0))
  repeat {
    yk <- D2 %*% xk
    yk <- yk / sqrt(sum(yk * yk))
    if (max(xk - yk) > epsilon) {
      xk <- yk
    }
    else break
  }
  x0 <- as.vector(yk, mode = "numeric")
  result$pro <- x0 / sum(x0)    # ponderation propre
  result$met <- x0 * x0    # ponderation propre
  restot <- list()
  restot$value <- DivCoef(cbind.data.frame(result$num), dis)[,1]
  restot$vectors <- result
  return(restot)
}





# http://sph.bu.edu/otlt/MPH-Modules/BS/BS704_Confidence_Intervals/BS704_Confidence_Intervals8.html
# sas:    http://support.sas.com/documentation/cdl/en/statug/63347/HTML/default/viewer.htm#statug_surveyfreq_a0000000227.htm

# discussion:   http://tolstoy.newcastle.edu.au/R/e2/help/06/11/4982.html
#
# RelRisk0 <- function(x, conf.level = NA) {
#
#   rr <- (x[1,1]/sum(x[,1])) / (x[1,2]/sum(x[,2]))
#   if (is.na(conf.level)) {
#     res <- rr
#   } else {
#     sigma <- x[1,2]/(x[1,1]*sum(x[1,])) + x[2,2]/(x[2,1]*sum(x[2,]))
#     qn <- qnorm(1-(1-conf.level)/2)
#     ci <- exp(log(rr) + c(-1,1)*qn*sqrt(sigma))
#     res <- c("rel. risk"=rr, lwr.ci=ci[1], upr.ci=ci[2])
#   }
#   return(res)
# }


RelRisk <- function(x, y = NULL, conf.level = NA, method = c("score", "wald", "use.or"), delta = 0.5, ...) {

  if(!is.null(y)) x <- table(x, y, ...)

  p <- (d <- dim(x))[1L]
  if(!is.numeric(x) || length(d) != 2L || p != d[2L] || p !=2L)
    stop("'x' is not a 2x2 numeric matrix")

  x1 <- x[1,1]
  x2 <- x[2,1]
  n1 <- x[1,1] + x[1,2]
  n2 <- x[2,1] + x[2,2]

  rr <- (x[1,1]/sum(x[1,])) / (x[2,1]/sum(x[2,]))

  if( !is.na(conf.level)) {
    switch( match.arg( arg = method, choices = c("score", "wald", "use.or") )
            , "score" = {
              # source:
              # Agresti-Code:        http://www.stat.ufl.edu/~aa/cda/R/two-sample/R2/

              # R Code for large-sample score confidence interval for a relative risk
              # in a 2x2 table (Koopman 1984, Miettinen and Nurminen 1985, Nurminen 1986).

              z =  abs(qnorm((1-conf.level)/2))
              if ((x2==0) &&(x1==0)){
                ul = Inf
                ll = 0
              }
              else{
                a1 =  n2*(n2*(n2+n1)*x1+n1*(n2+x1)*(z^2))
                a2 = -n2*(n2*n1*(x2+x1)+2*(n2+n1)*x2*x1+n1*(n2+x2+2*x1)*(z^2))
                a3 = 2*n2*n1*x2*(x2+x1)+(n2+n1)*(x2^2)*x1+n2*n1*(x2+x1)*(z^2)
                a4 = -n1*(x2^2)*(x2+x1)
                b1 = a2/a1
                b2 = a3/a1
                b3 = a4/a1
                c1 = b2-(b1^2)/3
                c2 = b3-b1*b2/3+2*(b1^3)/27
                ceta = acos(sqrt(27)*c2/(2*c1*sqrt(-c1)))
                t1 = -2*sqrt(-c1/3)*cos(pi/3-ceta/3)
                t2 = -2*sqrt(-c1/3)*cos(pi/3+ceta/3)
                t3 = 2*sqrt(-c1/3)*cos(ceta/3)
                p01 = t1-b1/3
                p02 = t2-b1/3
                p03 = t3-b1/3
                p0sum = p01+p02+p03
                p0up = min(p01,p02,p03)
                p0low = p0sum-p0up-max(p01,p02,p03)

                if( (x2==0) && (x1!=0) ){
                  ll = (1-(n1-x1)*(1-p0low)/(x2+n1-(n2+n1)*p0low))/p0low
                  ul = Inf
                }
                else if( (x2!=n2) && (x1==0)){
                  ul = (1-(n1-x1)*(1-p0up)/(x2+n1-(n2+n1)*p0up))/p0up
                  ll = 0
                }
                else if( (x2==n2) && (x1==n1)){
                  ul = (n2+z^2)/n2
                  ll =  n1/(n1+z^2)
                }
                else if( (x1==n1) || (x2==n2) ){
                  if((x2==n2) && (x1==0)) { ll = 0 }
                  if((x2==n2) && (x1!=0)) {
                    phat1  = x2/n2
                    phat2  =  x1/n1
                    phihat = phat2/phat1
                    phil = 0.95*phihat
                    chi2 = 0
                    while (chi2 <= z){
                      a = (n2+n1)*phil
                      b = -((x2+n1)*phil+x1+n2)
                      c = x2+x1
                      p1hat = (-b-sqrt(b^2-4*a*c))/(2*a)
                      p2hat = p1hat*phil
                      q2hat = 1-p2hat
                      var = (n2*n1*p2hat)/(n1*(phil-p2hat)+n2*q2hat)
                      chi2 = ((x1-n1*p2hat)/q2hat)/sqrt(var)
                      ll = phil
                      phil = ll/1.0001}}
                  i = x2
                  j = x1
                  ni = n2
                  nj = n1
                  if( x1==n1 ){
                    i = x1
                    j = x2
                    ni = n1
                    nj = n2
                  }
                  phat1  = i/ni
                  phat2  =  j/nj
                  phihat = phat2/phat1
                  phiu = 1.1*phihat
                  if((x2==n2) && (x1==0)) {
                    if(n2<100) {phiu = .01}
                    else {phiu=0.001}
                  }
                  chi1 = 0
                  while (chi1 >= -z){
                    a = (ni+nj)*phiu
                    b = -((i+nj)*phiu+j+ni)
                    c = i+j
                    p1hat = (-b-sqrt(b^2-4*a*c))/(2*a)
                    p2hat = p1hat*phiu
                    q2hat = 1-p2hat
                    var = (ni*nj*p2hat)/(nj*(phiu-p2hat)+ni*q2hat)
                    chi1  = ((j-nj*p2hat)/q2hat)/sqrt(var)
                    phiu1 = phiu
                    phiu = 1.0001*phiu1
                  }

                  if(x1==n1) {
                    ul = (1-(n1-x1)*(1-p0up)/(x2+n1-(n2+n1)*p0up))/p0up
                    ll = 1/phiu1
                  }
                  else{ ul = phiu1}
                }

                else{
                  ul = (1-(n1-x1)*(1-p0up)/(x2+n1-(n2+n1)*p0up))/p0up
                  ll = (1-(n1-x1)*(1-p0low)/(x2+n1-(n2+n1)*p0low))/p0low
                }
              }
            }
            , "wald" = {
              # based on code by Michael Dewey, 2006

              x1.d <- x1 + delta
              x2.d <- x2 + delta
              lrr <- log(rr)
              se.lrr <- sqrt(1/x1.d - 1/n1 + 1/x2.d - 1/n2)
              mult <- abs(qnorm((1-conf.level)/2))
              ll <- exp(lrr - mult * se.lrr)
              ul <- exp(lrr + mult * se.lrr)
            }
            , "use.or" = {
              or <- OddsRatio(x, conf.level=conf.level)
              p2 <- x2/n2
              rr.ci <- or/((1-p2) + p2 * or)
              ll <- unname(rr.ci[2])
              ul <- unname(rr.ci[3])
            }
    )
  }

  if (is.na(conf.level)) {
    res <- rr
  } else {
    res <- c("rel. risk"=rr, lwr.ci=ll, upr.ci=ul)
  }
  return(res)

}



OddsRatio <- function (x, conf.level = NULL, ...) {
  UseMethod("OddsRatio")
}



OddsRatio.glm <- function(x, conf.level = NULL, digits=3, use.profile=TRUE, ...) {

  if(is.null(conf.level)) conf.level <- 0.95

  # Fasst die Ergebnisse eines binomialen GLMs als OR summary zusammen

  d.res <- data.frame(summary(x)$coefficients)
  names(d.res)[c(2,4)] <- c("Std. Error","Pr(>|z|)")

  d.res$or <- exp(d.res$Estimate)
  # ci or
  d.res$"or.lci" <- exp(d.res$Estimate + qnorm(0.025)*d.res$"Std. Error" )
  d.res$"or.uci" <- exp(d.res$Estimate + qnorm(0.975)*d.res$"Std. Error" )


  if(use.profile)
    ci <- exp(confint(x, level = conf.level))
  else
    ci <- exp(confint.default(x, level = conf.level))

  # exclude na coefficients here, as summary does not yield those
  d.res[, c("or.lci","or.uci")] <- ci[!is.na(coefficients(x)), ]
  
  d.res$sig <- Format(d.res$"Pr(>|z|)", fmt="*")
  d.res$pval <- Format(d.res$"Pr(>|z|)", fmt="p")

  # d.res["(Intercept)",c("or","or.lci","or.uci")] <- NA
  # d.res["(Intercept)","Pr(>|z|)"] <- "NA"
  # d.res["(Intercept)"," "] <- ""

  d.print <- data.frame(lapply(d.res[, 5:7], Format, digits=digits),
                        pval=d.res$pval, sig=d.res$sig, stringsAsFactors = FALSE)
  rownames(d.print) <- rownames(d.res)
  colnames(d.print)[4:5] <- c("Pr(>|z|)","")

  
  mterms <- {
    res <- lapply(labels(terms(x)), function(y) 
      colnames(model.matrix(formula(gettextf("~ 0 + %s", y)), data=model.frame(x))))
    names(res) <- labels(terms(x))
    res
  } 
  
  
  res <- list(or=d.print, call=x$call,
              BrierScore=BrierScore(x), PseudoR2=PseudoR2(x, which="all"), res=d.res,
              nobs=nobs(x), terms=mterms, model=x$model)

  class(res) <- "OddsRatio"

  return(res)

}


OddsRatio.multinom <- function(x, conf.level=NULL, digits=3, ...) {

  if(is.null(conf.level)) conf.level <- 0.95

  # class(x) <- class(x)[class(x)!="regr"]
  r.summary <- summary(x, Wald.ratios = TRUE)

  coe <- t(r.summary$coefficients)
  coe <- reshape(data.frame(coe, id=row.names(coe)), varying=1:ncol(coe), idvar="id"
                 , times=colnames(coe), v.names="or", direction="long")

  se <- t(r.summary$standard.errors)
  se <- reshape(data.frame(se), varying=1:ncol(se),
                times=colnames(se), v.names="se", direction="long")[, "se"]

  # d.res <- r.summary
  d.res <- data.frame(
    "or"= exp(coe[, "or"]),
    "or.lci" = exp(coe[, "or"] + qnorm(0.025) * se),
    "or.uci" = exp(coe[, "or"] - qnorm(0.025) * se),
    "pval" = 2*(1-pnorm(q = abs(coe[, "or"]/se), mean=0, sd=1)),
    "sig" = 2*(1-pnorm(q = abs(coe[, "or"]/se), mean=0, sd=1))
  )
  
  d.print <- data.frame(
    "or"= Format(exp(coe[, "or"]), digits=digits),
    "or.lci" = Format(exp(coe[, "or"] + qnorm(0.025) * se), digits=digits),
    "or.uci" = Format(exp(coe[, "or"] - qnorm(0.025) * se), digits=digits),
    "pval" = Format(2*(1-pnorm(q = abs(coe[, "or"]/se), mean=0, sd=1)), fmt="p", digits=3),
    "sig" = Format(2*(1-pnorm(q = abs(coe[, "or"]/se), mean=0, sd=1)), fmt="*"),
    stringsAsFactors = FALSE
  )

  colnames(d.print)[4:5] <- c("Pr(>|z|)","")
  rownames(d.print) <- paste(coe$time, coe$id, sep=":")
  
  rownames(d.res) <- rownames(d.print)

  res <- list(or = d.print, call = x$call,
              BrierScore = NA, # BrierScore(x),
              PseudoR2 = PseudoR2(x, which="all"), res=d.res)

  class(res) <- "OddsRatio"
  return(res)

}



OddsRatio.zeroinfl <- function (x, conf.level = NULL, digits = 3, ...) {

  if(is.null(conf.level)) conf.level <- 0.95

  d.res <- data.frame(summary(x)$coefficients$zero)
  names(d.res)[c(2, 4)] <- c("Std. Error", "Pr(>|z|)")

  d.res$or <- exp(d.res$Estimate)
  d.res$or.lci <- exp(d.res$Estimate + qnorm(0.025) * d.res$"Std. Error")
  d.res$or.uci <- exp(d.res$Estimate + qnorm(0.975) * d.res$"Std. Error")
  d.res["(Intercept)", c("or", "or.lci", "or.uci")] <- NA
  d.res$sig <- format(as.character(cut(d.res$"Pr(>|z|)", breaks = c(0,
                                                                    0.001, 0.01, 0.05, 0.1, 1), include.lowest = TRUE, labels = c("***",
                                                                                                                                  "**", "*", ".", " "))), justify = "left")
  d.res$"Pr(>|z|)" <- Format(d.res$"Pr(>|z|)", fmt="p")
  d.res["(Intercept)", "Pr(>|z|)"] <- "NA"
  d.res["(Intercept)", " "] <- ""
  d.print <- data.frame(lapply(d.res[, 5:7], Format, digits=digits),
                        p.value = d.res[,4], sig = d.res[, 8], stringsAsFactors = FALSE)

  rownames(d.print) <- rownames(d.res)
  res <- list(or = d.print, call = x$call,
              BrierScore = BrierScore(resp=(model.response(model.frame(x)) > 0) * 1L,
                                      pred=predict(x, type="zero")),
              PseudoR2 = PseudoR2(x, which="all"), res=d.res)

  class(res) <- "OddsRatio"

  return(res)
}


print.OddsRatio <- function(x, ...){

    cat("\nCall:\n")
    print(x$call)

    cat("\nOdds Ratios:\n")
    print(x$or)
    cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 \n\n")

    if(!is.null(x$BrierScore)){
      cat(gettextf("Brier Score: %s     Nagelkerke R2: %s\n\n",
                   round(x$BrierScore,3), round(x$PseudoR2["Nagelkerke"],3)))
    }

}



plot.OddsRatio <- function(x, intercept=FALSE, group=NULL, subset = NULL, ...){

  if(!intercept)
    # x$res <- x$res[rownames(x$res)!="(Intercept)", ]
    x$res <- x$res[!grepl("(Intercept)", rownames(x$res)), ]
  
  args <- list(...)

  # here the defaults
  args.errbars1 <- list(from=cbind(x$res$or, x$res$or.lci, x$res$or.uci))

  # overwrite with userdefined values
  if (!is.null(args[["args.errbars"]])) {
    args.errbars1[names(args[["args.errbars"]])] <- args[["args.errbars"]][]
    args[["args.errbars"]] <- NULL
  }

  # here the defaults for PlotDot
  args.plotdot1 <- list(x=x$res$or, args.errbars=args.errbars1, labels=rownames(x$res),
                        panel.first=quote(abline(v=1, col="grey")))

  if (!is.null(args)) {
    args.plotdot1[names(args)] <- args
  }

  do.call(PlotDot, args=args.plotdot1)

}




OddsRatio.default <- function(x, conf.level = NULL, y = NULL, method=c("wald", "mle", "midp")
                      , interval = c(0, 1000), ...) {

  if(!is.null(y)) x <- table(x, y, ...)

  if(is.null(conf.level)) conf.level <- NA

  p <- (d <- dim(x))[1L]
  if(!is.numeric(x) || length(d) != 2L || p != d[2L] || p != 2L)
    stop("'x' is not a 2x2 numeric matrix")

  switch( match.arg( arg = method, choices = c("wald", "mle", "midp") )
          , "wald" = {
            if (any(x == 0)) x <- x + 0.5
            lx <- log(x)
            or <- exp(lx[1, 1] + lx[2, 2] - lx[1, 2] - lx[2, 1])

            if(is.na(conf.level)){
              res <- or
            } else {
              # Agresti Categorical Data Analysis, 3.1.1
              sigma2lor <- sum(1/x)
              ci <- or * exp(c(1,-1) * qnorm((1-conf.level)/2) * sqrt(sigma2lor))
              res <- c("odds ratio"=or, lwr.ci=ci[1], upr.ci=ci[2])
            }
          }
          , "mle" = {
            if(is.na(conf.level)){
              res <- unname(fisher.test(x, conf.int=FALSE)$estimate)
            } else {
              res <- fisher.test(x, conf.level=conf.level)
              res <- c(res$estimate, lwr.ci=res$conf.int[1], upr.ci=res$conf.int[2])
            }
          }
          , "midp" = {

            # based on code from Tomas J. Aragon Developer <aragon at berkeley.edu>

            a1 <- x[1,1]; a0 <- x[1,2]; b1 <- x[2,1]; b0 <- x[2,2]; or <- 1

            # median-unbiased estimate function
            mue <- function(a1, a0, b1, b0, or){
              mm <- matrix(c(a1,a0,b1,b0), 2, 2, byrow=TRUE)
              fisher.test(mm, or=or, alternative="l")$p-fisher.test(x=x, or=or, alternative="g")$p
            }
            ##mid-p function
            midp <- function(a1, a0, b1, b0, or = 1){
              mm <- matrix(c(a1,a0,b1,b0),2,2, byrow=TRUE)
              lteqtoa1 <- fisher.test(mm,or=or,alternative="l")$p.val
              gteqtoa1 <- fisher.test(mm,or=or,alternative="g")$p.val
              0.5*(lteqtoa1-gteqtoa1+1)
            }

            # root finding
            EST <- uniroot(
              function(or){ mue(a1, a0, b1, b0, or)},
              interval = interval)$root

            if(is.na(conf.level)){
              res <- EST
            } else {

              alpha <- 1 - conf.level
              LCL <- uniroot(function(or){
                1-midp(a1, a0, b1, b0, or)-alpha/2
              },  interval = interval)$root
              UCL <- 1/uniroot(function(or){
                midp(a1, a0, b1, b0, or=1/or)-alpha/2
              },  interval = interval)$root

              res <- c("odds ratio" = EST, lwr.ci=LCL, upr.ci= UCL)
            }
          }
  )
  return(res)
}


## odds ratio (OR) to relative risk (RR)


ORToRelRisk <- function(...) {
  UseMethod("ORToRelRisk")
}
  

ORToRelRisk.default <- function(or, p0, ...) {
  
  if(any(or <= 0))
    stop("'or' has to be positive")
  
  if(!all(ZeroIfNA(p0) %[]% c(0,1)))
    stop("'p0' has to be in (0,1)")
  
  or / (1 - p0 + p0*or)
  
}



ORToRelRisk.OddsRatio <- function(x, ...){
  
  .PredPrevalence <- function(model) {
    
    isNumericPredictor <- function(model, term){
      unname(attr(attr(model, "terms"), "dataClasses")[term] == "numeric")
    }
    
    # mean of response ist used for all numeric predictors
    meanresp <- mean(as.numeric(model.response(model)) - 1)
    # this is ok, as the second level is the one we predict in glm
    # https://stackoverflow.com/questions/23282048/logistic-regression-defining-reference-level-in-r
    
    preds <- attr(terms(model), "term.labels")

    # first the intercept
    res <- NA_real_
    
    for(i in seq_along(preds))
      if(isNumericPredictor(model=model, term=preds[i]))
        res <- c(res, meanresp)
      else {
        # get the proportions of the levels of the factor with the response ...
        fprev <- prop.table(table(model.frame(model)[, preds[i]], 
                                  model.response(model)), 1)
        # .. and use the proportion of positive response of the reference level
        res <- c(res, rep(fprev[1, 2], times=nrow(fprev)-1))
    }
 
    return(res)
  }
 
  
  or <- x$res[, c("or", "or.lci", "or.uci")]
  pprev <-  .PredPrevalence(x$model)
  
  res <- sapply(or, function(x) ORToRelRisk(x, pprev))
  rownames(res) <- rownames(or)
  colnames(res) <- c("rr", "rr.lci", "rr.uci")
  
  return(res)  
  
} 






# Cohen, Jacob. 1988. Statistical power analysis for the behavioral
# sciences, (2nd edition). Lawrence Erlbaum Associates, Hillsdale, New
# Jersey, United States.

# Garson, G. David. 2007. Statnotes: Topics in Multivariate
# Analysis. URL:
# http://www2.chass.ncsu.edu/garson/pa765/statnote.htm. Visited Spring
# 2006 -- Summer 2007.

# Goodman, Leo A. and William H. Kruskal. 1954. Measures of Association
# for Cross-Classifications. Journal of the American Statistical
# Association, Vol. 49, No. 268 (December 1954), pp. 732-764.

# Liebetrau, Albert M. 1983. Measures of Association. Sage University
# Paper series on Quantitative Applications in the Social Sciences,
# 07-032. Sage Publications, Beverly Hills and London, United
# States/England.

# Margolin, Barry H. and Richard J. Light. 1974. An Analysis of Variance
# for Categorical Data II: Small Sample Comparisons with Chi Square and
# Other Competitors. Journal of the American Statistical Association,
# Vol. 69, No. 347 (September 1974), pp. 755-764.

# Reynolds, H. T. 1977. Analysis of Nominal Data. Sage University Paper
# series on Quantitative Applications in the Social Sciences, 08-007,
# Sage Publications, Beverly Hills/London, California/UK.

# SAS Institute. 2007. Measures of Association
# http://support.sas.com/onlinedoc/913/getDoc/en/statug.hlp/freq_sect20.htm
# Visited January 2007.

# Theil, Henri. 1970. On the Estimation of Relationships Involving
# Qualitative Variables.  The American Journal of Sociology, Vol. 76,
# No. 1 (July 1970), pp. 103-154.

# N.B. One should use the values for the significance of the
# Goodman-Kruskal lambda and Theil's UC with reservation, as these
# have been modeled to mimic the behavior of the same statistics
# in SPSS.



GoodmanKruskalTau <- function(x, y = NULL, direction = c("row", "column"), conf.level = NA, ...){

  if(!is.null(y)) x <- table(x, y, ...)

  n <- sum(x)
  n.err.unconditional <- n^2
  sum.row <- rowSums(x)
  sum.col <- colSums(x)

  switch( match.arg( arg = direction, choices = c("row", "column") )
          , "column" = {             # Tau Column|Row

            for(i in 1:nrow(x))
              n.err.unconditional <- n.err.unconditional-n*sum(x[i,]^2/sum.row[i])
            n.err.conditional <- n^2-sum(sum.col^2)
            tau.CR <- 1-(n.err.unconditional/n.err.conditional)
            v <- n.err.unconditional/(n^2)
            d <- n.err.conditional/(n^2)
            f <- d*(v+1)-2*v
            var.tau.CR <- 0
            for(i in 1:nrow(x))
              for(j in 1:ncol(x))
                var.tau.CR <- var.tau.CR + x[i,j]*(-2*v*(sum.col[j]/n)+d*((2*x[i,j]/sum.row[i])-sum((x[i,]/sum.row[i])^2))-f)^2/(n^2*d^4)
            ASE.tau.CR <- sqrt(var.tau.CR)
            est <- tau.CR
            sigma2 <- ASE.tau.CR^2
          }
          , "row" = {             # Tau Row|Column

            for(j in 1:ncol(x))
              n.err.unconditional <- n.err.unconditional-n*sum(x[,j]^2/sum.col[j])
            n.err.conditional <- n^2-sum(sum.row^2)
            tau.RC <- 1-(n.err.unconditional/n.err.conditional)
            v <- n.err.unconditional/(n^2)
            d <- n.err.conditional/(n^2)
            f <- d*(v+1)-2*v
            var.tau.RC <- 0
            for(i in 1:nrow(x))
              for(j in 1:ncol(x))
                var.tau.RC <- var.tau.RC + x[i,j]*(-2*v*(sum.row[i]/n)+d*((2*x[i,j]/sum.col[j])-sum((x[,j]/sum.col[j])^2))-f)^2/(n^2*d^4)
            ASE.tau.RC <- sqrt(var.tau.RC)
            est <- tau.RC
            sigma2 <- ASE.tau.RC^2
          }
  )

  if(is.na(conf.level)){
    res <- est
  } else {
    pr2 <- 1 - (1 - conf.level)/2
    ci <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + est
    res <- c(tauA=est, lwr.ci=ci[1], upr.ci=ci[2])
  }

  return(res)
}


# good description
# http://salises.mona.uwi.edu/sa63c/Crosstabs%20Measures%20for%20Nominal%20Data.htm

Lambda <- function(x, y = NULL, direction = c("symmetric", "row", "column"), conf.level = NA, ...){

  if(!is.null(y)) x <- table(x, y, ...)

  # Guttman'a lambda (1941), resp. Goodman Kruskal's Lambda (1954)

  n <- sum(x)
  csum <- colSums(x)
  rsum <- rowSums(x)
  rmax <- apply(x, 1, max)
  cmax <- apply(x, 2, max)
  max.rsum <- max(rsum)
  max.csum <- max(csum)

  nr <- nrow(x)
  nc <- ncol(x)

  switch( match.arg( arg = direction, choices = c("symmetric", "row", "column") )
          , "symmetric" = { res <- 0.5*(sum(rmax, cmax) - (max.csum +  max.rsum)) / (n - 0.5*(max.csum +  max.rsum)) }
          , "column" = { res <- (sum(rmax) - max.csum) / (n - max.csum) }
          , "row" = { res <- (sum(cmax) - max.rsum) / (n - max.rsum) }
  )

  if(is.na(conf.level)){
    res <- res
  } else {

    L.col <- matrix(,nc)
    L.row <- matrix(,nr)

    switch( match.arg( arg = direction, choices = c("symmetric", "row", "column") )
            , "symmetric" = {

              #     How to see:
              #     http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
              #     pp. 1744
              #     Author:   Nina

              l <- which.max(csum)
              k <- which.max(rsum)
              li <- apply(x,1,which.max)
              ki <- apply(x,2,which.max)

              w <- 2*n-max.csum-max.rsum
              v <- 2*n -sum(rmax,cmax)
              xx <- sum(rmax[li==l], cmax[ki==k], rmax[k], cmax[l])
              y <- 8*n-w-v-2*xx

              t <- rep(NA, length(li))
              for (i in 1:length(li)){
                t[i] <- (ki[li[i]]==i & li[ki[li[i]]]==li[i])
              }

              sigma2 <- 1/w^4*(w*v*y-2 *w^2*(n - sum(rmax[t]))-2*v^2*(n-x[k,l]))

            }
            , "column" = {
              L.col.max <- min(which(csum == max.csum))
              for(i in 1:nr) {
                if(length(which(x[i, intersect(which(x[i,] == max.csum), which(x[i,] == max.rsum))] == n))>0)
                  L.col[i] <- min(which(x[i, intersect(which(x[i,] == max.csum), which(x[i,] == max.rsum))] == n))
                else
                  if(x[i, L.col.max] == max.csum)
                    L.col[i] <- L.col.max
                  else
                    L.col[i] <- min(which(x[i,] == rmax[i]))
              }
              sigma2 <- (n-sum(rmax))*(sum(rmax) + max.csum -
                                         2*(sum(rmax[which(L.col == L.col.max)])))/(n-max.csum)^3
            }
            , "row" = {
              L.row.max <- min(which(rsum == max.rsum))
              for(i in 1:nc) {
                if(length(which(x[intersect(which(x[,i] == max.rsum), which(x[,i] == max.csum)),i] == n))>0)
                  L.row[i] <- min(which(x[i,intersect(which(x[i,] == max.csum), which(x[i,] == max.rsum))] == n))
                else
                  if(x[L.row.max,i] == max.rsum)
                    L.row[i] <- L.row.max
                  else
                    L.row[i] <- min(which(x[,i] == cmax[i]))
              }
              sigma2 <- (n-sum(cmax))*(sum(cmax) + max.rsum -
                                         2*(sum(cmax[which(L.row == L.row.max)])))/(n-max.rsum)^3
            }
    )

    pr2 <- 1 - (1 - conf.level)/2
    ci <- pmin(1, pmax(0, qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + res))
    res <- c(lambda = res,  lwr.ci=ci[1], upr.ci=ci[2])
  }

  return(res)
}



UncertCoef <- function(x, y = NULL, direction = c("symmetric", "row", "column"),
                       conf.level = NA, p.zero.correction = 1/sum(x)^2, ... ) {
  # Theil's UC (1970)
  # slightly nudge zero values so that their logarithm can be calculated (cf. Theil 1970: x->0 => xlogx->0)
  if(!is.null(y)) x <- table(x, y, ...)

  x[x == 0] <- p.zero.correction

  n <- sum(x)
  rsum <- rowSums(x)
  csum <- colSums(x)

  hx <- -sum((apply(x, 1, sum) * log(apply(x, 1, sum)/n))/n)
  hy <- -sum((apply(x, 2, sum) * log(apply(x, 2, sum)/n))/n)
  hxy <- -sum(apply(x, c(1, 2), sum) * log(apply(x, c(1, 2), sum)/n)/n)

  switch( match.arg( arg = direction, choices = c("symmetric", "row", "column") )
          , "symmetric" = { res <- 2 * (hx + hy - hxy)/(hx + hy) }
          , "row" = { res <- (hx + hy - hxy)/hx }
          , "column" = { res <- (hx + hy - hxy)/hy }
  )

  if(!is.na(conf.level)){
    var.uc.RC <- var.uc.CR <- 0
    for(i in 1:nrow(x))
      for(j in 1:ncol(x))
      { var.uc.RC <- var.uc.RC + x[i,j]*(hx*log(x[i,j]/csum[j])+((hy-hxy)*log(rsum[i]/n)))^2/(n^2*hx^4);
      var.uc.CR <- var.uc.CR + x[i,j]*(hy*log(x[i,j]/rsum[i])+((hx-hxy)*log(csum[j]/n)))^2/(n^2*hy^4);
      }
    switch( match.arg( arg = direction, choices = c("symmetric", "row", "column") )
            , "symmetric" = {
              sigma2 <- 4*sum(x * (hxy * log(rsum %o% csum/n^2) - (hx+hy)*log(x/n))^2 ) /
                (n^2*(hx+hy)^4)
            }
            , "row" = { sigma2 <- var.uc.RC }
            , "column" = { sigma2 <- var.uc.CR }
    )

    pr2 <- 1 - (1 - conf.level)/2
    ci <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + res

    res <- c(uc = res,  lwr.ci=max(ci[1], -1), upr.ci=min(ci[2], 1))
  }
  return(res)
}


TheilU <- function(a, p, type = c(2, 1), na.rm = FALSE){

  if(na.rm) {
    idx <- complete.cases(a, p)
    a <- a[idx]
    p <- p[idx]
  }
  n <- length(a)
  if(length(p)!=n) {
    warning("a must have same length as p")
    res <- NA
  } else {
    switch( match.arg(as.character(type), c("2", "1"))
            , "1" = { res <- sqrt(sum((a-p)^2/n))/(sqrt(sum(a^2)/n) + sqrt(sum(p^2)/n)) }
            , "2" = { res <- sqrt(sum((a-p)^2))/(sqrt(sum(a^2))) }
    )
  }
  return(res)

}


#S function SomersDelta
#
#    Calculates concordance probability and Somers'  Dxy  rank  correlation
#    between  a  variable  X  (for  which  ties are counted) and a binary
#    variable Y (having values 0 and 1, for which ties are not  counted).
#    Uses short cut method based on average ranks in two groups.
#
#    Usage:
#
#         SomersDelta(x, y, weights)
#
#    Returns vector whose elements are C Index, Dxy, n and missing, where
#    C Index is the concordance probability and Dxy=2(C Index-.5).
#
#    F. Harrell 28 Nov 90     6 Apr 98: added weights
#
# SomersDelta2 <- function(x, y, weights=NULL, normwt=FALSE, na.rm=TRUE) {
#
#   wtd.mean <- function(x, weights=NULL, normwt='ignored', na.rm=TRUE)
#   {
#     if(!length(weights)) return(mean(x, na.rm=na.rm))
#     if(na.rm) {
#       s <- !is.na(x + weights)
#       x <- x[s]
#       weights <- weights[s]
#     }
#
#     sum(weights*x)/sum(weights)
#   }
#
#   wtd.table <- function(x, weights=NULL, type=c('list','table'),
#                         normwt=FALSE, na.rm=TRUE)
#   {
#     type <- match.arg(type)
#     if(!length(weights))
#       weights <- rep(1, length(x))
#
#     isdate <- IsDate(x)  ### 31aug02 + next 2
#     ax <- attributes(x)
#     ax$names <- NULL
#     x <- if(is.character(x)) as.category(x)
#          else unclass(x)
#
#     lev <- levels(x)
#     if(na.rm) {
#       s <- !is.na(x + weights)
#       x <- x[s,drop=FALSE]    ### drop is for factor class
#       weights <- weights[s]
#     }
#
#     n <- length(x)
#     if(normwt)
#       weights <- weights*length(x)/sum(weights)
#
#     i <- order(x)  ### R does not preserve levels here
#     x <- x[i]; weights <- weights[i]
#
#     if(any(diff(x)==0)) {  ### slightly faster than any(duplicated(xo))
#       weights <- tapply(weights, x, sum)
#       if(length(lev)) {    ### 3apr03
#         levused <- lev[sort(unique(x))]  ### 7sep02
#         ### Next 3 lines 21apr03
#         if((length(weights) > length(levused)) &&
#            any(is.na(weights)))
#           weights <- weights[!is.na(weights)]
#
#         if(length(weights) != length(levused))
#           stop('program logic error')
#
#         names(weights) <- levused   ### 10Apr01  length 16May01
#       }
#
#       if(!length(names(weights)))
#         stop('program logic error')  ### 16May01
#
#       if(type=='table')
#         return(weights)
#
#       x <- all.is.numeric(names(weights),'vector')
#       if(isdate)
#         attributes(x) <- c(attributes(x),ax)   ### 31aug02
#
#       names(weights) <- NULL
#       return(list(x=x, sum.of.weights=weights))
#     }
#
#     xx <- x  ### 31aug02
#     if(isdate)
#       attributes(xx) <- c(attributes(xx),ax)
#
#     if(type=='list')
#       list(x=if(length(lev))lev[x]
#              else xx,
#            sum.of.weights=weights)
#     else {
#       names(weights) <- if(length(lev)) lev[x]
#                         else xx
#       weights
#     }
#   }
#
#
#   wtd.rank <- function(x, weights=NULL, normwt=FALSE, na.rm=TRUE)
#   {
#     if(!length(weights))
#       return(rank(x),na.last=if(na.rm)NA else TRUE)
#
#     tab <- wtd.table(x, weights, normwt=normwt, na.rm=na.rm)
#
#     freqs <- tab$sum.of.weights
#     ### rank of x = ### <= x - .5 (# = x, minus 1)
#     r <- cumsum(freqs) - .5*(freqs-1)
#     ### Now r gives ranks for all unique x values.  Do table look-up
#     ### to spread these ranks around for all x values.  r is in order of x
#     approx(tab$x, r, xout=x)$y
#   }
#
#
#   if(length(y)!=length(x))stop("y must have same length as x")
#   y <- as.integer(y)
#   wtpres <- length(weights)
#   if(wtpres && (wtpres != length(x)))
#     stop('weights must have same length as x')
#
#   if(na.rm) {
#       miss <- if(wtpres) is.na(x + y + weights)
#       else is.na(x + y)
#
#       nmiss <- sum(miss)
#       if(nmiss>0) {
#           miss <- !miss
#           x <- x[miss]
#           y <- y[miss]
#           if(wtpres) weights <- weights[miss]
#         }
#     }
#   else nmiss <- 0
#
#   u <- sort(unique(y))
#   if(any(! y %in% 0:1)) stop('y must be binary')
#
#   if(wtpres) {
#       if(normwt)
#         weights <- length(x)*weights/sum(weights)
#       n <- sum(weights)
#     }
#   else n <- length(x)
#
#   if(n<2) stop("must have >=2 non-missing observations")
#
#   n1 <- if(wtpres)sum(weights[y==1]) else sum(y==1)
#
#   if(n1==0 || n1==n)
#     return(c(C=NA,Dxy=NA,n=n,Missing=nmiss))
#
#   mean.rank <- if(wtpres)
#       wtd.mean(wtd.rank(x, weights, na.rm=FALSE), weights*y)
#     else
#       mean(rank(x)[y==1])
#
#   c.index <- (mean.rank - (n1+1)/2)/(n-n1)
#   dxy <- 2*(c.index-.5)
#   r <- c(c.index, dxy, n, nmiss)
#   names(r) <- c("C", "Dxy", "n", "Missing")
#   r
# }
#


SomersDelta <- function(x,  y = NULL, direction=c("row","column"), conf.level = NA, ...) {

  if(!is.null(y)) tab <- table(x, y, ...)
  else tab <- as.table(x)

  # tab is a matrix of counts
  x <- ConDisPairs(tab)

  # use .DoCount
  #   if(is.na(conf.level)) {
  #     d.tab <- as.data.frame.table(tab)
  #     x <- .DoCount(d.tab[,1], d.tab[,2], d.tab[,3])
  #   } else {
  #     x <- ConDisPairs(tab)
  #   }

  m <- min(dim(tab))
  n <- sum(tab)
  switch( match.arg( arg = direction, choices = c("row","column") )
          , "row" = { ni. <- colSums(tab) }
          , "column" = { ni. <- rowSums(tab) }
  )
  wt <- n^2 - sum(ni.^2)
  # Asymptotic standard error: sqrt(sigma2)
  sigma2 <- 4/wt^4 * (sum(tab * (wt*(x$pi.c - x$pi.d) - 2*(x$C-x$D)*(n-ni.))^2))
  # debug: print(sqrt(sigma2))

  somers <- (x$C - x$D) / (n * (n-1) /2 - sum(ni. * (ni. - 1) /2 ))

  pr2 <- 1 - (1 - conf.level)/2
  ci <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + somers

  if(is.na(conf.level)){
    result <- somers
  } else {
    result <- c(somers = somers,  lwr.ci=max(ci[1], -1), upr.ci=min(ci[2], 1))
  }

  return(result)

}


# Computes rank correlation measures between a variable X and a possibly
# censored variable Y, with event/censoring indicator EVENT
# Rank correlation is extension of Somers' Dxy = 2(Concordance Prob-.5)
# See Harrell et al JAMA 1984(?)
# Set outx=T to exclude ties in X from computations (-> Goodman-Kruskal
#  gamma-type rank correlation)
# based on rcorr.cens in Hmisc, https://stat.ethz.ch/pipermail/r-help/2003-March/030837.html
# author Frank Harrell


# GoodmanGammaF <- function(x, y) {

# ### Fortran implementation of Concordant/Discordant, but still O(n^2)

# x <- as.numeric(x)
# y <- as.numeric(y)

# event <-  rep(TRUE, length(x))
# if(length(y)!=length(x))
# stop("y must have same length as x")

# outx <- TRUE
# n <- length(x)
# ne <- sum(event)

# z <- .Fortran("cidxcn", x, y, event, length(x), nrel=double(1), nconc=double(1),
# nuncert=double(1),
# c.index=double(1), gamma=double(1), sd=double(1), as.logical(outx)
# )

# r <- c(z$c.index, z$gamma, z$sd, n, ne, z$nrel, z$nconc, z$nuncert)
# names(r) <- c("C Index","Dxy","S.D.","n","uncensored",
# "Relevant Pairs",
# "Concordant","Uncertain")
# unname(r[2])

# }


# GoodmanGamma(as.numeric(d.frm$Var1), as.numeric(d.frm$Var2))
# cor(as.numeric(d.frm$Var1), as.numeric(d.frm$Var2))

GoodmanKruskalGamma <- function(x, y = NULL, conf.level = NA, ...) {

  if(!is.null(y)) tab <- table(x, y, ...)
  else tab <- as.table(x)

  # tab is a matrix of counts
  # Based on code of Michael Friendly and Laura Thompson
  # Confidence interval calculation and output from Greg Rodd

  x <- ConDisPairs(tab)

  psi <- 2 * (x$D * x$pi.c - x$C * x$pi.d)/(x$C + x$D)^2
  # Asymptotic standard error: sqrt(sigma2)
  sigma2 <- sum(tab * psi^2) - sum(tab * psi)^2

  gamma <- (x$C - x$D)/(x$C + x$D)

  if(is.na(conf.level)){
    result <- gamma
  } else {
    pr2 <- 1 - (1 - conf.level)/2
    ci <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + gamma
    result <- c(gamma = gamma,  lwr.ci=max(ci[1], -1), upr.ci=min(ci[2], 1))
  }

  return(result)

}


# KendallTauB.table <- function(tab, conf.level = NA) {

# http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
# pp 1738

# tab is a matrix of counts

# x <- ConDisPairs(tab)

# n <- sum(tab)
# ni. <- apply(tab, 1, sum)
# n.j <- apply(tab, 2, sum)
# wr <- n^2 - sum(ni.^2)
# wc <- n^2 - sum(n.j^2)
# w <- sqrt(wr * wc)
# vij <- ni. * wc + n.j * wr
# dij <- x$pi.c - x$pi.d    ### Aij - Dij

# Asymptotic standard error: sqrt(sigma2)
# sigma2 <- 1/w^4 * (sum(tab * (2*w*dij + taub*vij)^2) - n^3 * taub^2 * (wr + wc)^2)

# this is the H0 = 0 variance:
# sigma2 <- 4/(wr * wc) * (sum(tab * (x$pi.c - x$pi.d)^2) - 4*(x$C - x$D)^2/n )


# taub <- 2*(x$C - x$D)/sqrt(wr * wc)

# if(is.na(conf.level)){
# result <- taub
# } else {
# pr2 <- 1 - (1 - conf.level)/2
# ci <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + taub
# result <- c(taub = taub,  lwr.ci=max(ci[1], -1), ups.ci=min(ci[2], 1))
# }

# return(result)

# }



KendallTauA <- function(x, y = NULL, direction = c("row", "column"), conf.level = NA, ...){

  if(!is.null(y)) tab <- table(x, y, ...)
  else tab <- as.table(x)

  x <- ConDisPairs(tab)

  n <- sum(tab)
  n0 <- n*(n-1)/2

  taua <- (x$C - x$D) / n0

  # Hollander, Wolfe pp. 415/416
  # think we should not consider ties here, so take only the !=0 part
  Ci <- as.vector((x$pi.c - x$pi.d) * (tab!=0))
  Ci <- Ci[Ci!=0]
  C_ <- sum(Ci)/n
  sigma2 <- 2/(n*(n-1)) * ((2*(n-2))/(n*(n-1)^2) * sum((Ci - C_)^2) + 1 - taua^2)

  if (is.na(conf.level)) {
    result <- taua
  }
  else {

    pr2 <- 1 - (1 - conf.level)/2
    ci <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + taua
    result <- c(tau_a = taua, lwr.ci = max(ci[1], -1), upr.ci = min(ci[2], 1))
  }

  return(result)

}


# KendallTauB <- function(x, y = NULL, conf.level = NA, test=FALSE, alternative = c("two.sided", "less", "greater"), ...){
KendallTauB <- function(x, y = NULL, conf.level = NA, ...){

  # Ref: http://www.fs.fed.us/psw/publications/lewis/LewisHMP.pdf
  # pp 2-9
  #
  if (!is.null(y)) {
    dname <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
  } else {
    dname <- deparse(substitute(x))
  }

  if(!is.null(y)) tab <- table(x, y, ...)
  else tab <- as.table(x)

  x <- ConDisPairs(tab)

  n <- sum(tab)
  n0 <- n*(n-1)/2
  ti <- rowSums(tab)  # apply(tab, 1, sum)
  uj <- colSums(tab)  # apply(tab, 2, sum)
  n1 <- sum(ti * (ti-1) / 2)
  n2 <- sum(uj * (uj-1) / 2)

  taub <- (x$C - x$D) / sqrt((n0-n1)*(n0-n2))

  pi <- tab / sum(tab)

  pdiff <- (x$pi.c - x$pi.d) / sum(tab)
  Pdiff <- 2 * (x$C - x$D) / sum(tab)^2

  rowsum <- rowSums(pi)  # apply(pi, 1, sum)
  colsum <- colSums(pi)  # apply(pi, 2, sum)

  rowmat <- matrix(rep(rowsum, dim(tab)[2]), ncol = dim(tab)[2])
  colmat <- matrix(rep(colsum, dim(tab)[1]), nrow = dim(tab)[1], byrow = TRUE)

  delta1 <- sqrt(1 - sum(rowsum^2))
  delta2 <- sqrt(1 - sum(colsum^2))

  # Compute asymptotic standard errors taub
  tauphi <- (2 * pdiff + Pdiff * colmat) * delta2 * delta1 + (Pdiff * rowmat * delta2)/delta1
  sigma2 <- ((sum(pi * tauphi^2) - sum(pi * tauphi)^2)/(delta1 * delta2)^4) / n
  
  # for very small pi/tauph it's possible that sigma2 gets negative so we cut small negative values here
  # example:  KendallTauB(table(iris$Species, iris$Species))
  if(sigma2 < .Machine$double.eps * 10) sigma2 <- 0

  if (is.na(conf.level)) {
    result <- taub
  }
  else {
    pr2 <- 1 - (1 - conf.level)/2
    ci <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + taub
    result <- c(tau_b = taub, lwr.ci = max(ci[1], -1), upr.ci = min(ci[2], 1))
  }

  #   if(test){
  #
  #     alternative <- match.arg(alternative)
  #
  #     zstat <- taub / sqrt(sigma2)
  #
  #     if (alternative == "less") {
  #       pval <- pnorm(zstat)
  #       cint <- c(-Inf, zstat + qnorm(conf.level))
  #     }
  #     else if (alternative == "greater") {
  #       pval <- pnorm(zstat, lower.tail = FALSE)
  #       cint <- c(zstat - qnorm(conf.level), Inf)
  #     }
  #     else {
  #       pval <- 2 * pnorm(-abs(zstat))
  #       alpha <- 1 - conf.level
  #       cint <- qnorm(1 - alpha/2)
  #       cint <- zstat + c(-cint, cint)
  #     }
  #
  #     RVAL <- list()
  #     RVAL$p.value <- pval
  #     RVAL$method <- "Kendall's rank correlation tau"
  #     RVAL$data.name <- dname
  #     RVAL$statistic <- x$C - x$D
  #     names(RVAL$statistic) <- "T"
  #     RVAL$estimate <- taub
  #     names(RVAL$estimate) <- "tau-b"
  #     RVAL$conf.int <- c(max(ci[1], -1), min(ci[2], 1))
  #   #  attr(RVAL$conf.int, "conf.level") = round(attr(ci,"conf.level"), 3)
  #     class(RVAL) <- "htest"
  #     return(RVAL)
  #
  # #     rval <- list(statistic = zstat, p.value = pval,
  # #                  parameter = sd_pop,
  # #                  conf.int = cint, estimate = estimate, null.value = mu,
  # #                  alternative = alternative, method = method, data.name = dname)
  #
  #   } else {
  return(result)
  #   }

}

# KendallTauB(x, y, conf.level = 0.95, test=TRUE)
#
# cor.test(x,y, method="kendall")

# tab <- as.table(rbind(c(26,26,23,18,9),c(6,7,9,14,23)))
# KendallTauB(tab, conf.level = 0.95)
# Assocs(tab)

StuartTauC <- function(x, y = NULL, conf.level = NA, ...) {

  if(!is.null(y)) tab <- table(x, y, ...)
  else tab <- as.table(x)

  # Reference:
  # http://v8doc.sas.com/sashtml/stat/chap28/sect18.htm
  x <- ConDisPairs(tab)

  m <- min(dim(tab))
  n <- sum(tab)
  # Asymptotic standard error: sqrt(sigma2)
  sigma2 <- 4 * m^2 / ((m-1)^2 * n^4) * (sum(tab * (x$pi.c - x$pi.d)^2) - 4 * (x$C -x$D)^2/n)
  # debug: print(sqrt(sigma2))

  # Tau-c = (C - D)*[2m/(n2(m-1))]
  tauc <- (x$C - x$D) * 2 * min(dim(tab)) / (sum(tab)^2*(min(dim(tab))-1))

  if(is.na(conf.level)){
    result <- tauc
  } else {
    pr2 <- 1 - (1 - conf.level)/2
    CI <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + tauc
    result <- c(tauc = tauc,  lwr.ci=max(CI[1], -1), upr.ci=min(CI[2], 1))
  }

  return(result)

}




# SpearmanRho <- function(x, y = NULL, use = c("everything", "all.obs", "complete.obs",
#                                              "na.or.complete","pairwise.complete.obs"), conf.level = NA ) {
# 
#   if(is.null(y)) {
#     x <- Untable(x)
#     y <- x[,2]
#     x <- x[,1]
#   }
#   # Reference:
#   #   https://stat.ethz.ch/pipermail/r-help/2006-October/114319.html
#   # fisher z transformation for calc SpearmanRho ci :
#   # Conover WJ, Practical Nonparametric Statistics (3rd edition). Wiley 1999.
# 
#   # http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
#   # pp 1738
# 
# 
#   # n <- sum(tab)
#   # ni. <- apply(tab, 1, sum)
#   # n.j <- apply(tab, 2, sum)
#   # F <- n^3 - sum(ni.^3)
#   # G <- n^3 - sum(n.j^3)
#   # w <- 1/12*sqrt(F * G)
# 
#   # ### Asymptotic standard error: sqrt(sigma2)
#   # sigma2 <- 1
#   # ### debug: print(sqrt(sigma2))
# 
#   # ### Tau-c = (C - D)*[2m/(n2(m-1))]
#   # est <- 1
# 
#   # if(is.na(conf.level)){
#   # result <- tauc
#   # } else {
#   # pr2 <- 1 - (1 - conf.level)/2
#   # CI <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + est
#   # result <- c(SpearmanRho = est,  lwr.ci=max(CI[1], -1), ups.ci=min(CI[2], 1))
#   # }
# 
#   # return(result)
# 
# 
#   # Ref:
#   # http://www-01.ibm.com/support/docview.wss?uid=swg21478368
# 
#   use <- match.arg(use, choices=c("everything", "all.obs", "complete.obs",
#                                   "na.or.complete","pairwise.complete.obs"))
# 
#   rho <- cor(as.numeric(x), as.numeric(y), method="spearman", use = use)
# 
#   e_fx <- exp( 2 * ((.5 * log((1+rho) / (1-rho))) - c(1, -1) *
#                       (abs(qnorm((1 - conf.level)/2))) * (1 / sqrt(sum(complete.cases(x,y)) - 3)) ))
#   ci <- (e_fx - 1) / (e_fx + 1)
# 
#   if (is.na(conf.level)) {
#     result <- rho
#   } else {
#     pr2 <- 1 - (1 - conf.level) / 2
#     result <- c(rho = rho, lwr.ci = max(ci[1], -1), upr.ci = min(ci[2], 1))
#   }
#   return(result)
# 
# }


# replaced by DescTools v 0.99.36
# as Untable() is a nogo for tables with high frequencies...

SpearmanRho <- function(x, y = NULL, use = c("everything", "all.obs", "complete.obs",
                                             "na.or.complete","pairwise.complete.obs"), conf.level = NA ) {

  if(is.null(y)) {
    # implemented following
    # https://support.sas.com/documentation/onlinedoc/stat/151/freq.pdf
    # S. 3103
    
    # http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
    # pp 1738
    
    # Old References:
    # https://stat.ethz.ch/pipermail/r-help/2006-October/114319.html
    # fisher z transformation for calc SpearmanRho ci :
    # Conover WJ, Practical Nonparametric Statistics (3rd edition). Wiley 1999.
    
    
    n <- sum(x)
    ni. <- apply(x, 1, sum)
    n.j <- apply(x, 2, sum)
    
    ri <- rank(rownames(x))
    ci <- rank(colnames(x))
    ri <- 1:nrow(x)
    ci <- 1:ncol(x)
    
    R1i <- c(sapply(seq_along(ri), 
                    function(i) ifelse(i==1, 0, cumsum(ni.)[i-1]) + ni.[i]/2))
    C1i <- c(sapply(seq_along(ci), 
                    function(i) ifelse(i==1, 0, cumsum(n.j)[i-1]) + n.j[i]/2))
    
    Ri <- R1i - n/2
    Ci <- C1i - n/2
    
    v <- sum(x * outer(Ri, Ci))
    F <- n^3 - sum(ni.^3)
    G <- n^3 - sum(n.j^3)
    
    w <- 1/12*sqrt(F * G)
    
    rho <- v/w
    
  } else {
    
    # http://www-01.ibm.com/support/docview.wss?uid=swg21478368
    
    use <- match.arg(use, choices=c("everything", "all.obs", "complete.obs",
                                    "na.or.complete","pairwise.complete.obs"))
    
    rho <- cor(as.numeric(x), as.numeric(y), method="spearman", use = use)
    
    n <- complete.cases(x,y)
    
  }
  
  
  e_fx <- exp( 2 * ((.5 * log((1+rho) / (1-rho))) - c(1, -1) *
                      (abs(qnorm((1 - conf.level)/2))) * (1 / sqrt(sum(n) - 3)) ))
  ci <- (e_fx - 1) / (e_fx + 1)
  
  if (is.na(conf.level)) {
    result <- rho
  } else {
    
    if(identical(rho, 1)){     # will blast the fisher z transformation
      result <- c(rho=1, lwr.ci=1, upr.ci=1)
      
    } else {
      pr2 <- 1 - (1 - conf.level) / 2
      result <- c(rho = rho, lwr.ci = max(ci[1], -1), upr.ci = min(ci[2], 1))
    }
  }
  
  return(result)

}




# Definitions:
# http://v8doc.sas.com/sashtml/stat/chap28/sect18.htm

ConDisPairs <-function(x){

  # tab is a matrix of counts
  # Based on code of Michael Friendly and Laura Thompson

  # slooooow because of 2 nested for clauses O(n^2)
  # this is NOT faster when implemented with a mapply(...)

  # Lookin for alternatives in C
  # http://en.verysource.com/code/1169955_1/kendl2.cpp.html
  # cor(..., "kendall") is for dimensions better

  n <- nrow(x)
  m <- ncol(x)
  pi.c <- pi.d <- matrix(0, nrow = n, ncol = m)

  row.x <- row(x)
  col.x <- col(x)

  for(i in 1:n){
    for(j in 1:m){
      pi.c[i, j] <- sum(x[row.x<i & col.x<j]) + sum(x[row.x>i & col.x>j])
      pi.d[i, j] <- sum(x[row.x<i & col.x>j]) + sum(x[row.x>i & col.x<j])
    }
  }
  C <- sum(pi.c * x)/2
  D <- sum(pi.d * x)/2

  return(list(pi.c = pi.c, pi.d = pi.d, C = C, D = D))

}


BinTree <- function(n) {

  ranks <- rep(0L, n)
  yet.to.do <- 1:n
  depth <- floor(logb(n, 2))
  start <- as.integer(2^depth)
  lastrow.length <- 1 + n - start
  indx <- seq(1L, by = 2L, length = lastrow.length)
  ranks[yet.to.do[indx]] <- start + 0:(length(indx) - 1L)
  yet.to.do <- yet.to.do[-indx]

  while (start > 1) {
    start <- as.integer(start/2)
    indx <- seq(1L, by = 2L, length = start)
    ranks[yet.to.do[indx]] <- start + 0:(start - 1L)
    yet.to.do <- yet.to.do[-indx]
  }

  return(ranks)

}



PlotBinTree <- function(x, main="Binary tree", horiz=FALSE, cex=1.0, col=1, ...){

  bimean <- function(x){
    (x[rep(c(TRUE, FALSE), length.out=length(x))] +
       x[rep(c(FALSE, TRUE), length.out=length(x))]) / 2
  }

  n <- length(x)
  s <- floor(log(n, 2))

  # if(sortx)
  #   x <- sort(x)
  # else
  #   x <- x[BinTree(length(x))]

  lst <- list()
  lst[[s+1]] <- 1:2^s
  for(i in s:1){
    lst[[i]] <- bimean(lst[[i+1]])
  }

  d.frm <- merge(
    x=data.frame(x=x, binpos=BinTree(length(x))),
    y=data.frame(xpos = unlist(lst),
                 ypos = -rep(1:length(lst), unlist(lapply(lst, length))),
                 pos  = 1:(2^(s+1)-1)
    ), by.x="binpos", by.y="pos")

  if(horiz){

    Canvas(xlim=c(1, s+1.5), ylim=c(0, 2^s+1), main=main,
           asp=FALSE, mar=c(0,0,2,0)+1 )

    ii <- 0
    for(i in 1L:(length(lst)-1)){
      for(j in seq_along(lst[[i]])){
        ii <- ii + 1
        if(ii < n)
          segments(y0=lst[[i]][j], x0=i, y1=lst[[i+1]][2*(j-1)+1], x1=i+1, col=col)
        ii <- ii + 1
        if(ii < n)
          segments(y0=lst[[i]][j], x0=i, y1=lst[[i+1]][2*(j-1)+2], x1=i+1, col=col)
      }
    }

    # Rotate positions for the text
    # rotxy <- Rotate(d.frm$xpos, d.frm$ypos, theta=pi/2)
    # d.frm$xpos <- rotxy$x
    # d.frm$ypos <- rotxy$y

    m <- d.frm$xpos
    d.frm$xpos <- -d.frm$ypos
    d.frm$ypos <- m

  } else {

    Canvas(xlim=c(0,2^s+1), ylim=c(-s,1)-1.5, main=main,
           asp=FALSE, mar=c(0,0,2,0)+1, ...)

    ii <- 0
    for(i in 1L:(length(lst)-1)){
      for(j in seq_along(lst[[i]])){
        ii <- ii + 1
        if(ii < n)
          segments(x0=lst[[i]][j], y0=-i, x1=lst[[i+1]][2*(j-1)+1], y1=-i-1, col=col)
        ii <- ii + 1
        if(ii < n)
          segments(x0=lst[[i]][j], y0=-i, x1=lst[[i+1]][2*(j-1)+2], y1=-i-1, col=col)
      }
    }

  }
  BoxedText(x=d.frm$xpos, y=d.frm$ypos, labels=d.frm$x, cex=cex,
            border=NA, xpad = 0.5, ypad = 0.5)

  invisible(d.frm)

}





.DoCount <- function(y, x, wts) {

  # O(n log n):
  # http://www.listserv.uga.edu/cgi-bin/wa?A2=ind0506d&L=sas-l&P=30503



  if(missing(wts)) wts <- rep_len(1L, length(x))

  ord <- order(y)
  ux <- sort(unique(x))
  n2 <- length(ux)
  idx <- BinTree(n2)[match(x[ord], ux)] - 1L
  y <- cbind(y,1)
  res <- .Call("conc", PACKAGE="DescTools", y[ord,], as.double(wts[ord]),
               as.integer(idx), as.integer(n2))

  return(list(pi.c = NA, pi.d = NA, C = res[2], D = res[1], T=res[3], N=res[4]))

}


.assocs_condis <- function(x, y = NULL, conf.level = NA, ...) {

  # (very) fast function for calculating all concordant/discordant pairs based measures
  # all table operations are cheap compared to the counting of cons/disc...
  # no implementation for confidence levels so far.

  if(!is.null(y))
    x <- table(x, y)

  # we need rowsums and colsums, so tabling is mandatory...
  # use weights
  x <- as.table(x)
  min_dim <- min(dim(x))
  n <- sum(x)
  ni. <- rowSums(x)
  nj. <- colSums(x)

  n0 <- n*(n-1L)/2
  n1 <- sum(ni. * (ni.-1L) / 2)
  n2 <- sum(nj. * (nj.-1L) / 2)

  x <- as.data.frame(x)
  z <- .DoCount(x[,1], x[,2], x[,3])

  gamma <- (z$C - z$D)/(z$C + z$D)

  somers_r <- (z$C - z$D) / (n0 - n2)
  somers_c <- (z$C - z$D) / (n0 - n1)

  taua <- (z$C - z$D) / n0
  taub <- (z$C - z$D) / sqrt((n0-n1)*(n0-n2))
  tauc <- (z$C - z$D) * 2 * min_dim / (n^2*(min_dim-1L))


  if(is.na(conf.level)){
    result <- c(gamma=gamma, somers_r=somers_r, somers_c=somers_c,
                taua=taua, taub=taub, tauc=tauc)

  } else {

    # psi <- 2 * (x$D * x$pi.c - x$C * x$pi.d)/(x$C + x$D)^2
    # # Asymptotic standard error: sqrt(sigma2)
    # gamma_sigma2 <- sum(tab * psi^2) - sum(tab * psi)^2
    #
    # pr2 <- 1 - (1 - conf.level)/2
    # ci <- qnorm(pr2) * sqrt(gamma_sigma2) * c(-1, 1) + gamma
    # result <- c(gamma = gamma,  lwr.ci=max(ci[1], -1), ups.ci=min(ci[2], 1))

    result <- NA

  }

  return(result)

}




# all association measures combined

Assocs <- function(x, conf.level = 0.95, verbose=NULL){

  if(is.null(verbose)) verbose <- "3"
  if(verbose != "3") conf.level <- NA


  res <- rbind(
    "Phi Coeff." = c(Phi(x), NA, NA)
    , "Contingency Coeff." = c(ContCoef(x),NA, NA)
  )

  if(is.na(conf.level)){
    res <- rbind(res, "Cramer V" = c(CramerV(x), NA, NA))
  } else {
    res <- rbind(res, "Cramer V" = CramerV(x, conf.level=conf.level))
  }

  if(verbose=="3") {

    # this is from boot::corr combined with ci logic from cor.test
    r <- boot::corr(d=CombPairs(1:nrow(x), 1:ncol(x)), as.vector(x))
    r.ci <- CorCI(rho = r, n = sum(x), conf.level = conf.level)

    res <- rbind(res
      , "Goodman Kruskal Gamma" = GoodmanKruskalGamma(x, conf.level=conf.level)
      , "Kendall Tau-b" = KendallTauB(x, conf.level=conf.level)
      , "Stuart Tau-c" = StuartTauC(x, conf.level=conf.level)
      , "Somers D C|R" = SomersDelta(x, direction="column", conf.level=conf.level)
      , "Somers D R|C" = SomersDelta(x, direction="r", conf.level=conf.level)
      #    , "Pearson Correlation" =c(cor.p$estimate, lwr.ci=cor.p$conf.int[1], upr.ci=cor.p$conf.int[2])
      , "Pearson Correlation" =c(r.ci[1], lwr.ci=r.ci[2], upr.ci=r.ci[3])
      , "Spearman Correlation" = SpearmanRho(x, conf.level=conf.level)
      , "Lambda C|R" = Lambda(x, direction="column", conf.level=conf.level)
      , "Lambda R|C" = Lambda(x, direction="row", conf.level=conf.level)
      , "Lambda sym" = Lambda(x, direction="sym", conf.level=conf.level)
      , "Uncertainty Coeff. C|R" = UncertCoef(x, direction="column", conf.level=conf.level)
      , "Uncertainty Coeff. R|C" = UncertCoef(x, direction="row", conf.level=conf.level)
      , "Uncertainty Coeff. sym" = UncertCoef(x, direction="sym", conf.level=conf.level)
      , "Mutual Information" = c(MutInf(x),NA,NA)
    ) }

  if(verbose=="3")
    dimnames(res)[[2]][1] <- "estimate"
  else
    dimnames(res)[[2]] <- c("estimate", "lwr.ci", "upr.ci")

  class(res) <- c("Assocs", class(res))
  return(res)

}


print.Assocs <- function(x, digits=4, ...){

  out <- apply(round(x, digits), 2, Format, digits=digits)

  if(nrow(x) == 3){

  } else {
    out[c(1,2,17), 2:3] <- "      -"
  }
  dimnames(out) <- dimnames(x)

  print(data.frame(out), quote=FALSE)
}




## This is an exact copy from Hmisc
## Changes since sent to statlib: improved printing N matrix in print.hoeffd

HoeffD <- function(x, y) {

  phoeffd <- function(d, n)  {

    d <- as.matrix(d); n <- as.matrix(n)
    b <- d + 1/36/n
    z <- .5*(pi^4)*n*b
    zz <- as.vector(z)
    zz[is.na(zz)] <- 1e30   # so approx won't bark

    tabvals <- c(5297,4918,4565,4236,3930,
                 3648,3387,3146,2924,2719,2530,2355,
                 2194,2045,1908,1781,1663,1554,1453,
                 1359,1273,1192,1117,1047,0982,0921,
                 0864,0812,0762,0716,0673,0633,0595,
                 0560,0527,0496,0467,0440,0414,0390,
                 0368,0347,0327,0308,0291,0274,0259,
                 0244,0230,0217,0205,0194,0183,0173,
                 0163,0154,0145,0137,0130,0123,0116,
                 0110,0104,0098,0093,0087,0083,0078,
                 0074,0070,0066,0063,0059,0056,0053,
                 0050,0047,0045,0042,0025,0014,0008,
                 0005,0003,0002,0001)/10000

    P <- ifelse(z<1.1 | z>8.5, pmax(1e-8,pmin(1,exp(.3885037-1.164879*z))),
                matrix(approx(c(seq(1.1, 5,by=.05),
                                seq(5.5,8.5,by=.5)),
                              tabvals, zz)$y,
                       ncol=ncol(d)))

    dimnames(P) <- dimnames(d)
    P
  }

  if(!missing(y))
    x <- cbind(x, y)

  x[is.na(x)] <- 1e30
  storage.mode(x) <-
    #   if(.R.)
    "double"
  #  else
  #    "single"

  p <- as.integer(ncol(x))
  if(p<1)
    stop("must have >1 column")

  n <- as.integer(nrow(x))
  if(n<5)
    stop("must have >4 observations")

  h <-
    #     if(.R.)
    .Fortran("hoeffd", x, n, p, hmatrix=double(p*p), aad=double(p*p),
             maxad=double(p*p), npair=integer(p*p),
             double(n), double(n),  double(n), double(n), double(n),
             PACKAGE="DescTools")
  #   else
  #     .Fortran("hoeffd", x, n, p, hmatrix=single(p*p), npair=integer(p*p),
  #              single(n), single(n),  single(n), single(n), single(n),
  #              single(n), integer(n))

  nam <- dimnames(x)[[2]]
  npair <- matrix(h$npair, ncol=p)
  aad <- maxad <- NULL
  # if(.R.) {
  aad <- matrix(h$aad, ncol=p)
  maxad <- matrix(h$maxad, ncol=p)
  dimnames(aad) <- dimnames(maxad) <- list(nam, nam)
  #  }
  h <- matrix(h$hmatrix, ncol=p)
  h[h>1e29] <- NA
  dimnames(h) <- list(nam, nam)
  dimnames(npair) <- list(nam, nam)
  P <- phoeffd(h, npair)
  diag(P) <- NA
  structure(list(D=30*h, n=npair, P=P, aad=aad, maxad=maxad), class="HoeffD")
}


print.HoeffD <- function(x, ...)
{
  cat("D\n")
  print(round(x$D,2))
  if(length(aad <- x$aad)) {
    cat('\navg|F(x,y)-G(x)H(y)|\n')
    print(round(aad,4))
  }
  if(length(mad <- x$maxad)) {
    cat('\nmax|F(x,y)-G(x)H(y)|\n')
    print(round(mad,4))
  }
  n <- x$n
  if(all(n==n[1,1]))
    cat("\nn=",n[1,1],"\n")
  else {
    cat("\nn\n")
    print(x$n)
  }

  cat("\nP\n")
  P <- x$P
  P <- ifelse(P<.0001,0,P)
  p <- format(round(P,4))
  p[is.na(P)] <- ""
  print(p, quote=FALSE)
  invisible()
}


# find non-centrality parameter for the F-distribution
ncparamF <- function(type1, type2, nu1, nu2){
  .C("fpow",  PACKAGE = "DescTools", as.double(type1), as.double(type2), as.double(nu1), as.double(nu2), lambda=double(1))$lambda
}

