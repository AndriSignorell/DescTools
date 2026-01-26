
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

# internal function, no use to export it.. (?)
.NormWeights <- function(x, weights, na.rm=FALSE, zero.rm=FALSE, normwt=FALSE) {
  
  # Idea Henrik Bengtsson
  # we remove values with zero (and negative) weight. 
  # This would:
  #  1) take care of the case when all weights are zero,
  #  2) it will most likely speed up the sorting.
  
  if (na.rm){
    
    keep <- !is.na(x) & !is.na(weights)
    
    if(zero.rm)
      # remove values with zero weights
      keep <- keep & (weights > 0)

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








MAD_old <- function(x, weights = NULL, center = Median, constant = 1.4826, 
                median.type = c("standard", "low", "high"), na.rm = FALSE) {
  
  
  median.type <- match.arg(median.type)
  
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
    z <- .NormWeights(x, weights, na.rm=na.rm, zero.rm=TRUE)
    
    res <- constant *  Median(abs(z$x - center), weights = z$weights)
    
  } else {
    # fall back to mad(), if there are no weights
    res <- mad(x, center = center, constant = constant, 
               na.rm = na.rm, 
               low= (median.type == "low"), 
               high= (median.type == "high"))
    
  }
  
  return(res)
  
}


MAD <- function(x,
                weights = NULL,
                center = Median,
                constant = 1.4826,
                median.type = c("standard", "low", "high"),
                na.rm = FALSE) {
  
  median.type <- match.arg(median.type)
  
  ## NA handling
  if (na.rm) {
    ok <- !is.na(x)
    x <- x[ok]
    if (!is.null(weights))
      weights <- weights[ok]
  }
  
  ## determine center 
  if (is.function(center)) {
    center <- if (is.null(weights)) {
      center(x)
    } else {
      center(x, weights = weights)
    }
  }
  
  ## deviations
  d <- abs(x - center)
  
  ## weights
  if (is.null(weights))
    weights <- rep(1, length(d))
  
  z <- .NormWeights(d, weights, na.rm = FALSE, zero.rm = TRUE)
  
  ## Median-Index
  n <- length(z$x)
  
  if (median.type == "standard" || n %% 2 == 1) {
    m <- Median(z$x, z$weights)
  } else {
    k <- n %/% 2
    o <- order(z$x)
    m <- if (median.type == "low")
      z$x[o[k]]
    else
      z$x[o[k + 1]]
  }
  
  return(constant * m)
  
}



# from stats
SD <- function (x, estimator = c("unbiased", "ML"), weights = NULL, na.rm = FALSE, ...)
  sqrt(Var(if (is.vector(x) || is.factor(x)) x else as.double(x),
           estimator = estimator, weights=weights, na.rm = na.rm, ...))


Var <- function (x, ...)
  UseMethod("Var")



Var.default <- function(x, estimator = c("unbiased", "ML"),
                        weights = NULL, na.rm = FALSE, ...) {
  
  estimator <- match.arg(estimator)
  
  ## NA-Handling
  if (na.rm) {
    ok <- !is.na(x)
    x <- x[ok]
    if (!is.null(weights))
      weights <- weights[ok]
  }
  
  ## Weights?
  if (is.null(weights)) {
    res <- var(x=x, na.rm=na.rm)
    
    if(estimator == "ML")
      res <- res * ((n <- sum(ok)) - 1) / n
    
  } else {
    
    z <- .NormWeights(x, weights, na.rm = FALSE, zero.rm = TRUE)
    
    if (estimator == "ML"){
      res <- as.numeric(stats::cov.wt(cbind(z$x), z$weights, method = "ML")$cov)
      
    } else {
      
      xbar <- sum(z$weights * x) / z$wsum
      res <- sum(z$weights * ((z$x - xbar)^2))/(z$wsum - 1)
    }

  }
  
  return( res )
  
}



Var.Freq <- function(x, breaks, ...)  {
  n <- sum(x$freq)
  mu <- sum(head(MoveAvg(breaks, order=2, align="left"), -1) * x$perc)
  s2 <- (sum(head(MoveAvg(breaks, order=2, align="left"), -1)^2 * x$freq) - n*mu^2) / (n-1)

  return(s2)
}




Cov <- cov
Cor <- cor



# SDN <- function(x, na.rm = FALSE){
#   sd(x, na.rm=na.rm) * sqrt(((n <- sum(!is.na(x)))-1) /n)
# }


# VarN <- function(x, na.rm = FALSE){
#   var(x, na.rm=na.rm) * ((n <- sum(!is.na(x)))-1) /n
# }


EX <- function(x, p) sum(x * p)

VarX <- function(x, p) sum((x - EX(x, p))^2 * p)



# multiple gsub
Mgsub <- function(pattern, replacement, x, ...) {
  
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  
  result
  
}




# Length(x)
# Table(x)
# Log(x)
# Abs(x)
#
# "abs", "sign", "sqrt", "ceiling", "floor", "trunc", "cummax", "cummin", "cumprod", "cumsum",
# "log", "log10", "log2", "log1p", "acos", "acosh", "asin", "asinh", "atan", "atanh",
# "exp", "expm1", "cos", "cosh", "cospi", "sin", "sinh", "sinpi", "tan", "tanh",
# "tanpi", "gamma", "lgamma", "digamma", "trigamma"




Median <- function(x, ...)
  UseMethod("Median")


Median.default <- function(x, weights = NULL, na.rm = FALSE, ...) {
  if(is.null(weights))
    median(x=x, na.rm=na.rm)
  else 
    Quantile(x, weights, probs=0.5, na.rm=na.rm, names=FALSE)
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




# further weighted quantiles in Hmisc and modi, both on CRAN

Quantile <- function(x, weights = NULL, probs = seq(0, 1, 0.25),
                             na.rm = FALSE, names=TRUE, type = 7, digits=7) {

  
  if(is.null(weights)){
    quantile(x=x, probs=probs, na.rm=na.rm, names=names, type=type, digits=digits)
    
  } else {
  
  # this is a not exported stats function
  format_perc <- function (x, digits = max(2L, getOption("digits")), probability = TRUE, 
            use.fC = length(x) < 100, ...) {
    if (length(x)) {
      if (probability) 
        x <- 100 * x
      ans <- paste0(if (use.fC) 
        formatC(x, format = "fg", width = 1, digits = digits)
        else format(x, trim = TRUE, digits = digits, ...), "%")
      ans[is.na(x)] <- ""
      ans
    }
    else character(0)
  }
  
  
  
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
  # currently only type 5
  if (type == 5) {
    qs <- sapply(probs,
                function(p) {
                  if (p == 0) return(x[1])
                  else if (p == 1) return(x[n])
                  select <- min(which(rw >= p))
                  if(rw[select] == p) mean(x[select:(select+1)])
                  else x[select]
                })
    
  } else if(type == 7){
    
    if(is.null(weights)){
      index <- 1 + max(n - 1, 0) * probs
      lo <- pmax(floor(index), 1)
      hi <- ceiling(index)
      x <- sort(x, partial = if (n == 0) 
        numeric()
        else unique(c(lo, hi)))
      qs <- x[lo]
      i <- which((index > lo & x[hi] != qs))
      h <- (index - lo)[i]
      qs[i] <- (1 - h) * qs[i] + h * x[hi[i]]
    
    } else {
      n     <- sum(weights)
      ord <- 1 + (n - 1) * probs
      low   <- pmax(floor(ord), 1)
      high  <- pmin(low + 1, n)
      ord <- ord %% 1
      ## Find low and high order statistics
      ## These are minimum values of x such that the cum. freqs >= c(low,high)
      allq <- approx(cumsum(weights), x, xout=c(low, high), 
                     method='constant', f=1, rule=2)$y
      k <- length(probs)
      qs <- (1 - ord)*allq[1:k] + ord*allq[-(1:k)]
    }
    
  } else {
    qs <- NA
    warning(gettextf("type %s is not implemented", type))
  }
  
  # return(unname(q))
  # why unname? change to named.. 14.10.2020
  
  if (names && length(probs) > 0L) {
    stopifnot(is.numeric(digits), digits >= 1)
    names(qs) <- format_perc(probs, digits = digits)
  }
  
  return(qs)
  
  }
}




IQRw <- function (x, weights = NULL, na.rm = FALSE, type = 7) {
  
  if(is.null(weights))
    IQR(x=x, na.rm=na.rm, type=type)
  
  else 
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


# AUC_deprecated <- function(x, y, from=min(x, na.rm=TRUE), to = max(x, na.rm=TRUE),
#                 method=c("trapezoid", "step", "spline", "linear"),
#                 absolutearea = FALSE, subdivisions = 100, na.rm = FALSE, ...) {
# 
#   # calculates Area unter the curve
#   # example:
#   #   AUC( x=c(1,2,3,5), y=c(0,1,1,2))
#   #   AUC( x=c(2,3,4,5), y=c(0,1,1,2))
# 
#   if(na.rm) {
#     idx <- complete.cases(cbind(x,y))
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


# New version, publish as soon as package sobir is updated

AUC <- function(x, y, from = min(x, na.rm=TRUE), to = max(x, na.rm=TRUE),
                method=c("trapezoid", "step", "spline", "linear"), absolutearea = FALSE,
                subdivisions = 100,  na.rm = FALSE, ...)  {


  if(identical(method, "linear")){
    warning("method linear is no longer supported!")
    return(NA)
  }
    
  
  # calculates Area unter the curve
  # example:
  #   AUC( x=c(1,2,3,5), y=c(0,1,1,2))
  #   AUC( x=c(2,3,4,5), y=c(0,1,1,2))

  if(na.rm) {
    idx <- complete.cases(cbind(x,y))
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



MESS_auc <- function(x, y, from = min(x, na.rm=TRUE), to = max(x, na.rm=TRUE), type=c("linear", "spline"),
                     absolutearea=FALSE, subdivisions =100, ...) {
  
  type <- match.arg(type)
  
  # Sanity checks
  stopifnot(length(x) == length(y))
  stopifnot(!is.na(from))
  
  if (length(unique(x)) < 2)
    return(NA)
  
  if (type=="linear") {
    ## Default option
    if (absolutearea==FALSE) {
      values <- approx(x, y, xout = sort(unique(c(from, to, x[x > from & x < to]))), ...)
      res <- 0.5 * sum(diff(values$x) * (values$y[-1] + values$y[-length(values$y)]))
    } else { ## Absolute areas
      ## This is done by adding artificial dummy points on the x axis
      o <- order(x)
      ox <- x[o]
      oy <- y[o]
      
      idx <- which(diff(oy >= 0)!=0)
      newx <- c(x, x[idx] - oy[idx]*(x[idx+1]-x[idx]) / (y[idx+1]-y[idx]))
      newy <- c(y, rep(0, length(idx)))
      values <- approx(newx, newy, xout = sort(unique(c(from, to, newx[newx > from & newx < to]))), ...)
      res <- 0.5 * sum(diff(values$x) * (abs(values$y[-1]) + abs(values$y[-length(values$y)])))
    }
    
  } else { ## If it is not a linear approximation
    if (absolutearea)
      myfunction <- function(z) { abs(splinefun(x, y, method="natural")(z)) }
    
    else
      myfunction <- splinefun(x, y, method="natural")
    
    res <- integrate(myfunction, lower=from, upper=to, subdivisions=subdivisions)$value
    
  }
  
  res
  
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
    # return(structure(x, freq = 1L)) 
    # changed to: only one value in x, no mode defined
    return(structure(NA_real_, freq = NA_integer_))
  
  # we don't have NAs so far, either there were then we've already stopped
  # or they've been stripped above
  res <- fastModeX(x, narm=FALSE)
  
  # no mode existing, if max freq is only 1 observation
  if(length(res)== 0L & attr(res, "freq")==1L)
    return(structure(NA_real_, freq = NA_integer_))
  
  else
    # order results kills the attribute
    return(structure(res[order(res)], freq = attr(res, "freq")))

}



Gmean <- function (x, conf.level = NA, sides = c("two.sided","left","right"),
                   method = c("classic", "boot"),
                   na.rm = FALSE, ...) {

  # see also: http://www.stata.com/manuals13/rameans.pdf

  if(na.rm) x <- na.omit(x)
  
  if(any(is.na(x) | (is_neg <- x < 0))){
    
    if(any(na.omit(is_neg))) 
      warning("x contains negative values")

    if(is.na(conf.level))
      NA
    else
      c(NA, NA, NA)
    
  } else if(any(x==0)) {
    
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



Hmean <- function(x, conf.level = NA, 
                  sides = c("two.sided","left","right"), method = c("classic", "boot"),
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



TukeyBiweight <- function(x, conf.level = NA, sides = c("two.sided","left","right"), 
                          method = c("boot"), const=9,  na.rm = FALSE,  ...) {

  if(na.rm) x <- na.omit(x)
  if(anyNA(x)) return(NA)

  if(is.na(conf.level)){
    res <- .Call("_DescTools_tbrm", PACKAGE = "DescTools", x, const)
    
  } else {

    sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
    if(sides!="two.sided")
      conf.level <- 1 - 2*(1-conf.level)
    
    # boot arguments in dots ...
    # adjusted bootstrap percentile (BCa) interval
    btype <- InDots(..., arg="type", default="bca")
    R <- InDots(..., arg="R", default=999)
    parallel <- InDots(..., arg="parallel", default="no")
    ncpus <- InDots(..., arg="ncpus", default=getOption("boot.ncpus", 1L))
    
    boot.fun <- boot::boot(x, 
                           function(x, d) 
                             .Call("_DescTools_tbrm", PACKAGE="DescTools", x[d], const), 
                           R=R, parallel=parallel, ncpus=ncpus)
    ci <- boot::boot.ci(boot.fun, conf=conf.level, type=btype)
    
    if(btype == "norm"){
      res <- c(est=boot.fun$t0, lci=ci[[4]][2], uci=ci[[4]][3])
    } else {
      res <- c(est=boot.fun$t0, lci=ci[[4]][4], uci=ci[[4]][5])
    }
    
    if(sides=="left")        res[3] <- Inf
    else if(sides=="right")  res[2] <- -Inf
    
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



HuberM <- function(x, conf.level = NA, sides = c("two.sided","left","right"), 
                   method = c("wald", "boot"), 
                   k = 1.345, mu = median(x), s = mad(x, center=mu),
                   na.rm = FALSE,  ...){

  # new interface to HuberM, making it less complex
  # refer to robustbase::huberM if more control is required

  if(na.rm) x <- na.omit(x)
  if(anyNA(x)) return(NA)


  if(is.na(conf.level)){
    res <- .huberM(x=x, k=k, mu=mu, s=s, warn0scale=TRUE)$mu

    return(res)

  } else {

    switch(match.arg(method)
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
             
             res <- c(est=res$mu, lci=res$mu - ci, uci=res$mu + ci)
             
           }
           ,"boot" ={
             
             sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
             if(sides!="two.sided")
               conf.level <- 1 - 2*(1-conf.level)
             
             # boot arguments in dots ...
             # adjusted bootstrap percentile (BCa) interval
             btype <- InDots(..., arg="type", default="bca")
             R <- InDots(..., arg="R", default=999)
             parallel <- InDots(..., arg="parallel", default="no")
             ncpus <- InDots(..., arg="ncpus", default=getOption("boot.ncpus", 1L))
             
             boot.fun <- boot::boot(x, 
                                    function(x, d){
                                      hm <- .huberM(x=x[d], k=k, mu=mu, s=s, se=TRUE)
                                      return(c(hm$mu, hm$s^2))
                                      }, R=R, parallel=parallel, ncpus=ncpus)
             ci <- boot::boot.ci(boot.fun, conf=conf.level, type=btype)
             
             if(btype == "norm"){
               res <- c(est=boot.fun$t0[1], lci=ci[[4]][2], uci=ci[[4]][3])
             } else {
               res <- c(est=boot.fun$t0[1], lci=ci[[4]][4], uci=ci[[4]][5])
             }
             
             if(sides=="left")        res[3] <- Inf
             else if(sides=="right")  res[2] <- -Inf

           }
    )
    return(res)

  }

}







Outlier <- function(x, method=c("boxplot", "hampel"), value=TRUE, na.rm=FALSE){

  switch(match.arg(arg = method, choices = c("boxplot", "hampel")),
         
         boxplot =  {
             qq <- quantile(as.numeric(x), c(0.25, 0.75), na.rm = na.rm, names = FALSE)
             iqr <- diff(qq)
             id <- x < (qq[1] - 1.5 * iqr) | x > (qq[2] + 1.5 * iqr)
           },
         
         hampel = {
           med_x <- median(x, na.rm=na.rm)
           
           # hampel considers values outside of median ± 3*(median absolute deviation) 
           # to be outliers
           id <- x %][% (med_x + c(-3, 3) * mad(x, na.rm=na.rm, center = med_x))
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
      # function that calculates the local reachability density
      # of Breuing(2000) for each observation in a matrix, using
      # a matrix (distdata) of k nearest neighbors computed by the function dist.to.knn2

      p=dim(distdata)[2]
      lrd=rep(0,p)

      for (i in 1:p)
      {
        j=seq(3,3+(distdata[2,i]-distdata[1,i]))
        # compare the k-distance from each observation to its kth neighbor
        # to the actual distance between each observation and its neighbors
        numneigh=distdata[2,i]-distdata[1,i]+1
        temp=rbind(diag(distdata[distdata[2,distdata[j,i]],distdata[j,i]]),distdata[j+numneigh,i])

        # calculate reachability
        reach=1/(sum(apply(temp,2,max))/numneigh)
        lrd[i]=reach
      }
      lrd
    }


  data=as.matrix(data)

  # find k nearest neighbors and their distance from each observation
  # in data
  distdata=dist.to.knn(data,k)
  p=dim(distdata)[2]

  # calculate the local reachability density for each observation in data
  lrddata=reachability(distdata,k)

  lof=rep(0,p)

  # computer the local outlier factor of each observation in data
  for ( i in 1:p)
  {
    nneigh=distdata[2,i]-distdata[1,i]+1
    j=seq(0,(nneigh-1))
    local.factor=sum(lrddata[distdata[3+j,i]]/lrddata[i])/nneigh
    lof[i]=local.factor
  }

  # return lof, a vector with the local outlier factor of each observation
  lof
}




BootCI <- function(x, y=NULL, FUN, ..., bci.method = c("norm", "basic", "stud", "perc", "bca"),
                   conf.level = 0.95, sides = c("two.sided", "left", "right"), R = 999) {

  dots <- as.list(substitute(list( ... ))) [-1]
  bci.method <- match.arg(bci.method)

  sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
  if(sides!="two.sided")
    conf.level <- 1 - 2*(1-conf.level)

  if(is.null(y)) {
    if(is.matrix(x) || is.data.frame(x)){
      boot.fun <- boot(x, function(x, d) 
        do.call(FUN, append(list(x[d, , drop=FALSE]), dots)), R = R)
    } else {
      boot.fun <- boot(x, function(x, d) 
        do.call(FUN, append(list(x[d]), dots)), R = R)
    }
  } else
    boot.fun <- boot(x, function(x, d) do.call(FUN, append(list(x[d], y[d]), dots)), R = R)

  ci <- boot.ci(boot.fun, conf=conf.level, type=bci.method)[[4]]
  
  res <- c(est = boot.fun$t0, 
           lci = ci[ncol(ci)-1],
           uci = ci[ncol(ci)])

  if(sides=="left")
    res[3] <- Inf
  else if(sides=="right")
    res[2] <- -Inf

  names(res)[1] <- deparse(substitute(FUN))
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





# standard error of mean
MeanSE <- function(x, sd = NULL, na.rm = FALSE) {
  if(na.rm) x <- na.omit(x)
  if(is.null(sd)) s <- sd(x)
  s/sqrt(length(x))
}




MeanCIn <- function(ci, sd, interval=c(2, 1e5), conf.level=0.95, norm=FALSE, 
                    tol = .Machine$double.eps^0.5) {
  
  width <- diff(ci)/2
  alpha <- (1-conf.level)/2
  
  if(width > sd){
    warning("Width of confidence intervall > 2*sd, samplesize n=1 is ok for that case.")
    return(1)
    
  } else {
    if(norm)
      uniroot(f = function(n) sd/sqrt(n) * qnorm(p = 1-alpha) - width, 
              interval = interval, tol = tol)$root
    else
      uniroot(f = function(n) (qt(1-alpha, df=n-1) * sd / sqrt(n)) - width, 
              interval = interval, tol = tol)$root
  }
}




# find non-centrality parameter for the F-distribution
ncparamF <- function(type1, type2, nu1, nu2) {
  
  # author Ali Baharev <ali.baharev at gmail.com>
  
  # Returns the noncentrality parameter of the noncentral F distribution 
  # if probability of Type I and Type II error, degrees of freedom of the 
  # numerator and the denominator in the F test statistics are given.
  
  
  .C("fpow",  PACKAGE = "DescTools", as.double(type1), as.double(type2), 
     as.double(nu1), as.double(nu2), lambda=double(1))$lambda
}




## stats: Lorenz, <- & ineq ====

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

  g <- Gini(x, weights=n, na.rm=na.rm)

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


# Gini <- function(x, n = rep(1, length(x)), unbiased = TRUE, conf.level = NA, R = 1000, type = "bca", na.rm = FALSE) {
# 
#   # cast to numeric, as else sum(x * 1:n) might overflow for integers
#   # http://stackoverflow.com/questions/39579029/integer-overflow-error-using-gini-function-of-package-desctools
#   x <- as.numeric(x)
# 
#   x <- rep(x, n)    # same handling as Lc
#   if(na.rm) x <- na.omit(x)
#   if (any(is.na(x)) || any(x < 0)) return(NA_real_)
# 
#   i.gini <- function (x, unbiased = TRUE){
#     n <- length(x)
#     x <- sort(x)
# 
#     res <- 2 * sum(x * 1:n) / (n*sum(x)) - 1 - (1/n)
#     if(unbiased) res <- n / (n - 1) * res
# 
#     # limit Gini to 0 here, if negative values appear, which is the case with
#     # Gini( c(10,10,10))
#     return( pmax(0, res))
# 
#     # other guy out there:
#     #     N <- if (unbiased) n * (n - 1) else n * n
#     #     dsum <- drop(crossprod(2 * 1:n - n - 1, x))
#     #     dsum / (mean(x) * N)
#     # is this slower, than above implementation??
#   }
# 
#   if(is.na(conf.level)){
#     res <- i.gini(x, unbiased = unbiased)
# 
#   } else {
#     # adjusted bootstrap percentile (BCa) interval
#     boot.gini <- boot(x, function(x, d) i.gini(x[d], unbiased = unbiased), R=R)
#     ci <- boot.ci(boot.gini, conf=conf.level, type=type)
#     res <- c(gini=boot.gini$t0, lwr.ci=ci[[4]][4], upr.ci=ci[[4]][5])
#   }
# 
#   return(res)
# 
# }



# recoded for better support weights 2022-09-14

Gini <- function(x, 
                 conf.level = NA, sides = c("two.sided", "left", "right"),
                 method = c("boot"), unbiased=TRUE, weights=NULL, 
                 na.rm=FALSE, ...) {
  
  # https://core.ac.uk/download/pdf/41339501.pdf
  
  if (is.null(weights)) {
    weights <- rep(1, length(x))
  }
  
  if (na.rm){
    na <- (is.na(x) | is.na(weights))
    x <- x[!na]
    weights <- weights[!na]
  } 
  
  if (any(is.na(x)) || any(x < 0)) 
    return(NA_real_)
  
  
  
  i.gini <- function(x, w, unbiased=FALSE) {
    
    w <- w/sum(w)
    
    x <- x[id <- order(x)]
    w <- w[id]
    
    f.hat <- w / 2 + c(0, head(cumsum(w), -1))
    wm <- Mean(x, w)
    
    res <- 2 / wm * sum(w * (x - wm) * (f.hat - Mean(f.hat, w)))
    
    if(unbiased)
      res <- res * 1/(1 - sum(w^2))
    
    return(res)
  }
  
  
  if (is.na(conf.level)) {
    res <- i.gini(x, weights, unbiased = unbiased)
    
  } else {
    
    sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
    if(sides!="two.sided")
      conf.level <- 1 - 2*(1-conf.level)
    
    
    # boot.gini <- boot(data = x,
    #                   statistic = function(z, i, u, unbiased) 
    #                     i.gini(x = z[i], w = u[i], unbiased = unbiased), 
    #                   R=R, u=weights, unbiased=unbiased)
    # ci <- boot.ci(boot.gini, conf = conf.level, type = type)
    # res <- c(gini = boot.gini$t0, lwr.ci = ci[[4]][4], upr.ci = ci[[4]][5])
    
    
    # boot arguments in dots ...
    # adjusted bootstrap percentile (BCa) interval
    btype <- InDots(..., arg="type", default="bca")
    R <- InDots(..., arg="R", default=999)
    parallel <- InDots(..., arg="parallel", default="no")
    ncpus <- InDots(..., arg="ncpus", default=getOption("boot.ncpus", 1L))
    
    
    # ToDo *******************
    # *******  implement here the two sample case!! ***********
    # ToDo *******************
    
    boot.fun <- boot::boot(x, 
                           function(z, i, u, unbiased) 
                              i.gini(x = z[i], w = u[i], unbiased = unbiased), 
                           u=weights, unbiased=unbiased, 
                           R=R, parallel=parallel, ncpus=ncpus)
    ci <- boot::boot.ci(boot.fun, conf=conf.level, type=btype)
    
    if(btype == "norm"){
      res <- c(est=boot.fun$t0, lci=ci[[4]][2], uci=ci[[4]][3])
    } else {
      res <- c(est=boot.fun$t0, lci=ci[[4]][4], uci=ci[[4]][5])
    }
    
    
    
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


GiniDeltas <- function (x, na.rm = FALSE) {
  
  # Deltas (2003, DOI:10.1162/rest.2003.85.1.226).
  
  if (!is.factor(x)) {
    warning("x is not a factor!")
    return(NA)
  }
  if (na.rm) 
    x <- na.omit(x)
  
  p <- prop.table(table(x))
  sum(p * (1 - p)) * length(p)/(length(p) - 1)
  
}



HunterGaston <- function(x, na.rm = FALSE){

  # Hunter-Gaston index (Hunter & Gaston, 1988, DOI:10.1128/jcm.26.11.2465-2466.1988)
  # Credits to Wim Bernasco
  # https://github.com/AndriSignorell/DescTools/issues/120
  
  # see: vegan::simpson.unb(BCI)
  
  
  if (is.factor(x) | is.character(x)) {
    if (na.rm) 
      x <- na.omit(x)
    tt <- table(x)
  } else {
    tt <- x
  }
  
  sum(tt * (tt - 1)) / (sum(tt) * (sum(tt) - 1))

  # shouldn't this be: 
  # 1 - sum(tt * (tt - 1)) / (sum(tt) * (sum(tt) - 1))
  # ???

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


CutAge <- function(x, breaks=c(seq(from=0, to=90, by=10), Inf), 
                   right=FALSE, ordered_result=TRUE, full=TRUE, 
                   labels=NULL, ...) {
  
  
  if(identical(labels, TRUE)){
    labels <- paste(Format(head(breaks, -1), ldigits=2, digits=0), 
                    c(head(breaks[-1], -1)-1, ".."), sep="-")
  }
  
  res <- cut(x, breaks = breaks, 
             right=right, ordered_result = ordered_result, 
             labels = labels, ...)
  
  if(!full)
    res <- factor(res, 
	              levels=levels(res)[do.call(seq, 
				              as.list(range(which(Freq(res)$freq != 0))))])			
  
  return(res)  
	  
}


Generation <- function(year){
  
  # Babyboomer   (1946-1964)
  # Generation X (1965-1979)
  # Generation Y (1980-1995) – also called Millennials
  # Generation Z (1996-2010)
  # Generation Alpha (ab 2011-2025)
  
  cut(year,
      breaks=c(1946, 1965, 1980, 1996, 2011, Inf), right=FALSE, 
      labels = c("Babyboomer","Gen X","Millennial","Gen Z","Gen Alpha"),
      ordered = TRUE)
  
}


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



cut.integer <- function(x, breaks, labels = NULL, include.lowest = FALSE, right = TRUE, 
                        ordered_result = FALSE, ...){
  
  # labels are constructed using "(a,b]" interval notation in cut.default, 
  # which is perfectly fine for numeric variables, but not well suited for 
  # integers, for which an explicit formulation is more appropriate
  
  
  .FmInf <- function(x){
    x[!is.finite(x)] <- ".."
    return(x)
  }
  
  if(is.null(labels)){
    from <- head(breaks, -1)
    to <- breaks[-1]
    
    if(right)
      labels <- paste(.FmInf(from + 1), .FmInf(to), sep="-")
    else
      labels <- paste(.FmInf(from), .FmInf(to - 1), sep="-")
    
  }
  
  res <- cut.default(x=x, breaks=breaks, labels=labels, include.lowest=include.lowest,
                     right=right, ordered_result=ordered_result, ...)
  
  return(res)  
  
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



.ncchisqCI <- function(chisq, df, conf) {
  
  alpha <- 1 - conf
  probs <- c(alpha/2, 1 - alpha/2)
  
  ncp <- suppressWarnings(optim(par = 1.1 * rep(chisq, 2), fn = function(x) {
    p <- pchisq(q = chisq, df = df, ncp = x)
    abs(max(p) - probs[2]) + abs(min(p) - probs[1])
  }, control = list(abstol = 0.000000001)))
  
  chisq_ncp <- unname(sort(ncp$par))
  
  return(chisq_ncp)
  
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
    # September 2013 Journal of the Korean Statistical Society 42(3)
    # DOI: 10.1016/j.jkss.2012.10.002
    # see also CramerV
    
    phi.hat <- chisq.hat / n
    as.numeric(sqrt(max(0, phi.hat - df/(n-1)) / 
                     (sqrt(prod(sapply(dim(x), function(i) i - 1 / (n-1) * (i-1)^2) - 1)))))
    
  } else {
    as.numeric( sqrt(chisq.hat/(n * sqrt(df))))
  }

}






# ICC(ratings)
# ICC_(ratings, type="ICC3", conf.level=0.95)
# ICC_(ratings, type="all", conf.level=0.95)

ICC <- function(x, type=c("all", "ICC1","ICC2","ICC3","ICC1k","ICC2k","ICC3k"), 
                conf.level = NA, na.rm = FALSE) {

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
      res <- results[idx, c(2)]
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
  
  # we remove the names here, because otherwise only the assignment of 
  # na.rm would cause the row names to be defined as characters.
  # see: https://github.com/AndriSignorell/DescTools/issues/172
  rmean <- unname(apply(dat, MARGIN = 1, FUN = mean))
  
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


#
# deprecated in 0.99.61 - 2025-08-18
#
# KrippAlpha <- function (x, method = c("nominal", "ordinal", "interval", "ratio")) {
# 
#   method  <-  match.arg(method)
# 
#   coincidence.matrix <- function(x) {
#     levx <- (levels(as.factor(x)))
#     nval <- length(levx)
#     cm <- matrix(rep(0, nval * nval), nrow = nval)
#     dimx <- dim(x)
#     vn <- function(datavec) sum(!is.na(datavec))
#     if(any(is.na(x))) mc <- apply(x, 2, vn) - 1
#     else mc <- rep(1, dimx[2])
#     for(col in 1:dimx[2]) {
#       for(i1 in 1:(dimx[1] - 1)) {
#         for(i2 in (i1 + 1):dimx[1]) {
#           if(!is.na(x[i1, col]) && !is.na(x[i2, col])) {
#             index1 <- which(levx == x[i1, col])
#             index2 <- which(levx == x[i2, col])
#             cm[index1, index2] <- cm[index1,index2] + (1 + (index1 == index2))/mc[col]
#             if(index1 != index2) cm[index2,index1] <- cm[index1,index2]
#           }
#         }
#       }
#     }
#     nmv  <-  sum(apply(cm, 2, sum))
#     return(structure(list(method="Krippendorff's alpha",
#                           subjects=dimx[2], raters=dimx[1],irr.name="alpha",
#                           value=NA,stat.name="nil",statistic=NULL,
#                           cm=cm,data.values=levx,nmatchval=nmv,data.level=NA),
#                      class = "irrlist"))
#   }
# 
#   ka <- coincidence.matrix(x)
#   ka$data.level <- method
#   dimcm <- dim(ka$cm)
#   utcm <- as.vector(ka$cm[upper.tri(ka$cm)])
#   diagcm <- diag(ka$cm)
#   occ <- sum(diagcm)
#   nc <- apply(ka$cm,1,sum)
#   ncnc <- sum(nc * (nc - 1))
#   dv <- as.numeric(ka$data.values)
#   diff2 <- rep(0,length(utcm))
#   ncnk <- rep(0,length(utcm))
#   ck <- 1
# 
#   if (dimcm[2]<2)
#     ka$value <- 1.0
#   else {
#     for(k in 2:dimcm[2]) {
#       for(c in 1:(k-1)) {
#         ncnk[ck] <- nc[c] * nc[k]
#         if(match(method[1],"nominal",0)) diff2[ck] <- 1
#         if(match(method[1],"ordinal",0)) {
#           diff2[ck] <- nc[c]/2
#           if(k > (c+1))
#             for(g in (c+1):(k-1)) diff2[ck] <- diff2[ck] + nc[g]
#             diff2[ck] <- diff2[ck]+nc[k]/2
#             diff2[ck] <- diff2[ck]^2
#         }
#         if(match(method[1],"interval",0)) diff2[ck] <- (dv[c]-dv[k])^2
#         if(match(method[1],"ratio",0)) diff2[ck] <- (dv[c]-dv[k])^2/(dv[c]+dv[k])^2
#         ck <- ck+1
#       }
#     }
#     ka$value <- 1-(ka$nmatchval-1)*sum(utcm*diff2)/sum(ncnk*diff2)
#   }
#   return(ka)
# }



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



OddsRatio.glm <- function(x, conf.level = NULL, digits=3, use.profile=FALSE, ...) {

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



# Replaced by new Rcpp Version
#   2025-06-15
# # Definitions:
# # http://v8doc.sas.com/sashtml/stat/chap28/sect18.htm
# 
# ConDisPairs <-function(x){
# 
#   # tab is a matrix of counts
#   # Based on code of Michael Friendly and Laura Thompson
# 
#   # slooooow because of 2 nested for clauses O(n^2)
#   # this is NOT faster when implemented with a mapply(...)
# 
#   # Lookin for alternatives in C
#   # http://en.verysource.com/code/1169955_1/kendl2.cpp.html
#   # cor(..., "kendall") is for dimensions better
# 
#   n <- nrow(x)
#   m <- ncol(x)
#   pi.c <- pi.d <- matrix(0, nrow = n, ncol = m)
# 
#   row.x <- row(x)
#   col.x <- col(x)
# 
#   for(i in 1:n){
#     for(j in 1:m){
#       pi.c[i, j] <- sum(x[row.x<i & col.x<j]) + sum(x[row.x>i & col.x>j])
#       pi.d[i, j] <- sum(x[row.x<i & col.x>j]) + sum(x[row.x>i & col.x<j])
#     }
#   }
#   C <- sum(pi.c * x)/2
#   D <- sum(pi.d * x)/2
# 
#   return(list(pi.c = pi.c, pi.d = pi.d, C = C, D = D))
# 
# }

ConDisPairs <-function(x){

  # tab is a matrix of counts
  .Call("_DescTools_ConDisPairs", PACKAGE = "DescTools", x)
  
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



TablePearson <- function(x, scores.type="table") {
  
  # based on Lecoutre 
  # https://stat.ethz.ch/pipermail/r-help/2005-July/076371.html
  # but the test might not be correctly implemented (negative values in sqrt)
  
  
  # Statistic
  sR <- scores(x, 1, scores.type)
  sC <- scores(x, 2, scores.type)
  n <- sum(x)
  Rbar <- sum(apply(x, 1, sum) * sR) / n
  Cbar <- sum(apply(x, 2, sum) * sC) / n
  ssr <- sum(x * (sR-Rbar)^2)
  ssc <- sum(t(x) * (sC-Cbar)^2)
  tmpij <- outer(sR, sC, FUN=function(a,b) return((a-Rbar)*(b-Cbar)))
  ssrc <-  sum(x*tmpij)
  v <- ssrc
  w <- sqrt(ssr*ssc)
  r <- v/w
  
  return(r)
  
}  


TableSpearman <-  function(x, scores.type="table"){
# following:
# https://stat.ethz.ch/pipermail/r-help/2005-July/076371.html
  
#   tablespearman=function(x)
#   {
#     # Details algorithme manuel SAS PROC FREQ page 540
#     # Statistic
#     n=sum(x)
#     nr=nrow(x)
#     nc=ncol(x)
#     tmpd=cbind(expand.grid(1:nr,1:nc))
#     ind=rep(1:(nr*nc),as.vector(x))
#     tmp=tmpd[ind,]
#     rhos=cor(apply(tmp,2,rank))[1,2]
#     # ASE
#     Ri=scores(x,1,"ranks")- n/2
#     Ci=scores(x,2,"ranks")- n/2
#     sr=apply(x,1,sum)
#     sc=apply(x,2,sum)
#     F=n^3 - sum(sr^3)
#     G=n^3 - sum(sc^3)
#     w=(1/12)*sqrt(F*G)
#     vij=data
#     for (i in 1:nrow(x))
#     {
#       qi=0
#       if (i<nrow(x))
#       {
#         for (k in i:nrow(x)) qi=qi+sum(x[k,]*Ci)
#       }
#     }
#     for (j in 1:ncol(x))
#     {
#       qj=0
#       if (j<ncol(x))
#       {
#         for (k in j:ncol(x)) qj=qj+sum(x[,k]*Ri)
#       }
#       vij[i,j]=n*(Ri[i]*Ci[j] +
#                     0.5*sum(x[i,]*Ci)+0.5*sum(data[,j]*Ri) +qi+qj)
#     }
#     
#     
#     v=sum(data*outer(Ri,Ci))
#     wij=-n/(96*w)*outer(sr,sc,FUN=function(a,b) return(a^2*G+b^2*F))
#     zij=w*vij-v*wij
#     zbar=sum(data*zij)/n
#     vard=(1/(n^2*w^4))*sum(x*(zij-zbar)^2)
#     ASE=sqrt(vard)		
#     # Test
#     vbar=sum(x*vij)/n
#     p1=sum(x*(vij-vbar)^2)
#     p2=n^2*w^2
#     var0=p1/p2
#     stat=rhos/sqrt(var0)
#     
#     # Output
#     out=list(estimate=rhos,ASE=ASE,name="Spearman
# correlation",bornes=c(-1,1))
#     class(out)="ordtest"
#     return(out)
#   }
#   
#   #tablespearman(data)
}



# all association measures combined

Assocs <- function(x, conf.level = 0.95, out = c("def", "ext")){

  out <- match.arg(out)
  # if(is.null(verbose)) verbose <- "3"
  # if(verbose != "3") conf.level <- NA


  res <- rbind(
    # "Phi Coeff." = c(Phi(x), NA, NA)
    "Contingency Coeff." = c(ContCoef(x),NA, NA)
  )

  if(is.na(conf.level)){
    res <- rbind(res, "Cramer V" = c(CramerV(x), NA, NA))
    res <- rbind(res, "Kendall Tau-b" = c(KendallTauB(x), NA, NA))
  } else {
    res <- rbind(res, "Cramer V" = CramerV(x, conf.level=conf.level))
    res <- rbind(res, "Kendall Tau-b" = c(KendallTauB(x, conf.level=conf.level)))
  }

  if(out == "ext") {

    # # this is from boot::corr combined with ci logic from cor.test
    # r <- boot::corr(d=CombPairs(1:nrow(x), 1:ncol(x)), as.vector(x))
    
    # the boot::corr does not respect ordinal values in dimnames
    # so do it ourselves
    r <- TablePearson(x)
    
    r.ci <- CorCI(rho = r, n = sum(x), conf.level = conf.level)

    res <- rbind(res
      , "Goodman Kruskal Gamma" = GoodmanKruskalGamma(x, conf.level=conf.level)
      # , "Kendall Tau-b" = KendallTauB(x, conf.level=conf.level)
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

  if(out == "ext")
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
    out[c(1,16), 2:3] <- "      -"
  }
  dimnames(out) <- dimnames(x)

  print(data.frame(out), quote=FALSE)
}




## This is an exact copy from Hmisc
## Changes since sent to statlib: improved printing N matrix in print.hoeffd

# https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.5/procstat/procstat_corr_details07.htm

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









