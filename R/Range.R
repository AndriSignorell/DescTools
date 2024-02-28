

#' (Robust) Range 
#' 
#' Determines the range of the data, which can possibly be trimmed before
#' calculating the extreme values. The robust range version is calculated on
#' the basis of the trimmed mean and variance (see Details). 
#' 
#' The R base function range returns the minimum and maximum value of a numeric
#' object. Here we return the span of a (possibly trimmed) numeric vector, say
#' the difference of maximum and minimum value.
#' 
#' If robust is set to \code{TRUE} the function determines the trimmed mean m
#' and then the "upper trimmed mean" s of absolute deviations from m,
#' multiplied by \code{fac} (fac is 3 by default). The robust minimum is then
#' defined as m-fac*s or min(x), whichever is larger, and similarly for the
#' maximum.
#' 
#' @param x a numeric vector.
#' @param trim the fraction (0 to 0.5) of observations to be trimmed from each
#' end of \code{x} before the mean is computed.  Values of trim outside that
#' range are taken as the nearest endpoint. Default is 0 for
#' \code{robust=FALSE} and 0.2 for \code{robust=TRUE}
#' @param robust logical, determining whether the robust or the convential
#' range should be returned.
#' @param na.rm a logical value indicating whether \code{NA} values should be
#' stripped before the computation proceeds.
#' @param ... the dots are sent to \code{RobRange} and can be used to set
#' \code{fac} (See details).
#' 
#' @return If \code{trim} is zero (the default), the arithmetic mean of the
#' values in \code{x} is computed, as a numeric or complex vector of length
#' one.  If \code{x} is not logical (coerced to numeric), numeric (including
#' integer) or complex, \code{NA_real_} is returned, with a warning.
#' 
#' If \code{trim} is non-zero, a symmetrically trimmed mean is computed with a
#' fraction of \code{trim} observations deleted from each end before the mean
#' is computed.
#' 
#' @author Werner Stahel, ETH Zurich (robust range)\cr Andri Signorell
#' <andri@@signorell.net> 
#' 
#' @seealso \code{\link{range}}, \code{\link{min}}, \code{\link{max}} 
#' 
#' @keywords univar
#' @examples
#' 
#' x <- c(0:10, 50)
#' xm <- Range(x)
#' c(xm, Range(x, trim = 0.10))
#' 
#' x <- c(rnorm(20), rnorm(3, 5, 20))
#' Range(x, robust=TRUE)
#' 
#' # compared to
#' Range(x)
#' 



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

