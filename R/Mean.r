


#' (Weighted) Arithmetic Mean
#' 
#' Generic function for the (trimmed) arithmetic mean, possibly with given
#' weights.
#' 
#' 
#' @aliases Mean Mean.default Mean.Freq
#' @param x An object.  Currently there are methods for numeric/logical vectors
#' and \link[=Dates]{date}, \link{date-time} and \link{time interval} objects.
#' Complex vectors are allowed for \code{trim = 0} only.

#' @param weights a numerical  weights 
#' giving the weights to use for elements of \code{x}.

#' @param weights
#' non-negative numeric vector of weights of the same length as \code{x}
#' interpreted as frequency (replication) weights. 
#' Observations with larger weights contribute
#' more strongly to the empirical distribution.
 
#' @param trim the fraction (0 to 0.5) of observations to be trimmed from each
#' end of \code{x} before the mean is computed.  Values of trim outside that
#' range are taken as the nearest endpoint.
#' @param na.rm a logical value indicating whether \code{NA} values should be
#' stripped before the computation proceeds.
#' @param breaks breaks for calculating the mean for classified data as
#' composed by \code{\link{Freq}}.
#' @param \dots further arguments passed to or from other methods.

#' @details
#' The argument \code{weights} is interpreted as frequency (replication)
#' weights. Conceptually, this corresponds to computing the statistic
#' on a reweighted empirical distribution, where observations with larger
#' weights represent a higher frequency in the population.
#' 
#' \bold{Note:}\verb{    }
#' Analytic (precision) weights, which assume observation-specific error
#' variances or likelihood-based weighting, are deliberately not supported.
#' This design ensures that all weighted statistics remain well-defined
#' for ordinal, robust, and distribution-based measures such as medians,
#' quantiles, and measures of dispersion.
#' 
#' \code{trim} and \code{weights} can't be used together at the same time.

#' @return If \code{trim} is zero (the default), the arithmetic mean of the
#' values in \code{x} is computed, as a numeric or complex vector of length
#' one.  If \code{x} is not logical (coerced to numeric), numeric (including
#' integer) or complex, \code{NA_real_} is returned, with a warning.
#' 
#' If \code{trim} is non-zero, a symmetrically trimmed mean is computed with a
#' fraction of \code{trim} observations deleted from each end before the mean
#' is computed.
#' 
#' @seealso \code{\link{weighted.mean}}, \code{\link{mean.POSIXct}},
#' \code{\link{colMeans}} for row and column means.
#' @references Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The
#' New S Language}.  Wadsworth & Brooks/Cole.
#' @keywords univar
#' @examples
#' 
#' x <- c(0:10, 50)
#' xm <- Mean(x)
#' c(xm, Mean(x, trim = 0.10))
#'



Mean <- function (x, ...)
  UseMethod("Mean")


#' @rdname Mean
#' @export
Mean.Freq <- function(x, breaks, ...)  {
  sum(head(MoveAvg(breaks, order=2, align="left"), -1) * x$perc)
}


#' @rdname Mean
#' @export
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
    z <- .NormWeights(x, weights, na.rm=na.rm, zero.rm=TRUE)
    
    # we get no 0-weights back here...
    sum(z$x * z$weights) / z$wsum
    
  }
  
}
