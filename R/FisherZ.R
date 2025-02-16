
#' Fisher z-Transformation
#' 
#' Convert a correlation to a z score or z to r using the Fisher transformation.
#' 
#' The sampling distribution of Pearson's r is not normally distributed. Fisher
#' developed a transformation now called "Fisher's z-transformation" that
#' converts Pearson's r to the normally distributed variable z. The formula for
#' the transformation is:
#' 
#' \deqn{z_r = tanh^{-1}(r) = \frac{1}{2}log\left ( \frac{1+r}{1-r}\right )}
#' 
#' @aliases FisherZ FisherZInv

#' @param rho the Pearson's correlation coefficient
#' @param z a Fisher z transformed value
#' @return z value corresponding to r (in FisherZ) \cr r corresponding to z (in
#' FisherZInv) \cr
#' @author William Revelle <revelle@@northwestern.edu>, \cr slight
#' modifications Andri Signorell <andri@@signorell.net> based on R-Core code
#' @seealso \code{\link{cor.test}}
#' @keywords multivariate models
#' @examples
#' 
#' cors <- seq(-.9, .9, .1)
#' 
#' zs <- FisherZ(cors)
#' rs <- FisherZInv(zs)
#' round(zs, 2)
#' n <- 30
#' r <- seq(0, .9, .1)
#' rc <- t(sapply(r, CorCI, n=n))
#' t <- r * sqrt(n-2) / sqrt(1-r^2)
#' p <- (1 - pt(t, n-2)) / 2
#' 
#' r.rc <- data.frame(r=r, z=FisherZ(r), lower=rc[,2], upper=rc[,3], t=t, p=p)
#' 
#' round(r.rc,2)
#' 

# old - replaced by 0.99.60
#  FisherZ <- function(rho)  { 0.5*log((1+rho)/(1-rho)) }   #converts r to z
#  FisherZInv <- function(z) {(exp(2*z)-1)/(1+exp(2*z)) }   #converts back again


#' @rdname FisherZ
#' @export
FisherZ <- function(rho)  { atanh(rho) }   # converts r to z

#' @rdname FisherZ
#' @export
FisherZInv <- function(z) { tanh(z)    }   # converts back again


