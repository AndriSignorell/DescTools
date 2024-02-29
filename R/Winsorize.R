

#' Winsorize (Replace Extreme Values by Less Extreme Ones)
#' 
#' Winsorizing a vector means that a predefined quantum of the smallest and/or
#' the largest values are replaced by less extreme values. Thereby the
#' substitute values are the most extreme retained values.
#' 
#' 
#' The winsorized vector is obtained by
#' 
#' \deqn{g(x) = }{wins(x) = -c if x < -c, c if x > c, x otherwise}\deqn{
#' \left\{\begin{array}{ll} }{wins(x) = -c if x < -c, c if x > c, x
#' otherwise}\deqn{ -c &\textup{for }x \le -c\\ }{wins(x) = -c if x < -c, c if
#' x > c, x otherwise}\deqn{ x &\textup{for } |x| < c\\ }{wins(x) = -c if x <
#' -c, c if x > c, x otherwise}\deqn{ c &\textup{for }x \ge c }{wins(x) = -c if
#' x < -c, c if x > c, x otherwise}\deqn{ \end{array}\right. }{wins(x) = -c if
#' x < -c, c if x > c, x otherwise}
#' 
#' You may also want to consider standardizing (possibly robustly) the data
#' before you perform a winsorization.
#' 
#' @param x a numeric vector to be winsorized.
#' 
#' @param val the low border, all values being lower than this will be
#' replaced by this value.  The default is set to the 5%-quantile of x.
#' 
#' @return A vector of the same length as the original data \code{x} containing
#' the winsorized data.
#' 
#' @author Andri Signorell <andri@@signorell.net>
#' 
#' @seealso \code{\link[robustHD]{winsorize}} from the package \code{robustHD} contains
#' an option to winsorize multivariate data
#' 
#' \code{\link{scale}}, \code{\link{RobScale}}
#' @keywords univar robust
#' @examples
#' 
#' 
#' library(DescTools)
#' 
#' ## generate data
#' set.seed(9128)
#' x <- round(runif(100) * 100, 1)
#' 
#' (d.frm <- DescTools::Sort(data.frame(
#'   x, 
#'   default   = Winsorize(x), 
#'   quantile  = Winsorize(x, quantile(x, probs=c(0.1, 0.8), na.rm = FALSE)), 
#'   fixed_val = Winsorize(x, val=c(15, 85)),
#'   fixed_n   = Winsorize(x, val=c(Small(x, k=3)[3], Large(x, k=3)[1])),
#'   closest   = Winsorize(x, val=Closest(x, c(30, 70))) 
#' )))[c(1:10, 90:100), ]
#' 
#' # use Large and Small, if a fix number of values should be winsorized (here k=3)
#' 
#' PlotLinesA(SetNames(d.frm, rownames=NULL), lwd=2, col=Pal("Tibco"), 
#'            main="Winsorized Vector")

#' z <- 0:10
#' # twosided (default):
#' Winsorize(z, val=c(2,8))
#' 
#' # onesided:
#' # ... replace all values > 8 with 8
#' Winsorize(z, val=c(min(z), 8))
#' # ... replace all values < 4 with 4
#' Winsorize(z, val=c(4, max(z)))
#' 



Winsorize <- function(x, val = quantile(x, probs=c(0.05, 0.95), na.rm = FALSE)) {
  
  x[x < val[1L]] <- val[1L]
  x[x > val[2L]] <- val[2L]
  
  return(x)
  
}



# Old (2024-02-27):
# Winsorize <- function(x, minval = NULL, maxval = NULL,
#                       probs=c(0.05, 0.95), na.rm = FALSE, type=7) {
# 
#   if(is.null(minval) || is.null(maxval)){
#     xq <- quantile(x=x, probs=probs, na.rm=na.rm, type=type)
#     if(is.null(minval)) minval <- xq[1L]
#     if(is.null(maxval)) maxval <- xq[2L]
#   }
#   
#   x[x<minval] <- minval
#   x[x>maxval] <- maxval
#   
#   return(x)
# 
# }

