


#' Add Up Partial Confidence Intervals to a Total CI
#' 
#' Starting with a response variable that obtains different 
#' confidence intervals (CI) when calculated with different 
#' explanatory variables, all the values of the response variable 
#' should be added up. This function returns the CI for the sum.
#' 
#' @param x a matrix with 3 columns, containing the estimate in the first column 
#' followed by the lower and the upper confidence interval .

#' @return a vector with the sum and the lower, upper confidence 
#' bound of the confidence interval

#' @author Andri Signorell <andri@signorell.net>

#' @seealso \code{\link{BinomCI}},

#' @references \url{StackExchange}{https://stats.stackexchange.com/questions/223924/how-to-add-up-partial-confidence-intervals-to-create-a-total-confidence-interval}

#' @examples
#' x <- do.call(rbind, 
#'              tapply(d.pizza$delivery_min, 
#'                     d.pizza$area, MeanCI))
#' SumCI(x)
#' 


SumCI <- function(x){

  # https://stats.stackexchange.com/questions/223924/how-to-add-up-partial-confidence-intervals-to-create-a-total-confidence-interval
  # x is a matrix with est, lci, uci

  # define half width of ci
  ci <- x[, 3] - x[, 1]

  # half ci for the sum is sqrt(sum(ci^2))
  ci_sum <- sqrt(sum(ci^2))

  res <- SetNames(c(sum(x[, 1]) +  c(0,-1,1) * ci_sum),
                  names=c("sum", "lwr.ci", "upr.ci"))
  
  return(res)

}

