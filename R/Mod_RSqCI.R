

#' Confidence Intervals for the R squared of a Linear Model
#'
#' Calculate bootstrap intervals for the the R squared of a linear model as returned
#' by \code{\link{lm}}.
#'
#' @param object the model object as returned by glm.
#' @param conf.level confidence level of the interval.
#' @param sides a character string specifying the side of the confidence
#' interval, must be one of \code{"two.sided"} (default), \code{"left"} or
#' \code{"right"}. \code{"left"} would be analogue to a hypothesis of
#' \code{"greater"} in a \code{t.test}. You can specify just the initial
#' letter.
#' @param adjusted logical, defining if the R squared or the adjusted R squared
#' should be used. Default is \code{TRUE}, returning the latter.
#' @param ... further arguments are passed to the \code{\link{boot}} function.
#' Supported arguments are \code{type} (\code{"norm"}, \code{"basic"},
#' \code{"stud"}, \code{"perc"}, \code{"bca"}), \code{parallel} and the number
#' of bootstrap replicates \code{R}. If not defined those will be set to their
#' defaults, being \code{"basic"} for \code{type}, option
#' \code{"boot.parallel"} (and if that is not set, \code{"no"}) for
#' \code{parallel} and \code{999} for \code{R}.

#' @return a numeric vector with 3 elements: \item{mean}{mean}
#' \item{lwr.ci}{lower bound of the confidence interval} \item{upr.ci}{upper
#' bound of the confidence interval}

#' @author Andri Signorell <andri@@signorell.net>
#'
#' @seealso \code{\link{BrierScore}}

#' @keywords confidence interval

#' @examples
#'
#' # get linear model
#' r.lm <- lm(Fertility ~ Agriculture + Examination + Education
#'                          + Catholic + Infant.Mortality, data=swiss)
#'
#' # calculate confidence intervals for the R2
#' summary(r.lm)$r.squared
#'
#' RSqCI(r.lm, R=99)   # use higher R in real life!


RSqCI <- function(object, conf.level = 0.95, sides = c("two.sided", "left", "right"),
                  adjusted=TRUE, ...){

  if(adjusted)
    R2 <- "adj.r.squared"
  else
    R2 <- "r.squared"

  .BootCI(DATA = object$model,
          FUN =  function(data, i)
            summary(lm( update( object, .~., data = data[i, ]) ))[[R2]],
          conf.level = conf.level, sides = sides, ... )

}
