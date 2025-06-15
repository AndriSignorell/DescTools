

#' Confidence Intervals for the C Statistic (AUC)
#'
#' Calculate bootstrap intervals for the C statistic (Area under the curve AUC),
#' based on a glm.
#'
#' @param object the model object as returned by glm.
#' @param conf.level confidence level of the interval.
#' @param sides a character string specifying the side of the confidence
#' interval, must be one of \code{"two.sided"} (default), \code{"left"} or
#' \code{"right"}. \code{"left"} would be analogue to a hypothesis of
#' \code{"greater"} in a \code{t.test}. You can specify just the initial
#' letter.
#' @param ... further arguments are passed to the \code{\link[boot]{boot}} function.
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
#' utils::data(Pima.te, package = "MASS")
#  get binomial model
#' r.logit <- glm(type ~ ., data=Pima.te, family="binomial")
#'
#' # calculate Brier score with confidence intervals
#' Cstat(r.logit)
#' CstatCI(r.logit, R=99)  # use higher R in real life!


CstatCI <- function(object, conf.level = 0.95,
                    sides = c("two.sided", "left", "right"), ...){
  UseMethod("CstatCI")
}


CstatCI.default <- function(object, conf.level = 0.95,
                            sides = c("two.sided", "left", "right"), ...)
  .NotThere(object)


CstatCI.glm <- function(object, conf.level = 0.95,
                        sides = c("two.sided", "left", "right"), ...){

  # example:
  #   CstatCI(r.base, R=99)
  #   CstatCI(1:5)

  .BootCI(DATA = object$data,
          FUN =  function(data, i)
            Cstat( update( object, .~., data = data[i, ]) ),
          conf.level = conf.level, sides = sides, ... )

}


CstatCI.FitMod <- function (object, conf.level = 0.95, 
                            sides = c("two.sided", "left", "right"), ...) {
  
  .BootCI(DATA = object$data, 
                      FUN = function(data, i) Cstat(update(object, . ~ ., data = data[i, ])), 
                      conf.level = conf.level, sides = sides, 
                      ...)
  
}



