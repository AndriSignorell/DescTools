#' Randolph's Free-Marginal Multirater Kappa
#'
#' Computes Randolph's free-marginal multirater kappa for \eqn{m} raters over
#' \eqn{N} subjects. This agreement coefficient does not assume fixed marginal
#' distributions (i.e., it is free-marginal).
#'
#' @name RandolphKappa
#'
#' @param x (Default method) A matrix of size \eqn{N \times m} with subjects in
#'   rows and raters in columns; cells contain the assigned categories.
#' @param conf.level Numeric confidence level (e.g., \code{0.95}) for bootstrap
#'   confidence intervals. Currently no interval is computed; passing
#'   \code{NA} (the default) skips any CI.
#' @param ... further arguments are passed to the \code{\link[boot]{boot}} function.
#' Supported arguments are \code{type} (\code{"norm"}, \code{"basic"},
#' \code{"stud"}, \code{"perc"}, \code{"bca"}), \code{parallel} and the number
#' of bootstrap replicates \code{R}. If not defined those will be set to their
#' defaults, being \code{"basic"} for \code{type}, option
#' \code{"boot.parallel"} (and if that is not set, \code{"no"}) for
#' \code{parallel} and \code{999} for \code{R}.
#'
#' @details
#' Let \eqn{k} be the number of distinct categories across all ratings,
#' \eqn{m} the number of raters, and \eqn{N} the number of subjects. Randolph's
#' kappa is
#' \deqn{\kappa = \frac{P_o - 1/k}{1 - 1/k},}
#' where the observed agreement \eqn{P_o} is
#' \deqn{P_o = \frac{1}{N} \sum_{i=1}^{N} \frac{\max_j n_{ij}}{m}.}
#' Here, \eqn{n_{ij}} denotes the number of raters who assigned subject \eqn{i}
#' to category \eqn{j}; \eqn{\max_j n_{ij}} is the modal (most frequent)
#' category count per subject.
#'
#' The \code{formula} method reshapes long-format data to a subjects\eqn{\times}raters
#' matrix internally (via \code{DescTools::.LongToSquare}) and then calls the
#' \code{default} method.
#'
#' @return
#' A numeric scalar: the value of Randolph's kappa.
#'
#' @references
#' Randolph, J. J. (2005). Free-Marginal Multirater Kappa (multirater \eqn{\kappa_{\mathrm{free}}}):
#' An Alternative to Fleiss’ Fixed-Marginal Multirater Kappa. Online submission.
#'
#' @seealso \code{\link{KrippAlpha}}, \code{\link{KappaM}}
#'
#' @examples
#' ## Matrix (subjects x raters), 5 subjects, 3 raters
#' x <- matrix(c(
#'   1,1,1,
#'   2,2,2,
#'   1,2,1,
#'   3,3,3,
#'   2,2,1
#' ), ncol = 3, byrow = TRUE)
#' RandolphKappa(x)
#'
#' ## Long format with a formula
#' df <- data.frame(
#'   subject = rep(1:5, each = 3),
#'   rater   = rep(paste0("r", 1:3), times = 5),
#'   rating  = c(1,1,1, 2,2,2, 1,2,1, 3,3,3, 2,2,1)
#' )
#' RandolphKappa(RaterFrame(rating ~ subject | rater, 
#'                          data = df, drop.subj=TRUE))



#' @rdname RandolphKappa
#' @export
RandolphKappa <- function(x, conf.level = NA, ...) {

    # x: matrix subjects x raters
    N <- nrow(x)
    m <- ncol(x)
    k <- length(unique(as.vector(x)))
    
    # the category with the highest agreement for each topic
    agree_per_item <- apply(x, 1, function(row) {
      tab <- table(row)
      max(tab) / m
    })
    
    Po <- mean(agree_per_item)
    
    Pe <- 1 / k
    kappa <- (Po - Pe) / (1 - Pe)
    
    return(kappa)

}





