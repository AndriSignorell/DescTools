#' Randolph's Free-Marginal Multirater Kappa
#'
#' Computes Randolph's free-marginal multirater kappa for \eqn{m} raters over
#' \eqn{N} subjects. This agreement coefficient does not assume fixed marginal
#' distributions (i.e., it is free-marginal).
#'
#' @name RandolphKappa
#' @aliases RandolphKappa RandolphKappa.default RandolphKappa.formula
#'
#' @param x (Default method) A matrix of size \eqn{N \times m} with subjects in
#'   rows and raters in columns; cells contain the assigned categories.
#' @param formula (Formula method) A model formula of the form
#'   \code{response ~ subject + rater} describing ratings in long format.
#' @param data (Formula method) A \code{data.frame} containing the variables
#'   referenced in \code{formula}.
#' @param subset (Formula method) Optional logical vector or expression to select
#'   a subset of rows from \code{data}.
#' @param na.action (Formula method) A function indicating what should happen
#'   when the data contain \code{NA}s.
#' @param ... Reserved for future extensions; currently unused.
#' @param conf.level Numeric confidence level (e.g., \code{0.95}) for bootstrap
#'   confidence intervals. Currently no interval is computed; passing
#'   \code{NA} (the default) skips any CI.
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
#' RandolphKappa(rating ~ subject + rater, data = df)


#' @export
RandolphKappa <- function(x, ..., conf.level = NA) UseMethod("RandolphKappa")


#' @rdname RandolphKappa
#' @export
#' @method RandolphKappa default
RandolphKappa.default <- function(x, ..., conf.level = NA) {

    # x: matrix subjects x raters
    N <- nrow(x)
    m <- ncol(x)
    k <- length(unique(as.vector(x)))
    
    # pro Subjekt die maximal übereinstimmende Kategorie
    agree_per_item <- apply(x, 1, function(row) {
      tab <- table(row)
      max(tab) / m
    })
    
    Po <- mean(agree_per_item)
    
    Pe <- 1 / k
    kappa <- (Po - Pe) / (1 - Pe)
    
    return(kappa)

}


#' @rdname RandolphKappa
#' @export
#' @method RandolphKappa formula

RandolphKappa.formula <- function(formula, data, subset, na.action,
# there is only bootstrap    ci_method = c("none", "bootstrap"),
                                  ..., conf.level = NA) {
  
  cl <- match.call(expand.dots = FALSE)
  cl[[1L]] <- getFromNamespace(".LongToSquare", "DescTools")
  m <- eval.parent(cl)
  
  res <- RandolphKappa.default(m[, -1, drop = FALSE], 
                               conf.level = conf.level, ...)
  
  res$data.name <- attr(m, "data.name")
  return(res)
  
}





