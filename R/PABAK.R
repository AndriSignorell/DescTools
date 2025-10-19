
# News message:
# * GwetAC1(), GwetAC2(), RandolphKappa() are further rater agreement measures. 
# * PABAK() returns the Prevalence- And Bias-Adjusted Kappa. 



# =========================================================
# PABAK  (2 Rater; optional generalisierte K-Variante)
# =========================================================

#' PABAK (Prevalence- And Bias-Adjusted Kappa)
#'
#' @description
#' Berechnet PABAK. Für 2 Rater gilt \code{PABAK = 2*Po - 1}. Optional kann
#' die generalisierte Mehrkategorien-Form \eqn{(K*Po - 1)/(K - 1)} benutzt
#' werden; für \eqn{K=2} sind beide identisch.
#' Calculates PABAK. For 2 raters, PABAK = 2*Po - 1. 
#' Optionally, the generalised multi-category form can be used; 
#' for K=2, both are identical.
#'
#' @param x (Default) \eqn{n\times m}-matrix/data.frame: rows = subjects,
#' columns = rater; values = categories (factor/char/integer), \code{NA} allowed.
#' @param y (optional), second vector of ratings
#' @param generalized logical, (default \code{FALSE}). If \code{TRUE}, 
#' the multicategory-form \eqn{(K*Po - 1)/(K - 1)} will be used (recommended for m=2),
#' if \eqn{K>2}).
#' @param conf.level Confidence level for bootstrap confidence intervals 
#'   of Krippendorff's alpha. If \code{NA} (default), no bootstrap is computed.
#' @param ... further arguments are passed to the \code{\link[boot]{boot}} function.
#' Supported arguments are \code{type} (\code{"norm"}, \code{"basic"},
#' \code{"stud"}, \code{"perc"}, \code{"bca"}), \code{parallel} and the number
#' of bootstrap replicates \code{R}. If not defined those will be set to their
#' defaults, being \code{"basic"} for \code{type}, option
#' \code{"boot.parallel"} (and if that is not set, \code{"no"}) for
#' \code{parallel} and \code{999} for \code{R}.
#'
#'
#' @return \code{htest}-object with \code{statistic = PABAK}, \code{estimate = Po},
#' \code{parameter = c(subjects, raters)} und \code{method}.

#' @examples
#' # https://support.sas.com/resources/papers/proceedings09/242-2009.pdf
#' m <- matrix(c(95, 1, 4, 0), nrow=2,
#'             dimnames = list("rater_A"=c("yes","no"),
#'                             "rater_B"=c("yes","no")))
#' d.frm <- Untable(m)
#' 
#' # the matrix interface, interpreted as confusion matrix
#' PABAK(m)
#' 
#' # when provided as data.frame, we think it's a wide form
#' PABAK(d.frm)
#' with(d.frm, PABAK(rater_A, rater_B, conf.level=0.95))
#' 
#' # most flexible and controllable in long form
#' d.long <- ToLong(d.frm, incl.rownames = TRUE, 
#'                  varnames=c("rater","rating","subj"))
#'                  
#' PABAK(RaterFrame(rating ~ subj | rater, 
#'                  data=d.long, incl.subj=FALSE))
#'                  
#' @concept{ ~irr }



#' @rdname PABAK
#' @export
PABAK <- function(x, y=NULL, generalized = FALSE, 
                  conf.level=NA, ...) {
  
  
  m <- NormalizeToConfusion(x, y)

  nc <- ncol(m)
  # if (nc != 2)
  #   warning("PABAK is classically defined for two raters; here, m =", m,
  #           ". Result is calculated using Po, interpretation with caution.")
  
  total <- sum(m)
  
  # observed agreement, mean pairwise agreement per subject
  Po <- sum(diag(m)) / total
  # categories (global) (nrow(X))
  K  <- nrow(m)
  # Gesamtzaehlung (bei Proportionen ~1); als numeric zur Transparenz
  n  <- as.numeric(total)   # bei normalisierter Matrix ~1

  stat <- if (!generalized) {
    2 * Po - 1 
    
  } else {
    if (K < 2) stop("K < 2 not sensible")
    (K * Po - 1) / (K - 1)
  }
  
  attr(stat, "raters") <- nc
  attr(stat, "subjects") <- n
  # observed agreement
  attr(stat, "Po") <- Po
  
  return(stat)
  
}

