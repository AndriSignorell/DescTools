
# =========================================================
# PABAK  (2 Rater; optional generalisierte K-Variante)
# =========================================================

#' PABAK (Prevalence- And Bias-Adjusted Kappa)
#'
#' @description
#' Berechnet PABAK. Für 2 Rater gilt \code{PABAK = 2*Po - 1}. Optional kann
#' die generalisierte Mehrkategorien-Form \eqn{(K*Po - 1)/(K - 1)} benutzt
#' werden; für \eqn{K=2} sind beide identisch.
#'
#' @param x (Default) \eqn{n\times m}-Matrix/Dataframe: Zeilen = Subjekte,
#' Spalten = Rater; Werte = Kategorien (Faktor/Char/Integer), \code{NA} erlaubt.
#' @param y (optional), second vector of ratings
#' @param formula (Formel) \code{y ~ subj | rater} for long shaped data.
#' @param data,subset,na.action,... wie üblich; werden an \code{LongToSquare()} übergeben.
#' @param generalized logical, Standard \code{FALSE}. Wenn \code{TRUE}, wird
#' die Mehrkategorien-Form \eqn{(K*Po - 1)/(K - 1)} verwendet (für m=2 empfohlen,
#' wenn \eqn{K>2}).
#'
#' @return \code{htest}-Objekt mit \code{statistic = PABAK}, \code{estimate = Po},
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
#'                  PABAK(rating ~ subj | rater, data=d.long)
#'                  
#' @concept{ ~irr }


#' @export
PABAK <- function(x, ..., generalized = FALSE) UseMethod("PABAK")


#' @rdname PABAK
#' @export
PABAK.default <- function(x, y=NULL, ..., generalized = FALSE) {
  
  
  m <- .NormalizeToConfusion(x, y)

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


#' @rdname PABAK
#' @export
PABAK.formula <- function(formula, data, subset, na.action, 
                          ..., generalized = FALSE) {
  
  cl <- match.call(expand.dots = FALSE)
  cl[[1L]] <- getFromNamespace(".LongToSquare", "DescTools")
  m <- eval.parent(cl)
  
  res <- PABAK.default(m[, -1, drop = FALSE], generalized = generalized)
  attr(res, "data.name") <- attr(m, "data.name")
  
  return(res)  
  
}

