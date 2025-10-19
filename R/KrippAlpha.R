
#' Krippendorff's Alpha for Wide Data

#' @description
#' Computes Krippendorff's alpha coefficient of interrater reliability 
#' from data in wide format (with \eqn{m} raters). The function 
#' supports nominal, ordinal, interval, and ratio measurement levels.
#'
#' @name KrippAlpha
#' @aliases KrippAlpha 
#' 
#' @param x A data frame, matrix, or similar wide-format object containing 
#'   ratings (columns = raters, rows = subjects/items).
#' @param method Character string specifying the measurement level. 
#'   One of \code{"nominal"}, \code{"ordinal"}, \code{"interval"}, 
#'   or \code{"ratio"}.
#' @param levels Optional vector specifying the set of possible categories, resp. 
#'  scale values corresponding to the rating categories (required for \code{"interval"} and 
#'   \code{"ratio"} methods).
#'   If \code{NULL}, levels are inferred from the data.
#' @param raters Optional vector specifying which columns of \code{x} 
#'   are the raters. If \code{NULL}, all columns are assumed to be raters.
#' @param conf.level Confidence level for bootstrap confidence intervals 
#'   of Krippendorff's alpha. If \code{NA} (default), no bootstrap is computed.
#' @param out One out of \code{c("def", "ext")} for extended results, \code{"def"} is default.
#' 
#' @param ... further arguments are passed to the \code{\link[boot]{boot}} function.
#' Supported arguments are \code{type} (\code{"norm"}, \code{"basic"},
#' \code{"stud"}, \code{"perc"}, \code{"bca"}), \code{parallel} and the number
#' of bootstrap replicates \code{R}. If not defined those will be set to their
#' defaults, being \code{"basic"} for \code{type}, option
#' \code{"boot.parallel"} (and if that is not set, \code{"no"}) for
#' \code{parallel} and \code{999} for \code{R}.
#'
#' @details
#' The function constructs the coincidence matrix from the wide-format data 
#' using \code{CoincidenceFromWide} and then calculates 
#' Krippendorff's alpha based on the chosen distance metric:
#' \itemize{
#'   \item \code{"nominal"}: Disagreement is 0 if equal, 1 otherwise.
#'   \item \code{"ordinal"}: Squared difference of cumulative proportions.
#'   \item \code{"interval"}: Squared Euclidean distances of scale values.
#'   \item \code{"ratio"}: Squared relative differences of scale values.
#' }
#'
#' @return A list with elements:
#' \describe{
#'   \item{alpha}{Krippendorff's alpha coefficient.}
#'   \item{Do}{Observed disagreement.}
#'   \item{De}{Expected disagreement under chance.}
#'   \item{O}{Coincidence matrix.}
#'   \item{n_v}{Category totals in coincidence space.}
#'   \item{delta2}{Pairwise distance matrix according to the method.}
#'   \item{ci}{Bootstrap confidence interval, or \code{NA} if not computed.}
#' }
#'
#' @references
#' Krippendorff, K. (2018). \emph{Content Analysis: An Introduction to Its Methodology}.
#' Sage Publications.  
#'
#' @seealso \code{\link{BootCI}}
#'
#' @examples
#' # Example with nominal data (3 raters, 5 subjects)
#' dat <- data.frame(
#'   r1 = c(1, 2, 1, 3, 2),
#'   r2 = c(1, 2, 2, 3, 2),
#'   r3 = c(1, 2, 1, 3, 1)
#' )
#' KrippAlpha(dat, method = "nominal")
#'
#' # Interval-scaled example
#' dat2 <- data.frame(
#'   r1 = c(1, 4, 5, 7, 2),
#'   r2 = c(2, 5, 6, 7, 1),
#'   r3 = c(1, 4, 6, 6, 2)
#' )
#' KrippAlpha(dat2, method = "interval", levels = 1:7)
#'



# Build Krippendorff's coincidence matrix O from wide data (m raters),
# with per-item weighting by 1 / (m_s - 1) as in KALPHA.


#' @rdname KrippAlpha
#' @export

# Krippendorff's alpha from wide data (m raters), using O from above.
KrippAlpha <- function(x, method = c("nominal","ordinal",
                                  "interval","ratio"),
                       levels = NULL, 
                       raters = NULL, 
                       conf.level = NA, out = c("def", "ext"),
                       ...) {
  
  method <- match.arg(method)
  out <- match.arg(out)
  
  O <- .CoincidenceFromWide(x, raters = raters, levels = levels)
  
  n_v  <- rowSums(O)           # category totals in coincidence space
  n_pairs <- sum(n_v)          # total pairable values
  if (n_pairs <= 1) stop("Too few valid pairs (n < 2).")
  
  K <- nrow(O)
  delta2 <- matrix(0, K, K, dimnames = dimnames(O))
  
  if (method == "nominal") {
    delta2[] <- 1
    diag(delta2) <- 0
    
  } else if (method == "ordinal") {
    # mid-cumulative proportions (mu_k) based on n_v:
    p  <- n_v / sum(n_v)
    mu <- cumsum(p) - 0.5 * p
    delta2 <- (outer(mu, mu, `-`))^2
    diag(delta2) <- 0
    
  } else {
    
    if (is.null(levels)) levels <- seq_len(K)
    if (length(levels) != K) stop("'levels' must have length K.")
    levels <- as.numeric(levels)
    
    if (method == "interval") {
      D <- outer(levels, levels, `-`)
      delta2 <- D * D
      diag(delta2) <- 0
    } else { # ratio
      S <- outer(levels, levels, `+`)
      D <- outer(levels, levels, `-`)
      delta2 <- (D / pmax(S, .Machine$double.eps))^2
      diag(delta2) <- 0
    }
  }
  
  Do <- sum(O * delta2)
  De <- sum(outer(n_v, n_v, `*`) * delta2) / (n_pairs - 1)
  
  alpha <- if (De <= 0) NA_real_ else 1 - Do / De
  
  if(!is.na(conf.level)){
    
    calc_alpha <- function(x) 
          KrippAlpha(x, 
             method = method,
             levels = levels, 
             raters = raters, 
             conf.level = NA)
    
    ci <- BootCI(x = x, FUN = calc_alpha, ...)
    names(ci) <- c("alpha", "lci","uci")
    
  } else {
    ci <- NA
  }

  if(out == "def"){
    if(!is.na(conf.level)) {
      res <- SetNames(c(alpha, ci[2], ci[3]), 
                      names=c("est","lci","uci"))
    } else {
      res <- alpha
    }
  } else {
    res <- list(alpha = alpha, Do = Do, De = De, 
                O = O, n_v = n_v, delta2 = delta2, ci=ci)
  }
  
  return(res)

}



.CoincidenceFromWide <- function(df, raters = NULL, levels = NULL) {
  
  if(is.matrix(df))
    df <- as.data.frame(df)
  
  # pick rater columns (drop typical ID col "Unit" if present)
  if (is.null(raters)) {
    raters <- setdiff(names(df), c("Unit","unit","ID","Id","id"))
  }
  X <- df[raters]
  
  # collect global levels if not given
  if (is.null(levels)) {
    levels <- sort(unique(unlist(X, use.names = FALSE)))
  }
  levels <- as.character(levels)
  K <- length(levels)
  
  O <- matrix(0, K, K, dimnames = list(levels, levels))
  
  for (s in seq_len(nrow(X))) {
    row <- unlist(X[s, ], use.names = FALSE)
    row <- row[!is.na(row)]
    m_s <- length(row)
    if (m_s < 2L) next
    
    tab <- table(factor(row, levels = levels))
    tvec <- as.numeric(tab)
    
    # unordered coincidences, weighted by 1/(m_s - 1)
    Os <- tvec %o% tvec
    diag(Os) <- diag(Os) - tvec
    
    O <- O + Os / (m_s - 1)
  }
  O
}


# 
# # "Krippendorff alpha ordinal (wide, Hayes/Krippendorff Beispiel)"
# 
# df <- data.frame(
#   Unit = 1:40, 
#   obs1 = c(1L, 1L, 2L, NA, 0L, 0L, 1L, 1L, 2L, 2L, NA, 0L, 1L, 
#            3L, 1L, 1L, 2L, 1L, 1L, 0L, 0L, 0L, 2L, 0L, 1L, 0L, 
#            0L, 1L, 1L, 1L, 1L, 2L, 2L, 3L, 2L, 2L, 2L, 2L, 2L, 1L), 
#   obs2 = c(1L, 1L, 3L, 0L, 0L, 0L, 0L, NA, 2L, 1L, 1L, 0L, 2L, 3L, 
#            1L, 1L, 1L, 2L, 1L, 0L, 0L, 0L, 3L, 0L, 2L, 1L, 0L, 2L, 
#            1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L), 
#   obs3 = c(2L, 0L, 3L, 0L, 0L, 0L, 2L, 2L, 2L, 1L, 0L, 0L, 2L, 2L, 
#            1L, 1L, 2L, 3L, 0L, 0L, 1L, NA, 3L, 0L, NA, 1L, 0L, 1L, 
#            2L, 2L, 0L, 2L, NA, 2L, 2L, 3L, 2L, NA, 2L, 1L), 
#   obs4 = c(NA, 1L, 3L, NA, NA, NA, NA, 0L, NA, 1L, 0L, 0L, 2L, 2L, 
#            NA, NA, NA, 3L, 1L, NA, 1L, 0L, 3L, 0L, 2L, 1L, 1L, 2L, 
#            2L, NA, NA, 1L, 2L, 2L, NA, NA, NA, 1L, 2L, NA), 
#   obs5 = c(2L, NA, NA, 0L, 0L, 0L, 1L, NA, 2L, NA, NA, NA, NA, 3L, 
#            1L, 1L, 2L, NA, NA, 0L, NA, 0L, NA, NA, 2L, NA, 0L, NA, 
#            NA, 2L, 0L, NA, 2L, NA, 2L, 2L, 2L, 2L, NA, 1L)) 
# 
# out <- KrippAlpha(df, 
#           method = "ordinal",
#           levels = 0:3, raters = paste0("obs",1:5))
# 
# testthat::expect_equal(out$alpha, 0.7598, tolerance = 1e-4)




