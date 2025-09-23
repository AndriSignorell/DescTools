
#' Normalize input to a 2-rater confusion/coincidence matrix
#'
#' Accepts diverse inputs and returns a square contingency table (matrix)
#' for exactly two raters across \eqn{k} categories.
#'
#' Supported inputs:
#' \itemize{
#'   \item \strong{table} (2D): already a confusion table.
#'   \item \strong{matrix} that looks like a confusion table (square, non-negative, etc.).
#'   \item \strong{matrix/data.frame} with \eqn{\ge}2 columns: interpreted as raw ratings
#'         (subjects in rows, raters in columns). If more than two raters are present,
#'         the pair to use is specified via \code{rater.pair}.
#'   \item \strong{list} of \eqn{\ge}2 vectors: each element is one rater's ratings.
#'   \item \strong{two vectors} \code{x}, \code{y}: ratings of two raters.
#' }
#'
#' @param x Input object (see details).
#' @param y Optional second vector of ratings (used only if \code{x} is a single vector).
#' @param levels Optional vector of category levels to enforce (same order for rows/cols).
#'               If \code{NULL}, levels are inferred from the union of observed categories.
#' @param useNA Passed to \code{table()} when building the contingency table
#'   (e.g., \code{"no"}, \code{"ifany"}, \code{"always"}).
#' @param rater.pair Integer length-2 vector giving the indices of the two raters to use
#'   when \code{x} has \eqn{\ge}2 columns/entries (default \code{c(1,2)}).
#'
#' @return A square numeric \code{matrix} with dimnames (rows = rater A levels, cols = rater B levels).
#'
#' @examples
#' # Two vectors:
#' A <- c("pos","neg","pos","inc")
#' B <- c("pos","pos","neg","inc")
#' NormalizeToConfusion(A, B)
#'
#' # table:
#' tab <- table(A, B)
#' NormalizeToConfusion(tab)
#'
#' # matrix/data.frame with >= 2 columns (subjects × raters):
#' set.seed(1)
#' C <- sample(c("pos","neg","inc"), length(A), TRUE)
#' df <- data.frame(R1=A, R2=B, R3=C)
#' NormalizeToConfusion(df)              # uses R1 vs R2
#' NormalizeToConfusion(df, rater.pair=c(1,3))  # R1 vs R3
#'
#' # list of rating vectors:
#' NormalizeToConfusion(list(A,B))
#'


#' @export
.NormalizeToConfusion <- function(
    x, y = NULL, levels = NULL, useNA = "no", rater.pair = c(1L, 2L)
) {

  # helper: two vectors -> table (matrix)
  two_vec_to_tab <- function(a, b, levels, useNA) {
    if (!is.null(levels)) {
      a <- factor(a, levels = levels)
      b <- factor(b, levels = levels)
    }
    as.matrix(table(a, b, useNA = useNA))
  }
  
  # 1) Already a (2D) table?
  if (inherits(x, "table") && length(dim(x)) == 2L) {
    tab <- as.matrix(x)
    # enforce levels if provided
    if (!is.null(levels)) {
      if (length(levels) != nrow(tab) || length(levels) != ncol(tab)) {
        stop("'levels' must match the dimension of the supplied table.")
      }
      dimnames(tab) <- list(levels, levels)
    } else if (is.null(rownames(tab)) || is.null(colnames(tab))) {
      warning("Input 'table' has no dimnames; consider supplying 'levels=' for stable ordering.")
    }
    return(tab)
  }
  
  # 2) Matrix that looks like a confusion table?
  if (is.matrix(x) && IsConfusionTable(x, require_dimnames = FALSE)) {
    tab <- as.matrix(x)
    if (!is.null(levels)) {
      if (length(levels) != nrow(tab)) {
        stop("'levels' must match the dimension of the supplied matrix.")
      }
      dimnames(tab) <- list(levels, levels)
    } else if (is.null(rownames(tab)) || is.null(colnames(tab))) {
      warning("Matrix looks like a confusion table but has no dimnames; supply 'levels=' for stable ordering.")
    }
    return(tab)
  }
  
  # 3) Two vectors provided?
  if (!is.null(y)) {
    return(two_vec_to_tab(x, y, levels, useNA))
  }
  
  # 4) data.frame with >= 2 columns -> choose rater pair
  if (is.data.frame(x)) {
    if (ncol(x) < 2L) stop("data.frame must have at least 2 columns (raters).")
    if (length(rater.pair) != 2L) stop("'rater.pair' must be length 2.")
    rp <- as.integer(rater.pair)
    if (any(is.na(rp)) || any(rp < 1L | rp > ncol(x))) {
      stop("'rater.pair' indices out of bounds for data.frame.")
    }
    return(two_vec_to_tab(x[[rp[1]]], x[[rp[2]]], levels, useNA))
  }
  
  # 5) matrix with >= 2 columns -> choose rater pair
  if (is.matrix(x)) {
    if (ncol(x) < 2L) stop("matrix must have at least 2 columns (raters).")
    if (length(rater.pair) != 2L) stop("'rater.pair' must be length 2.")
    rp <- as.integer(rater.pair)
    if (any(is.na(rp)) || any(rp < 1L | rp > ncol(x))) {
      stop("'rater.pair' indices out of bounds for matrix.")
    }
    return(two_vec_to_tab(x[, rp[1]], x[, rp[2]], levels, useNA))
  }
  
  # 6) list of rating vectors -> choose rater pair
  if (is.list(x)) {
    if (length(x) < 2L) stop("list must contain at least 2 rating vectors.")
    if (length(rater.pair) != 2L) stop("'rater.pair' must be length 2.")
    rp <- as.integer(rater.pair)
    if (any(is.na(rp)) || any(rp < 1L | rp > length(x))) {
      stop("'rater.pair' indices out of bounds for list.")
    }
    return(two_vec_to_tab(x[[rp[1]]], x[[rp[2]]], levels, useNA))
  }
  
  # 7) single vector without y -> not enough info
  stop("Unsupported input type or missing second rater: provide 'y' or a multi-column/list input.")
}

