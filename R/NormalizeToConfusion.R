
#' Normalize Input to a 2-Rater Confusion/Coincidence Matrix
#'
#' Accepts diverse inputs and returns a square contingency table (matrix)
#' for exactly two raters across \eqn{k} categories.
#'
#' Supported inputs:
#' \itemize{
#'   \item \strong{table} (2D): already a confusion table.
#'   \item \strong{matrix} that looks like a confusion table (square, non-negative, etc.).
#'   \item \strong{matrix/data.frame} with \eqn{\ge}2 columns and >2 rows: interpreted as raw ratings
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
#'
#' @return A square numeric \code{matrix} with dimnames (rows = rater A levels, cols = rater B levels).
#'
#' @seealso [IsConfusionTable()], [RaterFrame()]
#' @examples
#' A <- c("pos","neg","pos","inc")
#' B <- c("pos","pos","neg","inc")
#' NormalizeToConfusion(A, B)
#'
#' tab <- table(A, B)
#' NormalizeToConfusion(tab)
#'
#' set.seed(1)
#' C <- sample(c("pos","neg","inc"), length(A), TRUE)
#' df <- data.frame(R1=A, R2=B, R3=C)
#' NormalizeToConfusion(df[, 1:2])      # R1 vs R2
#' NormalizeToConfusion(df[, c(1,3)])   # R1 vs R3
#'
#' # list of rating vectors:
#' NormalizeToConfusion(list(A, B))
#'
#' # use NAs
#' B[2] <- NA
#' NormalizeToConfusion(A, B, useNA="always")
#' 
#' anxiety <- data.frame(
#'   rater1 = c(3,3,3,4,5,5,2),
#'   rater2 = c(3,6,4,6,2,4,2),
#'   rater3 = c(2,1,4,4,3,2,1)
#' )
#' 
#' x <- anxiety[, 1]
#' y <- anxiety[, 2]
#' 
#' # Two vectors:
#' NormalizeToConfusion(x, y)
#' # matrix/data.frame with 2 columns (subjects × raters):
#' NormalizeToConfusion(cbind(x, y))
#' NormalizeToConfusion(data.frame(x, y))
#' NormalizeToConfusion(list(x, y))
#' 
#' # table:
#' ratingscale <- sort(unique(c(x, y)))
#' NormalizeToConfusion(table(factor(x, levels=ratingscale), 
#'                            factor(y, levels=ratingscale)))
#' 
#' d.anxiety <- data.frame(
#'   rater  = c("rater1", "rater1", "rater1", "rater1", "rater1", "rater1", "rater1", 
#'              "rater2", "rater2", "rater2", "rater2", "rater2", "rater2", "rater2", 
#'              "rater3", "rater3", "rater3", "rater3", "rater3", "rater3", "rater3"), 
#'   rating = c(3, 3, 3, 4, 5, 5, 2, 
#'              3, 6, 4, 6, 2, 4, 2, 
#'              2, 1, 4, 4, 3, 2, 1), 
#'   subj   = c(1, 2, 3, 4, 5, 6, 7, 
#'              1, 2, 3, 4, 5, 6, 7, 
#'              1, 2, 3, 4, 5, 6, 7)
#' )
#' 
#' # matrix/data.frame with >= 2 columns (subjects × raters):
#' NormalizeToConfusion(
#'   RaterFrame(rating ~ subj | rater, 
#'              d.anxiety, subset=rater %in% c("rater1","rater2"))[, -1]
#' )



#' @keywords internal
#' @export
NormalizeToConfusion <- function(
    x, y = NULL, levels = NULL, useNA = "no") {

  # helper: two vectors -> table (matrix)
  two_vec_to_tab <- function(a, b, levels, useNA) {
    # confusion matrix must be symmetric with the same levels in 
    # x AND y !
    if (is.null(levels)) {
      levels <- sort(unique(c(a,b)))
    }
    a <- factor(a, levels = levels)
    b <- factor(b, levels = levels)
    
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
      
    } else if(!do.call(identical, unname(dimnames(x)))) {
      stop("rownames and columnnames must match in coincindence table.")
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
  
  # data.frame is handled by the list interface
  # # 4) data.frame with >= 2 columns -> choose rater pair
  # if (is.data.frame(x)) {
  #   if (ncol(x) < 2L) stop("data.frame must have at least 2 columns (raters).")
  #   if (length(rater.pair) != 2L) stop("'rater.pair' must be length 2.")
  #   rp <- as.integer(rater.pair)
  #   if (any(is.na(rp)) || any(rp < 1L | rp > ncol(x))) {
  #     stop("'rater.pair' indices out of bounds for data.frame.")
  #   }
  #   return(two_vec_to_tab(x[[rp[1]]], x[[rp[2]]], levels, useNA))
  # }
  
  # 5) matrix with >= 2 columns -> choose rater pair
  if (is.matrix(x)) {
    if (ncol(x) != 2L) stop("matrix must have exactly 2 columns (raters).")
    return(two_vec_to_tab(x[, 1], x[, 2], levels, useNA))
  }
  
  # 6) list of rating vectors -> choose rater pair
  if (is.list(x)) {
    if (length(x) != 2L) stop("data.frame/list must contain exactly 2 rating vectors.")
    return(two_vec_to_tab(x[[1]], x[[2]], levels, useNA))
  }
  
  # 7) single vector without y -> not enough info
  stop("Unsupported input type or missing second rater: provide 'y' or a multi-column/list input.")
}

