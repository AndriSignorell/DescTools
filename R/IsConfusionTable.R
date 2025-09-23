

#' Detect whether an object looks like a confusion/coincidence matrix
#'
#' Checks if \code{x} behaves like a rater-by-rater contingency table:
#' square 2D numeric counts (or, optionally, proportions), non-negative,
#' finite, and (optionally) with matching row/column names.
#'
#' @param x Object to check (typically a \code{table}, \code{matrix}, or
#'   numeric \code{data.frame}).
#' @param require_dimnames Logical; if \code{TRUE}, both row and column names
#'   must be present. Defaults to \code{TRUE}.
#' @param require_same_levels Logical; if \code{TRUE} and dimnames are present,
#'   row and column names must be the same set (order ignored). Defaults to \code{TRUE}.
#' @param integer_tol Numeric tolerance for "integer-like" counts. Defaults to
#'   \code{sqrt(.Machine$double.eps)}.
#' @param accept_proportions Logical; if \code{TRUE}, also accepts proportion
#'   tables (all entries in \[0,1\] and total sum approx. 1). Defaults to \code{TRUE}.
#' @param require_square Logical; require a square table. Defaults to \code{TRUE}.
#'
#' @return \code{TRUE} if \code{x} looks like a confusion/coincidence matrix,
#'   otherwise \code{FALSE}.
#'
#' @examples
#' tab <- table(sample(letters[1:3], 100, TRUE),
#'              sample(letters[1:3], 100, TRUE))
#' IsConfusionTable(tab)               # TRUE
#'
#' M <- as.matrix(tab)
#' IsConfusionTable(M)                 # TRUE (dimnames present)
#' IsConfusionTable(M, require_dimnames = FALSE)  # TRUE even without names
#'
#' df <- as.data.frame.matrix(tab)
#' IsConfusionTable(df)                # TRUE (numeric data.frame)
#'
#' # Two-column raw ratings are NOT a confusion table:
#' ratings <- cbind(r1 = sample(0:1, 50, TRUE), r2 = sample(0:1, 50, TRUE))
#' IsConfusionTable(ratings)           # FALSE (not square)
#'

#' @export
IsConfusionTable <- function(
    x,
    require_dimnames   = TRUE,
    require_same_levels= TRUE,
    integer_tol        = sqrt(.Machine$double.eps),
    accept_proportions = TRUE,
    require_square     = TRUE
) {
  
  # Normalize input to a numeric matrix 'm' if eligible
  if (inherits(x, "table")) {
    d <- dim(x)
    if (length(d) != 2L) return(FALSE)
    if (require_square && d[1L] != d[2L]) return(FALSE)
    m  <- as.matrix(x)
    rn <- rownames(m)
    cn <- colnames(m)
    
  } else if (is.matrix(x)) {
    d <- dim(x)
    if (length(d) != 2L) return(FALSE)
    if (require_square && d[1L] != d[2L]) return(FALSE)
    if (!is.numeric(x)) return(FALSE)
    m  <- x
    rn <- rownames(m)
    cn <- colnames(m)
    
  } else if (is.data.frame(x)) {
    # Only consider numeric data frames
    if (!all(vapply(x, is.numeric, logical(1)))) return(FALSE)
    d <- dim(x)
    if (length(d) != 2L) return(FALSE)
    if (require_square && d[1L] != d[2L]) return(FALSE)
    m  <- as.matrix(x)
    rn <- rownames(m)
    cn <- colnames(m)
    
  } else {
    return(FALSE)
  }
  
  # Value checks
  if (!all(is.finite(m))) return(FALSE)
  if (any(m < 0)) return(FALSE)
  
  # Dimname checks
  if (require_dimnames && (is.null(rn) || is.null(cn))) return(FALSE)
  if (require_same_levels && !is.null(rn) && !is.null(cn)) {
    if (length(rn) != length(cn)) return(FALSE)
    if (!setequal(rn, cn)) return(FALSE)
  }
  
  # Counts (integer-like) OR proportions (optional)
  if (all(abs(m - round(m)) <= integer_tol)) return(TRUE)
  
  if (accept_proportions) {
    s <- sum(m)
    if (s > 0 && abs(s - 1) <= 1e-8 && all(m <= 1)) return(TRUE)
  }
  
  FALSE
}



.IsNonNegIntMatrix <- function(x, tol = .Machine$double.eps^0.5) {
  
  ### Check if x is a nonnegative integer symmetric Matrix
  
  
  # 1) must be a (numeric) 2D-matrix
  if (!is.matrix(x)) return(FALSE)
  if (!is.numeric(x)) return(FALSE)
  
  if(length(dim(x)) != 2L) return(FALSE)
  
  nr <- nrow(x); nc <- ncol(x)
  if (nr != nc) return(FALSE)               # quadratic
  if (nr == 0L) return(TRUE)                # empty 0x0-matrix is OK
  
  if (!all(is.finite(x))) return(FALSE)     # no NA/NaN/Inf
  
  # 2) Nonnegative
  # small tolerance allowed (for numeric rounding errors)
  if (min(x) < -tol) return(FALSE)
  
  # 3) Integer: true integer storage or numerically ‘integer-like’
  if (!is.integer(x)) {
    # only check if not integer
    if(!isTRUE(all.equal(x, round(x), tol))) return(FALSE)
  }
  
  return(TRUE)
  
}










