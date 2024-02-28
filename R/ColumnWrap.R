
#' Column Wrap
#' 
#' Wraps text in a character matrix so, that it's displayed over more than one
#' line. 
#' 
#' A data.frame containing character columns with long texts is often wrapped
#' by columns. This can lead to a loss of overview. `ColumnWrap()` wraps the 
#' lines within the columns.
#' 
#' @param x the matrix with one row
#' @param width integer, the width of the columns in characters
#' 
#' @return a character matrix 
#' 
#' @author Andri Signorell <andri@@signorell.net>
#' 
#' @seealso [strwrap()]
#' @keywords print
#' @examples
#' 
#' Abstract(d.pizza)
#' 



ColumnWrap <- function(x, width = NULL) {
  
  if (is.null(width)) {
    width <- getOption("width") / length(x)
  }
  
  width <- rep(width, length.out = length(x))
  
  lst <- lapply(seq_along(x), function(i) strwrap(x[[i]], width = width[i]))
  
  maxdim <- max(unlist(lapply(lst, length)))
  lst <- lapply(lst, function(z) c(z, rep("", maxdim - length(z))))
  
  do.call(cbind, lst)
  
}



