

#' Count Complete Cases 
#'                                                                               
#' Return for each variable of a data frame the number of missing values and     
#' the complete cases to be expected if this variable would be omitted.           
#'                                                                               
#' @aliases CountCompCases print.CountCompCases                                  
#' @param x a data.frame containg the data.                                      
#' @param digits the number of digits to be used when printing the results.      
#' @param \\dots the dots are not further used.                                  
#' @return A list with three elements. The first gives the number of rows, the   
#' second the number of complete cases for the whole data frame. The third       
#' element \\code{tab} contains the data for the single variables.                
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \\code{\\link{PlotMiss}}, \\code{\\link{CompleteColumns}},           
#' \\code{\\link{complete.cases}}, \\code{\\link{is.na}}, \\code{\\link{na.omit}}
#' @examples                                                                     
#' \n#' CountCompCases(d.pizza)\n#'                                             


CountCompCases <- function(x){
  
  # x is a data.frame
  x <- sapply(x, is.na)
  rs <- rowSums(x)
  
  n <- nrow(x)
  cc <- sum(rs == 0)
  
  # NAs, columnwise left out
  z <- sapply(seq(ncol(x)), 
              function(i) sum((rs - x[, i]) == 0))
  
  m <- apply(x, 2, sum)
  
  res <- list(
    n = n, cc = cc, 
    tab = SetNames(
      data.frame(vname=colnames(x), 
                 nas=m, nas_p=m/n, 
                 cifnot=z, cifnot_p=z/n),
      rownames=NULL)
  )
  
  class(res) <- "CountCompCases"
  res
  
}




print.CountCompCases <- function(x, digits=1, ...){
  
  cat(gettextf("\nTotal rows:      %s\nComplete Cases:  %s (%s)\n\n", x$n, x$cc,
               Format(x$cc/x$n, fmt="%", digits=digits)))
  x$tab$nas_p <- Format(x$tab$nas_p, fmt="%", digits=digits)
  x$tab$cifnot_p <- Format(x$tab$cifnot_p, fmt="%", digits=digits)
  
  print(x$tab, print.gap = 2)
  cat("\n")
}
