
##' Convert Character Columns to Factors
##' 
##' A helper function to convert some or all columns of a data.frame to factors.
##' 
##' @param x the data.frame
##' @param columns names or indexes of the columns to be converted; 
##'  negative values can be used to omit columns.
##' @return the given data.frame including the converted factors
##' @author Andri Signorell <andri@signorell.net>
##' @examples
##' # get some data
##' d.dat <- data.frame(char_x=LETTERS[1:5],
##'                     char_y=LETTERS[6:10],
##'                     n=1:5)
##' 
##' # all characters
##' StringsAsFactors(d.dat)
##' # only char_y
##' StringsAsFactors(d.dat, columns="char_y")
##' # only char_x
##' StringsAsFactors(d.dat, columns="char_x")
##' 
##' # all characters, besides 2 (second column, so "char_y")
##' StringsAsFactors(d.dat, columns=-2)
##' 


StringsAsFactors <- function(x, columns=NULL){

  if(is.null(columns))
    # use all characters
    columns <- which(sapply(x, is.character))
  
  else if(is.numeric(columns))
    if(columns<0){
      # exclude columns
      columns <- which(sapply(x, is.character) & 
                         Unwhich(columns, ncol(x)))
      # else positive columns: 
      # leave unchanged and just use for selection
      
      # else if(is.character(columns))
      # use columns as columnnames
      
    } 
  
  x[columns] <- data.frame(as.list(x[columns]), stringsAsFactors = TRUE)
  
  return(x)
  
}
