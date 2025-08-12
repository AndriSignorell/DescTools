

ToBaseR <- function(x, ...){
  UseMethod("ToBaseR")
}


ToBaseR.tbl_df <- function(x, ...){
  # rollback a tibble to data.frame, with usual factors etc.
  res <- as.data.frame(x)
  
  # get rid of unimportant SPSS specific attributes
  res <- as.data.frame(
    lapply(res, 
           DescTools::StripAttr, 
           attr=c("format.spss", "display_width", "format.stata"))) 
  
  for(i in which(sapply(x, inherits, "haven_labelled") )){
    res[i] <- ToBaseR.haven_labelled(x[i])
  }
  
  return(res)
}


ToBaseR.haven_labelled <- function(x, ...){
  haven::as_factor(x, ...)
}


ToBaseR.default <- function(x, ...){
  warning(gettextf('Not implemented for class(es) "%s"', paste(class(x), collapse=", ")))
}
