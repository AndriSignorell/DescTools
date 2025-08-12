

# Old version, replace 2025-08-12
# 
# Agree <- function(x, tolerance = 0, na.rm = FALSE) {
# 
#   x <- as.matrix(x)
#   if(na.rm) x <- na.omit(x)
# 
#   if(anyNA(x)) return(NA)
# 
#   ns <- nrow(x)
#   nr <- ncol(x)
# 
#   if (is.numeric(x)) {
#     rangetab <- apply(x, 1, max) - apply(x, 1, min)
#     coeff <-  sum(rangetab <= tolerance)/ns
# 
#   } else {
#     rangetab <- as.numeric(sapply(apply(x, 1, table), length))
#     coeff <- (sum(rangetab == 1)/ns)
#     tolerance <- 0
#   }
# 
#   rval <- coeff
#   attr(rval, c("subjects")) <- ns
#   attr(rval, c("raters")) <- nr
# 
#   return(rval)
# 
# }



Agree <- function(x, grp=NULL, tolerance = 0, na.rm=FALSE){
  
  # coercing to matrix is a good idea, as ratings should be the same type 
  # for all the raters
  
  if(!is.null(grp))
    x <- ToWide(x=x, g=grp)
  
  if(inherits(x, "list"))
    x <- do.call(cbind, x)
  else
    x <- as.matrix(x)
  
  # if matrix is character switch to factor 
  # with all unique elements (!!) as levels
  if(mode(x) =="character")
    x[] <- factor(x)
  
  d <- sum(apply(x, 1, 
                 function(z) diff(as.numeric(range(z, na.rm = na.rm))) <= tolerance)) 
  
  res <- d / nrow(x)
  attr(res, c("subjects")) <- nrow(x)
  attr(res, c("raters")) <- ncol(x)
  
  return(res)
  
}


