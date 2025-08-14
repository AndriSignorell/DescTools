



.LongToSquare <- function(formula, data, subset, na.action, ...){
  
  # returns a 2-dim matrix of a subject-rater-rating formula
  
  # originally based on:   stats:::friedman.test.formula
  
  if (missing(formula)) 
    stop("formula missing")
  
  if ((length(formula) != 3L) || 
      (length(formula[[3L]]) != 3L) || 
      (formula[[3L]][[1L]] != as.name("|")) || 
      (length(formula[[3L]][[2L]]) != 1L) || 
      (length(formula[[3L]][[3L]]) != 1L)) 
    stop("incorrect specification for 'formula'")
  
  formula[[3L]][[1L]] <- as.name("+")
  
  m <- match.call(expand.dots = FALSE)
  m$formula <- formula
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  
  m[[1L]] <- quote(stats::model.frame)
  # in order to delete potentially provided tolerance or 
  # na.rm arguments to be passed later on 
  m$... <- NULL  
  
  
  ## >>> WICHTIG: subset korrekt behandeln wegen Kollistion mit der Funktion subset
  if (!missing(subset)) {
    m$subset <- substitute(subset)  # Ausdruck capturen
  } else {
    m$subset <- NULL                # ganz entfernen
  }
  ## (Optional) na.action unverändert durchreichen:
  # if (missing(na.action)) m$na.action <- NULL
  
  
  mf <- eval(m, parent.frame())
  
  DNAME <- gettextf("%s by %s (rows) and %s (columns)", 
                    names(mf)[1], names(mf)[2], names(mf)[3])
  
  # now reshaping to matrix form
  m <- reshape(mf, idvar=colnames(mf)[2], timevar=colnames(mf)[3],
               direction="wide")
  
  # get better order for rows and columns
  m <- m[order(m[,1]), ]
  m <- cbind(m[, 1, drop=FALSE], m[, -1][, order(colnames(m)[-1])])
  # remove response variable part from columnnames
  colnames(m) <- gsub(gettextf("%s\\.", names(mf)[1]), "", colnames(m))
  rownames(m) <- NULL
  
  if(!missing(na.action)){
    subj <- m[, names(mf)[2]]
    m <- na.action(m)
    # provide the names of omitted subjects
    attr(attr(m, "na.action"), "values") <- subj[as.numeric(attr(m, "na.action"))]
  }
  
  attr(m, "data.name") <- DNAME
  attr(m, "")
  return(m)
  
}



Agree <- function(x,  ...) {
  UseMethod("Agree")
}    


Agree.formula <- function(formula, data, subset, na.action, ...){

  m <- .LongToSquare(formula, data, subset, na.action, ...)
  res <- Agree(m[, -1], ...)

  if(!is.null(attr(m, "na.action"))){
    attr(res, "na.action") <- attr(m, "na.action")
  }

  return(res)

}



Agree.default <- function(x, tolerance = 0, na.rm=FALSE, ...){
  
  # coercing to matrix is a good idea, as ratings should be the same type 
  # for all the raters
  
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


# Old version, replaced 2025-08-12
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

