
#> This function handles the complex crunching from a formula input
#> based on a data.frame organized in long form to a square matrix
#> consisting of rating ~ subject | rater, subjects as rows and 
#> raters in columns as required in various functions
#> This is used in all interrater agreement and reliability functions.


.LongToSquare <- function(formula, data, subset, na.action){
  
  # we do not need ... arguments here...
  
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
  # m$... <- NULL  
  
  
  ## >>> IMPORTANT: Treat subset correctly due to collision with 
  ##                the subset function.
  if (!missing(subset)) {
    m$subset <- substitute(subset)  # capture the argument
  } else {
    m$subset <- NULL                # remove completely
  }
  ## (Optional) na.action pass through unchanged:
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


RaterFrame <- function(formula, data, subset, na.action, incl.subj=TRUE){

    # m <- .LongToSquare(formula, data, subset, na.action, ...)
  
    # *** Attention !!! ***: 
    # Above code does not work as subset is evaluated here, leading to:
    #    Error in `[.default`(xj, i) : invalid subscript type 'closure'
    # thus we simply pass the call unevaluated to the next function
    cl <- match.call(expand.dots = FALSE)
    cl$incl.subj <- NULL
    
    # cl[[1L]] <- quote(DescTools:::.LongToSquare) - No ::: allowed for CRAN check!
    cl[[1L]] <- getFromNamespace(".LongToSquare", "DescTools")
    
    m <- eval.parent(cl)
    if(!incl.subj) m <- m[, -1]
    
    class(m) <- c("raterframe", class(m))
    
    return(m)
    
}

