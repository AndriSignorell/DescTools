

Split <- function(x, ...){
  UseMethod("Split")
}

Split.default <- function(x, f, drop=FALSE, ...){
 split(x=x, f=f, drop=drop, ...) 
}


# experimental: formula interface for split

# raw early approach:
# split.formula <- function(x, f, drop = FALSE, data = NULL, ...) {
#   mf <- model.frame(x, data)
#   f <- mf[,2]
#   x <- mf[,1]
#   split(x, f, drop=drop, ...)
# }


Split.formula <- function(formula, data, subset, na.action, drop=FALSE, ...){

  if (missing(formula) || (length(formula) != 3L))
    stop("'formula' missing or incorrect")

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

  DNAME <- gettextf("%s by %s (rows)", names(mf)[1],
                    paste(names(mf)[-1], collapse=" : "))

  ff <- mf[, -1]
  if(ncol(mf[, -1, drop=FALSE]) > 1)
    ff <- as.list(mf[, -1])

  # now do the split
  res <- split(x=mf[, 1], f=ff, drop=drop, ...)

  # add na information
  if(!missing(na.action)){
    subj <- res[, names(mf)[2]]
    res <- na.action(res)
    # provide the names of omitted subjects
    attr(attr(res, "na.action"), "values") <- subj[as.numeric(attr(res, "na.action"))]
  }

  attr(res, "data.name") <- DNAME
  attr(res, "")
  return(res)

}

