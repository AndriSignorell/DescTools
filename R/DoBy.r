


DoBy <- function(x, ...){
  UseMethod("DoBy")
}


DoBy.formula <- function(formula, data=parent.frame(), subset, na.action, vnames=NULL, ...) {


  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval.parent(m$data)))
    m$data <- as.data.frame(data)
  m$... <- NULL
  m[[1L]] <- quote(stats::model.frame)
  m <- eval.parent(m)
  Terms <- attr(m, "terms")
  mf <- model.frame(m)
  response <- attr(attr(mf, "terms"), "response")

  res <- DoBy.default(x=mf[[response]], by=mf[-response], ...)


  colnames(res)[1] <- colnames(mf)[response]

  is(is.null(vnames))
    colnames(res)[length(colnames(res))] <- gsub("mf[[response]]", colnames(mf)[response],
                                                 colnames(res)[length(colnames(res))], fixed=TRUE)

  # res <- list(res)
  # res$terms <- Terms
  # res$call <- match.call()
  # res$na.action <- attr(m, "na.action")

  return(res)

}


DoBy.default <- function(x, by, FUN, vnames=NULL, collapse=FALSE, ...){

  # SQL-OLAP: sum() over (partition by g)
  # (more than 1 grouping variables are enumerated like by=list(g1,g2,g3),
  # as it is defined in tapply

  # see also ave, which only handles arguments otherwise..

  xname <- deparse(substitute(x))
  res <- x
  if(is.null(vnames)){
    vnames <- paste(deparse(substitute(FUN)), deparse(substitute(x)), sep=".")
  }

  if (missing(by))
    x[] <- FUN(x, ...)
  else {
    g <- interaction(by)
    split(x, g) <- lapply(split(x, g), FUN, ...)
  }


  if(collapse) {
    res <- unique(data.frame(by, x))
    colnames(res)[length(colnames(res))] <- vnames

  } else {
    res <- data.frame(res, by, x)
    colnames(res)[1] <- xname
    colnames(res)[length(colnames(res))] <- vnames

  }

  attr(res, "response")  <- xname
  return(res)


}


# str(SetNames(x <- c(1:2), names="myx"))


# x <- model.matrix(Terms, m, contrasts)
# y <- model.response(m)

# DoBy(breaks ~ ., data=warpbreaks, FUN=mean)
# DoBy(breaks ~ ., data=warpbreaks, FUN=mean, collapse=TRUE)

# DoBy(breaks ~ wool, data=warpbreaks, FUN=order)

# PartitionBy(warpbreaks$breaks, warpbreaks$wool,order)

# data.frame(DoBy(breaks ~ ., data=warpbreaks, FUN=order)

# DoBy(temperature ~ ., data=d.pizza[,c("area","operator","temperature")], FUN=mean)
##

