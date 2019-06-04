
## stats: distributions  ---------------------------------




dBenf <- function(x, ndigits = 1, log = FALSE) {
  if (!IsNumeric(ndigits, length.arg = 1,
                 positive = TRUE, integer.valued = TRUE) ||
      ndigits > 2)
    stop("argument 'ndigits' must be 1 or 2")
  lowerlimit <- ifelse(ndigits == 1, 1, 10)
  upperlimit <- ifelse(ndigits == 1, 9, 99)

  if (!is.logical(log.arg <- log) || length(log) != 1)
    stop("bad input for argument 'log'")
  rm(log)


  ans <- x * NA
  indexTF <- is.finite(x) & (x >= lowerlimit)

  ans[indexTF] <- log10(1 + 1/x[indexTF])
  ans[!is.na(x) & !is.nan(x) &
        ((x < lowerlimit) |
           (x > upperlimit) |
           (x != round(x)))] <- 0.0
  if (log.arg) log(ans) else ans
}


rBenf <- function(n, ndigits = 1) {
  if (!IsNumeric(ndigits, length.arg = 1,
                 positive = TRUE, integer.valued = TRUE) ||
      ndigits > 2)
    stop("argument 'ndigits' must be 1 or 2")
  lowerlimit <- ifelse(ndigits == 1, 1, 10)
  upperlimit <- ifelse(ndigits == 1, 9, 99)
  use.n <- if ((length.n <- length(n)) > 1) length.n else
    if (!IsNumeric(n, integer.valued = TRUE,
                   length.arg = 1, positive = TRUE))
      stop("bad input for argument 'n'") else n
  myrunif <- runif(use.n)

  ans <- rep(lowerlimit, length = use.n)
  for (ii in (lowerlimit+1):upperlimit) {
    indexTF <- (pBenf(ii-1, ndigits = ndigits) < myrunif) &
      (myrunif <= pBenf(ii, ndigits = ndigits))
    ans[indexTF] <- ii
  }
  ans
}


pBenf <- function(q, ndigits = 1, log.p = FALSE) {
  if (!IsNumeric(ndigits, length.arg = 1,
                 positive = TRUE, integer.valued = TRUE) ||
      ndigits > 2)
    stop("argument 'ndigits' must be 1 or 2")
  lowerlimit <- ifelse(ndigits == 1, 1, 10)
  upperlimit <- ifelse(ndigits == 1, 9, 99)

  ans <- q * NA
  floorq <- floor(q)
  indexTF <- is.finite(q) & (floorq >= lowerlimit)
  ans[indexTF] <- log10(1 + floorq[indexTF]) -
    ifelse(ndigits == 1, 0, 1)
  ans[!is.na(q) & !is.nan(q) & (q >= upperlimit)] <- 1
  ans[!is.na(q) & !is.nan(q) & (q <  lowerlimit)] <- 0
  if (log.p) log(ans) else ans
}




qBenf <- function(p, ndigits = 1) {
  if (!IsNumeric(ndigits, length.arg = 1,
                 positive = TRUE, integer.valued = TRUE) ||
      ndigits > 2)
    stop("argument 'ndigits' must be 1 or 2")
  lowerlimit <- ifelse(ndigits == 1, 1, 10)
  upperlimit <- ifelse(ndigits == 1, 9, 99)
  bad <- !is.na(p) & !is.nan(p) & ((p < 0) | (p > 1))
  if (any(bad))
    stop("bad input for argument 'p'")

  ans <- rep(lowerlimit, length = length(p))
  for (ii in (lowerlimit+1):upperlimit) {
    indexTF <- is.finite(p) &
      (pBenf(ii-1, ndigits = ndigits) < p) &
      (p <= pBenf(ii, ndigits = ndigits))
    ans[indexTF] <- ii
  }

  ans[ is.na(p) |  is.nan(p)] <- NA
  ans[!is.na(p) & !is.nan(p) & (p == 0)] <- lowerlimit
  ans[!is.na(p) & !is.nan(p) & (p == 1)] <- upperlimit
  ans
}



dRevGumbel <- function (x, location = 0, scale = 1) {
  # from VGAM  -- if (is.null(x)) FALSE else ifelse(is.na(x), FALSE, x)
  if (!IsNumeric(scale, positive=TRUE))
    stop("\"scale\" must be positive")
  temp = exp((x - location)/scale)
  temp * exp(-temp)/scale
}

pRevGumbel <- function (q, location = 0, scale = 1) {

  if (!IsNumeric(scale, positive=TRUE))
    stop("\"scale\" must be positive")
  1-exp(-exp((q - location)/scale))
}

qRevGumbel <- function (p, location = 0, scale = 1)
{
  if (!IsNumeric(scale, positive=TRUE))
    stop("\"scale\" must be positive")
  location + scale * log(-log(p))
}

qRevGumbelExp <- function (p) exp(qRevGumbel(p))

rRevGumbel <- function (n, location = 0, scale = 1)
{
  if (!IsNumeric(scale, positive=TRUE, integer.valued=TRUE))
    stop("bad input for argument \"n\"")
  if (!IsNumeric(scale, positive=TRUE))
    stop("\"scale\" must be positive")
  location + scale * log(-log(runif(n)))
}



rFrechet <-  function(n, loc = 0, scale = 1, shape = 1)  {
    if(min(scale) < 0 || min(shape) <= 0) stop("invalid arguments")
    loc + scale * rexp(n)^(-1/shape)
  }


rGumbel <- function(n, loc = 0, scale = 1) {

    rGenExtrVal(n, loc = loc, scale = scale, shape = 0)
  }

rRevWeibull <- function(n, loc = 0, scale = 1, shape = 1) {

    if(min(scale) < 0 || min(shape) <= 0) stop("invalid arguments")
    loc - scale * rexp(n)^(1/shape)
  }

rNegWeibull <- function(n, loc = 0, scale = 1, shape = 1) {

    if(min(scale) < 0 || min(shape) <= 0) stop("invalid arguments")
    loc - scale * rexp(n)^(1/shape)
  }

rGenExtrVal <- function(n, loc = 0, scale = 1, shape = 0) {

    if(min(scale) < 0) stop("invalid scale")
    if(length(shape) != 1) stop("invalid shape")
    if(shape == 0) return(loc - scale * log(rexp(n)))
    else return(loc + scale * (rexp(n)^(-shape) - 1)/shape)
  }

rGenPareto <- function(n, loc = 0, scale = 1, shape = 0) {

    if(min(scale) < 0) stop("invalid scale")
    if(length(shape) != 1) stop("invalid shape")
    if(shape == 0) return(loc + scale*rexp(n))
    else return(loc + scale * (runif(n)^(-shape) - 1) / shape)
  }

rExtrVal <- function(n, quantfun, ..., distn, mlen = 1, largest = TRUE) {

    if(!is.numeric(mlen) || length(mlen) != 1 || mlen < 1 ||
       mlen %% 1 != 0)
      stop("`mlen' must be a non-negative integer")
    if(missing(quantfun))
      quantfun <- get(paste("q", distn, sep=""), mode="function")
    if(largest)
      quantfun(rbeta(n, mlen, 1), ...)
    else
      quantfun(rbeta(n, 1, mlen), ...)
  }

rOrder <- function(n, quantfun, ..., distn,  mlen = 1, j = 1, largest = TRUE) {

    if(!is.numeric(mlen) || length(mlen) != 1 || mlen < 1 ||
       mlen %% 1 != 0)
      stop("`mlen' must be a non-negative integer")
    if(!is.numeric(j) || length(j) != 1 || j < 1 || j %% 1 != 0)
      stop("`j' must be a non-negative integer")
    if(j > mlen)
      stop("`j' cannot be greater than `mlen'")
    if(!largest) j <- mlen+1-j
    if(missing(quantfun))
      quantfun <- get(paste("q", distn, sep=""), mode="function")
    quantfun(rbeta(n, mlen+1-j, j), ...)
  }

qFrechet <- function(p, loc = 0, scale = 1, shape = 1, lower.tail = TRUE) {

  if(min(p, na.rm = TRUE) <= 0 || max(p, na.rm = TRUE) >=1)
      stop("`p' must contain probabilities in (0,1)")
    if(min(scale) < 0 || min(shape) <= 0) stop("invalid arguments")
    if(!lower.tail) p <- 1 - p
    loc + scale * (-log(p))^(-1/shape)
  }

qGumbel <- function(p, loc = 0, scale = 1, lower.tail = TRUE) {

    qGenExtrVal(p, loc = loc, scale = scale, shape = 0, lower.tail = lower.tail)
  }

qRevWeibull <- function(p, loc = 0, scale = 1, shape = 1, lower.tail = TRUE) {

    if(min(p, na.rm = TRUE) <= 0 || max(p, na.rm = TRUE) >=1)
      stop("`p' must contain probabilities in (0,1)")
    if(min(scale) < 0 || min(shape) <= 0) stop("invalid arguments")
    if(!lower.tail) p <- 1 - p
    loc - scale * (-log(p))^(1/shape)
  }

qNegWeibull <- function(p, loc = 0, scale = 1, shape = 1, lower.tail = TRUE) {

    if(min(p, na.rm = TRUE) <= 0 || max(p, na.rm = TRUE) >=1)
      stop("`p' must contain probabilities in (0,1)")
    if(min(scale) < 0 || min(shape) <= 0) stop("invalid arguments")
    if(!lower.tail) p <- 1 - p
    loc - scale * (-log(p))^(1/shape)
  }

qGenExtrVal <- function(p, loc = 0, scale = 1, shape = 0, lower.tail = TRUE) {

    if(min(p, na.rm = TRUE) <= 0 || max(p, na.rm = TRUE) >=1)
      stop("`p' must contain probabilities in (0,1)")
    if(min(scale) < 0) stop("invalid scale")
    if(length(shape) != 1) stop("invalid shape")
    if(!lower.tail) p <- 1 - p
    if(shape == 0) return(loc - scale * log(-log(p)))
    else return(loc + scale * ((-log(p))^(-shape) - 1)/shape)
  }

qGenPareto <- function(p, loc = 0, scale = 1, shape = 0, lower.tail = TRUE) {

    if(min(p, na.rm = TRUE) <= 0 || max(p, na.rm = TRUE) >=1)
      stop("`p' must contain probabilities in (0,1)")
    if(min(scale) < 0) stop("invalid scale")
    if(length(shape) != 1) stop("invalid shape")
    if(lower.tail) p <- 1 - p
    if(shape == 0) return(loc - scale*log(p))
    else return(loc + scale * (p^(-shape) - 1) / shape)
  }


qExtrVal <- function(p, quantfun, ..., distn, mlen = 1, largest = TRUE, lower.tail = TRUE) {

    if(min(p, na.rm = TRUE) <= 0 || max(p, na.rm = TRUE) >=1)
      stop("`p' must contain probabilities in (0,1)")
    if(!is.numeric(mlen) || length(mlen) != 1 || mlen < 1 ||
       mlen %% 1 != 0)
      stop("`mlen' must be a non-negative integer")
    if(missing(quantfun))
      quantfun <- get(paste("q", distn, sep=""), mode="function")
    if(!lower.tail) p <- 1 - p
    if(largest)
      quantfun(p^(1/mlen), ...)
    else
      quantfun(1-(1-p)^(1/mlen), ...)
  }

pFrechet <- function(q, loc = 0, scale = 1, shape = 1, lower.tail = TRUE)  {

    if(min(scale) <= 0 || min(shape) <= 0) stop("invalid arguments")
    q <- pmax((q - loc)/scale,0)
    p <- exp(-q^(-shape))
    if(!lower.tail) p <- 1 - p
    p
  }

pGumbel <- function(q, loc = 0, scale = 1, lower.tail = TRUE) {
    pGenExtrVal(q, loc = loc, scale = scale, shape = 0, lower.tail = lower.tail)
  }

pRevWeibull <- function(q, loc = 0, scale = 1, shape = 1, lower.tail = TRUE) {

    if(min(scale) <= 0 || min(shape) <= 0) stop("invalid arguments")
    q <- pmin((q - loc)/scale,0)
    p <- exp(-(-q)^shape)
    if(!lower.tail) p <- 1 - p
    p
  }

pNegWeibull <- function(q, loc = 0, scale = 1, shape = 1, lower.tail = TRUE) {

    if(min(scale) <= 0 || min(shape) <= 0) stop("invalid arguments")
    q <- pmin((q - loc)/scale,0)
    p <- exp(-(-q)^shape)
    if(!lower.tail) p <- 1 - p
    p
  }

pGenExtrVal <- function(q, loc = 0, scale = 1, shape = 0, lower.tail = TRUE) {

    if(min(scale) <= 0) stop("invalid scale")
    if(length(shape) != 1) stop("invalid shape")
    q <- (q - loc)/scale
    if(shape == 0) p <- exp(-exp(-q))
    else p <- exp( - pmax(1 + shape * q, 0)^(-1/shape))
    if(!lower.tail) p <- 1 - p
    p
  }

pGenPareto <- function(q, loc = 0, scale = 1, shape = 0, lower.tail = TRUE) {

    if(min(scale) <= 0) stop("invalid scale")
    if(length(shape) != 1) stop("invalid shape")
    q <- pmax(q - loc, 0)/scale
    if(shape == 0) p <- 1 - exp(-q)
    else {
      p <- pmax(1 + shape * q, 0)
      p <- 1 - p^(-1/shape)
    }
    if(!lower.tail) p <- 1 - p
    p
  }

pExtrVal <- function(q, distnfun, ..., distn, mlen = 1, largest = TRUE, lower.tail = TRUE) {

    if(!is.numeric(mlen) || length(mlen) != 1 || mlen < 1 ||
       mlen %% 1 != 0)
      stop("`mlen' must be a non-negative integer")
    if(missing(distnfun))
      distnfun <- get(paste("p", distn, sep=""), mode="function")
    distn <- distnfun(q, ...)
    if(!largest) distn <- 1-distn
    p <- distn^mlen
    if(largest != lower.tail) p <- 1 - p
    p
  }

pOrder <- function(q, distnfun, ..., distn, mlen = 1, j = 1, largest = TRUE,
           lower.tail = TRUE) {
    if(!is.numeric(mlen) || length(mlen) != 1 || mlen < 1 ||
       mlen %% 1 != 0)
      stop("`mlen' must be a non-negative integer")
    if(!is.numeric(j) || length(j) != 1 || j < 1 || j %% 1 != 0)
      stop("`j' must be a non-negative integer")
    if(j > mlen)
      stop("`j' cannot be greater than `mlen'")
    lachooseb <- function(a,b) lgamma(a+1) - lgamma(b+1) - lgamma(a-b+1)
    if(largest) svec <- (mlen+1-j):mlen
    else  svec <- 0:(j-1)
    if(missing(distnfun))
      distnfun <- get(paste("p", distn, sep=""), mode="function")
    distn <- distnfun(q, ...)
    store <- matrix(0,nrow=length(q),ncol=j)
    for(k in 1:j)
      store[,k] <- exp(lachooseb(mlen,svec[k]) + svec[k]*log(distn) +
                         (mlen-svec[k])*log(1-distn))
    p <- apply(store,1,sum)
    if(largest != lower.tail) p <- 1 - p
    p
  }

dFrechet <-  function(x, loc = 0, scale = 1, shape = 1, log = FALSE){

    if(min(scale) <= 0 || min(shape) <= 0) stop("invalid arguments")
    x <- (x - loc)/scale
    xpos <- x[x>0 | is.na(x)]
    nn <- length(x)
    scale <- rep(scale, length.out = nn)[x>0 | is.na(x)]
    shape <- rep(shape, length.out = nn)[x>0 | is.na(x)]
    d <- numeric(nn)
    d[x>0 | is.na(x)] <- log(shape/scale) - (1+shape) * log(xpos) -
      xpos^(-shape)
    d[x<=0 & !is.na(x)] <- -Inf
    if(!log) d <- exp(d)
    d
  }

dGumbel <- function(x, loc = 0, scale = 1, log = FALSE){

    dGenExtrVal(x, loc = loc, scale = scale, shape = 0, log = log)
  }

dRevWeibull <- function(x, loc = 0, scale = 1, shape = 1, log = FALSE) {

    if(min(scale) <= 0 || min(shape) <= 0) stop("invalid arguments")
    x <- (x - loc)/scale
    xneg <- x[x<0 | is.na(x)]
    nn <- length(x)
    scale <- rep(scale, length.out = nn)[x<0 | is.na(x)]
    shape <- rep(shape, length.out = nn)[x<0 | is.na(x)]
    d <- numeric(nn)
    d[x<0 | is.na(x)] <- log(shape/scale) + (shape-1) * log(-xneg) -
      (-xneg)^shape
    d[x>=0 & !is.na(x)] <- -Inf
    if(!log) d <- exp(d)
    d
  }

dNegWeibull <- function(x, loc = 0, scale = 1, shape = 1, log = FALSE){

    if(min(scale) <= 0 || min(shape) <= 0) stop("invalid arguments")
    x <- (x - loc)/scale
    xneg <- x[x<0 | is.na(x)]
    nn <- length(x)
    scale <- rep(scale, length.out = nn)[x<0 | is.na(x)]
    shape <- rep(shape, length.out = nn)[x<0 | is.na(x)]
    d <- numeric(nn)
    d[x<0 | is.na(x)] <- log(shape/scale) + (shape-1) * log(-xneg) -
      (-xneg)^shape
    d[x>=0 & !is.na(x)] <- -Inf
    if(!log) d <- exp(d)
    d
  }


dGenExtrVal <- function(x, loc = 0, scale = 1, shape = 0, log = FALSE) {

    if(min(scale) <= 0) stop("invalid scale")
    if(length(shape) != 1) stop("invalid shape")
    x <- (x - loc)/scale
    if(shape == 0)
      d <- log(1/scale) - x - exp(-x)
    else {
      nn <- length(x)
      xx <- 1 + shape*x
      xxpos <- xx[xx>0 | is.na(xx)]
      scale <- rep(scale, length.out = nn)[xx>0 | is.na(xx)]
      d <- numeric(nn)
      d[xx>0 | is.na(xx)] <- log(1/scale) - xxpos^(-1/shape) -
        (1/shape + 1)*log(xxpos)
      d[xx<=0 & !is.na(xx)] <- -Inf
    }
    if(!log) d <- exp(d)
    d
  }

dGenPareto <- function(x, loc = 0, scale = 1, shape = 0, log = FALSE) {

    if(min(scale) <= 0) stop("invalid scale")
    if(length(shape) != 1) stop("invalid shape")
    d <- (x - loc)/scale
    nn <- length(d)
    scale <- rep(scale, length.out = nn)
    index <- (d > 0 & ((1 + shape * d) > 0)) | is.na(d)
    if(shape == 0) {
      d[index] <- log(1/scale[index]) - d[index]
      d[!index] <- -Inf
    }
    else {
      d[index] <- log(1/scale[index]) - (1/shape + 1) *
        log(1 + shape * d[index])
      d[!index] <- -Inf
    }
    if(!log) d <- exp(d)
    d
  }

dExtrVal <- function(x, densfun, distnfun, ..., distn, mlen = 1, largest = TRUE, log = FALSE)
  {
    if(!is.numeric(mlen) || length(mlen) != 1 || mlen < 1 ||
       mlen %% 1 != 0)
      stop("`mlen' must be a non-negative integer")
    if(missing(densfun))
      densfun <- get(paste("d", distn, sep=""), mode="function")
    if(missing(distnfun))
      distnfun <- get(paste("p", distn, sep=""), mode="function")
    dens <- densfun(x, ..., log = TRUE)
    distn <- distnfun(x, ...)[!is.infinite(dens)]
    if(!largest) distn <- 1 - distn
    distn <- (mlen-1) * log(distn)
    d <- numeric(length(x))
    d[!is.infinite(dens)] <- log(mlen) + dens[!is.infinite(dens)] + distn
    d[is.infinite(dens)] <- -Inf
    if(!log) d <- exp(d)
    d
  }

dOrder <- function(x, densfun, distnfun, ..., distn, mlen = 1, j = 1, largest = TRUE,
           log = FALSE) {

    if(!is.numeric(mlen) || length(mlen) != 1 || mlen < 1 ||
       mlen %% 1 != 0)
      stop("'mlen' must be a non-negative integer")
    if(!is.numeric(j) || length(j) != 1 || j < 1 || j %% 1 != 0)
      stop("'j' must be a non-negative integer")
    if(j > mlen)
      stop("`j' cannot be greater than `mlen'")
    if(!largest) j <- mlen + 1 - j
    if(missing(densfun))
      densfun <- get(paste("d", distn, sep=""), mode="function")
    if(missing(distnfun))
      distnfun <- get(paste("p", distn, sep=""), mode="function")
    dens <- densfun(x, ..., log = TRUE)
    distn <- distnfun(x, ...)[!is.infinite(dens)]
    distn <- (mlen-j) * log(distn) + (j-1) * log(1-distn)
    comb <- lgamma(mlen+1) - lgamma(j) - lgamma(mlen-j+1)
    d <- numeric(length(x))
    d[!is.infinite(dens)] <- comb + dens[!is.infinite(dens)] + distn
    d[is.infinite(dens)] <- -Inf
    if(!log) d <- exp(d)
    d
  }

# this is a verbatim copy from the package flexsurv (Christopher Jackson)

dGompertz <- function(x, shape, rate=1, log=FALSE) {
  dgompertz_work(x, shape, rate, log)
}

pGompertz <- function(q, shape, rate=1, lower.tail = TRUE, log.p = FALSE) {
  pgompertz_work(q, shape, rate, lower.tail, log.p)
}

qGompertz <- function(p, shape, rate=1, lower.tail = TRUE, log.p = FALSE) {
  d <- dbase("gompertz", lower.tail=lower.tail, log=log.p, p=p, shape=shape, rate=rate)
  for (i in seq_along(d)) assign(names(d)[i], d[[i]])
  ret[ind][shape==0] <- qexp(p[shape==0], rate=rate[shape==0])
  sn0 <- shape!=0
  if (any(sn0)) {
    p <- p[sn0]; shape <- shape[sn0]; rate <- rate[sn0]
    asymp <- 1 - exp(rate/shape)
    immortal <- shape < 0 & p > asymp
    ret[ind][sn0][immortal] <- Inf
    ret[ind][sn0][!immortal] <- 1 / shape[!immortal] *
      log1p(-log1p(-p[!immortal]) * shape[!immortal] / rate[!immortal])
  }
  ret
}

rGompertz <- function(n, shape = 1, rate = 1){
  r <- rbase("gompertz", n=n, shape=shape, rate=rate)
  for (i in seq_along(r)) assign(names(r)[i], r[[i]])
  ret[ind] <- qGompertz(p=runif(sum(ind)), shape=shape, rate=rate)
  ret
}


### Standardised procedure for defining density, cumulative
### distribution, hazard and cumulative hazard functions for
### time-to-event distributions

dbase <- function(dname, lower.tail=TRUE, log=FALSE, ...){
  args <- list(...)
  ## Vectorise all arguments, replicating to length of longest argument
  n <- max(sapply(args, length))
  for (i in seq_along(args)) {
    args[[i]] <- rep(args[[i]], length=n)
  }
  ret <- numeric(n)
  ## Check for parameters out of range, give warning and return NaN
  ## for those
  check.fn <- paste("check.",dname,sep="")
  check.ret <- do.call(check.fn, args[-1])
  ret[!check.ret] <- NaN
  for (i in seq_along(args))
    ret[is.nan(args[[i]])] <- NaN
  ## name of first arg is x for PDF, haz, or cum haz, q for CDF and p for quantile function
  stopifnot( !(names(args)[1]=="x" && lower.tail==FALSE))
  if (names(args)[1] %in% c("x","q")){
    x <- args[[1]]
    ## PDF, CDF, hazard and cumulative hazard is 0 for any negative time
    ret[!is.nan(ret) & (x<0)] <- if (lower.tail) { if (log) -Inf else 0 } else { if (log) 0 else 1 }
  }
  if (names(args)[1] == "p") {
    p <- args[[1]]
    if (log) p <- exp(p)
    if (!lower.tail) p <- 1 - p
    args[[1]] <- p
    ret[p < 0 | p > 1] <- NaN
    ## should be 0,Inf for p=0,1, but hopefully always handled anyway
    ## Result is NA if x or a parameter is NA
  }
  ## Result is NA if x or a parameter is NA
  nas <- rep(FALSE, n)
  for (i in seq_along(args)) nas <- nas | (is.na(args[[i]]) & !is.nan(args[[i]]))
  ret[nas] <- NA
  ind <- !is.nan(ret) & !nas
  if (names(args)[1] %in% c("x", "q")) ind <- ind & (x>=0)
  ## Any remaining elements of vector are filled in by standard
  ## formula for hazard
  li <- list(ret=ret, ind=ind)
  for(i in seq_along(args)) args[[i]] <- args[[i]][ind]
  c(li, args)
}

### Standardised procedure for defining random sampling functions

rbase <- function(dname, n, ...){
  ## Vectorise all arguments, replicating to sample length
  if (length(n) > 1) n <- length(n)
  args <- list(...)
  for (i in seq_along(args)) {
    args[[i]] <- rep(args[[i]], length=n)
  }
  ret <- numeric(n)
  ## Check for parameters out of range, give warning and return NaN
  ## for those
  check.fn <- paste("check.",dname,sep="")
  check.ret <- do.call(check.fn, args)
  ret[!check.ret] <- NaN
  for (i in seq_along(args))
    ret[is.nan(args[[i]])] <- NaN
  nas <- rep(FALSE, n)
  for (i in seq_along(args)) nas <- nas | (is.na(args[[i]]) & !is.nan(args[[i]]))
  ret[nas] <- NA
  ind <- !is.nan(ret) & !nas
  li <- list(ret=ret, ind=ind)
  for(i in seq_along(args)) args[[i]] <- args[[i]][ind]
  c(li, args)
}

