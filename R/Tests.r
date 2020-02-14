
## stats: tests ====


#### ******************************
#### ******TODO*TODO***************
#### ******xxxxxxxxx***************
#### ******************************

# original:

# https://github.com/nicebread/WRS
# Rand Wilcox,
# http://www.psychology.mcmaster.ca/bennett/boot09/rt2.pdf

#
#  Compute a 1-alpha confidence interval for the difference between
#  the trimmed means corresponding to two independent groups.
#  The bootstrap percentile t method is used.
#
#  The default amount of trimming is tr=.2
#  side=T indicates two-sided method using absolute value of the
#  test statistics within the bootstrap; otherwise the equal-tailed method
#  is used.
#
#  This function uses trimse.
#

# side<-as.logical(side)
# p.value<-NA
# yuenbt<-vector(mode="numeric",length=2)
# if(SEED)set.seed(2) # set seed of random number generator so that
# #             results can be duplicated.
# x<-x[!is.na(x)]  # Remove missing values in x
# y<-y[!is.na(y)]  # Remove missing values in y
# xcen<-x-mean(x,tr)
# ycen<-y-mean(y,tr)
# if(!side){
#   if(pr)print("NOTE: p-value computed only when side=T")
# }
# test<-(mean(x,tr)-mean(y,tr))/sqrt(trimse(x,tr=tr)^2+trimse(y,tr=tr)^2)
# datax<-matrix(sample(xcen,size=length(x)*nboot,replace=TRUE),nrow=nboot)
# datay<-matrix(sample(ycen,size=length(y)*nboot,replace=TRUE),nrow=nboot)
# top<-apply(datax,1,mean,tr)-apply(datay,1,mean,tr)
# botx<-apply(datax,1,trimse,tr)
# boty<-apply(datay,1,trimse,tr)
# tval<-top/sqrt(botx^2+boty^2)
# if(plotit){
#   if(op == 1)
#     akerd(tval)
#   if(op == 2)
#     rdplot(tval)
# }
# if(side)tval<-abs(tval)
# tval<-sort(tval)
# icrit<-floor((1-alpha)*nboot+.5)
# ibot<-floor(alpha*nboot/2+.5)
# itop<-floor((1-alpha/2)*nboot+.5)
# se<-sqrt((trimse(x,tr))^2+(trimse(y,tr))^2)
# yuenbt[1]<-mean(x,tr)-mean(y,tr)-tval[itop]*se
# yuenbt[2]<-mean(x,tr)-mean(y,tr)-tval[ibot]*se
# if(side){
#   yuenbt[1]<-mean(x,tr)-mean(y,tr)-tval[icrit]*se
#   yuenbt[2]<-mean(x,tr)-mean(y,tr)+tval[icrit]*se
#   p.value<-(sum(abs(test)<=abs(tval)))/nboot
# }
# list(ci=yuenbt,test.stat=test,p.value=p.value,est.1=mean(x,tr),est.2=mean(y,tr),est.dif=mean(x,tr)-mean(y,tr),
#      n1=length(x),n2=length(y))



# getAnywhere(t.test.default)
#
# function (x, y = NULL, alternative = c("two.sided", "less", "greater"),
#           mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95,
#           trim = 0, nboot = 599, na.rm = FALSE
#           ...)

.YuenTTestB <- function(x, y, trim = 0, conf.level = 0.95, nboot=599
                        , alternative = c("two.sided", "less", "greater"), mu = 0, na.rm = FALSE){


  TrimSE <- function(x, trim = 0, na.rm = FALSE) {

    #  Estimate the standard error of the gamma trimmed mean
    #  The default amount of trimming is trim = 0.2

    if(na.rm) x <- na.omit(x)

    winvar <- var(Winsorize(x, probs = c(trim, 1-trim)))

    trimse <- sqrt(winvar) / ((1 - 2 * trim) * sqrt(length(x)))
    trimse
  }


  alternative <- match.arg(alternative)
  method <- "Yuen Two Sample bootstrap t-test"
  dname <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))

  if(na.rm) x <- na.omit(x)
  if(na.rm) y <- na.omit(y)

  meanx <- mean(x, trim = trim)
  meany <- mean(y, trim = trim)

  tstat <- (meanx - meany ) / sqrt(TrimSE(x, trim = trim)^2 + TrimSE(y, trim = trim)^2)

  sampx <- matrix(sample(x - meanx, size=length(x) * nboot, replace=TRUE), nrow=nboot)
  sampy <- matrix(sample(y - meany, size=length(y) * nboot, replace=TRUE), nrow=nboot)

  top <- apply(sampx, 1, mean, trim) - apply(sampy, 1, mean, trim)
  botx <- apply(sampx, 1, TrimSE, trim)
  boty <- apply(sampy, 1, TrimSE, trim)
  tval <- top / sqrt(botx^2 + boty^2)


  alpha <- 1 - conf.level
  se <- sqrt((TrimSE(x, trim = trim))^2 + (TrimSE(y, trim = trim))^2)

  if(alternative == "two.sided") {
    tval <- abs(tval)
    icrit <- floor((1 - alpha) * nboot + .5)
    cint <- meanx - meany + c(-1, 1) * tval[icrit] * se
    pval <- (sum(abs(tstat) <= abs(tval))) / nboot

  } else {
    tval <- sort(tval)
    ibot <- floor(alpha/2 * nboot + .5)
    itop <- floor((1 - alpha/2) * nboot + .5)
    cint <- meanx - meany - tval[c(itop, ibot)] * se

  }

  names(tstat) <- "t"
  names(mu) <- "difference in means"
  estimate <- c(meanx, meany)
  names(estimate) <- c("mean of x", "mean of y")

  attr(cint, "conf.level") <- conf.level
  rval <- list(statistic = tstat, p.value = pval,
               conf.int = cint, estimate = estimate, null.value = mu,
               alternative = alternative, method = method, data.name = dname)
  class(rval) <- "htest"
  return(rval)

}



YuenTTest <- function (x, ...)
  UseMethod("YuenTTest")


YuenTTest.formula <- function (formula, data, subset, na.action, ...)  {

  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame())
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  g <- factor(mf[[-response]])
  if (nlevels(g) != 2L)
    stop("grouping factor must have exactly 2 levels")
  DATA <- setNames(split(mf[[response]], g), c("x", "y"))
  y <- DoCall("YuenTTest", c(DATA, list(...)))
  y$data.name <- DNAME
  if (length(y$estimate) == 2L)
    names(y$estimate) <- paste("trimmed mean in group", levels(g))
  y
}


YuenTTest.default <- function (x, y = NULL, alternative = c("two.sided", "less", "greater"),
                               mu = 0, paired = FALSE, conf.level = 0.95, trim = 0.2, ...) {

  alternative <- match.arg(alternative)
  if (!missing(mu) && (length(mu) != 1 || is.na(mu)))
    stop("'mu' must be a single number")
  if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) ||
                               conf.level < 0 || conf.level > 1))
    stop("'conf.level' must be a single number between 0 and 1")
  if (!is.null(y)) {
    dname <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    if (paired)
      xok <- yok <- complete.cases(x, y)
    else {
      yok <- !is.na(y)
      xok <- !is.na(x)
    }
    y <- y[yok]
  }
  else {
    dname <- deparse(substitute(x))
    if (paired)
      stop("'y' is missing for paired test")
    xok <- !is.na(x)
    yok <- NULL
  }
  x <- x[xok]

  nx <- length(x)
  mx <- mean(x, trim = trim)
  vx <- var(Winsorize(x, probs = c(trim, 1-trim)))

  if (is.null(y) | paired) {
    if (nx < 2)
      stop("not enough 'x' observations")

    df <- nx - 2 * floor(trim * nx) - 1

    if(paired){
      my <- mean(y, trim = trim)
      vy <- var(Winsorize(y, probs = c(trim, 1-trim)))
      covxy <- var(Winsorize(x, probs = c(trim, 1-trim)), Winsorize(y, probs = c(trim, 1-trim)))
      stderr <- sqrt( (nx-1) * (vx + vy - 2 * covxy) / ((df + 1) * df) )
    } else {
      stderr <- sqrt(vx) / ((1 - 2 * trim) * sqrt(nx))
    }

    if (stderr < 10 * .Machine$double.eps * abs(mx))
      stop("data are essentially constant")

    if(paired){
      method <- "Yuen Paired t-test"
      tstat <- (mx - my - mu) / stderr
      estimate <- setNames(mx - my, "difference of trimmed means")

    } else {
      method <- "Yuen One Sample t-test"
      tstat <- (mx - mu)/stderr
      estimate <- setNames(mx, "trimmed mean of x")
    }

  }
  else {
    ny <- length(y)
    if (nx < 2)
      stop("not enough 'x' observations")
    if (ny < 2)
      stop("not enough 'y' observations")
    my <- mean(y, trim = trim)
    vy <- var(Winsorize(y, probs = c(trim, 1-trim)))
    method <- "Yuen Two Sample t-test"
    estimate <- c(mx, my)
    names(estimate) <- c("trimmed mean of x", "trimmed mean of y")

    dfx <- length(x) - 2 * floor(trim * length(x)) - 1
    dfy <- length(y) - 2 * floor(trim * length(y)) - 1

    stderrx <- (length(x) - 1) * vx / ((dfx + 1) * dfx)
    stderry <- (length(y) - 1) * vy / ((dfy + 1) * dfy)

    df <- (stderrx + stderry)^2 / (stderrx^2 / dfx + stderry^2 / dfy)

    stderr <- sqrt(stderrx + stderry)

    if (stderr < 10 * .Machine$double.eps * max(abs(mx), abs(my)))
      stop("data are essentially constant")
    tstat <- (mx - my - mu) / stderr
  }
  if (alternative == "less") {
    pval <- pt(tstat, df)
    cint <- c(-Inf, tstat + qt(conf.level, df))
  }
  else if (alternative == "greater") {
    pval <- pt(tstat, df, lower.tail = FALSE)
    cint <- c(tstat - qt(conf.level, df), Inf)
  }
  else {
    pval <- 2 * pt(-abs(tstat), df)
    alpha <- 1 - conf.level
    cint <- qt(1 - alpha/2, df)
    cint <- tstat + c(-cint, cint)
  }
  cint <- mu + cint * stderr
  names(tstat) <- "t"
  names(df) <- "df"
  names(trim) <- "trim"
  names(mu) <- if (paired || !is.null(y))
    "difference in trimmed means"
  else "trimmed mean"
  attr(cint, "conf.level") <- conf.level
  rval <- list(statistic = tstat, parameter = c(df, trim), p.value = pval,
               conf.int = cint, estimate = estimate, null.value = mu,
               alternative = alternative, method = method, data.name = dname)
  class(rval) <- "htest"
  return(rval)
}



TTestA <- function (mx, sx, nx, my=NULL, sy = NULL, ny=NULL,
                     alternative = c("two.sided", "less", "greater"),
          mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95,
          ...) {

  if (paired)
    stop("paired option is not supported, use one-sample-test for the differences instead")
  
  alternative <- match.arg(alternative)
  if (!missing(mu) && (length(mu) != 1 || is.na(mu)))
    stop("'mu' must be a single number")
  if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) ||
                               conf.level < 0 || conf.level > 1))
    stop("'conf.level' must be a single number between 0 and 1")

  if (!is.null(my)) {
    dname <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))

  } else {
    dname <- deparse(substitute(x))
    if (paired)
      stop("'y' is missing for paired test")
  }

  vx <- sx^2

  if (is.null(my)) {
    if (nx < 2)
      stop("not enough 'x' observations")
    df <- nx - 1
    stderr <- sqrt(vx/nx)
    if (stderr < 10 * .Machine$double.eps * abs(mx))
      stop("data are essentially constant")
    tstat <- (mx - mu)/stderr
    method <- if (paired)
      "Paired t-test"
    else "One Sample t-test"
    estimate <- setNames(mx, if (paired)
      "mean of the differences"
      else "mean of x")

  } else {
    # ny <- length(y)
    if (nx < 1 || (!var.equal && nx < 2))
      stop("not enough 'x' observations")
    if (ny < 1 || (!var.equal && ny < 2))
      stop("not enough 'y' observations")
    if (var.equal && nx + ny < 3)
      stop("not enough observations")
    # my <- mean(y)
    # vy <- var(y)
    vy <- sy^2
    method <- paste(if (!var.equal)
      "Welch", "Two Sample t-test")
    estimate <- c(mx, my)
    names(estimate) <- c("mean of x", "mean of y")
    if (var.equal) {
      df <- nx + ny - 2
      v <- 0
      if (nx > 1)
        v <- v + (nx - 1) * vx
      if (ny > 1)
        v <- v + (ny - 1) * vy
      v <- v/df
      stderr <- sqrt(v * (1/nx + 1/ny))
    }
    else {
      stderrx <- sqrt(vx/nx)
      stderry <- sqrt(vy/ny)
      stderr <- sqrt(stderrx^2 + stderry^2)
      df <- stderr^4/(stderrx^4/(nx - 1) + stderry^4/(ny - 1))
    }
    if (stderr < 10 * .Machine$double.eps * max(abs(mx),
                                                abs(my)))
      stop("data are essentially constant")
    tstat <- (mx - my - mu)/stderr
  }
  if (alternative == "less") {
    pval <- pt(tstat, df)
    cint <- c(-Inf, tstat + qt(conf.level, df))
  }
  else if (alternative == "greater") {
    pval <- pt(tstat, df, lower.tail = FALSE)
    cint <- c(tstat - qt(conf.level, df), Inf)
  }
  else {
    pval <- 2 * pt(-abs(tstat), df)
    alpha <- 1 - conf.level
    cint <- qt(1 - alpha/2, df)
    cint <- tstat + c(-cint, cint)
  }
  cint <- mu + cint * stderr
  names(tstat) <- "t"
  names(df) <- "df"
  names(mu) <- if (paired || !is.null(my))
    "difference in means"
  else "mean"
  attr(cint, "conf.level") <- conf.level
  rval <- list(statistic = tstat, parameter = df, p.value = pval,
               conf.int = cint, estimate = estimate, null.value = mu, stderr = stderr, 
               alternative = alternative, method = method, data.name = dname)
  class(rval) <- "htest"
  return(rval)
}



SignTest <- function (x, ...)  UseMethod("SignTest")

SignTest.formula <- function (formula, data, subset, na.action, ...) {

  # this is designed just like wilcox.test.formula

  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- as.name("model.frame")
  m$... <- NULL
  mf <- eval(m, parent.frame())
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  g <- factor(mf[[-response]])
  if (nlevels(g) != 2L)
    stop("grouping factor must have exactly 2 levels")
  DATA <- split(mf[[response]], g)
  names(DATA) <- c("x", "y")
  y <- DoCall("SignTest", c(DATA, list(...)))
  y$data.name <- DNAME
  y

}

# test:
#  cbind( c(NA,sort(x)), 0:n, dbinom(0:n, size=n, prob=0.5),  pbinom(0:n, size=n, prob=0.5))

SignTest.default <- function(x, y = NULL, alternative = c("two.sided", "less", "greater"),
                             mu = 0, conf.level = 0.95, ...) {

  # MedianCI_Binom <- function( x, conf.level = 0.95,
  #                             alternative = c("two.sided", "less", "greater"), na.rm = FALSE ){
  #   # http://www.stat.umn.edu/geyer/old03/5102/notes/rank.pdf
  #   # http://de.scribd.com/doc/75941305/Confidence-Interval-for-Median-Based-on-Sign-Test
  #   if(na.rm) x <- na.omit(x)
  #   n <- length(x)
  #   switch( match.arg(alternative)
  #           , "two.sided" = {
  #             k <- qbinom(p = (1 - conf.level) / 2, size=n, prob=0.5, lower.tail=TRUE)
  #             ci <- sort(x)[c(k, n - k + 1)]
  #             attr(ci, "conf.level") <- 1 - 2 * pbinom(k-1, size=n, prob=0.5)
  #           }
  #           , "greater" = {
  #             k <- qbinom(p = (1 - conf.level), size=n, prob=0.5, lower.tail=TRUE)
  #             ci <- c(sort(x)[k], Inf)
  #             attr(ci, "conf.level") <- 1 - pbinom(k-1, size=n, prob=0.5)
  #           }
  #           , "less" = {
  #             k <- qbinom(p = conf.level, size=n, prob=0.5, lower.tail=TRUE)
  #             ci <- c(-Inf, sort(x)[k])
  #             attr(ci, "conf.level") <- pbinom(k, size=n, prob=0.5)
  #           }
  #   )
  #   return(ci)
  # }

  alternative <- match.arg(alternative)

  if (!missing(mu) && ((length(mu) > 1L) || !is.finite(mu)))
    stop("'mu' must be a single number")

  if (!((length(conf.level) == 1L) && is.finite(conf.level) &&
        (conf.level > 0) && (conf.level < 1)))
    stop("'conf.level' must be a single number between 0 and 1")

  if (!is.numeric(x))
    stop("'x' must be numeric")

  if (!is.null(y)) {
    if (!is.numeric(y))
      stop("'y' must be numeric")
    if (length(x) != length(y))
      stop("'x' and 'y' must have the same length")

    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    OK <- complete.cases(x, y)
    x <- x[OK]
    y <- y[OK]
    METHOD <- "Dependent-samples Sign-Test"
    x <- (x - y)

  } else {
    DNAME <- deparse(substitute(x))
    x <- x[is.finite(x)]
    METHOD <- "One-sample Sign-Test"
  }

  d <- (x - mu)

  # Naive version:
  n.valid <- sum(d > 0) + sum(d < 0)
  if(n.valid > 0) {
    RVAL <- binom.test(x=sum(d > 0), n=n.valid, p=0.5, alternative = alternative, conf.level = conf.level )
  } else {
    RVAL <- binom.test(x=1, n=1)
  }

  RVAL$method <- METHOD
  RVAL$data.name <- DNAME
  names(mu) <- if (!is.null(y)) "median difference" else "median"

  names(RVAL$statistic) <- "S"
  RVAL$estimate <- median(d + mu, na.rm=TRUE)
  names(RVAL$parameter) <- "number of differences"
  mci <- MedianCI(d + mu, conf.level=conf.level, 
                  sides=if(alternative=="less") "right" else if(alternative=="greater") "left" else "two.sided", na.rm=TRUE)
  RVAL$conf.int <- mci[-1]
  attr(RVAL$conf.int, "conf.level") = round(attr(mci,"conf.level"), 3)

  names(RVAL$estimate) <- "median of the differences"
  RVAL$null.value <- mu
  class(RVAL) <- "htest"
  return(RVAL)

}



ZTest <- function (x, ...)
  UseMethod("ZTest")


ZTest.formula <- function (formula, data, subset, na.action, ...)  {

  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame())
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  g <- factor(mf[[-response]])
  if (nlevels(g) != 2L)
    stop("grouping factor must have exactly 2 levels")
  DATA <- setNames(split(mf[[response]], g), c("x", "y"))
  y <- DoCall("ZTest", c(DATA, list(...)))
  y$data.name <- DNAME
  if (length(y$estimate) == 2L)
    names(y$estimate) <- paste("mean in group", levels(g))
  y
}


ZTest.default <- function (x, y = NULL, alternative = c("two.sided", "less", "greater"),
                           paired = FALSE, mu = 0, sd_pop, conf.level = 0.95,  ...)  {

  alternative <- match.arg(alternative)
  if (!missing(mu) && (length(mu) != 1 || is.na(mu)))
    stop("'mu' must be a single number")
  if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) ||
                               conf.level < 0 || conf.level > 1))
    stop("'conf.level' must be a single number between 0 and 1")
  if (!is.null(y)) {
    dname <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))

    if (paired)
      xok <- yok <- complete.cases(x, y)
    else {
      yok <- !is.na(y)
      xok <- !is.na(x)
    }

    y <- y[yok]
  }
  else {
    dname <- deparse(substitute(x))
    if (paired)
      stop("'y' is missing for paired test")
    xok <- !is.na(x)
    yok <- NULL
  }
  x <- x[xok]

  if (paired) {
    x <- x - y
    y <- NULL
  }

  nx <- length(x)
  mx <- mean(x)
  # vx <- sd_pop^2

  if (is.null(y)) {
    if (nx < 2)
      stop("not enough 'x' observations")
    stderr <- sqrt(sd_pop^2/nx)
    if (stderr < 10 * .Machine$double.eps * abs(mx))
      stop("data are essentially constant")
    zstat <- (mx - mu)/stderr

    method <- method <- if (paired)
      "Paired z-test" else "One Sample z-test"
    estimate <- setNames(mx, if (paired)
      "mean of the differences"
      else "mean of x")
  }
  else {
    ny <- length(y)
    if (nx < 1)
      stop("not enough 'x' observations")
    if (ny < 1)
      stop("not enough 'y' observations")
    if (nx + ny < 3)
      stop("not enough observations")
    my <- mean(y)

    method <- paste("Two Sample z-test")
    estimate <- c(mx, my)
    names(estimate) <- c("mean of x", "mean of y")

    stderr <- sqrt(sd_pop^2 * (1/nx + 1/ny))

    if (stderr < 10 * .Machine$double.eps * max(abs(mx),
                                                abs(my)))
      stop("data are essentially constant")
    zstat <- (mx - my - mu)/stderr
  }
  if (alternative == "less") {
    pval <- pnorm(zstat)
    cint <- c(-Inf, zstat + qnorm(conf.level))
  }
  else if (alternative == "greater") {
    pval <- pnorm(zstat, lower.tail = FALSE)
    cint <- c(zstat - qnorm(conf.level), Inf)
  }
  else {
    pval <- 2 * pnorm(-abs(zstat))
    alpha <- 1 - conf.level
    cint <- qnorm(1 - alpha/2)
    cint <- zstat + c(-cint, cint)
  }
  cint <- mu + cint * stderr
  names(zstat) <- "z"
  names(mu) <- if (paired || !is.null(y))
    "difference in means"
  else "mean"
  names(sd_pop) <- "Std. Dev. Population"
  attr(cint, "conf.level") <- conf.level
  rval <- list(
    statistic = zstat, parameter = sd_pop, p.value = pval,
    conf.int = cint, estimate = estimate, null.value = mu, stderr = stderr,
    alternative = alternative, method = method, data.name = dname )
  class(rval) <- "htest"
  return(rval)
}





VarTest <- function(x, ...) UseMethod("VarTest")


VarTest.default <- function (x, y = NULL, alternative = c("two.sided", "less", "greater"), ratio = 1,
                             sigma.squared = 1, conf.level = 0.95, ...) {

  if(is.null(y)){
    # perform a one sample variance test

    alternative <- match.arg(alternative)
    if (!missing(sigma.squared) && (length(sigma.squared) != 1 || is.na(sigma.squared)))
      stop("'sigma.squared' must be a single number")

    if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) ||
                                 conf.level < 0 || conf.level > 1))
      stop("'conf.level' must be a single number between 0 and 1")

    dname <- deparse(substitute(x))
    x <- na.omit(x)

    nx <- length(x)
    if (nx < 2)
      stop("not enough 'x' observations")
    df <- nx - 1
    vx <- var(x)

    xstat <- vx * df / sigma.squared

    method <- "One Sample Chi-Square test on variance"
    estimate <- vx

    if (alternative == "less") {
      pval <- pchisq(xstat, df)
      cint <- c(0, df * vx/qchisq((1 - conf.level), df))

    } else if (alternative == "greater") {
      pval <- pchisq(xstat, df, lower.tail = FALSE)
      cint <- c(df * vx/qchisq((1 - conf.level), df, lower.tail = FALSE), Inf)
    } else {
      pval <- 2 * min(pchisq(xstat, df), pchisq(xstat, df, lower.tail = FALSE))
      alpha <- 1 - conf.level
      cint <- qt(1 - alpha/2, df)
      cint <- xstat + c(-cint, cint)
      cint <- df * vx / c(qchisq((1 - conf.level)/2, df, lower.tail = FALSE),
                          qchisq((1 - conf.level)/2, df))
    }

    names(xstat) <- "X-squared"
    names(df) <- "df"
    names(sigma.squared) <- "variance"
    names(estimate) <- "variance of x"
    attr(cint, "conf.level") <- conf.level
    rval <- list(statistic = xstat, parameter = df, p.value = pval,
                 conf.int = cint, estimate = estimate, null.value = sigma.squared,
                 alternative = alternative, method = method, data.name = dname)
    class(rval) <- "htest"

    return(rval)

  } else {
    # perform a normal F-test
    var.test(x=x, y=y, ratio=ratio, alternative=alternative, conf.level=conf.level)
  }

}


VarTest.formula <- function (formula, data, subset, na.action, ...) {

  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame())
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  g <- factor(mf[[-response]])
  if (nlevels(g) != 2L)
    stop("grouping factor must have exactly 2 levels")
  DATA <- setNames(split(mf[[response]], g), c("x", "y"))
  y <- do.call("VarTest", c(DATA, list(...)))
  y$data.name <- DNAME
  y

}





# moved from Rcmdr 13 July 2004

# levene.test.default function slightly modified and generalized from Brian Ripley via R-help
# the original generic version was contributed by Derek Ogle
# last modified 28 January 2010 by J. Fox

LeveneTest <- function (y, ...) {
  UseMethod("LeveneTest")
}

LeveneTest.default <- function (y, group, center=median, ...) { # original levene.test

  if (!is.numeric(y))
    stop(deparse(substitute(y)), " is not a numeric variable")

  if (!is.factor(group)) {
    warning(deparse(substitute(group)), " coerced to factor.")
    group <- as.factor(group)
  }

  valid <- complete.cases(y, group)
  meds <- tapply(y[valid], group[valid], center, ...)
  resp <- abs(y - meds[group])
  table <- anova(lm(resp ~ group))[, c(1, 4, 5)]
  rownames(table)[2] <- " "
  dots <- deparse(substitute(...))

  attr(table, "heading") <- paste("Levene's Test for Homogeneity of Variance (center = ",
                                  deparse(substitute(center)), if(!(dots == "NULL")) paste(":", dots),  ")", sep="")
  table
}


LeveneTest.formula <- function(formula, data, ...) {
  form <- formula
  mf <- if (missing(data)) model.frame(form) else model.frame(form, data)
  if (any(sapply(2:dim(mf)[2], function(j) is.numeric(mf[[j]]))))
    stop("Levene's test is not appropriate with quantitative explanatory variables.")
  y <- mf[,1]
  if(dim(mf)[2]==2) group <- mf[,2]
  else {
    if (length(grep("\\+ | \\| | \\^ | \\:",form))>0) stop("Model must be completely crossed formula only.")
    group <- interaction(mf[,2:dim(mf)[2]])
  }
  LeveneTest.default(y = y, group=group, ...)
}


# LeveneTest.formula <- function (formula, data, subset, na.action, ...) {
#
#   # replaced as the original did not support subsets
#
#   if (missing(formula) || (length(formula) != 3L))
#     stop("'formula' missing or incorrect")
#   m <- match.call(expand.dots = FALSE)
#   if (is.matrix(eval(m$data, parent.frame())))
#     m$data <- as.data.frame(data)
#   m[[1L]] <- quote(stats::model.frame)
#   mf <- eval(m, parent.frame())
#
#   if (any(sapply(2:dim(mf)[2], function(j) is.numeric(mf[[j]]))))
#     stop("Levene's test is not appropriate with quantitative explanatory variables.")
#
#   # from kruskal not to be applied here
#   # if (length(mf) > 2L)
#   #   stop("'formula' should be of the form response ~ group")
#
#   if(dim(mf)[2]==2)
#     group <- mf[, 2]
#   else {
#     if (length(grep("\\+ | \\| | \\^ | \\:", formula)) > 0)
#       stop("Model must be completely crossed formula only.")
#     group <- interaction(mf[, 2:dim(mf)[2]])
#   }
#
#   DNAME <- paste(names(mf), collapse = " by ")
#   names(mf) <- NULL
#   # y <- do.call("LeveneTest", as.list(mf))
#   LeveneTest.default(y=mf[, 1], group=group, ...)
#   # y$data.name <- DNAME
#   # y
# }



LeveneTest.lm <- function(y, ...) {
  LeveneTest.formula(formula(y), data=model.frame(y), ...)
}




RunsTest <- function (x, ...)  UseMethod("RunsTest")

RunsTest.formula <- function (formula, data, subset, na.action, ...) {

  # this is a taken analogue to wilcox.test.formula

  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- as.name("model.frame")
  m$... <- NULL
  mf <- eval(m, parent.frame())
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  g <- factor(mf[[-response]])
  if (nlevels(g) != 2L)
    stop("grouping factor must have exactly 2 levels")
  DATA <- split(mf[[response]], g)
  names(DATA) <- c("x", "y")
  y <- DoCall("RunsTest", c(DATA, list(...)))
  y$data.name <- DNAME
  y

}


RunsTest.default <- function(x, y=NULL, alternative=c("two.sided", "less", "greater"), exact=NULL, correct=TRUE, na.rm = FALSE, ...) {

  # exact values:
  # http://www.reiter1.com/Glossar/Wald_Wolfowitz.htm

  # example:   x <- sample(c(0,1), size=20, r=TRUE)

  pruns <- function(r, n1, n2, alternative=c("two.sided", "less", "greater")) {

    # source: randomizeBE
    # author: D. Labes <detlewlabes at gmx.de>

    # function for calculating the denominator of the runs distribution
    .druns_nom <- function(r, n1, n2){
      pp <- vector(mode="numeric",length=length(r))
      for (i in seq_along(r)){
        if (2*r[i]%/%2==r[i]){
          # even 2*k
          k <- r[i]/2
          pp[i] <- 2*choose(n1-1, k-1)*choose(n2-1, k-1)
        } else {
          # odd 2*k+1
          k <- (r[i]-1)/2
          pp[i] <- choose(n1-1,k-1) * choose(n2-1,k) +
            choose(n1-1,k)   * choose(n2-1,k-1)
        }
      }
      pp
    }

    alternative <- match.arg(alternative)

    n <- n1+n2

    if(r<=1) stop("Number of runs must be > 1")
    if(r>n) stop("Number of runs must be < (n1+n2")
    if(n1<1 | n2<1) return(0) #??? is not random!

    E <- 1 + 2*n1*n2/n

    denom <- choose(n,n1)
    # how long should we make the r vector?
    # in very unsymmetric cases only a few elements of
    # pp = density have values > 0 if rmax=n1+n2
    # number of runs possible: 2*m if n=m, 2*m+1 if m<n
    rmax <- ifelse(n1==n2, 2*n1, 2*min(n1,n2)+1)
    rv <- 2:rmax
    pp <- .druns_nom(rv, n1, n2)

    # pL is p(R<=r) -> left/lower tail
    pL <- sum(pp[rv<=r])/denom
    #pU is p(R>=r) -> right/upper tail
    pU <- 1 - sum(pp[rv<=(r-1)])/denom

    # Equn. 4.7 of the SPSS documentation
    p2 <- sum(pp[abs(rv-E)>=abs(r-E)])/denom

    # Next is the rule from:
    # Gibbons "Nonparametric Methods for Quantitative Analysis"
    # 0.5 is to avoid p>1 if both pL and pU are >0.5
    p2min <- 2*min(c(pL, pU, 0.5))

    # we are using the SPSS approach wich takes into account the
    # unsymmetric form of the distribution if n1 << n2

    return(
      switch( alternative
              , "less" = pL
              , "greater" = pU
              , "two.sided" = p2
      )
    )

  }



  if(!is.null(y)) {
    dname <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    # perform Wald-Wolfowitz-Test with 2 variables
    xy <- Sort(cbind(c(x,y), c(rep(0, length(x)), rep(1, length(y)))))[,2]

    TIES <- length(unique(u <- c(unique(x), unique(y)))) != length(u)
    rm(u)

    res <- RunsTest(x=xy, alternative=alternative, exact=exact, na.rm=na.rm)

    if (TIES)
      warning("cannot compute reliable p-values with inter-group ties between x and y")

    res$data.name <- dname
    res$method <- "Wald-Wolfowitz Runs Test "
    return(res)
  }

  alternative <- match.arg(alternative)
  dname <- deparse(substitute(x))

  # Strip NAs
  if (na.rm) x <- na.omit(x)

  # let's have a 0,1 vector if x is a numeric vector with more than 2 values
  if(is.numeric(x) & (length(unique(x))>2)) {
    est <- median(x, na.rm=TRUE)
    names(est) <- "median(x)"
    x <- ((x > est)*1)

  } else {
    est <- NULL
  }

  x <- factor(x)
  if( nlevels(x) %nin% c(1,2) ) stop("Can only process dichotomous variables")
  x <- as.numeric(x) - 1

  # x <- sample(c(0,1), 100000000, replace=TRUE)
  # ### user  system elapsed
  #   9.39    6.76   16.30    system.time(rle(x))
  #   4.49    3.45    8.00    system.time(sum(diff(x) != 0) + 1)
  # so don't use rle...

  runs <- sum(diff(x) != 0) + 1
  m <- sum(x==0)
  n <- sum(x==1)

  if(is.null(exact)) { exact <- ((m +n) <= 30) }

  E <- 1 + 2*n*m / (n + m)
  s2 <- (2*n*m * (2*n*m - n - m)) / ((n + m)^2 * (n + m - 1))

  # this is the SPSS-Definition
  # http://publib.boulder.ibm.com/infocenter/spssstat/v20r0m0/index.jsp?topic=%2Fcom.ibm.spss.statistics.help%2Fidh_idd_npar_onesample_settings_tests_runs.htm
  # if( n+m >= 50) {
  if(correct){
    switch( as.character(cut(runs - E, breaks=c(-Inf, -0.5, 0.5, Inf), labels=c("a", "b", "c")))
            , "a" = statistic <- (runs - E + 0.5) / sqrt(s2)
            , "b" = statistic <- 0
            , "c" = statistic <- (runs - E - 0.5) / sqrt(s2)
    )
  } else {
    statistic <- (runs - E) / sqrt(s2)
  }

  switch( alternative
          , "less" = {
            p.value <- ifelse(exact, pruns(runs, m, n, alternative="less"), pnorm(statistic) )
            alternative <- "true number of runs is less than expected"
          }
          , "greater" = {
            p.value = ifelse(exact, pruns(runs, m, n, alternative="greater"), 1 - pnorm(statistic) )
            alternative <- "true number of runs is greater than expected"
          }
          , "two.sided" = {
            p.value = ifelse(exact, pruns(runs, m, n, alternative="two.sided"),
                             2 * min(pnorm(statistic), 1 - pnorm(statistic)) )
            alternative <- "true number of runs is not equal the expected number"
          }
  )

  method = "Runs Test for Randomness"
  names(statistic) <- "z"  # Standardized Runs Statistic

  # do not report statistic when exact p-value is calculated
  if(exact) statistic <- NULL

  structure(list(
    statistic = statistic,
    p.value = p.value,
    method = method,
    alternative = alternative,
    data.name = dname,
    estimate = est,
    parameter = c(runs=runs, m=m, n=n)),
    class = "htest")

}





DurbinWatsonTest <- function(formula, order.by = NULL, alternative = c("greater", "two.sided", "less"),
                             iterations = 15, exact = NULL, tol = 1e-10, data = list()) {

  dname <- paste(deparse(substitute(formula)))
  alternative <- match.arg(alternative)

  if(!inherits(formula, "formula")) {
    if(!is.null(w <- weights(formula))) {
      if(!isTRUE(all.equal(as.vector(w), rep(1L, length(w)))))
        stop("weighted regressions are not supported")
    }
    X <- if(is.matrix(formula$x))
      formula$x
    else model.matrix(terms(formula), model.frame(formula))
    y <- if(is.vector(formula$y))
      formula$y
    else model.response(model.frame(formula))
  } else {
    mf <- model.frame(formula, data = data)
    y <- model.response(mf)
    X <- model.matrix(formula, data = data)
  }

  if(!is.null(order.by))
  {
    if(inherits(order.by, "formula")) {
      z <- model.matrix(order.by, data = data)
      z <- as.vector(z[,ncol(z)])
    } else {
      z <- order.by
    }
    X <- as.matrix(X[order(z),])
    y <- y[order(z)]
  }

  n <- nrow(X)
  if(is.null(exact)) exact <- (n < 100)
  k <- ncol(X)

  res <- lm.fit(X,y)$residuals
  dw <- sum(diff(res)^2)/sum(res^2)
  Q1 <- chol2inv(qr.R(qr(X)))
  if(n < 3) {
    warning("not enough observations for computing a p value, set to 1")
    pval <- 1
  } else {
    if(exact)
    {
      A <- diag(c(1,rep(2, n-2), 1))
      A[abs(row(A)-col(A))==1] <- -1
      MA <- diag(rep(1,n)) - X %*% Q1 %*% t(X)
      MA <- MA %*% A
      ev <- eigen(MA)$values[1:(n-k)]
      if(any(Im(ev)>tol)) warning("imaginary parts of eigenvalues discarded")
      ev <- Re(ev)
      ev <- ev[ev>tol]

      pdw <- function(dw) .Fortran("pan", as.double(c(dw,ev)), as.integer(length(ev)),
                                   as.double(0), as.integer(iterations), x=double(1),
                                   PACKAGE = "DescTools")$x
      pval <- switch(alternative,
                     "two.sided" = (2*min(pdw(dw), 1-pdw(dw))),
                     "less" = (1 - pdw(dw)),
                     "greater" = pdw(dw))

      if(is.na(pval) || ((pval > 1) | (pval < 0)))
      {
        warning("exact p value cannot be computed (not in [0,1]), approximate p value will be used")
        exact <- FALSE
      }
    }
    if(!exact)
    {
      if(n < max(5, k)) {
        warning("not enough observations for computing an approximate p value, set to 1")
        pval <- 1
      } else {
        AX <- matrix(as.vector(filter(X, c(-1, 2, -1))), ncol = k)
        AX[1,] <- X[1,] - X[2,]
        AX[n,] <- X[n,] - X[(n-1),]
        XAXQ <- t(X) %*% AX %*% Q1
        P <- 2*(n-1) - sum(diag(XAXQ))
        Q <- 2*(3*n - 4) - 2* sum(diag(crossprod(AX) %*% Q1)) + sum(diag(XAXQ %*% XAXQ))
        dmean <- P/(n-k)
        dvar <- 2/((n-k)*(n-k+2)) * (Q - P*dmean)
        pval <- switch(alternative,
                       "two.sided" = (2*pnorm(abs(dw-dmean), sd=sqrt(dvar), lower.tail = FALSE)),
                       "less" = pnorm(dw, mean = dmean, sd = sqrt(dvar), lower.tail = FALSE),
                       "greater" = pnorm(dw, mean = dmean, sd = sqrt(dvar)))
      }
    }
  }

  alternative <- switch(alternative,
                        "two.sided" = "true autocorrelation is not 0",
                        "less" = "true autocorrelation is less than 0",
                        "greater" = "true autocorrelation is greater than 0")

  names(dw) <- "DW"
  RVAL <- list(statistic = dw, method = "Durbin-Watson test",
               alternative = alternative, p.value= pval, data.name=dname)
  class(RVAL) <- "htest"
  return(RVAL)
}




VonNeumannTest <- function (x, alternative = c("two.sided", "less", "greater"), unbiased=TRUE) {


  ## ToDo: use incomplete beta for exact p-values
  ## ************************
  ## see: von Neumann Successive Difference 1941
  ##
  # n <- 50
  # vx <- 1
  #
  # mu2 <- (4 * (3*n - 4)/(n-1)^2) * vx^2
  #
  # q2 <- (3*n^4 - 10*n^3 -18*n^2 + 79*n - 60) / (8*n^3 - 50*n + 48)
  # q1 <- (4 - mu2 * (q2 + 1) * (q2 + 3)) / (4 - mu2 * (q2 + 1))
  # a2 <- 2 * (q1 - q2 - 2) / (q2 + 1)
  # cc <- a2 ^(q1 - q2 - 2) / beta(q1 - q2 -1, q2+1)
  #
  # c(q1, q2, a2, cc)
  #
  # pbeta(0.75, shape1 = q1 - q2 -1, shape2= q2+1)
  # pbeta(0.75, shape1 = q1 - q2 -1, shape2= q2+1)
  #
  # beta(q1 - q2 -1, q2+1)


  alternative <- match.arg(alternative)

  dname <- deparse(substitute(x))

  x <- x[!is.na(x)]

  d <- diff(x)
  n <- length(x)
  mx <- mean(x)

  if(unbiased) {

    # http://www.chegg.com/homework-help/detecting-autocorrelation-von-neumann-ratio-test-assuming-re-chapter-12-problem-4-solution-9780073375779-exc

    VN <- sum(d^2) / sum((x - mx)^2) * n/(n-1)
    Ex <- 2 * n/(n-1)
    Vx <- 4 * n^2 * (n-2) / ((n+1) * (n-1)^3)
    z <- (VN - Ex) / Vx

  } else {
    VN <- sum(d^2) / sum((x - mx)^2)
    z <- (1-(VN/2)) / sqrt((n-2)/(n^2 - 1))
  }


  if (alternative == "less") {
    pval <- pnorm(z)
  }
  else if (alternative == "greater") {
    pval <- pnorm(z, lower.tail = FALSE)
  }
  else {
    pval <- 2 * pnorm(-abs(z))
  }
  names(VN) <- "VN"
  method <- "Von Neumann Successive Difference Test"

  rval <- list(statistic = c(VN, z=z), p.value = pval,
               method = method,
               alternative = alternative, data.name = dname,
               z = z)

  class(rval) <- "htest"
  return(rval)

}





BartelsRankTest <- function(x, alternative = c("two.sided", "trend", "oscillation"),
                            method = c("normal", "beta", "auto")) {

  # Performs Bartels Ratio Test for Randomness.
  #
  # Args:
  #   x: a numeric vector containing the data.
  #   alternative hypothesis, must be one of "two.sided" (default), "left.sided" or "right.sided"
  #   pv.method: asymptotic aproximation method used to compute the p-value.
  #
  # Returns:
  #   statistic: the value of the RVN statistic test and the theoretical mean value and variance of the RVN statistic test.
  #   n: the sample size, after the remotion of consecutive duplicate values.
  #   p.value: the asymptotic p-value.
  #   method: a character string indicating the test performed.
  #   data.name: a character string giving the name of the data.
  #   alternative: a character string describing the alternative.

  pvalue <- match.arg(method, c("normal", "beta", "auto"))

  dname <- deparse(substitute(x))

  # Remove NAs
  x <- na.omit(x)
  stopifnot(is.numeric(x))
  n <- length(x)


  # if (alternative == "t"){alternative <- "two.sided"}
  # if (alternative == "l"){alternative <- "left.sided"}
  # if (alternative == "r"){alternative <- "right.sided"}
  # if (alternative != "two.sided" & alternative != "left.sided" & alternative != "right.sided")
  # {stop("must give a valid alternative")}

  alternative <- match.arg(alternative)

  if (n < 10){stop("sample size must be greater than 9")}

  # unique
  rk <- rank(x)
  d <- diff(rk)
  d.rank <- sum(rk^2) - n * (mean(rk)^2)
  RVN <- sum(d^2) / d.rank
  mu <- 2
  vr <- (4*(n-2)*(5*n^2-2*n-9))/(5*n*(n+1)*(n-1)^2)

  # Computes the p-value
  if (pvalue == "auto"){
    pvalue <- ifelse(n <= 100, "beta", "normal")
  }

  if (pvalue == "beta"){
    btp <- (5*n*(n+1)*(n-1)^2)/(2*(n-2)*(5*n^2-2*n-9))-1/2
    pv0 <- pbeta(RVN/4, shape1=btp, shape2=btp)
  }
  if (pvalue=="normal"){
    pv0 <- pnorm((RVN - mu) / sqrt(vr))
  }

  if (alternative=="two.sided"){
    pv <- 2 * min(pv0, 1 - pv0)
    alternative <- "nonrandomness"
  }
  if (alternative == "trend"){
    pv <- pv0
    alternative <- "trend"
  }
  if (alternative == "oscillation"){
    pv <- 1 - pv0
    alternative <- "systematic oscillation"
  }

  test <- (RVN - mu) / sqrt(vr)
  rval <- list(statistic = c(RVN=RVN, z=test), nm=sum(d^2), rvn=RVN, mu=mu, var=vr, p.value = pv,
               method = "Bartels Ratio Test", data.name = dname, parameter=c(n=n), n=n, alternative=alternative)
  class(rval) <- "htest"
  return(rval)

}



MosesTest <- function (x, ...)  UseMethod("MosesTest")

# Extremreaktionen nach Moses: Nullhypothese: Die Spannweite der Werte ist
# in beiden Gruppen gleich gross. Die Werte beider Gruppen werden in eine gemeinsame
# Reihenfolge gebracht. Anschliessend werden ihnen Rangwerte zugewiesen.
# Eine der Gruppen (die Gruppe des Wertes, der in dem Dialogfeld
#                   Gruppen definieren als erstes angegeben ist) wird als Kontrollgruppe verwendet.
# Fuer diese Gruppe wird die Spannweite der Raenge als Differenz zwischen
# dem groessten und kleinsten Rangwert berechnet. Anhand dieser Spannweite errechnet
# sich die einseitige Signifikanz. Zusaetzlich wird der Test ein zweites
# Mal durchgefuehrt, wobei die Ausreisser der Gesamtstichprobe ausgeschlossen
# werden (insgesamt 5% der Faelle). Dabei werden sowohl die hoechsten als auch
# die niedrigsten Raenge entfernt. Das Testergebnis teilt die Anzahl der Faelle beider
# Gruppen, die Spannweiten und die entsprechenden einseitigen Signifikanzen
# fuer beide Tests (mit und ohne Ausreisser) mit. Fuer ein Beispiel siehe oben
# Abschnitt Moses-Test, S. 760.


MosesTest.formula <- function (formula, data, subset, na.action, ...) {

  # this is a taken analogue to wilcox.test.formula

  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- as.name("model.frame")
  m$... <- NULL
  mf <- eval(m, parent.frame())
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  g <- factor(mf[[-response]])
  if (nlevels(g) != 2L)
    stop("grouping factor must have exactly 2 levels")
  DATA <- split(mf[[response]], g)
  names(DATA) <- c("x", "y")
  y <- DoCall("MosesTest", c(DATA, list(...)))
  y$data.name <- DNAME
  y

}



MosesTest.default <- function(x, y, extreme = NULL, ...){

  # example
  # x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
  # y <- c(1.15, 0.88, 0.90, 0.74, 1.21)
  # MosesTest(y, x)

  if(is.null(extreme)) extreme <- pmax(floor(0.05 * length(x)), 1)
  h <- extreme
  if(2*h > length(x)-2) h <- floor((length(x)-2)/2)

  # no alternative for the moses.test
  DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))

  nk <- length(x)
  ne <- length(y)
  # decreasing ranks following SPSS-calculation
  R1 <- rank(-c(x, y))[1:nk]
  R1 <- sort(R1)[(h+1):(length(R1)-h)]

  S <- ceiling(max(R1) - min(R1) + 1)

  tmp <- 0
  for( i in 0 : (S - nk + 2*h)) {
    tmp <- tmp + choose(i + nk - 2*h - 2, i) * choose(ne + 2*h + 1 - i, ne - i)
  }

  PVAL <- (tmp / choose(nk + ne, nk))

  structure(list(statistic = c(S = S),
                 p.value = PVAL,
                 method = "Moses Test of Extreme Reactions",
                 alternative = "extreme values are more likely in x than in y",
                 data.name = DNAME),
            class = "htest")

}



SiegelTukeyTest <- function (x, ...)  UseMethod("SiegelTukeyTest")

SiegelTukeyTest.formula <- function (formula, data, subset, na.action, ...)
{
  # this is a taken analogue to wilcox.test.formula

  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- as.name("model.frame")
  m$... <- NULL
  mf <- eval(m, parent.frame())
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  g <- factor(mf[[-response]])
  if (nlevels(g) != 2L)
    stop("grouping factor must have exactly 2 levels")
  DATA <- split(mf[[response]], g)
  names(DATA) <- c("x", "y")
  y <- DoCall("SiegelTukeyTest", c(DATA, list(...)))
  y$data.name <- DNAME
  y

}



SiegelTukeyRank <- function(x, g, drop.median = TRUE) {

  # they do not drop the median in:
  # http://en.wikipedia.org/wiki/Siegel%E2%80%93Tukey_test
  # A <- c(33,62,84,85,88,93,97); B <- c(4,16,48,51,66,98)
  # this is wrong there, as the author did not leave the median out

  ord.x <- order(x, g)
  sort.x <- x[ord.x]
  sort.id <- g[ord.x]

  n <- length(x)
  if(drop.median){
    if(n %% 2 > 0) {
      # gonna have to drop the (first) median value
      # as we sorted by the groupsize, this will be the one out of the bigger group (if existing)
      fm <- which( sort.x == median(sort.x))[1]
      sort.x <- sort.x[-fm]
      sort.id <- sort.id[-fm]
      n <- n-1
    }
  }

  base1 <- c(1, 4)
  iterator1 <- matrix(seq(from = 1, to = n, by = 4)) - 1
  rank1 <- apply(iterator1, 1, function(x) x + base1)

  iterator2 <- matrix(seq(from = 2, to = n, by = 4))
  base2 <- c(0, 1)
  rank2 <- apply(iterator2, 1, function(x) x + base2)

  if (length(rank1) == length(rank2)) {
    rank <- c(rank1[1:floor(n/2)], rev(rank2[1:ceiling(n/2)]))
  } else {
    rank <- c(rank1[1:ceiling(n/2)], rev(rank2[1:floor(n/2)]))
  }

  unique.ranks <- tapply(rank, sort.x, mean)
  unique.x <- as.numeric(as.character(names(unique.ranks)))

  ST.matrix <- merge(
    data.frame(sort.x, sort.id),          # this are the original values in x-order
    data.frame(unique.x, unique.ranks),   # this is the rank.matrix
    by.x = "sort.x", by.y = "unique.x")

  return(ST.matrix)
}


SiegelTukeyTest.default <- function(x, y, adjust.median = FALSE,
                                    alternative = c("two.sided","less","greater"), mu = 0,
                                    exact = NULL, correct = TRUE, conf.int = FALSE, conf.level = 0.95, ...) {
  ###### published on:
  #   http://www.r-statistics.com/2010/02/siegel-tukey-a-non-parametric-test-for-equality-in-variability-r-code/
  #   Main author of the function:  Daniel Malter

  # Doku: http://www.crcnetbase.com/doi/abs/10.1201/9781420036268.ch14


  if (!missing(mu) && ((length(mu) > 1L) || !is.finite(mu)))
    stop("'mu' must be a single number")

  if (conf.int) {
    if (!((length(conf.level) == 1L) && is.finite(conf.level) &&
          (conf.level > 0) && (conf.level < 1)))
      stop("'conf.level' must be a single number between 0 and 1")
  }

  if (!is.numeric(x))
    stop("'x' must be numeric")

  if (!is.null(y)) {
    if (!is.numeric(y))
      stop("'y' must be numeric")
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    x <- x[is.finite(x)]
    y <- y[is.finite(y)]
  }
  else {
    DNAME <- deparse(substitute(x))
    x <- x[is.finite(x)]
  }

  # adjusting median
  if (adjust.median) {
    x <- x - median(x)
    y <- y - median(y)
  }

  # the larger group comes first
  if( length(x) > length(y) ){
    xx <- c(x, y)
    id <- c(rep(0, length(x)), rep(1, length(y)))
  } else {
    xx <- c(y,x)
    id <- c(rep(0, length(y)), rep(1, length(x)))
  }

  strank <- SiegelTukeyRank(xx, g = id)
  ranks0 <- strank$unique.ranks[strank$sort.id == 0]
  ranks1 <- strank$unique.ranks[strank$sort.id == 1]

  RVAL <- wilcox.test(ranks0, ranks1, alternative = alternative,
                      mu = mu, paired = FALSE, exact = exact, correct = correct,
                      conf.int = conf.int, conf.level = conf.level)

  RVAL$statistic <- sum(ranks1)
  names(RVAL$statistic)  <- "ST"
  RVAL$data.name <- DNAME
  RVAL <- c(RVAL, list(stranks = strank, MeanRanks = c(mean(ranks0), mean(ranks1))))
  RVAL$method <- "Siegel-Tukey-test for equal variability"
  RVAL$null.value <- 1
  names(RVAL$null.value) <- "ratio of scales"
  class(RVAL) <- "htest"
  return(RVAL)

  if(suppressWarnings(wilcox.test(x,y)$p.value) < 0.05) warning("SiegelTukeyTest: wilcox.test(x, y) is significant! Consider setting adjust.median = TRUE." )

}




JonckheereTerpstraTest <- function (x, ...)  UseMethod("JonckheereTerpstraTest")

JonckheereTerpstraTest.formula <- function (formula, data, subset, na.action, ...) {

  if (missing(formula) || (length(formula) != 3L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- as.name("model.frame")
  mf <- eval(m, parent.frame())
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  y <- DoCall("JonckheereTerpstraTest", as.list(mf))
  y$data.name <- DNAME
  y
}

JonckheereTerpstraTest.default <- function (x, g, alternative = c("two.sided", "increasing", "decreasing"), nperm=NULL, ...) {

  if (is.list(x)) {
    if (length(x) < 2L)
      stop("'x' must be a list with at least 2 elements")
    DNAME <- deparse(substitute(x))
    x <- lapply(x, function(u) u <- u[complete.cases(u)])
    k <- length(x)
    l <- sapply(x, "length")
    if (any(l == 0))
      stop("all groups must contain data")
    g <- factor(rep(1:k, l))
    x <- unlist(x)
  }
  else {
    if (length(x) != length(g))
      stop("'x' and 'g' must have the same length")
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
    OK <- complete.cases(x, g)
    x <- x[OK]
    g <- g[OK]
    if (!all(is.finite(g)))
      stop("all group levels must be finite")
    g <- factor(g)
    k <- nlevels(g)
    if (k < 2)
      stop("all observations are in the same group")
  }
  n <- length(x)
  if (n < 2)
    stop("not enough observations")

  # start calculating

  jtpdf <- function(gsize) {
    ng <- length(gsize)
    cgsize <- rev(cumsum(rev(gsize)))
    mxsum <- sum(gsize[-ng]*cgsize[-1]) + 1
    zz <- .Fortran("jtpdf",
                   as.integer(mxsum),
                   pdf=double(mxsum),
                   as.integer(ng),
                   as.integer(cgsize),
                   double(mxsum),
                   double(mxsum))
    zz$pdf
  }

  jtperm.p <- function(x, ng, gsize, cgsize, alternative, nperm) {
    # this function computes the pdf using the convolution by Mark van de Wiel

    n <- length(x)
    pjtrsum <- rep(0, nperm)
    for (np in 1:nperm){
      jtrsum <- 0
      for(i in 1L:(ng-1)) {
        na <- gsize[i]
        nb <- n-cgsize[i+1]
        # this jtrsum will be small if data are increasing and large if decreasing
        jtrsum <- jtrsum + sum(rank(x[(cgsize[i]+1):n])[1:na]) - na*(na+1)/2
      }
      pjtrsum[np] <- jtrsum
      # permute the data; this way the first value is the original statistic
      x <- sample(x)
    }
    # one-sided p-values
    # number of permuted values at least as small as original
    iPVAL <- sum(pjtrsum <= pjtrsum[1])/nperm
    # number of permuted values at least as large as original
    dPVAL <- sum(pjtrsum >= pjtrsum[1])/nperm
    # return p-value for the alternative of interest
    PVAL <- switch(alternative,
                   "two.sided" = 2*min(iPVAL, dPVAL, 1),
                   "increasing" = iPVAL,
                   "decreasing" = dPVAL)
    PVAL
  }


  # Alternative for the JT-Statistic
  # JT <- function(z){
  #
  #   w <- function(x, y){
  #     # verbatim from wilcox.test STATISTIC
  #     r <- rank(c(x, y))
  #     n.x <- as.double(length(x))
  #     n.y <- as.double(length(y))
  #     STATISTIC <- c(W = sum(r[seq_along(x)]) - n.x * (n.x + 1)/2)
  #     STATISTIC
  #   }
  #
  #   k <- length(z)
  #   u <- 0
  #
  #   for(i in 2:k){
  #     for(j in 1:(i-1))	{
  #       u <- u + w(z[[i]], z[[j]])
  #     } }
  #   u
  # }


  # see:
  #   library(NSM3)
  #   HOllander Wolfe pp 218
  #   piece <- c(40,35,38,43,44,41, 38,40,47,44,40,42, 48,40,45,43,46,44)
  #   grp <- factor(rep(c("ctrl","A","B"), each=6), ordered=TRUE, levels=c("ctrl","A","B"))
  #
  #   JonckheereTerpstraTest(piece, grp)
  #   pJCK(piece, grp)


  if(!is.numeric(x)) stop("data values should be numeric")
  if(!is.numeric(g) & !is.ordered(g)) stop("group should be numeric or ordered factor")
  alternative <- match.arg(alternative)
  METHOD <- "Jonckheere-Terpstra test"
  PERM <- !missing(nperm)
  n <- length(x)
  if(length(g) != n) stop("lengths of data values and group don't match")
  TIES <- length(unique(x)) != n
  gsize <- table(g)
  ng <- length(gsize)
  cgsize <- c(0,cumsum(gsize))
  x <- x[order(g)]
  jtrsum <- jtmean <- jtvar <- 0
  for(i in 1:(ng-1)) {
    na <- gsize[i]
    nb <- n-cgsize[i+1]
    jtrsum <- jtrsum + sum(rank(x[(cgsize[i]+1):n])[1:na]) - na*(na+1)/2
    jtmean <- jtmean + na*nb/2
    jtvar <- jtvar + na*nb*(na+nb+1)/12
  }
  # this jtrsum will be small if data are increasing and large if decreasing
  # to reverse this use 2*jtmean - jtrsum
  jtrsum <- 2*jtmean - jtrsum
  STATISTIC <- jtrsum
  names(STATISTIC) <- "JT"
  if (PERM) {
    PVAL <- jtperm.p(x, ng, gsize, cgsize, alternative, nperm)
  } else {
    if (n > 100 | TIES) {
      warning("Sample size > 100 or data with ties \n p-value based on normal approximation. Specify nperm for permutation p-value")
      zstat <- (STATISTIC-jtmean)/sqrt(jtvar)
      PVAL <- pnorm(zstat)
      PVAL <- switch(alternative,
                     "two.sided" = 2*min(PVAL, 1-PVAL, 1),
                     "increasing" = 1-PVAL,
                     "decreasing" = PVAL)
    } else {
      dPVAL <- sum(jtpdf(gsize)[1:(jtrsum+1)])
      iPVAL <- 1-sum(jtpdf(gsize)[1:(jtrsum)])
      PVAL <- switch(alternative,
                     "two.sided" = 2*min(iPVAL, dPVAL, 1),
                     "increasing" = iPVAL,
                     "decreasing" = dPVAL)
    }
  }

  RVAL <- list(statistic = STATISTIC,
               p.value = as.numeric(PVAL),
               alternative = alternative,
               method = METHOD,
               data.name = DNAME)
  class(RVAL) <- "htest"
  RVAL

}



# ***********************************
# Tests aus library(nortest)

ShapiroFranciaTest <- function (x) {

  DNAME <- deparse(substitute(x))
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  if ((n < 5 || n > 5000))
    stop("sample size must be between 5 and 5000")
  y <- qnorm(ppoints(n, a = 3/8))
  W <- cor(x, y)^2
  u <- log(n)
  v <- log(u)
  mu <- -1.2725 + 1.0521 * (v - u)
  sig <- 1.0308 - 0.26758 * (v + 2/u)
  z <- (log(1 - W) - mu)/sig
  pval <- pnorm(z, lower.tail = FALSE)
  RVAL <- list(statistic = c(W = W), p.value = pval, method = "Shapiro-Francia normality test",
               data.name = DNAME)
  class(RVAL) <- "htest"

  return(RVAL)

}


PearsonTest <- function (x, n.classes = ceiling(2 * (n^(2/5))), adjust = TRUE) {

  DNAME <- deparse(substitute(x))
  x <- x[complete.cases(x)]
  n <- length(x)
  if (adjust) {
    dfd <- 2
  }
  else {
    dfd <- 0
  }
  num <- floor(1 + n.classes * pnorm(x, mean(x), sd(x)))
  count <- tabulate(num, n.classes)
  prob <- rep(1/n.classes, n.classes)
  xpec <- n * prob
  h <- ((count - xpec)^2)/xpec
  P <- sum(h)
  pvalue <- pchisq(P, n.classes - dfd - 1, lower.tail = FALSE)
  RVAL <- list(statistic = c(P = P), p.value = pvalue, method = "Pearson chi-square normality test",
               data.name = DNAME, n.classes = n.classes, df = n.classes -
                 1 - dfd)
  class(RVAL) <- "htest"
  return(RVAL)
}


LillieTest <- function (x) {

  DNAME <- deparse(substitute(x))
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  if (n < 5)
    stop("sample size must be greater than 4")
  p <- pnorm((x - mean(x))/sd(x))
  Dplus <- max(seq(1:n)/n - p)
  Dminus <- max(p - (seq(1:n) - 1)/n)
  K <- max(Dplus, Dminus)
  if (n <= 100) {
    Kd <- K
    nd <- n
  }
  else {
    Kd <- K * ((n/100)^0.49)
    nd <- 100
  }
  pvalue <- exp(-7.01256 * Kd^2 * (nd + 2.78019) + 2.99587 *
                  Kd * sqrt(nd + 2.78019) - 0.122119 + 0.974598/sqrt(nd) +
                  1.67997/nd)
  if (pvalue > 0.1) {
    KK <- (sqrt(n) - 0.01 + 0.85/sqrt(n)) * K
    if (KK <= 0.302) {
      pvalue <- 1
    }
    else if (KK <= 0.5) {
      pvalue <- 2.76773 - 19.828315 * KK + 80.709644 *
        KK^2 - 138.55152 * KK^3 + 81.218052 * KK^4
    }
    else if (KK <= 0.9) {
      pvalue <- -4.901232 + 40.662806 * KK - 97.490286 *
        KK^2 + 94.029866 * KK^3 - 32.355711 * KK^4
    }
    else if (KK <= 1.31) {
      pvalue <- 6.198765 - 19.558097 * KK + 23.186922 *
        KK^2 - 12.234627 * KK^3 + 2.423045 * KK^4
    }
    else {
      pvalue <- 0
    }
  }
  RVAL <- list(statistic = c(D = K), p.value = pvalue, method = "Lilliefors (Kolmogorov-Smirnov) normality test",
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}


CramerVonMisesTest <- function (x) {
  DNAME <- deparse(substitute(x))
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  if (n < 8)
    stop("sample size must be greater than 7")
  p <- pnorm((x - mean(x))/sd(x))
  W <- (1/(12 * n) +
          sum(
            (p - (2 * seq(1:n) - 1)/(2 * n))^2
          ))
  WW <- (1 + 0.5/n) * W
  if (WW < 0.0275) {
    pval <- 1 - exp(-13.953 + 775.5 * WW - 12542.61 * WW^2)
  }
  else if (WW < 0.051) {
    pval <- 1 - exp(-5.903 + 179.546 * WW - 1515.29 * WW^2)
  }
  else if (WW < 0.092) {
    pval <- exp(0.886 - 31.62 * WW + 10.897 * WW^2)
  }
  else if (WW < 1.1) {
    pval <- exp(1.111 - 34.242 * WW + 12.832 * WW^2)
  }
  else {
    warning("p-value is smaller than 7.37e-10, cannot be computed more accurately")
    pval <- 7.37e-10
  }
  RVAL <- list(statistic = c(W = W), p.value = pval, method = "Cramer-von Mises normality test",
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}


#
# AndersonDarlingTest <- function (x) {
#
#     DNAME <- deparse(substitute(x))
#     x <- sort(x[complete.cases(x)])
#     n <- length(x)
#     if (n < 8)
#         stop("sample size must be greater than 7")
#     p <- pnorm((x - mean(x))/sd(x))
#     h <- (2 * seq(1:n) - 1) * (log(p) + log(1 - rev(p)))
#     A <- -n - mean(h)
#     AA <- (1 + 0.75/n + 2.25/n^2) * A
#
#     if (AA < 0.2) {
#         pval <- 1 - exp(-13.436 + 101.14 * AA - 223.73 * AA^2)
#     }
#     else if (AA < 0.34) {
#         pval <- 1 - exp(-8.318 + 42.796 * AA - 59.938 * AA^2)
#     }
#     else if (AA < 0.6) {
#         pval <- exp(0.9177 - 4.279 * AA - 1.38 * AA^2)
#     }
#     else if (AA < 160) {
#         pval <- exp(1.2937 - 5.709 * AA + 0.0186 * AA^2)
#     }
#     else {
#       pval <-0
#     }
#       RVAL <- list(statistic = c(A = A), p.value = pval, method = "Anderson-Darling normality test",
#         data.name = DNAME)
#     class(RVAL) <- "htest"
#     return(RVAL)
# }


##
## andarl.R
##
##  Anderson-Darling test and null distribution
##
## $Revision: 1.6 $ $Date: 2014/06/24 02:12:20 $
##

AndersonDarlingTest <- function(x, null="punif", ..., nullname) {

  .recogniseCdf <- function(s="punif") {
    if(!is.character(s) || length(s) != 1) return(NULL)
    if(nchar(s) <= 1 || substr(s,1,1) != "p") return(NULL)
    root <- substr(s, 2, nchar(s))
    a <- switch(root,
                beta     = "beta",
                binom    = "binomial",
                birthday = "birthday coincidence",
                cauchy   = "Cauchy",
                chisq    = "chi-squared",
                exp      = "exponential",
                f        = "F",
                gamma    = "Gamma",
                geom     = "geometric",
                hyper    = "hypergeometric",
                lnorm    = "log-normal",
                logis    = "logistic",
                nbinom   = "negative binomial",
                norm     = "Normal",
                pois     = "Poisson",
                t        = "Student's t",
                tukey    = "Tukey (Studentized range)",
                unif     = "uniform",
                weibull  = "Weibull",
                NULL)
    if(!is.null(a))
      return(paste(a, "distribution"))
    b <- switch(root,
                AD     = "Anderson-Darling",
                CvM    = "Cramer-von Mises",
                wilcox = "Wilcoxon Rank Sum",
                NULL)
    if(!is.null(b))
      return(paste("null distribution of", b, "Test Statistic"))
    return(NULL)
  }


  xname <- deparse(substitute(x))
  nulltext <- deparse(substitute(null))
  if(is.character(null)) nulltext <- null
  if(missing(nullname) || is.null(nullname)) {
    reco <- .recogniseCdf(nulltext)
    nullname <- if(!is.null(reco)) reco else
      paste("distribution", sQuote(nulltext))
  }
  stopifnot(is.numeric(x))
  x <- as.vector(x)
  n <- length(x)
  F0 <- if(is.function(null)) null else
    if(is.character(null)) get(null, mode="function") else
      stop("Argument 'null' should be a function, or the name of a function")
  U <- F0(x, ...)
  if(any(U < 0 | U > 1))
    stop("null distribution function returned values outside [0,1]")
  U <- sort(U)
  k <- seq_len(n)
  ## call Marsaglia C code
  z <- .C("ADtestR",
          x = as.double(U),
          n = as.integer(n),
          adstat = as.double(numeric(1)),
          pvalue = as.double(numeric(1))
  )
  STATISTIC <- z$adstat
  names(STATISTIC) <- "An"
  PVAL <- z$pvalue
  METHOD <- c("Anderson-Darling test of goodness-of-fit",
              paste("Null hypothesis:", nullname))
  extras <- list(...)
  parnames <- intersect(names(extras), names(formals(F0)))
  if(length(parnames) > 0) {
    pars <- extras[parnames]
    pard <- character(0)
    for(i in seq_along(parnames))
      pard[i] <- paste(parnames[i], "=", paste(Format(pars[[i]], digits=DescToolsOptions("digits")), collapse=" "))
    pard <- paste("with",
                  ngettext(length(pard), "parameter", "parameters"),
                  "  ",
                  paste(pard, collapse=", "))
    METHOD <- c(METHOD, pard)
  }
  out <- list(statistic = STATISTIC,
              p.value = PVAL,
              method = METHOD,
              data.name = xname)
  class(out) <- "htest"
  return(out)
}

.pAD <- function(q, n=Inf, lower.tail=TRUE, fast=TRUE) {
  q <- as.numeric(q)
  p <- rep(NA_real_, length(q))
  if(any(ones <- is.infinite(q) & (q == Inf)))
    p[ones] <- 1
  if(any(zeroes <- (is.finite(q) & q <= 0) | (is.infinite(q) & (q == -Inf))))
    p[zeroes] <- 0
  ok <- is.finite(q) & (q > 0)
  nok <- sum(ok)
  if(nok > 0) {
    if(is.finite(n)) {
      z <- .C("ADprobN",
              a       = as.double(q[ok]),
              na      = as.integer(nok),
              nsample = as.integer(n),
              prob    = as.double(numeric(nok))
      )
      p[ok] <- z$prob
    } else if(fast) {
      ## fast version adinf()
      z <- .C("ADprobApproxInf",
              a    = as.double(q[ok]),
              na   = as.integer(nok),
              prob = as.double(numeric(nok))
      )
      p[ok] <- z$prob
    } else {
      ## slow, accurate version ADinf()
      z <- .C("ADprobExactInf",
              a    = as.double(q[ok]),
              na   = as.integer(nok),
              prob = as.double(numeric(nok))
      )
      p[ok] <- z$prob
    }

  }
  if(!lower.tail)
    p <- 1 - p
  return(p)
}


# .qAD <- local({
#
#   f <- function(x, N, P, Fast) {
#     .pAD(x, N, fast=Fast) - P
#   }
#
#   .qAD <- function(p, n=Inf, lower.tail=TRUE, fast=TRUE) {
#     ## quantiles of null distribution of Anderson-Darling test statistic
#     stopifnot(all(p >= 0))
#     stopifnot(all(p <= 1))
#     if(!lower.tail) p <- 1-p
#     ans <- rep(NA_real_, length(p))
#     for(i in which(p >= 0 & p < 1))
#       ans[i] <- uniroot(f, c(0, 1), N=n, P=p[i], Fast=fast, extendInt="up")$root
#     return(ans)
#   }
#
#   .qAD
# })
#
#
#




# ***********************************
# Tests aus library(tseries)
#
# JarqueBeraTest <- function(x, robust=TRUE, na.rm=FALSE) {
#
#   # Author: Adrian Trapletti
#
#   if(NCOL(x) > 1)
#       stop("x is not a vector or univariate time series")
#
#   if(na.rm) x <- na.omit(x)
#
#   DNAME <- deparse(substitute(x))
#   n <- length(x)
#   m1 <- sum(x)/n
#   m2 <- sum((x-m1)^2)/n
#   m3 <- sum((x-m1)^3)/n
#   m4 <- sum((x-m1)^4)/n
#   b1 <- (m3/m2^(3/2))^2
#   b2 <- (m4/m2^2)
#   STATISTIC <- n * b1 / 6 + n * (b2 - 3)^2 / 24
#   names(STATISTIC) <- "X-squared"
#   PARAMETER <- 2
#   names(PARAMETER) <- "df"
#   PVAL <- 1 - pchisq(STATISTIC,df = 2)
#   METHOD <- "Jarque Bera Test"
#   structure(list(statistic = STATISTIC,
#                  parameter = PARAMETER,
#                  p.value = PVAL,
#                  method = METHOD,
#                  data.name = DNAME),
#             class = "htest")
# }
#
#



JarqueBeraTest <- function (x, robust=TRUE, method=c("chisq", "mc"), N=0, na.rm=FALSE) {

  method <- match.arg(method)

  if (NCOL(x) > 1){ stop("x is not a vector or univariate time series") }
  if(na.rm) x <- na.omit(x)

  if ((method == "mc") & (N==0)) {
    stop("number of Monte Carlo simulations N should be provided for the empirical critical values")
  }

  DNAME <- deparse(substitute(x))

  ## Calculate the first 4 central moments
  n <- length(x)
  m1 <- sum(x)/n
  m2 <- sum((x - m1)^2)/n
  m3 <- sum((x - m1)^3)/n
  m4 <- sum((x - m1)^4)/n

  ## User can choose the Standard Jarque Bera Test or Robust Jarque Bera Test
  ## Robust Jarque Bera Test is default
  if(!robust) {
    b1 <- (m3/m2^(3/2))^2;
    b2 <- (m4/m2^2);
    statistic <- n * b1/6 + n * (b2 - 3)^2/24

  } else {
    J <- sqrt(pi/2) * mean(abs(x-median(x)))
    J2 <- J^2
    b1 <- (m3/(J2)^(3/2))^2
    b2 <- (m4/(J2)^2)
    vk<-64/n
    vs<-6/n
    ek<-3
    statistic <- b1/vs + (b2 - ek)^2/vk

  }

  if(method == "mc"){
    if(!robust) {
      ## computes empirical critical values for the JB statistic

      jb<-double(N)

      for (k in 1:N) {
        e <- rnorm(length(x), mean=0, sd = sqrt(1))
        m1 <- sum(e)/n
        m2 <- sum((e - m1)^2)/n
        m3 <- sum((e - m1)^3)/n
        m4 <- sum((e - m1)^4)/n
        b1 <- (m3/m2^(3/2))^2
        b2 <- (m4/m2^2)
        vk <- 24/n
        vs <- 6/n
        ek <- 3
        jb[k] <- b1/vs + (b2 - ek)^2/vk
      }

      y <- sort(jb)

      if (statistic >= max(y)) {
        p.value <- 0

      } else if (statistic<=min(y)) {
        p.value <- 1

      } else {
        bn <- which(y==min(y[I(y>=statistic)]))
        an <- which(y==max(y[I(y<statistic)]))
        a <- max(y[I(y<statistic)])
        b <- min(y[I(y>=statistic)])
        pa <- (an - 1) / (N - 1)
        pb <- (bn - 1) / (N - 1)
        alpha <- (statistic-a)/(b-a)
        p.value <- 1-alpha*pb-(1-alpha)*pa
      }

    } else {
      ## computes empirical critical values for the RJB statistic
      rjb <- double(N)

      for (k in 1:N) {
        e <- rnorm(length(x), mean=0, sd = sqrt(1))
        J <- sqrt(pi/2)*mean(abs(e-median(e)))
        J2 <- J^2
        m1 <- sum(e)/n
        m2 <- sum((e - m1)^2)/n
        m3 <- sum((e - m1)^3)/n
        m4 <- sum((e - m1)^4)/n
        b1 <- (m3/(J2)^(3/2))^2
        b2 <- (m4/(J2)^2)
        vk <- 64/n
        vs <- 6/n
        ek <- 3
        rjb[k] <- b1/vs + (b2 - ek)^2/vk
      }

      y <- sort(rjb)

      if (statistic >= max(y)) {
        p.value <- 0

      } else if (statistic <= min(y)) {
        p.value <- 1

      } else {
        bn <- which(y==min(y[I(y>=statistic)]))
        an <- which(y==max(y[I(y<statistic)]))
        a <- max(y[I(y<statistic)])
        b <- min(y[I(y>=statistic)])
        pa <- (an - 1) / (N - 1)
        pb <- (bn - 1) / (N - 1)
        alpha <- (statistic-a)/(b-a)
        p.value <- 1-alpha*pb-(1-alpha)*pa
      }
    }

  } else {
    p.value <- 1 - pchisq(statistic, df = 2)
  }

  METHOD <- ifelse(!robust, "Jarque Bera Test", "Robust Jarque Bera Test")
  STATISTIC=statistic
  names(STATISTIC) <- "X-squared"
  PARAMETER <- 2
  names(PARAMETER) <- "df"

  structure(list(statistic = STATISTIC, parameter = PARAMETER,
                 p.value = p.value, method = METHOD, data.name = DNAME),
            class = "htest")

}




# PageTest <- function(x) {
#
#   DNAME <- deparse(substitute(x))
#   x <- x[complete.cases(x),]
#
#   rnksum <- apply(apply(x, 1, rank),1, sum)
#   L <- sum(seq_along(rnksum) * rnksum)
#   nc <- ncol(x)
#   nr <- nrow(x)
#   mu <- nr * nc * (nc+1)^2 / 4
#   sig <- nr * nc^2 * (nc+1)^2*(nc-1) / 144
#   z <- (L - mu)/sqrt(sig)
#
#   pval <- pnorm(z, lower.tail = FALSE)
#   RVAL <- list(statistic = c(L = L), p.value = pval, method = "Page test for ordered alternatives",
#       data.name = DNAME)
#   class(RVAL) <- "htest"
#   return(RVAL)
#
# }



# PageTest<-function(x) {

# ### Alternative: package coin
# ### independence_test(scores ~ product | sitting, data = egg_data,
# ### scores = list(product = 1:10),
# ### ytrafo = yt)

# ### http://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/pagesL


# if(missing(x))
# stop("Usage: PageTest(x)\n\twhere x is a matrix of ranks")

# dname <- deparse(substitute(x))

# dimx <- dim(x)

# ### This one only requires two dimensions
# page.crit3 <- array(
# c(28,41,54,66,79,91,104,116,128,141,153,165,178,190,202,215,227,239,251,
# NA,42,55,68,81,93,106,119,131,144,156,169,181,194,206,218,231,243,256,
# NA,NA,56,70,83,96,109,121,134,147,160,172,185,197,210,223,235,248,260),
# c(19,3)
# )

# ### the rest require three
# page.crit4plus <- array(
# c(58,84,111,137,163,189,214,240,266,292,317,
# 103,150,197,244,291,338,384,431,477,523,570,
# 166,244,321,397,474,550,625,701,777,852,928,
# 252,370,487,603,719,835,950,1065,1180,1295,1410,
# 362,532,701,869,1037,1204,1371,1537,1703,1868,2035,
# 500,736,971,1204,1436,1668,1900,2131,2361,2592,2822,
# 670,987,1301,1614,1927,2238,2549,2859,3169,3478,3788,
# 60,87,114,141,167,193,220,246,272,298,324,
# 106,155,204,251,299,346,393,441,487,534,581,
# 173,252,331,409,486,563,640,717,793,869,946,
# 261,382,501,620,737,855,972,1088,1205,1321,1437,
# 376,549,722,893,1063,1232,1401,1569,1736,1905,2072,
# 520,761,999,1236,1472,1706,1940,2174,2407,2639,2872,
# 696,1019,1339,1656,1972,2288,2602,2915,3228,3541,3852,
# NA,89,117,145,172,198,225,252,278,305,331,
# 109,160,210,259,307,355,403,451,499,546,593,
# 178,260,341,420,499,577,655,733,811,888,965,
# 269,394,516,637,757,876,994,1113,1230,1348,1465,
# 388,567,743,917,1090,1262,1433,1603,1773,1943,2112,
# 544,790,1032,1273,1512,1750,1987,2223,2459,2694,2929,
# 726,1056,1382,1704,2025,2344,2662,2980,3296,3612,3927),
# c(11,7,3)
# )

# mean.ranks <- apply(x, 2, mean)
# Lval <- NA
# p.table <- NA
# L <- sum(apply(x, 2, sum) * 1:dimx[2])

# if((dimx[1] > 1 && dimx[1] < 13) && (dimx[2] > 3 && dimx[2] < 11))
# Lval <- page.crit4plus[dimx[1]-1,dimx[2]-3,]

# if((dimx[1] > 1 && dimx[1] < 21) && dimx[2] == 3)
# Lval <- page.crit3[dimx[1]-1,]

# p.table <-
# ifelse(L > Lval[1],ifelse(L > Lval[2],ifelse(L > Lval[3],"<=.001","<=.01"),"<=.05"),"NS")
# #### print(Lval)

# ### if there was no tabled value, calculate the normal approximation
# if(length(Lval)<2) {
# munum <- dimx[1]*dimx[2]*(dimx[2]+1)*(dimx[2]+1)
# muL <- munum/4
# cat("muL =",muL,"\n")
# sigmaL <- (dimx[1]*dimx[2]*dimx[2]*(dimx[2]*dimx[2]-1)*(dimx[2]*dimx[2]-1))/
# (144*(dimx[2]-1))
# cat("sigmaL =",sigmaL,"\n")
# zL <- ((12*L-3*munum)/(dimx[2]*(dimx[2]-1)))*sqrt((dimx[2]-1)/dimx[1])
# pZ <- pnorm(zL,lower.tail=FALSE)
# } else {
# zL <- NA
# pZ <- NA
# }

# #### ptt <- list(ranks=x, mean.ranks=mean.ranks, L=L, p.table=p.table, Z=zL, pZ=pZ)
# #### class(ptt) <- "PageTest"
# #### return(ptt)

# if(is.na(p.table)) pval <- pZ else pval <- p.table

# RVAL <- list(statistic = c(L = L), p.value = pval, method = "Page test for ordered alternatives",
# data.name = dname)
# class(RVAL) <- "htest"
# return(RVAL)

# }

# print.PageTest<-function(x,...) {

# cat("\nPage test for ordered alternatives\n")
# cat("L =",x$L)

# if(is.na(x$p.table)) {
# plabel<-paste("Z =",x$Z,", p =",x$pZ,sep="",collapse="")
# cat(plabel,x$p.chisq,"\n\n")
# }
# else cat("  p(table) ",x$p.table,"\n\n")
# }


PageTest <- function (y, ...) UseMethod("PageTest")


PageTest.default <- function (y, groups, blocks, ...) {

  p.page <- function(k, n, L){

    qvec <- .PageDF[k][[1]]
    f1 <- qvec

    for (i in 1:(n-1)) {
      erg <- convolve(f1, qvec, conj = TRUE, type = "open")
      f1 <- erg
    }
    p <- cumsum(erg)[n * k * (k+1) * (2*k+1)/6 + 1 - L]
    return(p)

  }


  DNAME <- deparse(substitute(y))
  if (is.matrix(y)) {
    groups <- factor(c(col(y)))
    blocks <- factor(c(row(y)))
  }
  else {
    if (any(is.na(groups)) || any(is.na(blocks)))
      stop("NA's are not allowed in 'groups' or 'blocks'")
    if (any(diff(c(length(y), length(groups), length(blocks))) !=
            0L))
      stop("'y', 'groups' and 'blocks' must have the same length")
    DNAME <- paste(DNAME, ", ", deparse(substitute(groups)),
                   " and ", deparse(substitute(blocks)), sep = "")
    if (any(table(groups, blocks) != 1))
      stop("not an unreplicated complete block design")
    groups <- factor(groups)
    blocks <- factor(blocks)
    o <- order(groups, blocks)
    y <- y[o]
    groups <- groups[o]
    blocks <- blocks[o]
  }
  k <- nlevels(groups)
  y <- matrix(unlist(split(y, blocks)), ncol = k, byrow = TRUE)
  y <- y[complete.cases(y), ]
  n <- nrow(y)


  rnksum <- apply(apply(y, 1, rank), 1, sum)
  L <- sum(seq_along(rnksum) * rnksum)
  nc <- ncol(y)
  nr <- nrow(y)

  if(nc < 16){
    pval <- p.page(k=nc, n=nr, L=L)
  } else {
    mu <- nr * nc * (nc + 1)^2/4
    # sig <- nr * nc^2 * (nc + 1)^2 * (nc - 1)/144
    sigma <- nr * nc^2 * (nc+1) * (nc^2-1) / 144
    z <- (L - mu)/sqrt(sigma)
    pval <- pnorm(z, lower.tail = FALSE)

  }

  structure(list(statistic = c(L = L), p.value = pval, method = "Page test for ordered alternatives",
                 data.name = DNAME),
            class = "htest")
}


PageTest.formula <- function (formula, data, subset, na.action, ...) {

  if (missing(formula))
    stop("formula missing")
  if ((length(formula) != 3L) || (length(formula[[3L]]) !=
                                  3L) || (formula[[3L]][[1L]] != as.name("|")) || (length(formula[[3L]][[2L]]) !=
                                                                                   1L) || (length(formula[[3L]][[3L]]) != 1L))
    stop("incorrect specification for 'formula'")
  formula[[3L]][[1L]] <- as.name("+")
  m <- match.call(expand.dots = FALSE)
  m$formula <- formula
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- as.name("model.frame")
  mf <- eval(m, parent.frame())
  DNAME <- paste(names(mf), collapse = " and ")
  names(mf) <- NULL
  y <- DoCall("PageTest", as.list(mf))
  y$data.name <- DNAME
  y

}



CochranQTest <- function(y, ...){

  # Cochran's Q Test is analogue to the friedman.test with 0,1 coded response

  res <- friedman.test(y, ...)
  attr(res$statistic, "names") <- "Q"
  res$method <- "Cochran's Q test"
  return(res)
}

CochranQTest.default <- function(y, groups, blocks, ...){
  res <- friedman.test(y, groups, blocks, ...)
  attr(res$statistic, "names") <- "Q"
  res$method <- "Cochran's Q test"
  return(res)
}

CochranQTest.formula <- function(formula, data, subset, na.action, ...){
  res <- friedman.test(formula, data, subset, na.action, ...)
  attr(res$statistic, "names") <- "Q"
  res$method <- "Cochran's Q test"
  return(res)
}


MHChisqTest <- function(x, srow=1:nrow(x), scol=1:ncol(x)){

  # calculates Mantel-Haenszel Chisquare test

  # check for rxc 2-dim matrix
  p <- (d <- dim(x))[1L]
  if(!is.numeric(x) || length(d) != 2L)
    stop("'x' is not a rxc numeric matrix")

  DNAME <- deparse(substitute(x))

  STATISTIC <- (sum(x) - 1) * corr(d=CombPairs(srow, scol), as.vector(x))^2
  PARAMETER <- 1
  names(STATISTIC) <- "X-squared"
  names(PARAMETER) <- "df"
  PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
  METHOD <- "Mantel-Haenszel Chi-Square"

  structure(list(statistic = STATISTIC, parameter = PARAMETER,
                 p.value = PVAL, method = METHOD, data.name = DNAME), class = "htest")
}



GTest <- function(x, y = NULL, correct=c("none", "williams", "yates"), p = rep(1/length(x), length(x))) {


  # Log-likelihood tests of independence & goodness of fit
  # Does Williams' and Yates' correction
  # does Monte Carlo simulation of p-values, via gtestsim.c
  #
  # G & q calculation from Sokal & Rohlf (1995) Biometry 3rd ed.
  # TOI Yates' correction taken from Mike Camann's 2x2 G-test fn.
  # GOF Yates' correction as described in Zar (2000)
  # more stuff taken from ctest's chisq.test()
  #
  # ToDo:
  # 1) Beautify
  # 2) Add warnings for violations
  # 3) Make appropriate corrections happen by default
  #
  # V3.3 Pete Hurd Sept 29 2001. phurd@ualberta.ca


  DNAME <- deparse(substitute(x))
  if (is.data.frame(x)) x <- as.matrix(x)
  if (is.matrix(x)) {
    if (min(dim(x)) == 1)
      x <- as.vector(x)
  }
  if (!is.matrix(x) && !is.null(y)) {
    if (length(x) != length(y))
      stop("x and y must have the same length")
    DNAME <- paste(DNAME, "and", deparse(substitute(y)))
    OK <- complete.cases(x, y)
    x <- as.factor(x[OK])
    y <- as.factor(y[OK])
    if ((nlevels(x) < 2) || (nlevels(y) < 2))
      stop("x and y must have at least 2 levels")
    x <- table(x, y)
  }
  if (any(x < 0) || any(is.na(x)))
    stop("all entries of x must be nonnegative and finite")
  if ((n <- sum(x)) == 0)
    stop("at least one entry of x must be positive")

  correct <- match.arg(correct)

  #If x is matrix, do test of independence
  if (is.matrix(x)) {
    #Test of Independence
    nrows<-nrow(x)
    ncols<-ncol(x)
    if (correct=="yates"){ # Do Yates' correction?
      if(dim(x)[1]!=2 || dim(x)[2]!=2) # check for 2x2 matrix
        stop("Yates' correction requires a 2 x 2 matrix")
      if((x[1,1]*x[2,2])-(x[1,2]*x[2,1]) > 0)
      {
        #         x[1,1] <- x[1,1] - 0.5
        #         x[2,2] <- x[2,2] - 0.5
        #         x[1,2] <- x[1,2] + 0.5
        #         x[2,1] <- x[2,1] + 0.5
        #   this can be done quicker: 14.5.2015 AS
        x <- x + 0.5
        diag(x) <- diag(x) - 1

      } else {

        x <- x - 0.5
        diag(x) <- diag(x) + 1

        #         x[1,1] <- x[1,1] + 0.5
        #         x[2,2] <- x[2,2] + 0.5
        #         x[1,2] <- x[1,2] - 0.5
        #         x[2,1] <- x[2,1] - 0.5
      }
    }

    sr <- apply(x,1,sum)
    sc <- apply(x,2,sum)
    E <- outer(sr,sc, "*")/n
    # are we doing a monte-carlo?
    # no monte carlo GOF?
    #     if (simulate.p.value){
    #       METHOD <- paste("Log likelihood ratio (G-test) test of independence\n\t with simulated p-value based on", B, "replicates")
    #       tmp <- .C("gtestsim", as.integer(nrows), as.integer(ncols),
    #                 as.integer(sr), as.integer(sc), as.integer(n), as.integer(B),
    #                 as.double(E), integer(nrows * ncols), double(n+1),
    #                 integer(ncols), results=double(B), PACKAGE= "ctest")
    #       g <- 0
    #       for (i in 1:nrows){
    #         for (j in 1:ncols){
    #           if (x[i,j] != 0) g <- g + x[i,j] * log(x[i,j]/E[i,j])
    #         }
    #       }
    #       STATISTIC <- G <- 2 * g
    #       PARAMETER <- NA
    #       PVAL <- sum(tmp$results >= STATISTIC)/B
    #     }
    #     else {
    # no monte-carlo
    # calculate G
    g <- 0
    for (i in 1:nrows){
      for (j in 1:ncols){
        if (x[i,j] != 0) g <- g + x[i,j] * log(x[i,j]/E[i,j])
      }
      # }
      q <- 1
      if (correct=="williams"){ # Do Williams' correction
        row.tot <- col.tot <- 0
        for (i in 1:nrows){ row.tot <- row.tot + 1/(sum(x[i,])) }
        for (j in 1:ncols){ col.tot <- col.tot + 1/(sum(x[,j])) }
        q <- 1+ ((n*row.tot-1)*(n*col.tot-1))/(6*n*(ncols-1)*(nrows-1))
      }
      STATISTIC <- G <- 2 * g / q
      PARAMETER <- (nrow(x)-1)*(ncol(x)-1)
      PVAL <- 1-pchisq(STATISTIC,df=PARAMETER)
      if(correct=="none")
        METHOD <- "Log likelihood ratio (G-test) test of independence without correction"
      if(correct=="williams")
        METHOD <- "Log likelihood ratio (G-test) test of independence with Williams' correction"
      if(correct=="yates")
        METHOD <- "Log likelihood ratio (G-test) test of independence with Yates' correction"
    }
  }
  else {
    # x is not a matrix, so we do Goodness of Fit
    METHOD <- "Log likelihood ratio (G-test) goodness of fit test"
    if (length(x) == 1)
      stop("x must at least have 2 elements")
    if (length(x) != length(p))
      stop("x and p must have the same number of elements")
    E <- n * p

    if (correct=="yates"){ # Do Yates' correction
      if(length(x)!=2)
        stop("Yates' correction requires 2 data values")
      if ( (x[1]-E[1]) > 0.25) {
        x[1] <- x[1]-0.5
        x[2] <- x[2]+0.5
      }
      else if ( (E[1]-x[1]) > 0.25){
        x[1] <- x[1]+0.5
        x[2] <- x[2]-0.5
      }
    }
    names(E) <- names(x)
    g <- 0
    for (i in 1:length(x)){
      if (x[i] != 0) g <- g + x[i] * log(x[i]/E[i])
    }
    q <- 1
    if (correct=="williams"){ # Do Williams' correction
      q <- 1+(length(x)+1)/(6*n)
    }
    STATISTIC <- G <- 2*g/q
    PARAMETER <- length(x) - 1
    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
  }
  names(STATISTIC) <- "G"
  names(PARAMETER) <- "X-squared df"
  names(PVAL) <- "p.value"
  structure(list(statistic=STATISTIC,parameter=PARAMETER,p.value=PVAL,
                 method=METHOD,data.name=DNAME, observed=x, expected=E),
            class="htest")
}






StuartMaxwellTest <- function (x, y = NULL) {

  # stuart.maxwell.mh computes the marginal homogeneity test for
  # a CxC matrix of assignments of objects to C categories or an
  # nx2 or 2xn matrix of category scores for n data objects by two
  # raters. The statistic is distributed as Chi-square with C-1
  # degrees of freedom.

  # The core code is form Jim Lemon, package concord
  # the intro is taken from mcnemar.test (core)

  if (is.matrix(x)) {
    r <- nrow(x)
    if ((r < 2) || (ncol(x) != r))
      stop("'x' must be square with at least two rows and columns")
    if (any(x < 0) || anyNA(x))
      stop("all entries of 'x' must be nonnegative and finite")
    DNAME <- deparse(substitute(x))
  }
  else {
    if (is.null(y))
      stop("if 'x' is not a matrix, 'y' must be given")
    if (length(x) != length(y))
      stop("'x' and 'y' must have the same length")
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    OK <- complete.cases(x, y)
    x <- as.factor(x[OK])
    y <- as.factor(y[OK])
    r <- nlevels(x)
    if ((r < 2) || (nlevels(y) != r))
      stop("'x' and 'y' must have the same number of levels (minimum 2)")
    x <- table(x, y)
  }

  # get the marginals
  rowsums <- rowSums(x)
  colsums <- colSums(x)
  equalsums <- rowsums == colsums

  if(any(equalsums)) {
    # dump any categories with perfect agreement
    x <- x[!equalsums, !equalsums]
    # bail out if too many categories have disappeared
    if(dim(x)[1] < 2) stop("Too many equal marginals, cannot compute")
    # get new marginals
    rowsums <- rowSums(x)
    colsums <- colSums(x)
  }

  # use K-1 marginals
  Kminus1 <- length(rowsums) - 1
  smd <- (rowsums-colsums)[1:Kminus1]
  smS <- matrix(0, nrow=Kminus1, ncol=Kminus1)
  for(i in 1:Kminus1) {
    for(j in 1:Kminus1) {
      if(i == j) smS[i,j] <- rowsums[i] + colsums[j] - 2 * x[i,j]
      else smS[i,j] <- -(x[i,j] + x[j,i])
    }
  }

  STATISTIC <- t(smd) %*% solve(smS) %*% smd

  PARAMETER <- r - 1
  METHOD <- "Stuart-Maxwell test"

  PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
  names(STATISTIC) <- "chi-squared"
  names(PARAMETER) <- "df"
  RVAL <- list(statistic = STATISTIC, parameter = PARAMETER,
               p.value = PVAL, method = METHOD, data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)

}




BreslowDayTest <- function(x, OR = NA, correct = FALSE) {

  # Function to perform the Breslow and Day (1980) test including the
  # corrected test by Tarone Uses the equations in Lachin (2000),
  # Biostatistical Methods, Wiley, p. 124-125.
  #
  # Programmed by Michael Hoehle <http://www.math.su.se/~hoehle>
  # Code taken originally from a Biostatistical Methods lecture
  # held at the Technical University of Munich in 2008.
  #
  # Params:
  #  x - a 2x2xK contingency table
  #  correct - if TRUE Tarones correction is returned
  #
  # Returns:
  #  a vector with three values
  #   statistic - Breslow and Day test statistic
  #   pval - p value evtl. based on the Tarone test statistic
  #               using a \chi^2(K-1) distribution
  #


  if(is.na(OR)) {
    #Find the common OR based on Mantel-Haenszel
    or.hat.mh <- mantelhaen.test(x)$estimate
  } else {
    or.hat.mh <- OR
  }

  #Number of strata
  K <- dim(x)[3]
  #Value of the Statistic
  X2.HBD <- 0
  #Value of aj, tildeaj and Var.aj
  a <- tildea <- Var.a <- numeric(K)

  for (j in 1:K) {
    #Find marginals of table j
    mj <- apply(x[,,j], MARGIN=1, sum)
    nj <- apply(x[,,j], MARGIN=2, sum)

    #Solve for tilde(a)_j
    coef <- c(-mj[1]*nj[1] * or.hat.mh, nj[2]-mj[1]+or.hat.mh*(nj[1]+mj[1]),
              1-or.hat.mh)
    sols <- Re(polyroot(coef))
    #Take the root, which fulfills 0 < tilde(a)_j <= min(n1_j, m1_j)
    tildeaj <- sols[(0 < sols) &  (sols <= min(nj[1],mj[1]))]
    #Observed value
    aj <- x[1,1,j]

    #Determine other expected cell entries
    tildebj <- mj[1] - tildeaj
    tildecj <- nj[1] - tildeaj
    tildedj <- mj[2] - tildecj

    #Compute \hat{\Var}(a_j | \widehat{\OR}_MH)
    Var.aj <- (1/tildeaj + 1/tildebj + 1/tildecj + 1/tildedj)^(-1)

    #Compute contribution
    X2.HBD <- X2.HBD + as.numeric((aj - tildeaj)^2 / Var.aj)

    #Assign found value for later computations
    a[j] <- aj ;  tildea[j] <- tildeaj ; Var.a[j] <- Var.aj
  }

  # Compute Tarone corrected test
  # Add on 2015: The original equation from the 2008 lecture is incorrect
  # as pointed out by Jean-Francois Bouzereau.
  # X2.HBDT <-as.numeric( X2.HBD -  (sum(a) - sum(tildea))^2/sum(Var.aj) )
  X2.HBDT <-as.numeric( X2.HBD -  (sum(a) - sum(tildea))^2/sum(Var.a) )

  DNAME <- deparse(substitute(x))

  STATISTIC <- if(correct) X2.HBDT else X2.HBD
  PARAMETER <- K - 1
  # Compute p-value based on the Tarone corrected test
  PVAL <- 1 - pchisq(STATISTIC, PARAMETER)
  METHOD <- if(correct) "Breslow-Day Test on Homogeneity of Odds Ratios (with Tarone correction)" else
    "Breslow-Day test on Homogeneity of Odds Ratios"
  names(STATISTIC) <- "X-squared"
  names(PARAMETER) <- "df"
  structure(list(statistic = STATISTIC, parameter = PARAMETER,
                 p.value = PVAL, method = METHOD, data.name = DNAME
  ), class = "htest")

}




# BreslowDayTest <- function(x, OR = NA, correct = FALSE) {
#
#   # Function to perform the Breslow and Day (1980) test including
#   # the corrected test by Tarone
#   # Uses the equations in Lachin (2000) p. 124-125.
#   #
#   # Programmed by Michael Hoehle <http://www-m4.ma.tum.de/pers/hoehle>
#   # Note that the results of the Tarone corrected test do
#   # not correspond to the numbers in the Lachin book...
#   #
#   # Params:
#   #  x - a 2x2xK contingency table
#   #  correct - if TRUE Tarones correction is returned
#   #
#   # Returns:
#   #  a vector with three values
#   #   statistic - Breslow and Day test statistic
#   #   pval - p value evtl. based on the Tarone test statistic
#   #               using a \chi^2(K-1) distribution
#   #
#
#
#   if(is.na(OR)) {
#     #Find the common OR based on Mantel-Haenszel
#     or.hat.mh <- mantelhaen.test(x)$estimate
#   } else {
#     or.hat.mh <- OR
#   }
#
#   #Number of strata
#   K <- dim(x)[3]
#   #Value of the Statistic
#   X2.HBD <- 0
#   #Value of aj, tildeaj and Var.aj
#   a <- tildea <- Var.a <- numeric(K)
#
#   for (j in 1:K) {
#     #Find marginals of table j
#     mj <- apply(x[,,j], MARGIN=1, sum)
#     nj <- apply(x[,,j], MARGIN=2, sum)
#
#     #Solve for tilde(a)_j
#     coef <- c(-mj[1]*nj[1] * or.hat.mh, nj[2]-mj[1]+or.hat.mh*(nj[1]+mj[1]),
#               1-or.hat.mh)
#     sols <- Re(polyroot(coef))
#     #Take the root, which fulfills 0 < tilde(a)_j <= min(n1_j, m1_j)
#     tildeaj <- sols[(0 < sols) &  (sols <= min(nj[1],mj[1]))]
#     #Observed value
#     aj <- x[1,1,j]
#
#     #Determine other expected cell entries
#     tildebj <- mj[1] - tildeaj
#     tildecj <- nj[1] - tildeaj
#     tildedj <- mj[2] - tildecj
#
#     #Compute \hat{\Var}(a_j | \widehat{\OR}_MH)
#     Var.aj <- (1/tildeaj + 1/tildebj + 1/tildecj + 1/tildedj)^(-1)
#
#     #Compute contribution
#     X2.HBD <- X2.HBD + as.numeric((aj - tildeaj)^2 / Var.aj)
#
#     #Assign found value for later computations
#     a[j] <- aj ;  tildea[j] <- tildeaj ; Var.a[j] <- Var.aj
#   }
#
#   # Compute Tarone corrected test
#   # This is incorrect as correctly pointed out by Jean-Francois Bouzereau..
#   # X2.HBDT <-as.numeric( X2.HBD -  (sum(a) - sum(tildea))^2/sum(Var.aj) )
#   X2.HBDT <-as.numeric( X2.HBD -  (sum(a) - sum(tildea))^2/sum(Var.a) )
#
#   DNAME <- deparse(substitute(x))
#
#   STATISTIC <- if(correct) X2.HBDT else X2.HBD
#   PARAMETER <- K - 1
#   # Compute p-value based on the Tarone corrected test
#   PVAL <- 1 - pchisq(STATISTIC, PARAMETER)
#   METHOD <- if(correct) "Breslow-Day Test on Homogeneity of Odds Ratios (with Tarone correction)" else
#     "Breslow-Day test on Homogeneity of Odds Ratios"
#   names(STATISTIC) <- "X-squared"
#   names(PARAMETER) <- "df"
#   structure(list(statistic = STATISTIC, parameter = PARAMETER,
#                  p.value = PVAL, method = METHOD, data.name = DNAME
#   ), class = "htest")
#
# }



# the VCD package (available via CRAN) has a function called woolf_test()

WoolfTest <- function(x) {

  DNAME <- deparse(substitute(x))
  if (any(x == 0))
    x <- x + 1 / 2
  k <- dim(x)[3]
  or <- apply(x, 3, function(x) (x[1,1] * x[2,2]) / (x[1,2] * x[2,1]))
  w <-  apply(x, 3, function(x) 1 / sum(1 / x))
  o <- log(or)
  e <- weighted.mean(log(or), w)
  STATISTIC <- sum(w * (o - e)^2)
  PARAMETER <- k - 1
  PVAL <- 1 - pchisq(STATISTIC, PARAMETER)
  METHOD <- "Woolf Test on Homogeneity of Odds Ratios (no 3-Way assoc.)"
  names(STATISTIC) <- "X-squared"
  names(PARAMETER) <- "df"
  structure(list(statistic = STATISTIC, parameter = PARAMETER,
                 p.value = PVAL, method = METHOD, data.name = DNAME, observed = o,
                 expected = e), class = "htest")

}


LehmacherTest <- function(x, y = NULL) {

  if (is.matrix(x)) {
    r <- nrow(x)
    if ((r < 2) || (ncol(x) != r))
      stop("'x' must be square with at least two rows and columns")
    if (any(x < 0) || anyNA(x))
      stop("all entries of 'x' must be nonnegative and finite")
    DNAME <- deparse(substitute(x))
  }
  else {
    if (is.null(y))
      stop("if 'x' is not a matrix, 'y' must be given")
    if (length(x) != length(y))
      stop("'x' and 'y' must have the same length")
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    OK <- complete.cases(x, y)
    x <- as.factor(x[OK])
    y <- as.factor(y[OK])
    r <- nlevels(x)
    if ((r < 2) || (nlevels(y) != r))
      stop("'x' and 'y' must have the same number of levels (minimum 2)")
    x <- table(x, y)
  }

  rsum <- rowSums(x)
  csum <- colSums(x)

  STATISTIC <- (rsum-csum)^2 / (rsum + csum - 2*diag(x))
  PARAMETER <- 1
  PVAL <- 1 - pchisq(STATISTIC, PARAMETER)
  METHOD <- "Lehmacher-Test on Marginal Homogeneity"
  names(STATISTIC) <- "X-squared"
  names(PARAMETER) <- "df"
  structure(list(statistic = STATISTIC, parameter = PARAMETER,
                 p.value = PVAL, p.value.corr = p.adjust(PVAL, "hochberg"),
                 method = METHOD, data.name = DNAME),
            class = "mtest")

}


print.mtest <- function (x, digits = 4L, ...) {

  cat("\n")
  cat(strwrap(x$method, prefix = "\t"), sep = "\n")
  cat("\n")
  cat("data:  ", x$data.name, "\n", sep = "")

  out <- character()
  out <- cbind(format(round(x$statistic, 4)), format.pval(x$p.value, digits = digits),
               format.pval(x$p.value.corr, digits = digits),
               symnum(x$p.value.corr, corr = FALSE, na = FALSE,
                      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                      symbols = c("***", "**", "*", ".", " ")))
  colnames(out) <- c("X-squared", "pval", "pval adj", " ")
  rownames(out) <- if(is.null(rownames(x))) 1:length(x$statistic) else rownames(x)
  print.default(out, digits = 3, quote = FALSE, right = TRUE)

  cat("\n")
  cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  invisible(x)
}




CochranArmitageTest <- function(x, alternative = c("two.sided","increasing","decreasing")) {

  # based on:
  # http://tolstoy.newcastle.edu.au/R/help/05/07/9442.html
  DNAME <- deparse(substitute(x))

  if (!(any(dim(x)==2)))
    stop("Cochran-Armitage test for trend must be used with rx2-table", call.=FALSE)

  if (dim(x)[2]!=2) x <- t(x)

  nidot <- apply(x, 1, sum)
  n <- sum(nidot)

  # Ri <- scores(x, 1, "table")
  Ri <- 1:dim(x)[1]
  Rbar <- sum(nidot*Ri)/n

  s2 <- sum(nidot*(Ri-Rbar)^2)
  pdot1 <- sum(x[,1])/n
  z <- sum(x[,1]*(Ri-Rbar))/sqrt(pdot1*(1-pdot1)*s2)
  STATISTIC <- z

  alternative <- match.arg(alternative)

  PVAL <- switch(alternative,
                 two.sided = 2*pnorm(abs(z), lower.tail=FALSE),
                 increasing = pnorm(z),
                 decreasing = pnorm(z, lower.tail=FALSE) )

  PARAMETER <- dim(x)[1]
  names(STATISTIC) <- "Z"
  names(PARAMETER) <- "dim"

  METHOD <- "Cochran-Armitage test for trend"
  structure(list(statistic = STATISTIC, parameter = PARAMETER, alternative = alternative,
                 p.value = PVAL, method = METHOD, data.name = DNAME
  ), class = "htest")

}



BarnardTest <- function (x, y = NULL, alternative = c("two.sided", "less", "greater"), dp = 0.001, pooled = TRUE ) {

  if (is.matrix(x)) {
    r <- nrow(x)
    if ((r < 2) || (ncol(x) != r))
      stop("'x' must be square with at least two rows and columns")
    if (any(x < 0) || anyNA(x))
      stop("all entries of 'x' must be nonnegative and finite")
    DNAME <- deparse(substitute(x))
  } else {
    if (is.null(y))
      stop("if 'x' is not a matrix, 'y' must be given")
    if (length(x) != length(y))
      stop("'x' and 'y' must have the same length")
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    OK <- complete.cases(x, y)
    x <- as.factor(x[OK])
    y <- as.factor(y[OK])
    r <- nlevels(x)
    if ((r < 2) || (nlevels(y) != r))
      stop("'x' and 'y' must have the same number of levels (minimum 2)")
    x <- table(x, y)
  }

  nr <- nrow(x)
  nc <- ncol(x)

  # if ((nr == 2) && (nc == 2)) {
  #   alternative <- char.expand(alternative, c("two.sided", "less", "greater"))
  #   if (length(alternative) > 1L || is.na(alternative))
  #     stop("alternative must be \"two.sided\", \"less\" or \"greater\"")
  # }
  alternative <- match.arg(alternative)

  method <- c("Wald", "Score")[1 + pooled]
  METHOD <- gettextf("Barnards Unconditional 2x2-test", method)

  vec.size <- 1.0 + 1.0 / dp
  mat.size <- 4.0 * prod(rowSums(x) + 1) # (n1 + n3 + 1) * (n2 + n4 + 1)


  if(pooled)
    ret1 <- .C( "ScoreS",
                as.integer(x[1]), as.integer(x[2]), as.integer(x[3]), as.integer(x[4]),
                as.numeric(dp),
                mat.size = as.integer(0),
                statistic.table = as.double(vector("double", mat.size)),
                statistic = as.double(0.0))
  else
    ret1 <- .C( "WaldS",
                as.integer(x[1]), as.integer(x[2]), as.integer(x[3]), as.integer(x[4]),
                as.numeric(dp),
                mat.size = as.integer(0),
                statistic.table = as.double(vector("double", mat.size)),
                statistic = as.double(0.0))


  xr <- seq(1, ret1$mat.size, 4) + 2

  ret1$statistic.table[xr + 1][
    (ret1$statistic <= 0 & ret1$statistic.table[xr] <= ret1$statistic) |
      (ret1$statistic >= 0 & ret1$statistic.table[xr] >= ret1$statistic)] <- 1

  ret1$statistic.table[xr + 1][
    (ret1$statistic <= 0 & ret1$statistic.table[xr] >= -ret1$statistic) |
      (ret1$statistic >= 0 & ret1$statistic.table[xr] <= -ret1$statistic)] <- 2

  ret2 <- .C("Barnard",
             as.integer(x[1]), as.integer(x[2]), as.integer(x[3]), as.integer(x[4]),
             as.numeric(dp),
             as.integer(ret1$mat.size),
             nuisance.vector.x = as.double(vector("double",vec.size)),
             nuisance.vector.y0 = as.double(vector("double",vec.size)),
             nuisance.vector.y1 = as.double(vector("double",vec.size)),
             statistic.table = as.double(ret1$statistic.table))

  np0 <- which.max(ret2$nuisance.vector.y0)
  np1 <- which.max(ret2$nuisance.vector.y1)

  nuisance.matrix <- matrix(cbind(ret2$nuisance.vector.x, ret2$nuisance.vector.y0, ret2$nuisance.vector.y1), ncol=3)
  statistic.table <- matrix(ret1$statistic.table, ncol=4, byrow=TRUE, dimnames=list(c(), c("n1", "n2", "statistic", "include.in.p.value")))


  STATISTIC <- ret1$statistic
  if(alternative == "two.sided"){
    PVAL <- ret2$nuisance.vector.y1[np1]
    ESTIMATE <- c(`Nuisance parameter` = ret2$nuisance.vector.x[np1])
  } else {
    PVAL <- ret2$nuisance.vector.y0[np0]
    ESTIMATE <- c(`Nuisance parameter` = ret2$nuisance.vector.x[np0])
  }

  names(STATISTIC) <- gettextf("%s statistic", method)
  RVAL <- list(statistic = STATISTIC, alternative = alternative, estimate = ESTIMATE,
               p.value = PVAL, method = METHOD, data.name = DNAME,
               statistic.table = statistic.table, nuisance.matrix = nuisance.matrix)

  class(RVAL) <- "htest"
  return(RVAL)
}




BreuschGodfreyTest <- function(formula, order = 1, order.by = NULL, type = c("Chisq", "F"),
                               data = list(), fill = 0) {

  # from lmtest

  dname <- paste(deparse(substitute(formula)))

  if(!inherits(formula, "formula")) {
    X <- if(is.matrix(formula$x))
      formula$x
    else model.matrix(terms(formula), model.frame(formula))
    y <- if(is.vector(formula$y))
      formula$y
    else model.response(model.frame(formula))
  } else {
    mf <- model.frame(formula, data = data)
    y <- model.response(mf)
    X <- model.matrix(formula, data = data)
  }

  if(!is.null(order.by))
  {
    if(inherits(order.by, "formula")) {
      z <- model.matrix(order.by, data = data)
      z <- as.vector(z[,ncol(z)])
    } else {
      z <- order.by
    }
    X <- as.matrix(X[order(z),])
    y <- y[order(z)]
  }

  n <- nrow(X)
  k <- ncol(X)
  order <- 1:order
  m <- length(order)
  resi <- lm.fit(X,y)$residuals

  Z <- sapply(order, function(x) c(rep(fill, length.out = x), resi[1:(n-x)]))
  if(any(na <- !complete.cases(Z))) {
    X <- X[!na, , drop = FALSE]
    Z <- Z[!na, , drop = FALSE]
    y <- y[!na]
    resi <- resi[!na]
    n <- nrow(X)
  }
  auxfit <- lm.fit(cbind(X,Z), resi)

  cf <- auxfit$coefficients
  vc <- chol2inv(auxfit$qr$qr) * sum(auxfit$residuals^2) / auxfit$df.residual
  names(cf) <- colnames(vc) <- rownames(vc) <- c(colnames(X), paste("lag(resid)", order, sep = "_"))

  switch(match.arg(type),

         "Chisq" = {
           bg <- n * sum(auxfit$fitted^2)/sum(resi^2)
           p.val <- pchisq(bg, m, lower.tail = FALSE)
           df <- m
           names(df) <- "df"
         },

         "F" = {
           uresi <- auxfit$residuals
           bg <- ((sum(resi^2) - sum(uresi^2))/m) / (sum(uresi^2) / (n-k-m))
           df <- c(m, n-k-m)
           names(df) <- c("df1", "df2")
           p.val <- pf(bg, df1 = df[1], df2 = df[2], lower.tail = FALSE)
         })

  names(bg) <- "LM test"
  RVAL <- list(statistic = bg, parameter = df,
               method = paste("Breusch-Godfrey test for serial correlation of order up to", max(order)),
               p.value = p.val,
               data.name = dname,
               coefficients = cf,
               vcov = vc)
  class(RVAL) <- c("BreuschGodfreyTest", "htest")
  return(RVAL)
}


# vcov.BreuschGodfreyTest <- function(object, ...) object$vcov
# df.residual.BreuschGodfreyTest <- function(object, ...) if(length(df <- object$parameter) > 1L) df[2] else NULL





EtaSq <- function (x, type = 2, anova = FALSE) {
  UseMethod("EtaSq")
}

EtaSq.lm <- function (x, type = 2, anova = FALSE) {

  # file:    etaSquared.R
  # author:  Dan Navarro
  # contact: daniel.navarro@adelaide.edu.au
  # changed: 13 November 2013
  # modified by Daniel Wollschlaeger 17.9.2014

  # etaSquared() calculates eta-squared and partial eta-squared for linear models
  # (usually ANOVAs). It takes an lm object as input and computes the effect size
  # for all terms in the model. By default uses Type II sums of squares to calculate
  # the effect size, but Types I and III are also possible. By default the output
  # only displays the effect size, but if requested it will also print out the full
  # ANOVA table.

  if (!is(anova, "logical") | length(anova) != 1) {
    stop("\"anova\" must be a single logical value")
  }
  if (!is(type, "numeric") | length(type) != 1) {
    stop("type must be equal to 1, 2 or 3")
  }
  if (type == 1) {
    ss <- anova(x)[, "Sum Sq", drop = FALSE]
    ss.res <- ss[dim(ss)[1], ]
    ss.tot <- sum(ss)
    ss <- ss[-dim(ss)[1], , drop = FALSE]
    ss <- as.matrix(ss)
  }
  else {
    if (type == 2) {
      ss.tot <- sum((x$model[, 1] - mean(x$model[, 1]))^2)
      ss.res <- sum((x$residuals)^2)
      terms <- attr(x$terms, "factors")[-1, , drop = FALSE]
      l <- attr(x$terms, "term.labels")
      ss <- matrix(NA, length(l), 1)
      rownames(ss) <- l
      for (i in seq_along(ss)) {
        vars.this.term <- which(terms[, i] != 0)
        dependent.terms <- which(apply(terms[vars.this.term, , drop = FALSE], 2, prod) > 0)
        m0 <- lm(x$terms[-dependent.terms], x$model)
        if (length(dependent.terms) > 1) {
          m1 <- lm(x$terms[-setdiff(dependent.terms, i)], x$model)
          ss[i] <- anova(m0, m1)$`Sum of Sq`[2]
        }
        else {
          ss[i] <- anova(m0, x)$`Sum of Sq`[2]
        }
      }
    }
    else {
      if (type == 3) {
        ## check if model was fitted with sum-to-zero contrasts
        ## necessary for valid SS type 3 (e.g., contr.sum, contr.helmert)
        IVs <- names(attr(model.matrix(x), "contrasts"))
        ## only relevant for more than one factor
        ## (and for unbalanced cell sizes and interactions, not tested here)
        if(length(IVs) > 1) {
          isSumToZero <- function(IV) {
            ## check if factor has directly associated contrasts
            if(!is.null(attr(x$model[, IV], "contrasts"))) {
              cm <- contrasts(x$model[, IV])
              all(colSums(cm) == 0)
            } else {
              ## check attributes from model matrix
              attr(model.matrix(x), "contrasts")[[IV]] %in% c("contr.sum", "contr.helmert")
            }
          }

          valid <- vapply(IVs, isSumToZero, logical(1))

          if(!all(valid)) {
            warning(c(ifelse(sum(!valid) > 1, "Factors ", "Factor "),
                      paste(IVs[!valid], collapse=", "),
                      ifelse(sum(!valid) > 1, " are", " is"),
                      " not associated with sum-to-zero contrasts",
                      " necessary for valid SS type III",
                      " when cell sizes are unbalanced",
                      " and interactions are present.",
                      " Consider re-fitting the model after setting",
                      " options(contrasts=c(\"contr.sum\", \"contr.poly\"))"))
          }
        }

        mod <- drop1(x, scope = x$terms)
        ss <- mod[-1, "Sum of Sq", drop = FALSE]
        ss.res <- mod[1, "RSS"]
        ss.tot <- sum((x$model[, 1] - mean(x$model[, 1]))^2)
        ss <- as.matrix(ss)
      }
      else {
        stop("type must be equal to 1, 2 or 3")
      }
    }
  }
  if (anova == FALSE) {
    eta2 <- ss/ss.tot
    eta2p <- ss/(ss + ss.res)
    E <- cbind(eta2, eta2p)
    rownames(E) <- rownames(ss)
    colnames(E) <- c("eta.sq", "eta.sq.part")
  }
  else {
    ss <- rbind(ss, ss.res)
    eta2 <- ss/ss.tot
    eta2p <- ss/(ss + ss.res)
    k <- length(ss)
    eta2p[k] <- NA
    df <- anova(x)[, "Df"]
    ms <- ss/df
    Fval <- ms/ms[k]
    p <- 1 - pf(Fval, df, rep.int(df[k], k))
    E <- cbind(eta2, eta2p, ss, df, ms, Fval, p)
    E[k, 6:7] <- NA
    colnames(E) <- c("eta.sq", "eta.sq.part", "SS", "df", "MS", "F", "p")
    rownames(E) <- rownames(ss)
    rownames(E)[k] <- "Residuals"
  }
  return(E)
}


EtaSq.aovlist <-  function (x, type = 2, anova = FALSE) {

  # author:  Daniel Wollschlaeger
  # contact: contact@dwoll.de
  # changed: 13 October 2014

  # EtaSq.aovlist() calculates partial eta-squared and generalized eta-squared
  # for aovlists

  if (!is(anova, "logical") | length(anova) != 1) {
    stop("\"anova\" must be a single logical value")
  }
  if (!is(type, "numeric") | length(type) != 1) {
    stop("type must be equal to 1, 2 or 3")
  }

  ## alternative: check design has balanced cell sizes
  if (type != 1) {
    stop("type must be equal to 1")
  }

  details <- aovlDetails(x)
  ss      <- details$Sum.Sq             # effect SS
  ss.res  <- sum(aovlErrorTerms(x)$SS)  # total error SS
  ss.tot  <- sum(ss) + sum(ss.res)

  # eta squared
  eta2 <- ss / ss.tot

  # partial eta squared
  # cf. Bakeman, R. (2005) Behavior Research Methods. 37(3), 379-384. Tables 1, 2
  eta2p <- ss / (ss + details$SSE)

  # generalized eta squared
  # if all factors are manipulated
  # cf. Bakeman, R. (2005) Behavior Research Methods. 37(3), 379-384. Tables 1, 2
  geta2 <- ss / (ss + sum(ss.res))

  if (anova == FALSE) {
    E <- cbind(eta2, eta2p, geta2)
    rownames(E) <- details$tt
    colnames(E) <- c("eta.sq", "eta.sq.part", "eta.sq.gen")
  } else {
    E <- data.frame(eta2=eta2,
                    eta2p=eta2p,
                    geta2=geta2,
                    ss=ss,
                    df=details$Df,
                    ms=details$Mean.Sq,
                    sse=details$SSE,
                    dfe=details$dfE,
                    Fval=details$F.value,
                    p=details$Pr..F.)
    colnames(E) <- c("eta.sq", "eta.sq.part", "eta.sq.gen", "SS", "df", "MS", "SSE", "dfE", "F", "p")
    rownames(E) <- details$tt
  }
  return(E)
}

# author:  Daniel Wollschlaeger
aovlDetails <- function(aovObj) {
  aovSum  <- summary(aovObj)
  etNames <- names(aovSum)  # error terms

  getOneRes <- function(tt, tab) {  # tab=anova table, tt = tested term
    ttIdx <- which(DescTools::StrTrim(rownames(tab)) == tt)
    list(df=tab[ttIdx,       "Df"],
         SS=tab[ttIdx,       "Sum Sq"],
         MS=tab[ttIdx,       "Mean Sq"],
         dfE=tab["Residuals", "Df"],
         SSE=tab["Residuals", "Sum Sq"],
         MSE=tab["Residuals", "Mean Sq"],
         F=tab[ttIdx, "F value"],
         p=tab[ttIdx, "Pr(>F)"])
  }

  getTermRes <- function(et) { # et = error term
    tab <- aovSum[[et]][[1]]
    at  <- DescTools::StrTrim(rownames(tab)) # all terms
    tt  <- at[-which(at == "Residuals")]     # tested terms only

    if(length(tt) > 0)
    {
      # error terms
      etRes <- list(df=tab["Residuals", "Df"],
                    SS=tab["Residuals", "Sum Sq"],
                    MS=tab["Residuals", "Mean Sq"])
      ttRes <- lapply(tt, getOneRes, tab=tab)
      ttRes <- setNames(ttRes, tt)
      ttIdx <- which(DescTools::StrTrim(rownames(tab)) %in% tt)
      return(data.frame(tt=tt, et=et,
                        tab[ttIdx, , drop=FALSE],
                        dfE=etRes$df, SSE=etRes$SS, MSE=etRes$MS,
                        stringsAsFactors=FALSE))
    } else {
      emptyDf <- data.frame(matrix(ncol=10, nrow=0))
      return(setNames(emptyDf, c("tt", "et", "Df", "Sum.Sq", "Mean.Sq", "F.value",
                                 "Pr..F.", "dfE", "SSE", "MSE")))
    }
  }

  detailsL  <- setNames(lapply(etNames, getTermRes), etNames)
  detailsDf <- do.call("rbind", detailsL)
  rownames(detailsDf) <- NULL
  return(detailsDf)
}

aovlErrorTerms <- function(aovObj) {
  aovSum  <- summary(aovObj)
  etNames <- names(aovSum)
  getSS <- function(x) {
    aovSum[[x]][[1]]["Residuals", "Sum Sq"]
  }

  getMS <- function(x) {
    aovSum[[x]][[1]]["Residuals", "Mean Sq"]
  }

  getDF <- function(x) {
    aovSum[[x]][[1]]["Residuals", "Df"]
  }

  SS <- vapply(etNames, getSS, numeric(1))
  MS <- vapply(etNames, getMS, numeric(1))
  DF <- vapply(etNames, getDF, numeric(1))
  return(list(SS=SS, MS=MS, DF=DF))
}



Eps <- function(S, p, g, n) {

  ## Purpose: calculates the Greenhouse-Geisser and Huynh-Feldt epsilons
  ## -------------------------------------------------------------------
  ## Arguments: S pxp covariance matrix
  ##            p dimension of observation vectors
  ##            g number of groups
  ##            n number of subjects

  ## Lit:    E.F. Vonesh + V.M. Chinchilli (1997), p.84-86
  ##         M.J. Crowder and D.J. Hand (1990), p.54-55

  ## Author: H.-R. Roth
  ## Date:   23.07.2002
  ## -------------------------------------------------------------------

  # U is a matrix of (p-1) orthonormal contrasts
  U <- t(cbind(diag(p-1),0) - outer(1:(p-1), 1:p, "<") / ((p-1):1))
  a <- 1/sqrt(colSums(U^2))
  U <- U%*%diag(a)
  V <- t(U) %*% S %*% U
  e <- (sum(diag(V)))^2/sum(diag(V%*%V))/(p-1)

  GGepsilon <- e
  HFepsilon <- min(1, (n*(p-1)*e - 2) / ((p-1)* (n-g-(p-1)*e) ))
  t.output <- c(GGepsilon, HFepsilon)
  names(t.output)  <- c("G-G-epsilon", "H-F-epsilon")
  t.output

}



power.chisq.test <- function (n = NULL, w = NULL, df = NULL, sig.level = 0.05, power = NULL) {

  if (sum(sapply(list(w, n, df, power, sig.level), is.null)) != 1)
    stop("exactly one of w, n, df, power or sig.level must be NULL")
  if (!is.null(w) && w < 0)
    stop("w must be positive")
  if (!is.null(n) && n < 1)
    stop("number of observations must be at least 1")
  if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > sig.level | sig.level > 1))
    stop(sQuote("sig.level"), " must be numeric in [0, 1]")
  if (!is.null(power) && !is.numeric(power) || any(0 > power | power > 1))
    stop(sQuote("power"), " must be numeric in [0, 1]")
  p.body <- quote({
    k <- qchisq(sig.level, df = df, lower = FALSE)
    pchisq(k, df = df, ncp = n * w^2, lower = FALSE)
  })
  if (is.null(power))
    power <- eval(p.body)
  else if (is.null(w))
    w <- uniroot(function(w) eval(p.body) - power, c(1e-10, 1e+05))$root
  else if (is.null(n))
    n <- uniroot(function(n) eval(p.body) - power, c(1 + 1e-10, 1e+05))$root
  else if (is.null(sig.level))
    sig.level <- uniroot(function(sig.level) eval(p.body) -
                           power, c(1e-10, 1 - 1e-10))$root
  else stop("internal error")

  METHOD <- "Chi squared power calculation"
  NOTE <- "n is the number of observations"
  structure(list(w = w, n = n, df = df, sig.level = sig.level,
                 power = power, method = METHOD, note = NOTE), class = "power.htest")
}




Contrasts <- function (levs) {
  k = length(levs)
  M = data.frame(levs = levs)
  for (i in 1:(k - 1)) {
    for (j in (i + 1):k) {
      con = rep(0, k)
      con[i] = -1
      con[j] = 1
      nm = paste(levs[j], levs[i], sep = "-")
      M[[nm]] = con
    }
  }
  row.names(M) = levs

  return(M[-1])

}


ScheffeTest <- function (x, ...)
  UseMethod("ScheffeTest")

ScheffeTest.default <- function (x, g = NULL, which = NULL, contrasts = NULL, conf.level = 0.95, ...) {
  ScheffeTest(x=aov(x~g), which=which, contrasts=contrasts, conf.level=conf.level, ...)
}



ScheffeTest.aov <- function(x, which=NULL, contrasts = NULL, conf.level=0.95, ...){

  mm <- model.tables(x, "means")
  if (is.null(mm$n))
    stop("no factors in the fitted model")
  tabs <- mm$tables[-1L]

  if(is.null(which)) which <- seq_along(tabs)

  tabs <- tabs[which]
  nn <- mm$n[names(tabs)]
  nn_na <- is.na(nn)
  if (all(nn_na))
    stop("'which' specified no factors")
  if (any(nn_na)) {
    warning("'which' specified some non-factors which will be dropped")
    tabs <- tabs[!nn_na]
    nn <- nn[!nn_na]
  }
  out <- setNames(vector("list", length(tabs)), names(tabs))
  MSE <- sum(x$residuals^2)/x$df.residual

  autoContr <- is.null(contrasts)
  if(!is.null(contrasts)){
    contrasts <- data.frame(contrasts)
  }

  # nm <- "tension"
  for (nm in names(tabs)) {
    tab <- tabs[[nm]]
    means <- as.vector(tab)

    nms <- if (length(d <- dim(tab)) > 1L) {
      dn <- dimnames(tab)
      apply(do.call("expand.grid", dn), 1L, paste, collapse = ":")
    } else names(tab)

    n <- nn[[nm]]
    if (length(n) < length(means))
      n <- rep.int(n, length(means))

    if(autoContr) contrasts <- Contrasts(nms)

    psi <- apply(contrasts * means, 2, sum)
    sscoeff <- apply(contrasts * contrasts / n, 2, sum)
    mspsi <- (psi * psi) / sscoeff

    # Korrektur von Daniel Wollschlaeger 9.9.2014:
    #     psi <- contrasts %*% means
    #     sscoeff <- contrasts * contrasts %*% (1/n)

    dferr <- x$df.residual
    dfgrp <- length(x$residuals) - dferr - 1

    pval <- pf(psi^2/(MSE*sscoeff*dfgrp),
               df1=dfgrp, df2=dferr, lower.tail=FALSE)

    critvalue <- dfgrp * qf(1-conf.level, dfgrp, dferr, lower.tail=FALSE)

    lwr <- psi - sqrt(critvalue) * sqrt(MSE * sscoeff)
    upr <- psi + sqrt(critvalue) * sqrt(MSE * sscoeff)

    out[[nm]] <- cbind(diff=psi, lwr, upr, pval)
    colnames(out[[nm]]) <- c("diff","lwr.ci","upr.ci","pval")

    if(!autoContr) {
      # define contrasts rownames
      rownames(out[[nm]]) <-  apply(contrasts, 2, function(x)
        gettextf("%s-%s", paste(nms[x>0], collapse=","),
                 paste(nms[x<0], collapse=",")) )
      if(is.na(conf.level)) out[[nm]] <- out[[nm]][,-c(2:3)]
    }

    if(autoContr & is.na(conf.level)) {
      out[[nm]] <- matrix(NA, nrow=length(means), ncol=length(means))
      out[[nm]][lower.tri(out[[nm]], diag = FALSE)] <- pval
      dimnames(out[[nm]]) <- list(nms, nms)
      out[[nm]] <- out[[nm]][-1, -ncol(out[[nm]])]
    }

  }

  class(out) <- c("PostHocTest")
  attr(out, "orig.call") <- x$call
  attr(out, "conf.level") <- conf.level
  attr(out, "ordered") <- FALSE
  attr(out, "method") <- "Scheffe Test"
  attr(out, "method.str") <- gettextf("\n  Posthoc multiple comparisons of means : %s \n", attr(out, "method"))


  return(out)

}




VanWaerdenTest <- function (x, ...)    UseMethod("VanWaerdenTest")


VanWaerdenTest.formula <- function (formula, data, subset, na.action, ...) {
  
  if (missing(formula) || (length(formula) != 3L)) 
    stop("'formula' missing or incorrect")
  
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  
  mf <- eval(m, parent.frame())
  if (length(mf) > 2L) 
    stop("'formula' should be of the form response ~ group")
  
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  y <- do.call("VanWaerdenTest", as.list(mf))
  y$data.name <- DNAME
  y
}



VanWaerdenTest.default <- function (x, g, ...) {
  
  ## This is literally kruskal.test code
  
  if (is.list(x)) {
    if (length(x) < 2L) 
      stop("'x' must be a list with at least 2 elements")
    if (!missing(g)) 
      warning("'x' is a list, so ignoring argument 'g'")
    DNAME <- deparse1(substitute(x))
    x <- lapply(x, function(u) u <- u[complete.cases(u)])
    if (!all(sapply(x, is.numeric))) 
      warning("some elements of 'x' are not numeric and will be coerced to numeric")
    k <- length(x)
    l <- lengths(x)
    if (any(l == 0L)) 
      stop("all groups must contain data")
    g <- factor(rep.int(seq_len(k), l))
    x <- unlist(x)
  }  else {
    if (length(x) != length(g)) 
      stop("'x' and 'g' must have the same length")
    DNAME <- paste(deparse1(substitute(x)), "and", 
                   deparse1(substitute(g)))
    OK <- complete.cases(x, g)
    x <- x[OK]
    g <- g[OK]
    g <- factor(g)
    k <- nlevels(g)
    if (k < 2L) 
      stop("all observations are in the same group")
  }
  
  n <- length(x)
  if (n < 2L) 
    stop("not enough observations")
  r <- rank(x)
  
  z <- qnorm(r/(n + 1))
  
  STATISTIC <- (n - 1) / sum(z^2) * 
    sum(tapply(z, g, sum)^2 / tapply(z, g, length))
  
  PARAMETER <- k - 1L
  PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
  names(STATISTIC) <- "Van-der-Waerden chi-squared"
  names(PARAMETER) <- "df"
  RVAL <- list(statistic = STATISTIC, parameter = PARAMETER, 
               p.value = PVAL, method = "Van-der-Waerden normal scores test", 
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}





PostHocTest <- function (x, ...)
  UseMethod("PostHocTest")



PostHocTest.aov <- function (x, which = NULL,
                             method=c("hsd","bonferroni","lsd","scheffe","newmankeuls","duncan"),
                             conf.level = 0.95, ordered = FALSE, ...) {

  method <- match.arg(method)

  if(method=="scheffe"){
    out <- ScheffeTest(x=x, which=which, conf.level=conf.level, ...)

  } else {

    mm <- model.tables(x, "means")
    if (is.null(mm$n))
      stop("no factors in the fitted model")
    tabs <- mm$tables[-1L]

    if(is.null(which)) which <- seq_along(tabs)
    tabs <- tabs[which]

    nn <- mm$n[names(tabs)]
    nn_na <- is.na(nn)
    if (all(nn_na))
      stop("'which' specified no factors")
    if (any(nn_na)) {
      warning("'which' specified some non-factors which will be dropped")
      tabs <- tabs[!nn_na]
      nn <- nn[!nn_na]
    }
    out <- setNames(vector("list", length(tabs)), names(tabs))
    MSE <- sum(x$residuals^2)/x$df.residual
    for (nm in names(tabs)) {
      tab <- tabs[[nm]]
      means <- as.vector(tab)
      nms <- if (length(d <- dim(tab)) > 1L) {
        dn <- dimnames(tab)
        apply(do.call("expand.grid", dn), 1L, paste, collapse = ":")
      }
      else names(tab)
      n <- nn[[nm]]
      if (length(n) < length(means))
        n <- rep.int(n, length(means))

      # this will be ignored for bonferroni, lsd
      if (method %in% c("hsd", "newmankeuls", "duncan") & as.logical(ordered)) {
        ord <- order(means)
        means <- means[ord]
        n <- n[ord]
        if (!is.null(nms))
          nms <- nms[ord]
      }

      center <- outer(means, means, "-")
      keep <- lower.tri(center)
      center <- center[keep]

      switch(method
             ,"bonferroni" = {
               width <-  qt(1 - (1 - conf.level)/(length(means) * (length(means) - 1)), x$df.residual) *
                 sqrt(MSE * outer(1/n, 1/n, "+"))[keep]
               est <- center/sqrt(MSE * outer(1/n, 1/n, "+")[keep])

               pvals <- pmin(2 * pt(abs(est), df = x$df.residual, lower.tail = FALSE)
                             * ((length(means)^2 - length(means))/2), 1)
               method.str <- "Bonferroni"

             }
             ,"lsd" = {
               width <-  qt(1 - (1 - conf.level)/2, x$df.residual) *
                 sqrt(MSE * outer(1/n, 1/n, "+"))[keep]
               est <- center/sqrt(MSE * outer(1/n, 1/n, "+")[keep])
               pvals <- 2 * pt(abs(est), df = x$df.residual, lower.tail = FALSE)
               method.str <- "Fisher LSD"
             }
             ,"hsd" = {
               width <- qtukey(conf.level, length(means), x$df.residual) *
                 sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep]
               est <- center/(sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep])
               pvals <- ptukey(abs(est), length(means), x$df.residual,
                               lower.tail = FALSE)
               method.str <- "Tukey HSD"

             }
             ,"newmankeuls" ={
               nmean <- (abs(outer(rank(means), rank(means), "-")) + 1)[keep]

               width <- qtukey(conf.level, nmean, x$df.residual) *
                 sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep]

               est <- center/(sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep])

               pvals <- ptukey(abs(est), nmean, x$df.residual, lower.tail = FALSE)
               method.str <- "Newman-Keuls"

             }
             ,"duncan" = {
               # same as newmankeuls, but with bonferroni corrected alpha
               nmean <- (abs(outer(rank(means), rank(means), "-")) + 1)[keep]

               width <- qtukey(conf.level^(nmean-1), nmean, x$df.residual) *
                 sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep]

               est <- center/(sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep])
               pvals <- 1-(1-ptukey(abs(est), nmean, x$df.residual,
                                    lower.tail = FALSE))^(1/(nmean - 1))

               method.str <- "Duncan's new multiple range test"

             }
             ,"dunnett" = {
               method.str <- "Dunnett"
             }
             ,"scottknott" = {
               method.str <- "Scott Knott"
             }
             ,"waller" = {
               method.str <- "Waller"
             }
             ,"gabriel" = {
               method.str <- "Gabriel"
             }
      )

      if(!is.na(conf.level)){
        dnames <- list(NULL, c("diff", "lwr.ci", "upr.ci", "pval"))
        if (!is.null(nms))
          dnames[[1L]] <- outer(nms, nms, paste, sep = "-")[keep]
        out[[nm]] <- array(c(center, center - width,
                             center + width, pvals), c(length(width), 4L), dnames)
      } else {
        out[[nm]] <- matrix(NA, nrow=length(means), ncol=length(means))
        out[[nm]][lower.tri(out[[nm]], diag = FALSE)] <- pvals
        dimnames(out[[nm]]) <- list(nms, nms)
        out[[nm]] <- out[[nm]][-1, -ncol(out[[nm]])]

      }
    }

    class(out) <- c("PostHocTest")
    attr(out, "orig.call") <- x$call
    attr(out, "conf.level") <- conf.level
    attr(out, "ordered") <- ordered
    attr(out, "method") <- method.str
    attr(out, "method.str") <- gettextf("\n  Posthoc multiple comparisons of means : %s \n", attr(out, "method"))

  }

  return(out)

}


PostHocTest.matrix <- function(x, method = c("none","fdr","BH","BY","bonferroni","holm","hochberg","hommel"),
                               conf.level = 0.95, ...) {

  # http://support.sas.com/resources/papers/proceedings14/1544-2014.pdf

  # no conf.level supported so far
  conf.level  <- NA

  method <- match.arg(method)

  #  out <- setNames(vector("list", length(tabs)), names(tabs))

  pvals <- DescTools::PairApply(t(as.matrix(x)), FUN = function(y1, y2) chisq.test(cbind(y1,y2))$p.value, symmetric=TRUE)
  pvals[upper.tri(pvals, diag=TRUE)] <- NA

  if(method != "none")
    pvals[] <- p.adjust(pvals, method=method)

  #  pvals[] <- format.pval(pvals, digits = 2, na.form = "-")
  pvals <- pvals[-1, -ncol(pvals)]
  out <- list()
  out[[deparse(substitute(x))]] <- pvals

  class(out) <- c("PostHocTest")
  attr(out, "orig.call") <- "table"
  attr(out, "conf.level") <- conf.level
  attr(out, "ordered") <- FALSE
  attr(out, "method") <- method
  attr(out, "method.str") <- gettextf("\n  Posthoc multiple comparisons on chi-square test : %s \n", attr(out, "method"))

  return(out)

}


PostHocTest.table <- function(x, method = c("none","fdr","BH","BY","bonferroni","holm","hochberg","hommel"),
                              conf.level = 0.95, ...) {
  class(x) <- "matrix"
  PostHocTest(x, method=method, conf.level=conf.level, ...)
}




print.PostHocTest <- function (x, digits = getOption("digits", 3), ...) {

  cat(attr(x, "method.str"))
  if (!is.na(attr(x, "conf.level")))
    cat("    ", format(100 * attr(x, "conf.level"), 2), "% family-wise confidence level\n",
        sep = "")
  if (attr(x, "ordered"))
    cat("    factor levels have been ordered\n")
  if(!is.language(attr(x, "orig.call")) && !is.null(attr(x, "orig.call")))
    cat("\nFit: ", deparse(attr(x, "orig.call"), 500L), "\n\n", sep = "")
  else
    cat("\n")
  xx <- unclass(x)

  attr(xx, "orig.call") <- attr(xx, "conf.level") <-
    attr(xx, "ordered") <-  attr(xx, "method.str") <-  attr(xx, "method") <- NULL

  xx["data.name"] <- NULL

  if(!is.na(attr(x, "conf.level"))) {
    xx <- lapply(xx, as.data.frame)
    for(nm in names(xx)){
      xx[[nm]]$" " <- Format(xx[[nm]]$"pval", fmt="*")
      xx[[nm]]$"pval" <- format.pval(xx[[nm]]$"pval", digits=2, nsmall=4)
    }

    print.default(xx, digits=digits, ...)
    cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  } else {
    for(nm in names(xx)){
      xx[[nm]][] <- format.pval(xx[[nm]], 2, na.form = "-")
    }
    #     attributes(pp) <- attributes(x$p.value)
    print(xx, digits=digits, quote = FALSE, ...)
  }
  cat("\n")

  invisible(x)
}



plot.PostHocTest <- function(x, ...){
  # original:   stats:::plot.TukeyHSD(x, ...)

  # don't need that here..
  x$data.name <- NULL

  for (i in seq_along(x)) {
    xi <- x[[i]][, -4L, drop = FALSE]
    yvals <- nrow(xi):1L
    dev.hold()
    on.exit(dev.flush())
    plot(c(xi[, "lwr.ci"], xi[, "upr.ci"]), rep.int(yvals, 2L),
         type = "n", axes = FALSE, xlab = "", ylab = "", main = NULL,
         ...)
    axis(1, ...)
    axis(2, at = nrow(xi):1, labels = dimnames(xi)[[1L]],
         srt = 0, ...)
    abline(h = yvals, lty = 1, lwd = 0.5, col = "lightgray")
    abline(v = 0, lty = 2, lwd = 0.5, ...)
    segments(xi[, "lwr.ci"], yvals, xi[, "upr.ci"], yvals, ...)
    segments(as.vector(xi), rep.int(yvals - 0.1, 3L), as.vector(xi),
             rep.int(yvals + 0.1, 3L), ...)
    title(main = paste0(format(100 * attr(x, "conf.level"),
                               digits = 2L), "% family-wise confidence level\n"),
          xlab = paste("Differences in mean levels of", names(x)[i]))
    box()
    dev.flush()
    on.exit()
  }

}



DunnTest <- function (x, ...)
  UseMethod("DunnTest")



DunnTest.formula <- function (formula, data, subset, na.action, ...) {

  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame())
  if (length(mf) > 2L)
    stop("'formula' should be of the form response ~ group")
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  y <- DoCall("DunnTest", c(as.list(mf), list(...)))
  y$data.name <- DNAME
  y
}




DunnTest.default <- function (x, g, method = c("holm","hochberg","hommel","bonferroni","BH","BY","fdr","none"),
                              alternative = c("two.sided", "less", "greater"),
                              out.list = TRUE, ...) {

  alternative <- match.arg(alternative)

  if (is.list(x)) {
    if (length(x) < 2L)
      stop("'x' must be a list with at least 2 elements")
    DNAME <- deparse(substitute(x))
    x <- lapply(x, function(u) u <- u[complete.cases(u)])
    k <- length(x)
    l <- sapply(x, "length")
    if (any(l == 0))
      stop("all groups must contain data")
    g <- factor(rep(1:k, l))
    x <- unlist(x)
  }
  else {
    if (length(x) != length(g))
      stop("'x' and 'g' must have the same length")
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
    OK <- complete.cases(x, g)
    x <- x[OK]
    g <- g[OK]
    if (!all(is.finite(g)))
      stop("all group levels must be finite")
    g <- factor(g)
    k <- nlevels(g)
    if (k < 2)
      stop("all observations are in the same group")
  }
  N <- length(x)
  if (N < 2)
    stop("not enough observations")

  method <- match.arg(method)

  nms <- levels(g)

  n <- tapply(g, g, length)
  rnk <- rank(x)
  mrnk <- tapply(rnk, g, mean)

  tau <- table(rnk[AllDuplicated(rnk)])
  tiesadj <- sum(tau^3 - tau) / (12*(N-1))
  mrnkdiff <- outer(mrnk, mrnk, "-")

  z <- mrnkdiff / sqrt( ((N*(N+1)/12) - tiesadj) * outer(1/n, 1/n, "+"))

  # extension for alternative in v. 0.99.16:
  if (alternative == "less") {
    pvals <- pnorm(abs(z))
  }
  else if (alternative == "greater") {
    pvals <- pnorm(abs(z), lower.tail=FALSE)
  }
  else {
    pvals <- 2 * pnorm(abs(z), lower.tail=FALSE)
  }


  keep <- lower.tri(pvals)
  pvals <- pvals[keep]
  m <- sum(keep)

  out <- list()

  pvals <- p.adjust(pvals, method=method)
  method.str <- method

  if(out.list){
    dnames <- list(NULL, c("mean rank diff", "pval"))
    if (!is.null(nms))
      dnames[[1L]] <- outer(nms, nms, paste, sep = "-")[keep]
    out[[1]] <- array(c(mrnkdiff[keep], pvals), c(length(mrnkdiff[keep]), 2L), dnames)

  } else {
    out[[1]] <- matrix(NA, nrow=length(nms), ncol=length(nms))
    out[[1]][lower.tri(out[[1]], diag = FALSE)] <- pvals
    dimnames(out[[1]]) <- list(nms, nms)
    out[[1]] <- out[[1]][-1, -ncol(out[[1]])]

  }

  class(out) <- c("DunnTest")
  attr(out, "main") <- gettextf("Dunn's test of multiple comparisons using rank sums : %s ", method.str)
  attr(out, "method") <- method.str
  attr(out, "out.list") <- out.list

  return(out)

}




print.DunnTest <- function (x, digits = getOption("digits", 3), ...) {

  cat("\n", attr(x, "main"), "\n\n")
  xx <- unclass(x)

  if(attr(x, "out.list")==TRUE) {
    xx <- data.frame(x[1])
    xx$" " <- Format(xx$"pval", fmt="*")
    xx$"pval" <- format.pval(xx$"pval", digits=2, nsmall=4)

    print.data.frame(xx, digits=digits, ...)
    cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  } else {
    xx[[1]][] <- format.pval(xx[[1]], 2, na.form = "-")
    #     attributes(pp) <- attributes(x$p.value)
    print(xx[[1]], digits=digits, quote = FALSE, ...)
  }
  cat("\n")

  invisible(x)
}



ConoverTest <- function (x, ...)
  UseMethod("ConoverTest")


ConoverTest.default <- function (x, g,
  method = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
  alternative = c("two.sided", "less", "greater"), out.list = TRUE, ...) {

  alternative <- match.arg(alternative)

  if (is.list(x)) {
    if (length(x) < 2L)
      stop("'x' must be a list with at least 2 elements")
    DNAME <- deparse(substitute(x))
    x <- lapply(x, function(u) u <- u[complete.cases(u)])
    k <- length(x)
    l <- sapply(x, "length")
    if (any(l == 0))
      stop("all groups must contain data")
    g <- factor(rep(1:k, l))
    x <- unlist(x)
  } else {
    if (length(x) != length(g))
      stop("'x' and 'g' must have the same length")
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
    OK <- complete.cases(x, g)
    x <- x[OK]
    g <- g[OK]
    if (!all(is.finite(g)))
      stop("all group levels must be finite")
    g <- factor(g)
    k <- nlevels(g)
    if (k < 2)
      stop("all observations are in the same group")
  }

  N <- length(x)
  if (N < 2)
    stop("not enough observations")
  method <- match.arg(method)
  nms <- levels(g)
  n <- tapply(g, g, length)
  rnk <- rank(x)
  mrnk <- tapply(rnk, g, mean)
  tau <- table(rnk[AllDuplicated(rnk)])
  tiesadj <- 1-sum(tau^3 - tau)/(N^3 - N)
  mrnkdiff <- outer(mrnk, mrnk, "-")

  # Kruskal-Wallis H statistic
  H <- (12 / (N * (N + 1))) * sum(tapply(rnk, g, sum)^2 / n) - 3 * (N + 1)
  if (tiesadj == 1) {
    s2 <- N * (N + 1) / 12
  } else {
    s2 <-   ( 1 / (N - 1)) * (sum(rnk^2) - (N * (((N + 1)^2) / 4)))
  }

  tval <- mrnkdiff/sqrt(s2 * ((N - 1 - H/tiesadj) / (N - k)) * outer(1/n, 1/n, "+"))

  if (alternative == "less") {
    pvals <- pt(abs(tval), df=N - k)

  } else if (alternative == "greater") {
    pvals <- pt(abs(tval), df=N - k, lower.tail = FALSE)

  } else {
    pvals <- 2 * pt(abs(tval), df=N - k, lower.tail = FALSE)

  }

  keep <- lower.tri(pvals)
  pvals <- pvals[keep]
  m <- sum(keep)
  out <- list()
  pvals <- p.adjust(pvals, method = method)
  method.str <- method
  if (out.list) {
    dnames <- list(NULL, c("mean rank diff", "pval"))
    if (!is.null(nms))
      dnames[[1L]] <- outer(nms, nms, paste, sep = "-")[keep]
    out[[1]] <- array(c(mrnkdiff[keep], pvals), c(length(mrnkdiff[keep]),
                                                  2L), dnames)
  } else {
    out[[1]] <- matrix(NA, nrow = length(nms), ncol = length(nms))
    out[[1]][lower.tri(out[[1]], diag = FALSE)] <- pvals
    dimnames(out[[1]]) <- list(nms, nms)
    out[[1]] <- out[[1]][-1, -ncol(out[[1]])]
  }

  class(out) <- c("ConoverTest", "DunnTest")
  attr(out, "main") <- gettextf("Conover's test of multiple comparisons : %s ",
                                method.str)
  attr(out, "method") <- method.str
  attr(out, "out.list") <- out.list

  return(out)

}




ConoverTest.formula <- function (formula, data, subset, na.action, ...) {

  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame())
  if (length(mf) > 2L)
    stop("'formula' should be of the form response ~ group")
  DNAME <- paste(names(mf), collapse = " by ")

  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  y <- DoCall("ConoverTest", c(as.list(mf), list(...)))
  y$data.name <- DNAME

  y

}




# Test  NemenyiTest
#
# d.frm <- data.frame(x=c(28,30,33,35,38,41, 36,39,40,43,45,50, 44,45,47,49,53,54),
#                     g=c(rep(LETTERS[1:3], each=6)))
# NemenyiTest(x~g, d.frm)
#
# library(coin)
# library(multcomp)
# nem <- oneway_test(x ~ g, data=d.frm,
#                    ytrafo = function(data) trafo(data, numeric_trafo=rank),
#                    xtrafo = function(data) trafo(data, factor_trafo = function(x)
#                      model.matrix(~x - 1) %*% t(contrMat(table(x), "Tukey"))),
#                    teststat="max")
# nem
# pvalue(nem, method="single-step")


NemenyiTest <- function (x, ...)
  UseMethod("NemenyiTest")


NemenyiTest.formula <- function (formula, data, subset, na.action, ...) {

  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame())
  if (length(mf) > 2L)
    stop("'formula' should be of the form response ~ group")
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  y <- DoCall("NemenyiTest", c(as.list(mf), list(...)))
  y$data.name <- DNAME
  y
}




NemenyiTest.default <- function (x, g,
                                 dist = c("tukey", "chisq"), out.list = TRUE, ...) {

  if (is.list(x)) {
    if (length(x) < 2L)
      stop("'x' must be a list with at least 2 elements")
    DNAME <- deparse(substitute(x))
    x <- lapply(x, function(u) u <- u[complete.cases(u)])
    k <- length(x)
    l <- sapply(x, "length")
    if (any(l == 0))
      stop("all groups must contain data")
    g <- factor(rep(1:k, l))
    x <- unlist(x)
  }
  else {
    if (length(x) != length(g))
      stop("'x' and 'g' must have the same length")
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
    OK <- complete.cases(x, g)
    x <- x[OK]
    g <- g[OK]
    if (!all(is.finite(g)))
      stop("all group levels must be finite")
    g <- factor(g)
    k <- nlevels(g)
    if (k < 2)
      stop("all observations are in the same group")
  }
  N <- length(x)
  if (N < 2)
    stop("not enough observations")

  dist <- match.arg(dist, c("tukey", "chisq"))

  nms <- levels(g)

  n <- tapply(g, g, length)
  rnk <- rank(x)
  mrnk <- tapply(rnk, g, mean)

  tau <- table(rnk[AllDuplicated(rnk)])
  tiesadj <- min(1, 1 - sum(tau^3 - tau) / (N^3 - N))
  mrnkdiff <- outer(mrnk, mrnk, "-")

  if(dist == "chisq"){
    chi <- mrnkdiff^2 / ((N*(N+1)/12) * outer(1/n, 1/n, "+"))
    pvals <- pchisq(tiesadj * chi, df=k-1, lower.tail=FALSE)
  } else {
    z <- abs(mrnkdiff) / sqrt( (N*(N+1)/12) * outer(1/n, 1/n, "+"))
    pvals <- ptukey(z * sqrt(2), nmeans=k, df=Inf, lower.tail=FALSE)
  }


  keep <- lower.tri(pvals)
  pvals <- pvals[keep]
  m <- sum(keep)

  out <- list()

  # no p.adjustment in this test
  # pvals <- p.adjust(pvals, method=method)
  method.str <- "none" #method

  if(out.list){
    dnames <- list(NULL, c("mean rank diff", "pval"))
    if (!is.null(nms))
      dnames[[1L]] <- outer(nms, nms, paste, sep = "-")[keep]
    out[[1]] <- array(c(mrnkdiff[keep], pvals), c(length(mrnkdiff[keep]), 2L), dnames)

  } else {
    out[[1]] <- matrix(NA, nrow=length(nms), ncol=length(nms))
    out[[1]][lower.tri(out[[1]], diag = FALSE)] <- pvals
    dimnames(out[[1]]) <- list(nms, nms)
    out[[1]] <- out[[1]][-1, -ncol(out[[1]])]

  }

  class(out) <- c("DunnTest")
  attr(out, "main") <- gettextf("Nemenyi's test of multiple comparisons for independent samples (%s) ", dist)
  attr(out, "method") <- method.str
  attr(out, "out.list") <- out.list

  return(out)

}




DunnettTest <- function (x, ...)
  UseMethod("DunnettTest")



DunnettTest.formula <- function (formula, data, subset, na.action, ...) {

  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame())
  if (length(mf) > 2L)
    stop("'formula' should be of the form response ~ group")
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  y <- DoCall("DunnettTest", c(as.list(mf), list(...)))
  y$data.name <- DNAME
  y
}



DunnettTest.default <- function (x, g, control = NULL
                                 , conf.level = 0.95, ...) {

  if (is.list(x)) {
    if (length(x) < 2L)
      stop("'x' must be a list with at least 2 elements")
    DNAME <- deparse(substitute(x))
    x <- lapply(x, function(u) u <- u[complete.cases(u)])
    k <- length(x)
    l <- sapply(x, "length")
    if (any(l == 0))
      stop("all groups must contain data")
    g <- factor(rep(1:k, l))
    x <- unlist(x)
  } else {
    if (length(x) != length(g))
      stop("'x' and 'g' must have the same length")
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
    OK <- complete.cases(x, g)
    x <- x[OK]
    g <- g[OK]
    if (!all(is.finite(g)))
      stop("all group levels must be finite")
    g <- factor(g)
    k <- nlevels(g)
    if (k < 2)
      stop("all observations are in the same group")
  }
  N <- length(x)
  if (N < 2)
    stop("not enough observations")

  # just organisational stuff so far, got a fine x and g now

  if (is.null(control)) control <- levels(g)[1]

  ctrls <- control
  out <- list()

  for(ii in seq_along(ctrls)){

    control <- ctrls[ii]

    ni <- tapply(x, g, length)

    means <- tapply(x, g, mean)
    meandiffs <- means[names(means) != control] - means[control]

    fittedn <- ni[names(ni) != control]
    controln <- ni[control]

    s <- sqrt( sum(tapply(x, g, function(x) sum((x - mean(x))^2) )) /
                 (N - k))

    Dj <- meandiffs / (s * sqrt((1/fittedn) + (1/controln)))
    Rij <- sqrt(fittedn/(fittedn + controln))

    R <- outer(Rij, Rij, "*")
    diag(R) <- 1

    set.seed(5)  # for getting consistent results every run
    qvt <- mvtnorm::qmvt((1 - (1 - conf.level)/2), df = N - k, sigma = R, tail = "lower.tail")$quantile

    lower <- meandiffs - s * sqrt((1/fittedn) + (1/controln)) * qvt
    upper <- meandiffs + s * sqrt((1/fittedn) + (1/controln)) * qvt

    pval <- c()
    for (i in 1:(k-1)){
      pval[i] <- 1 - mvtnorm::pmvt(-abs(Dj[i]), abs(Dj[i]), corr=R, delta=rep(0, k-1), df=N - k)[1]
    }

    out[[ii]] <- cbind(diff=meandiffs, lower, upper, pval)
    dimnames(out[[ii]]) <- list(paste(names(meandiffs), control, sep="-"), c("diff", "lwr.ci", "upr.ci","pval"))
  }

  names(out) <- ctrls

  class(out) <- c("PostHocTest")
  #  attr(out, "orig.call") <- NA
  attr(out, "conf.level") <- conf.level
  attr(out, "ordered") <- FALSE
  attr(out, "method") <- ""
  attr(out, "method.str") <- gettextf("\n  Dunnett's test for comparing several treatments with a control : %s \n", attr(out, "method"))

  return(out)

}




HotellingsT2Test <- function(x,...) {
  UseMethod("HotellingsT2Test")
}

HotellingsT2Test.default <- function(x, y=NULL, mu=NULL, test="f",...) {


  `HotellingsT.internal`  <-  function(x, y=NULL, mu, test) {
    n <- dim(x)[1]
    p <- dim(x)[2]

    if(is.null(y))     #one sample case
    {
      test.statistic <- n*as.numeric(t(colMeans(x)-mu)%*%solve(cov(x))%*%(colMeans(x)-mu))*switch(test,f=(n-p)/(p*(n-1)),chi=1)
      df.1 <- p
      df.2 <- switch(test,f=n-p,chi=NA)
      p.value <- 1-switch(test,f=pf(test.statistic,df.1,df.2),chi=pchisq(test.statistic,df.1))
      return(list(test.statistic=test.statistic,p.value=p.value,df.1=df.1,df.2=df.2))
    }

    # else two sample case
    n1 <- n
    n2 <- dim(y)[1]
    xmeans <- colMeans(x)
    ymeans <- colMeans(y)
    x.diff <- sweep(x,2,xmeans)
    y.diff <- sweep(y,2,ymeans)
    S.pooled <- 1/(n1+n2-2)*(t(x.diff)%*%x.diff+t(y.diff)%*%y.diff)
    test.statistic <- n1*n2/(n1+n2)*t(xmeans-ymeans-mu)%*%solve(S.pooled)%*%(xmeans-ymeans-mu)*switch(test,f=(n1+n2-p-1)/(p*(n1+n2-2)),chi=1)
    df.1 <- p
    df.2 <- switch(test,f=n1+n2-p-1,chi=NA)
    p.value <- 1-switch(test,f=pf(test.statistic,df.1,df.2),chi=pchisq(test.statistic,df.1))
    list(test.statistic=test.statistic,p.value=p.value,df.1=df.1,df.2=df.2)
  }


  if (is.null(y)) {
    DNAME <- deparse(substitute(x))
  } else {
    DNAME=paste(deparse(substitute(x)),"and",deparse(substitute(y)))
  }

  xok <- complete.cases(x)
  x <- x[xok,]
  if(!all(sapply(x, is.numeric))) stop("'x' must be numeric")
  x <- as.matrix(x)

  p <- dim(x)[2]

  if (!is.null(y)) {
    yok <- complete.cases(y)
    y <- y[yok,]

    if(!all(sapply(y, is.numeric))) stop("'y' must be numeric")
    if (p!=dim(y)[2]) stop("'x' and 'y' must have the same number of columns")
    y <- as.matrix(y)
  }

  if (is.null(mu)) mu <- rep(0,p)
  else if (length(mu)!=p) stop("length of 'mu' must equal the number of columns of 'x'")

  test <- match.arg(test,c("f","chi"))

  if (is.null(y) & test=="f") version <- "one.sample.f"
  if (is.null(y) & test=="chi") version <- "one.sample.chi"
  if (!is.null(y) & test=="f") version <- "two.sample.f"
  if (!is.null(y) & test=="chi") version <- "two.sample.chi"

  res1 <- switch(version,
                 "one.sample.f"={
                   result <- HotellingsT.internal(x,mu=mu,test=test)
                   STATISTIC <- result$test.statistic
                   names(STATISTIC) <- "T.2"
                   PVAL <- result$p.value
                   METHOD <- "Hotelling's one sample T2-test"
                   PARAMETER <- c(result$df.1,result$df.2)
                   names(PARAMETER) <- c("df1","df2")
                   RVAL <- list(statistic=STATISTIC,p.value=PVAL,method=METHOD,parameter=PARAMETER)

                   RVAL}
                 ,
                 "one.sample.chi"={
                   result <- HotellingsT.internal(x,mu=mu,test=test)
                   STATISTIC <- result$test.statistic
                   names(STATISTIC) <- "T.2"
                   PVAL <- result$p.value
                   METHOD <- "Hotelling's one sample T2-test"
                   PARAMETER <- c(result$df.1)
                   names(PARAMETER) <- c("df")
                   RVAL <- list(statistic=STATISTIC,p.value=PVAL,method=METHOD,parameter=PARAMETER)

                   RVAL}
                 ,
                 "two.sample.f"={
                   result <- HotellingsT.internal(x,y,mu,test)
                   STATISTIC <- result$test.statistic
                   names(STATISTIC) <- "T.2"
                   PVAL <- result$p.value
                   METHOD <- "Hotelling's two sample T2-test"
                   PARAMETER <- c(result$df.1,result$df.2)
                   names(PARAMETER) <- c("df1","df2")
                   RVAL <- list(statistic=STATISTIC,p.value=PVAL,method=METHOD,parameter=PARAMETER)

                   RVAL}
                 ,
                 "two.sample.chi"={
                   result <- HotellingsT.internal(x,y,mu,test)
                   STATISTIC <- result$test.statistic
                   names(STATISTIC) <- "T.2"
                   PVAL <- result$p.value
                   METHOD <- "Hotelling's two sample T2-test"
                   PARAMETER <- c(result$df.1)
                   names(PARAMETER) <- c("df")
                   RVAL <- list(statistic=STATISTIC,p.value=PVAL,method=METHOD,parameter=PARAMETER)

                   RVAL}
  )
  ALTERNATIVE="two.sided"
  NVAL <- paste("c(",paste(mu,collapse=","),")",sep="")
  if (is.null(y)) names(NVAL) <- "location" else names(NVAL) <- "location difference"
  res <- c(res1,list(data.name=DNAME,alternative=ALTERNATIVE,null.value=NVAL))
  class(res) <- "htest"
  return(res)
}


HotellingsT2Test.formula <- function (formula, data, subset, na.action, ...) {

  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame())
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  g <- factor(mf[[-response]])
  if (nlevels(g) != 2L)
    stop("grouping factor must have exactly 2 levels")
  # DATA <- setNames(split(mf[[response]], g), c("x", "y"))
  DATA <- setNames(split(as.data.frame(mf[[response]]), g), c("x", "y"))

  y <- DoCall("HotellingsT2Test", c(DATA, list(...)))
  y$data.name <- DNAME
  y
}


###



HosmerLemeshowTest <- function (fit, obs, ngr = 10, X, verbose = FALSE){

  # woher kommt das?? -> klaeren!
  # - > MKmisc

  ngr1 <- ngr
  # Hosmer-Lemeshow C statistic
  brks <- unique(quantile(fit, probs = seq(0, 1, by = 1/ngr)))
  cutfit <- cut(fit, breaks = brks, include.lowest = TRUE)
  if(length(brks) < ngr+1){
    warning("Found only ", length(brks)-1, " different groups for Hosmer-Lemesho C statistic.")
    ngr <- length(brks)-1
  }
  if(verbose){
    cat("Groups for Hosmer-Lemeshow C statistic:\n")
    print(table(cutfit))
  }
  Obs <- xtabs(cbind("0s" = 1 - obs, "1s" = obs) ~ cutfit)
  Exp <- xtabs(cbind("Os" = 1 - fit, "1s" = fit) ~ cutfit)
  chisq <- sum((Obs - Exp)^2/Exp, na.rm = TRUE)
  names(chisq) <- "X-squared"
  param <- ngr-2
  names(param) <- "df"
  P <- 1 - pchisq(chisq, param)

  # Hosmer-Lemeshow H statistic
  cutfit1 <- cut(fit, breaks = ngr1, include.lowest = TRUE)
  if(verbose){
    cat("Groups for Hosmer-Lemeshow H statistic:\n")
    print(table(cutfit1))
  }
  Obs1 <- xtabs(cbind(1 - obs, obs) ~ cutfit1)
  Exp1 <- xtabs(cbind(1 - fit, fit) ~ cutfit1)
  chisq1 <- sum((Obs1 - Exp1)^2/Exp1, na.rm = TRUE)
  names(chisq1) <- "X-squared"
  param1 <- ngr1-2
  names(param1) <- "df"
  P1 <- 1 - pchisq(chisq1, param1)
  dname <- paste(deparse(substitute(fit)), "and", deparse(substitute(obs)))
  C <- structure(list(statistic = chisq, parameter = param,
                      p.value = P, method = "Hosmer-Lemeshow C statistic", data.name = dname,
                      observed = Obs, expected = Exp), class = "htest")
  H <- structure(list(statistic = chisq1, parameter = param1,
                      p.value = P1, method = "Hosmer-Lemeshow H statistic", data.name = dname,
                      observed = Obs1, expected = Exp1), class = "htest")


  if(!missing(X)){
    # le Cessie-van Houwelingen-Copas-Hosmer unweighted sum of squares test for global goodness of fit
    #        X <- cbind(1, X)
    y <- obs == 1
    p <- fit
    sse <- sum((y - p)^2)
    wt <- p * (1 - p)
    d <- 1 - 2 * p
    z <- lm.wfit(X, d, wt, method = "qr")
    res <- z$residuals * sqrt(z$weights)
    sd <- sqrt(sum(res^2))
    ev <- sum(wt)
    z <- (sse - ev)/sd
    names(z) <- "z"
    P2 <- 2 * pnorm(abs(z), lower.tail = FALSE)
    stats <- c(sse, ev, sd, z, P)
    names(stats) <- c("Sum of squared errors", "Expected value|H0",
                      "SD", "Z", "P")
    gof <- structure(list(statistic = z, p.value = P2,
                          method = "le Cessie-van Houwelingen-Copas-Hosmer global goodness of fit test",
                          data.name = dname,
                          observed = sse, expected = ev), class = "htest")

    return(list(C = C, H = H, gof = gof))
  }

  list(C = C, H = H)
}




