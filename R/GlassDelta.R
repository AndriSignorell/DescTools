

# # http://www.stata.com/videos13/data/webclass.dta
# webclass <- as.data.frame(haven::read_dta("http://www.stata.com/videos13/data/webclass.dta"))
# 
# x <- webclass$math[webclass$treated==0]
# y <- webclass$math[webclass$treated==1]
# 
# CohenD(webclass$math[webclass$treated==0],
#        webclass$math[webclass$treated==1], conf.level=0.95) 
# CohenD(webclass$math[webclass$treated==0],
#        webclass$math[webclass$treated==1]) 
# 
# CohenD(webclass$math[webclass$treated==0], correct=TRUE, 
#        webclass$math[webclass$treated==1]) 
# CohenD(webclass$math[webclass$treated==0], correct=TRUE, 
#        webclass$math[webclass$treated==1], conf.level=0.95) 


# GlassDelta(webclass$math[webclass$treated==0],
#            webclass$math[webclass$treated==1]) 
# GlassDelta(webclass$math[webclass$treated==0],
#            webclass$math[webclass$treated==1], conf.level=0.95) 
# 
# 
# GlassDelta(webclass$math[webclass$treated==0], use_control_sd = F, 
#            webclass$math[webclass$treated==1], conf.level=0.95) 
# 


GlassDelta <- function(x, y, conf.level=NULL, use_control_sd=TRUE, na.rm=FALSE){
  
  if(use_control_sd)
    .sd <- sd(y, na.rm=na.rm)
  else
    .sd <- sd(x, na.rm=na.rm)

  delta <- (mean(x, na.rm=na.rm) - mean(y, na.rm=na.rm))/.sd
  
  if(!is.null(conf.level)){
    nx <- length(x)
    ny <- length(y)
    
    ci <- ci.smd.c(smd.c = delta, 
                   n.C = nx, n.E = ny, 
                   conf.level=.95)
    res <- SetNames(unlist(ci[c(2,1,3)]), names=c("delta", "lwr.ci", "upr.ci"))
    
  } else {
    res <- delta
  }
  return(res)
}



# library(MBESS)
# author: Ken Kelley (University of Notre Dame; KKelley@ND.Edu)

ci.smd.c <- function (ncp = NULL, smd.c = NULL, n.C = NULL, n.E = NULL, conf.level = 0.95, 
          alpha.lower = NULL, alpha.upper = NULL, tol = 0.000000001, 
          ...) {
  
  if (is.null(ncp) & is.null(smd.c)) 
    stop("You must specify either the estimated noncentral parameter 'ncp' (generally the observed t-statistic) or the standardized mean difference 'smd.c' (as might be obtained from the 'smd.s' function.).", 
         call. = FALSE)
  if (length(ncp) == 1 & length(smd.c) == 1) 
    stop("You only need to specify either 'ncp' or 'smd.c', not both.", 
         call. = FALSE)
  if (is.null(n.C) | is.null(n.E)) 
    stop("You must specify sample size per group in order to determine confidence limits.", 
         call. = FALSE)
  if (!is.null(conf.level) & conf.level >= 1) 
    stop("There is a problem with your confidence level.", 
         call. = FALSE)
  if (is.null(conf.level) & sum(alpha.lower, alpha.upper) >= 
      1) 
    stop("There is a problem with your upper and or lower confidence limits.", 
         call. = FALSE)
  if (is.null(smd.c)) 
    smd.c <- ncp * sqrt((n.C + n.E)/(n.C * n.C))
  df <- n.C - 1
  if (length(ncp) == 1) {
    Limits <- conf.limits.nct(ncp, df, conf.level = conf.level, 
                              alpha.lower = alpha.lower, alpha.upper = alpha.upper, 
                              tol = tol)
    Limits.L <- Limits$Lower.Limit
    Limits.U <- Limits$Upper.Limit
    Lower.Conf.Limit <- Limits.L * sqrt((n.C + n.E)/(n.C * 
                                                       n.E))
    Upper.Conf.Limit <- Limits.U * sqrt((n.C + n.E)/(n.C * 
                                                       n.E))
    Result <- list(Lower.Conf.Limit.smd.c = Lower.Conf.Limit, 
                   smd.c = smd.c, Upper.Conf.Limit.smd.c = Upper.Conf.Limit)
    return(Result)
  }
  if (length(smd.c) == 1) {
    ncp <- smd.c * sqrt((n.C * n.E)/(n.C + n.E))
    Limits <- conf.limits.nct(ncp, df, conf.level = conf.level, 
                              alpha.lower = alpha.lower, alpha.upper = alpha.upper, 
                              tol = tol)
    Limits.L <- Limits$Lower.Limit
    Limits.U <- Limits$Upper.Limit
    Lower.Conf.Limit <- Limits.L * sqrt((n.C + n.E)/(n.C * 
                                                       n.E))
    Upper.Conf.Limit <- Limits.U * sqrt((n.C + n.E)/(n.C * 
                                                       n.E))
    Result <- list(Lower.Conf.Limit.smd.c = Lower.Conf.Limit, 
                   smd.c = smd.c, Upper.Conf.Limit.smd.c = Upper.Conf.Limit)
    return(Result)
  }
}


conf.limits.nct <- function (ncp, df, conf.level = 0.95, alpha.lower = NULL, alpha.upper = NULL, 
          t.value, tol = 0.000000001, sup.int.warns = TRUE, ...) {
  
  if (missing(ncp)) {
    if (missing(t.value)) 
      stop("You need to specify either 'ncp' or its alias, 't.value,' you have not specified either")
    ncp <- t.value
  }
  if (df <= 0) 
    stop("The degrees of freedom must be some positive value.", 
         call. = FALSE)
  if (abs(ncp) > 37.62) 
    print("The observed noncentrality parameter of the noncentral t-distribution has exceeded 37.62 in magnitude (R's limitation for accurate probabilities from the noncentral t-distribution) in the function's iterative search for the appropriate value(s). The results may be fine, but they might be inaccurate; use caution.")
  if (sup.int.warns == TRUE) 
    Orig.warn <- options()$warn
  options(warn = -1)
  if (!is.null(conf.level) & is.null(alpha.lower) & !is.null(alpha.upper)) 
    stop("You must choose either to use 'conf.level' or define the 'lower.alpha' and 'upper.alpha' values; here, 'upper.alpha' is specified but 'lower.alpha' is not", 
         call. = FALSE)
  if (!is.null(conf.level) & !is.null(alpha.lower) & is.null(alpha.upper)) 
    stop("You must choose either to use 'conf.level' or define the 'lower.alpha' and 'upper.alpha' values; here, 'lower.alpha' is specified but 'upper.alpha' is not", 
         call. = FALSE)
  if (!is.null(conf.level) & is.null(alpha.lower) & is.null(alpha.upper)) {
    alpha.lower <- (1 - conf.level)/2
    alpha.upper <- (1 - conf.level)/2
  }
  .conf.limits.nct.M1 <- function(ncp, df, conf.level = NULL, 
                                  alpha.lower, alpha.upper, tol = 0.000000001, sup.int.warns = TRUE, 
                                  ...) {
    if (sup.int.warns == TRUE) 
      Orig.warn <- options()$warn
    options(warn = -1)
    min.ncp = min(-150, -5 * ncp)
    max.ncp = max(150, 5 * ncp)
    .ci.nct.lower <- function(val.of.interest, ...) {
      (qt(p = alpha.lower, df = df, ncp = val.of.interest, 
          lower.tail = FALSE, log.p = FALSE) - ncp)^2
    }
    .ci.nct.upper <- function(val.of.interest, ...) {
      (qt(p = alpha.upper, df = df, ncp = val.of.interest, 
          lower.tail = TRUE, log.p = FALSE) - ncp)^2
    }
    if (alpha.lower != 0) {
      if (sup.int.warns == TRUE) 
        Low.Lim <- suppressWarnings(optimize(f = .ci.nct.lower, 
                                             interval = c(min.ncp, max.ncp), alpha.lower = alpha.lower, 
                                             df = df, ncp = ncp, maximize = FALSE, tol = tol))
      if (sup.int.warns == FALSE) 
        Low.Lim <- optimize(f = .ci.nct.lower, interval = c(min.ncp, 
                                                            max.ncp), alpha.lower = alpha.lower, df = df, 
                            ncp = ncp, maximize = FALSE, tol = tol)
    }
    if (alpha.upper != 0) {
      if (sup.int.warns == TRUE) 
        Up.Lim <- suppressWarnings(optimize(f = .ci.nct.upper, 
                                            interval = c(min.ncp, max.ncp), alpha.upper = alpha.upper, 
                                            df = df, ncp = ncp, maximize = FALSE, tol = tol))
      if (sup.int.warns == FALSE) 
        Up.Lim <- optimize(f = .ci.nct.upper, interval = c(min.ncp, 
                                                           max.ncp), alpha.upper = alpha.upper, df = df, 
                           ncp = ncp, maximize = FALSE, tol = tol)
    }
    if (alpha.lower == 0) 
      Result <- list(Lower.Limit = -Inf, Prob.Less.Lower = 0, 
                     Upper.Limit = Up.Lim$minimum, Prob.Greater.Upper = pt(q = ncp, 
                                                                           ncp = Up.Lim$minimum, df = df))
    if (alpha.upper == 0) 
      Result <- list(Lower.Limit = Low.Lim$minimum, Prob.Less.Lower = pt(q = ncp, 
                                                                         ncp = Low.Lim$minimum, df = df, lower.tail = FALSE), 
                     Upper.Limit = Inf, Prob.Greater.Upper = 0)
    if (alpha.lower != 0 & alpha.upper != 0) 
      Result <- list(Lower.Limit = Low.Lim$minimum, Prob.Less.Lower = pt(q = ncp, 
                                                                         ncp = Low.Lim$minimum, df = df, lower.tail = FALSE), 
                     Upper.Limit = Up.Lim$minimum, Prob.Greater.Upper = pt(q = ncp, 
                                                                           ncp = Up.Lim$minimum, df = df))
    if (sup.int.warns == TRUE) 
      options(warn = Orig.warn)
    return(Result)
  }
  .conf.limits.nct.M2 <- function(ncp, df, conf.level = NULL, 
                                  alpha.lower, alpha.upper, tol = 0.000000001, sup.int.warns = TRUE, 
                                  ...) {
    .ci.nct.lower <- function(val.of.interest, ...) {
      (qt(p = alpha.lower, df = df, ncp = val.of.interest, 
          lower.tail = FALSE, log.p = FALSE) - ncp)^2
    }
    .ci.nct.upper <- function(val.of.interest, ...) {
      (qt(p = alpha.upper, df = df, ncp = val.of.interest, 
          lower.tail = TRUE, log.p = FALSE) - ncp)^2
    }
    if (sup.int.warns == TRUE) {
      Low.Lim <- suppressWarnings(nlm(f = .ci.nct.lower, 
                                      p = ncp, ...))
      Up.Lim <- suppressWarnings(nlm(f = .ci.nct.upper, 
                                     p = ncp, ...))
    }
    if (sup.int.warns == FALSE) {
      Low.Lim <- nlm(f = .ci.nct.lower, p = ncp, ...)
      Up.Lim <- nlm(f = .ci.nct.upper, p = ncp, ...)
    }
    if (alpha.lower == 0) 
      Result <- list(Lower.Limit = -Inf, Prob.Less.Lower = 0, 
                     Upper.Limit = Up.Lim$estimate, Prob.Greater.Upper = pt(q = ncp, 
                                                                            ncp = Up.Lim$estimate, df = df))
    if (alpha.upper == 0) 
      Result <- list(Lower.Limit = Low.Lim$estimate, Prob.Less.Lower = pt(q = ncp, 
                                                                          ncp = Low.Lim$estimate, df = df, lower.tail = FALSE), 
                     Upper.Limit = Inf, Prob.Greater.Upper = 0)
    if (alpha.lower != 0 & alpha.upper != 0) 
      Result <- list(Lower.Limit = Low.Lim$estimate, Prob.Less.Lower = pt(q = ncp, 
                                                                          ncp = Low.Lim$estimate, df = df, lower.tail = FALSE), 
                     Upper.Limit = Up.Lim$estimate, Prob.Greater.Upper = pt(q = ncp, 
                                                                            ncp = Up.Lim$estimate, df = df))
    return(Result)
  }
  Res.M1 <- Res.M2 <- NULL
  try(Res.M1 <- .conf.limits.nct.M1(ncp = ncp, df = df, conf.level = NULL, 
                                    alpha.lower = alpha.lower, alpha.upper = alpha.upper, 
                                    tol = tol, sup.int.warns = sup.int.warns), silent = TRUE)
  if (length(Res.M1) != 4) 
    Res.M1 <- NULL
  try(Res.M2 <- .conf.limits.nct.M2(ncp = ncp, df = df, conf.level = NULL, 
                                    alpha.lower = alpha.lower, alpha.upper = alpha.upper, 
                                    tol = tol, sup.int.warns = sup.int.warns), silent = TRUE)
  if (length(Res.M2) != 4) 
    Res.M2 <- NULL
  Low.M1 <- Res.M1$Lower.Limit
  Prob.Low.M1 <- Res.M1$Prob.Less.Lower
  Upper.M1 <- Res.M1$Upper.Limit
  Prob.Upper.M1 <- Res.M1$Prob.Greater.Upper
  Low.M2 <- Res.M2$Lower.Limit
  Prob.Low.M2 <- Res.M2$Prob.Less.Lower
  Upper.M2 <- Res.M2$Upper.Limit
  Prob.Upper.M2 <- Res.M2$Prob.Greater.Upper
  Min.for.Best.Low <- min((c(Prob.Low.M1, Prob.Low.M2) - alpha.lower)^2)
  if (!is.null(Res.M1)) {
    if (Min.for.Best.Low == (Prob.Low.M1 - alpha.lower)^2) 
      Best.Low <- 1
  }
  if (!is.null(Res.M2)) {
    if (Min.for.Best.Low == (Prob.Low.M2 - alpha.lower)^2) 
      Best.Low <- 2
  }
  Min.for.Best.Up <- min((c(Prob.Upper.M1, Prob.Upper.M2) - 
                            alpha.upper)^2)
  if (!is.null(Res.M1)) {
    if (Min.for.Best.Up == (Prob.Upper.M1 - alpha.upper)^2) 
      Best.Up <- 1
  }
  if (!is.null(Res.M2)) {
    if (Min.for.Best.Up == (Prob.Upper.M2 - alpha.upper)^2) 
      Best.Up <- 2
  }
  if (is.null(Res.M1)) {
    Low.M1 <- NA
    Prob.Low.M1 <- NA
    Upper.M1 <- NA
    Prob.Upper.M1 <- NA
  }
  if (is.null(Res.M2)) {
    Low.M2 <- NA
    Prob.Low.M2 <- NA
    Upper.M2 <- NA
    Prob.Upper.M2 <- NA
  }
  Result <- list(Lower.Limit = c(Low.M1, Low.M2)[Best.Low], 
                 Prob.Less.Lower = c(Prob.Low.M1, Prob.Low.M2)[Best.Low], 
                 Upper.Limit = c(Upper.M1, Upper.M2)[Best.Up], Prob.Greater.Upper = c(Prob.Upper.M1, 
                                                                                      Prob.Upper.M2)[Best.Up])
  return(Result)
}

