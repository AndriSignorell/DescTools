



CoefVar <- function (x, ...) {
  UseMethod("CoefVar")
}


CoefVar.default <- function (x, weights = NULL, unbiased = FALSE, 
                             na.rm = FALSE, ...) {
  
  
  if (is.null(weights)) {
    if (na.rm) 
      x <- na.omit(x)
    res <- SD(x)/Mean(x)
    n <- length(x)
    
  } else {
    res <- SD(x, weights = weights) / Mean(x, weights = weights)
    n <- sum(weights)
  }
  
  if (unbiased) {
    res <- res * ((1 - (1/(4 * (n - 1))) + (1/n) * res^2) + (1/(2 * (n - 1)^2)))
  }

  return(res)
  
}



# special cases for lm and aov

CoefVar.lm <- function (x, unbiased = FALSE, na.rm = FALSE, ...) {
  
  # source:  http://www.ats.ucla.edu/stat/mult_pkg/faq/general/coefficient_of_variation.htm
  
  # In the modeling setting, the CV is calculated as the ratio of the root mean squared error (RMSE)
  # to the mean of the dependent variable.
  
  # root mean squared error
  rmse <- sqrt(sum(x$residuals^2) / x$df.residual)
  res <- rmse / mean(x$model[[1]], na.rm=na.rm)
  
  # This is the same approach as in CoefVar.default, but it's not clear
  # if it is correct in the environment of a model
  n <- x$df.residual
  if (unbiased) {
    res <- res * ((1 - (1/(4 * (n - 1))) + (1/n) * res^2) +
                    (1/(2 * (n - 1)^2)))
  }
  
  # if (!is.na(conf.level)) {
  #   ci <- .nctCI(sqrt(n)/res, df = n - 1, conf = conf.level)
  #   res <- c(est = res, low.ci = unname(sqrt(n)/ci["upr.ci"]),
  #            upr.ci = unname(sqrt(n)/ci["lwr.ci"]))
  # }
  
  return(res)
  
}


# aus agricolae: Variations Koeffizient aus aov objekt
#
# CoefVar.aov <- function(x){
#   return(sqrt(sum(x$residual^2) / x$df.residual) / mean(x$fitted.values))
# }


CoefVar.aov <- function (x, unbiased = FALSE, na.rm = FALSE, ...) {
  
  # source:  http://www.ats.ucla.edu/stat/mult_pkg/faq/general/coefficient_of_variation.htm
  
  # In the modeling setting, the CV is calculated as the ratio of the root mean squared error (RMSE)
  # to the mean of the dependent variable.
  
  # root mean squared error
  rmse <- sqrt(sum(x$residuals^2) / x$df.residual)
  res <- rmse / mean(x$model[[1]], na.rm=na.rm)
  
  # This is the same approach as in CoefVar.default, but it's not clear
  # if it is correct in the enviroment of a model
  n <- x$df.residual
  if (unbiased) {
    res <- res * ((1 - (1/(4 * (n - 1))) + (1/n) * res^2) +
                    (1/(2 * (n - 1)^2)))
  }
  
  # if (!is.na(conf.level)) {
  #   ci <- .nctCI(sqrt(n)/res, df = n - 1, conf = conf.level)
  #   res <- c(est = res, low.ci = unname(sqrt(n)/ci["upr.ci"]),
  #            upr.ci = unname(sqrt(n)/ci["lwr.ci"]))
  # }
  
  return(res)
  
}



# ============================================
#
#    Confidence intervals
#
# ============================================


CoefVarCI <- function (x, conf.level = 0.95, 
                       sides = c("two.sided", "left", "right"), 
                       method = c("nct","vangel","mckay","verrill","naive"), 
                       na.rm=FALSE, ... ) {
  ## NA-Handling
  if (na.rm) {
    ok <- !is.na(x)
    x <- x[ok]
  }
  
  res <- .coefVarCI(CoefVar(x), n = length(x), 
             conf.level=conf.level, sides=sides, method=method)
 
  if(nrow(res) == 1)
    res <- res[1, ]
  
  return(res)
  
}  



.coefVarCI <- function (K, n, conf.level = 0.95, 
                        sides = c("two.sided", "left", "right"), 
                        method = c("nct","vangel","mckay","verrill","naive")) {
  
  # Description of confidence intervals
  # https://www.itl.nist.gov/div898/software/dataplot/refman1/auxillar/coefvacl.htm
  
  
  .iCoefVarCI <- Vectorize(function(K, n, conf.level=0.95, 
                                    sides = c("two.sided", "left", "right"), 
                                    method = c("vangel","mckay","verrill","nct","naive")) {
    
    method <- match.arg(method)
    sides <- match.arg(sides, choices = c("two.sided", "left", "right"), 
                       several.ok = FALSE)
    
    # double alpha in case of one-sided intervals in order to be able
    # to generally calculate twosided intervals and select afterwards..
    if (sides != "two.sided") 
      conf.level <- 1 - 2 * (1 - conf.level)
    
    alpha <- 1 - conf.level
    
    df <- n - 1
    u1 <- qchisq(1-alpha/2, df)
    u2 <- qchisq(alpha/2, df)
    
    switch(method, verrill = {
      CI.lower <- 0
      CI.upper <- 1
      
    }, vangel = {
      CI.lower <- K / sqrt(((u1+2)/n - 1) * K^2 + u1/df)
      CI.upper <- K / sqrt(((u2+2)/n - 1) * K^2 + u2/df)
      
    }, mckay = {
      CI.lower <- K / sqrt((u1/n - 1) * K^2 + u1/df)
      CI.upper <- K / sqrt((u2/n - 1) * K^2 + u2/df)
      
    }, nct = {
      ci <- .nctCI(sqrt(n)/K, df = df, conf = conf.level)
      CI.lower <- unname(sqrt(n)/ci[2])
      CI.upper <- unname(sqrt(n)/ci[1])
      
    }, naive = {
      CI.lower <- K * sqrt(df / u1)
      CI.upper <- K * sqrt(df / u2)
    }
    )
    
    ci <- c(est = K, 
            lci = CI.lower, # max(0, CI.lower), 
            uci = CI.upper) # min(1, CI.upper))
    
    if (sides == "left") 
      ci[3] <- Inf
    
    else if (sides == "right") 
      ci[2] <- -Inf
    
    return(ci)
    
  })
  
  
  sides <- match.arg(sides)
  method <- match.arg(method)
  
  res <- t(.iCoefVarCI(K=K, n=n, method=method, sides=sides, conf.level = conf.level))
  
  return(res)
  
}  





