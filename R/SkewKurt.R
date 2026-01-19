

# Skew(
#   x,                               # 1) Data
#   conf.level = NA,                 # 2) CI-control
#   sides = c("two.sided", "left", "right"),
#   method = c("boot", "classic"),   # 3) CI-method
#   type = 3,                        # 4) Skewness-Definition
#   weights = NULL,
#   na.rm = FALSE,                   # 6) data sanity
#   ...


Skew <- function (x, 
                  conf.level = NA, sides=c("two.sided", "left", "right"), 
                  method = c("boot", "classic"), 
                  estimator = 3, weights=NULL, na.rm = FALSE, ...) {
  
  # C++ part for the expensive (x - mean(x))^2 etc. is roughly 14 times faster
  #   > x <- rchisq(100000000, df=2)
  #   > system.time(Skew(x))
  #   user  system elapsed
  #   6.32    0.30    6.62
  #   > system.time(Skew2(x))
  #   user  system elapsed
  #   0.47    0.00    0.47
  
  
  i.skew <- function(x, weights=NULL, estimator = 3) {
    
    # estimator 1: older textbooks
    if(!is.null(weights)){
      # use a standard treatment for weights
      z <- .NormWeights(x, weights, na.rm=na.rm, zero.rm=TRUE)
      r.skew <- .Call("rskeww", 
                      as.numeric(z$x), as.numeric(Mean(z$x, weights = z$weights)), 
                      as.numeric(z$weights), PACKAGE="DescTools")
      n <- z$wsum
      
    } else {
      if (na.rm) x <- na.omit(x)
      r.skew <- .Call("rskew", as.numeric(x), 
                      as.numeric(mean(x)), PACKAGE="DescTools")
      n <- length(x)
      
    }
    
    se <- sqrt((6*(n-2))/((n+1)*(n+3)))
    
    if (estimator == 2) {
      # estimator 2: SAS/SPSS
      r.skew <- r.skew * n^0.5 * (n - 1)^0.5/(n - 2)
      se <- se * sqrt(n*(n-1))/(n-2)
    }
    else if (estimator == 3) {
      # estimator 3: MINITAB/BDMP
      r.skew <- r.skew * ((n - 1)/n)^(3/2)
      se <- se * ((n - 1)/n)^(3/2)
    }
    
    return(c(r.skew, se^2))
  }
  
  
  
  
  
  if(is.na(conf.level)){
    res <- i.skew(x, weights=weights, estimator=estimator)[1]
    
  } else {
    
    sides <- match.arg(sides, choices = c("two.sided","left","right"), 
                       several.ok = FALSE)
    
    if(sides!="two.sided")
      conf.level <- 1 - 2*(1-conf.level)
    
    
    if(method == "classic") {
      res <- i.skew(x, weights=weights, estimator=estimator)
      res <- c(est=res[1],
               lci=qnorm((1-conf.level)/2) * sqrt(res[2]),
               uci=qnorm(1-(1-conf.level)/2) * sqrt(res[2]))
      
    } else {
      # Problematic standard errors and confidence intervals for skewness and kurtosis.
      # Wright DB, Herrington JA. (2011) recommend only bootstrap intervals
      # adjusted bootstrap percentile (BCa) interval

      # boot arguments in dots ...
      btype <- InDots(..., arg="type", default="bca")
      R <- InDots(..., arg="R", default=999)
      parallel <- InDots(..., arg="parallel", default="no")
      ncpus <- InDots(..., arg="ncpus", default=getOption("boot.ncpus", 1L))
      
      boot.fun <- boot(x, function(x, d) 
                          i.skew(x[d], weights=weights, estimator=estimator), 
                        R=R, parallel=parallel, ncpus=ncpus)
      ci <- boot.ci(boot.fun, conf=conf.level, type=btype)

      
      if(method == "norm"){
        res <- c(est=boot.fun$t0, lci=ci[[4]][2], uci=ci[[4]][3])
      } else {
        res <- c(est=boot.fun$t0, lci=ci[[4]][4], uci=ci[[4]][5])
      }
      
    }
    
    if(sides=="left")
      res[3] <- Inf
    else if(sides=="right")
      res[2] <- -Inf
    
  }
  
  return(res)

}



Kurt <- function (x, 
                  conf.level = NA, sides=c("two.sided", "left", "right"), 
                  method = c("boot", "classic"), 
                  estimator = 3, weights=NULL, na.rm = FALSE, ...) {
  
  
  i.kurt <- function(x, weights=NULL, na.rm = FALSE, estimator = 3) {
    
    # estimator 1: older textbooks
    if(!is.null(weights)){
      # use a standard treatment for weights
      z <- .NormWeights(x, weights, na.rm=na.rm, zero.rm=TRUE)
      r.kurt <- .Call("rkurtw", as.numeric(z$x), as.numeric(Mean(z$x, weights = z$weights)), as.numeric(z$weights), PACKAGE="DescTools")
      n <- z$wsum
      
    } else {
      if (na.rm) x <- na.omit(x)
      r.kurt <- .Call("rkurt", as.numeric(x), as.numeric(mean(x)), PACKAGE="DescTools")
      n <- length(x)
      
    }
    
    se <- sqrt((24*n*(n-2)*(n-3))/((n+1)^2*(n+3)*(n+5)))
    #    se <- sqrt((24 * n * (n - 1)^2) / ((n - 3) * (n - 2) * (n + 3) * (n + 5)))
    
    if (estimator == 2) {
      # estimator 2: SAS/SPSS
      r.kurt <- ((r.kurt + 3) * (n + 1)/(n - 1) - 3) * (n - 1)^2/(n - 2)/(n - 3)
      se <- se * (((n-1)*(n+1))/((n-2)*(n-3)))
    }
    else if (estimator == 3) {
      # estimator 3: MINITAB/BDMP
      r.kurt <- (r.kurt + 3) * (1 - 1/n)^2 - 3
      se <- se * ((n-1)/n)^2
    }
    return(c(r.kurt, se^2))
  }
  
  
  if(is.na(conf.level)){
    res <- i.kurt(x, weights=weights, na.rm=na.rm, estimator=estimator)[1]
    
  } else {

    sides <- match.arg(sides, choices = c("two.sided","left","right"), 
                       several.ok = FALSE)
    
    if(sides!="two.sided")
      conf.level <- 1 - 2*(1-conf.level)
    
    
    if(method == "classic") {
      
      res <- i.kurt(x, weights=weights, estimator=estimator)
      res <- c(
        est = res[1],
        lci = qnorm((1-conf.level)/2) * sqrt(res[2]),
        uci = qnorm(1-(1-conf.level)/2) * sqrt(res[2])) 

    } else {
      # Problematic standard errors and confidence intervals for skewness and kurtosis.
      # Wright DB, Herrington JA. (2011) recommend only bootstrap intervals
      # adjusted bootstrap percentile (BCa) interval
      
      # boot arguments in dots ...
      btype <- InDots(..., arg="type", default="bca")
      R <- InDots(..., arg="R", default=999)
      parallel <- InDots(..., arg="parallel", default="no")
      ncpus <- InDots(..., arg="ncpus", default=getOption("boot.ncpus", 1L))
      
      boot.fun <- boot(x, function(x, d) 
        i.kurt(x[d], weights=weights, estimator=estimator), 
        R=R, parallel=parallel, ncpus=ncpus)
      ci <- boot.ci(boot.fun, conf=conf.level, type=btype)
      
      
      if(method == "norm"){
        res <- c(est=boot.fun$t0, lci=ci[[4]][2], uci=ci[[4]][3])
      } else {
        res <- c(est=boot.fun$t0, lci=ci[[4]][4], uci=ci[[4]][5])
      }
      
    }
    
    if(sides=="left")
      res[3] <- Inf
    else if(sides=="right")
      res[2] <- -Inf
    
  }
  
  return(res)
  
}
























