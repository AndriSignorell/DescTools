


QuantileCI <- function(x, conf.level = 0.95, sides = c("two.sided", "left", "right"),
                       method = c("exact", "boot"), probs=seq(0, 1, .25), na.rm = FALSE, ...) {
  
  .QuantileCI <- function(x, prob, conf.level = 0.95, sides = c("two.sided", "left", "right")) {
    # Near-symmetric distribution-free confidence interval for a quantile `q`.
    
    # https://stats.stackexchange.com/questions/99829/how-to-obtain-a-confidence-interval-for-a-percentile
    
    # Search over a small range of upper and lower order statistics for the 
    # closest coverage to 1-alpha (but not less than it, if possible).
    
    n <- length(x)
    alpha <- 1- conf.level
    
    
    if(sides == "two.sided"){
      
      u <- qbinom(p = 1-alpha/2, size = n, prob = prob) + (-2:2) + 1
      l <- qbinom(p = alpha/2, size = n, prob = prob) + (-2:2)
      
      u[u > n] <- Inf
      l[l < 0] <- -Inf
      
      coverage <- outer(l, u, function(a,b) pbinom(b-1, n, prob = prob) - pbinom(a-1, n, prob=prob))
      
      if (max(coverage) < 1-alpha) i <- which(coverage==max(coverage)) else
        i <- which(coverage == min(coverage[coverage >= 1-alpha]))
      
      # minimal difference
      i <- i[1]
      
      # order statistics and the actual coverage.
      u <- rep(u, each=5)[i]
      l <- rep(l, 5)[i]
      
      coverage <- coverage[i]
      
    } else if(sides == "left"){
      
      l <- qbinom(p = alpha, size = n, prob = prob)
      u <- Inf
      
      coverage <- 1 - pbinom(q = l-1, size = n, prob = prob)
      
    } else if(sides == "right"){
      
      l <- -Inf
      u <- qbinom(p = 1-alpha, size = n, prob = prob)
      
      coverage <- pbinom(q = u, size = n, prob = prob)
      
    } 
    
    # get the values  
    if(prob %nin% c(0,1))
      s <- sort(x, partial= c(u, l)[is.finite(c(u, l))])
    else
      s <- sort(x)
    
    res <- c(lwr.ci=s[l], upr.ci=s[u])
    attr(res, "conf.level") <- coverage
    
    if(sides=="left")
      res[2] <- Inf
    else if(sides=="right")
      res[1] <- -Inf
    
    return(res)
    
  }
  
  
  if (na.rm) x <- na.omit(x)
  if(anyNA(x))
    stop("missing values and NaN's not allowed if 'na.rm' is FALSE")
  
  sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
  
  method <- match.arg(arg=method, choices=c("exact","boot"))
  
  switch( method
          , "exact" = { # this is the SAS-way to do it
            r <- lapply(probs, function(p) .QuantileCI(x, prob=p, conf.level = conf.level, sides=sides))
            coverage <- sapply(r, function(z) attr(z, "conf.level"))
            r <- do.call(rbind, r)
            attr(r, "conf.level") <- coverage
          }
          , "boot" = {
            
            if(sides!="two.sided")
              conf.level <- 1 - 2*(1-conf.level)
            
            # boot arguments in dots ...
            btype <- InDots(..., arg="type", default="bca")
            R <- InDots(..., arg="R", default=999)
            parallel <- InDots(..., arg="parallel", default="no")
            ncpus <- InDots(..., arg="ncpus", default=getOption("boot.ncpus", 1L))
            
            r <- t(sapply(probs, 
                          function(p) {
                            boot.fun <- boot::boot(x, 
                                                   function(x, d) quantile(x[d], probs=p, na.rm=na.rm), 
                                                   R=R, parallel=parallel, ncpus=ncpus)
                            ci <- boot::boot.ci(boot.fun, conf=conf.level, type=btype)
                            
                            if(btype == "norm"){
                              # res <- c(est=boot.fun$t0, lci=ci[[4]][2], uci=ci[[4]][3])
                              res <- c(lci=ci[[4]][2], uci=ci[[4]][3])
                            } else {
                              res <- c(lci=ci[[4]][4], uci=ci[[4]][5])
                            }
                            return(res)
                          }))
          } )
  
  qq <- quantile(x, probs=probs, na.rm=na.rm)
  
  if(length(probs)==1){
    res <- c(qq, r)
    names(res) <- c("est","lci","uci")
    # report the conf.level which can deviate from the required one
    if(method=="exact")  attr(res, "conf.level") <-  attr(r, "conf.level")
    
  } else {
    res <- cbind(qq, r)
    colnames(res) <- c("est","lci","uci")
    
    # report the conf.level which can deviate from the required one
    if(method=="exact")  
      # report coverages for all probs
      attr(res, "conf.level") <-  attr(r, "conf.level")
    
  }
  
  return( res )
  
}


