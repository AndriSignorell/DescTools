
VarCI <- function (x, 
                   conf.level = 0.95, sides = c("two.sided","left","right"), 
                   method = c("classic", "bonett", "boot"), 
                   na.rm = FALSE, ...) {
  
  if (na.rm) x <- na.omit(x)
  method <- match.arg(method, c("classic","bonett", "boot"))
  
  sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
  if(sides!="two.sided")
    conf.level <- 1 - 2*(1-conf.level)
  
  if(method == "classic"){
    df <- length(x) - 1
    v <- var(x)
    res <- c (var = v, lci = df * v/qchisq((1 - conf.level)/2, df, lower.tail = FALSE)
              , uci = df * v/qchisq((1 - conf.level)/2, df) )
    
  } else if(method=="bonett") {
    
    z <- qnorm(1-(1-conf.level)/2)
    n <- length(x)
    cc <- n/(n-z)
    v <- var(x)
    mtr <- mean(x, trim = 1/(2*(n-4)^0.5))
    m <- mean(x)
    gam4 <- n * sum((x-mtr)^4) / (sum((x-m)^2))^2
    se <- cc * sqrt((gam4 - (n-3)/n)/(n-1))
    lci <- exp(log(cc * v) - z*se)
    uci <- exp(log(cc * v) + z*se)
    
    res <- c(var=v, lci=lci, uci=uci)
    
  } else {
    
    # boot arguments in dots ...
    btype <- InDots(..., arg="type", default="bca")
    R <- InDots(..., arg="R", default=999)
    parallel <- InDots(..., arg="parallel", default="no")
    ncpus <- InDots(..., arg="ncpus", default=getOption("boot.ncpus", 1L))
    
    boot.fun <- boot::boot(x, function(x, d) var(x[d], na.rm=na.rm), 
                           R=R, parallel=parallel, ncpus=ncpus)
    ci <- boot::boot.ci(boot.fun, conf=conf.level, type=btype)
    
    if(method == "norm"){
      res <- c(var=boot.fun$t0, lci=ci[[4]][2], uci=ci[[4]][3])
    } else {
      res <- c(var=boot.fun$t0, lci=ci[[4]][4], uci=ci[[4]][5])
    }
  }
  
  if(sides=="left")
    res[3] <- Inf
  else if(sides=="right")
    res[2] <- 0
  
  return(res)
  
}

