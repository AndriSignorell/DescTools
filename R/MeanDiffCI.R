

MeanDiffCI <- function (x, y,
                        conf.level = 0.95, sides = c("two.sided","left","right"), 
                        method = c("classic", "boot"),
                        paired = FALSE, var.equal = FALSE, na.rm = FALSE, ...) {
  
  if (na.rm) {
    x <- na.omit(x)
    y <- na.omit(y)
  }
  
  sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
  if(sides!="two.sided")
    conf.level <- 1 - 2*(1-conf.level)
  
  method <- match.arg(method, c("classic", "boot"))
  if(method == "classic"){
    a <- t.test(x, y, conf.level = conf.level, paired = paired, var.equal = var.equal)
    if(paired)
      res <- c(meandiff = mean(x - y), lci = a$conf.int[1], uci = a$conf.int[2])
    else
      res <- c(meandiff = mean(x) - mean(y), lci = a$conf.int[1], uci = a$conf.int[2])
    
  } else {
    
    # boot arguments in dots ...
    btype <- InDots(..., arg="type", default="bca")
    R <- InDots(..., arg="R", default=999)
    parallel <- InDots(..., arg="parallel", default="no")
    ncpus <- InDots(..., arg="ncpus", default=getOption("boot.ncpus", 1L))
    
    diff.means <- function(d, f){
      n <- nrow(d)
      gp1 <- 1:table(as.numeric(d[,2]))[1]
      m1 <- sum(d[gp1,1] * f[gp1])/sum(f[gp1])
      m2 <- sum(d[-gp1,1] * f[-gp1])/sum(f[-gp1])
      m1 - m2
    }
    
    m <- cbind(c(x,y), c(rep(1,length(x)), rep(2,length(y))))
    
    if(paired)
      boot.fun <- boot::boot(x-y, function(d, i) mean(d[i]), stype="i", 
                             R=R, parallel=parallel, ncpus=ncpus)
    else
      boot.fun <- boot::boot(m, diff.means, stype="f", strata = m[,2], 
                             R=R, parallel=parallel, ncpus=ncpus)
    
    ci <- boot::boot.ci(boot.fun, conf=conf.level, type=btype)
    
    if(btype == "norm"){
      res <- c(meandiff=boot.fun$t0, lci=ci[[4]][2], uci=ci[[4]][3])
    } else {
      res <- c(meandiff=boot.fun$t0, lci=ci[[4]][4], uci=ci[[4]][5])
    }
  }
  
  if(sides=="left")
    res[3] <- Inf
  else if(sides=="right")
    res[2] <- -Inf
  
  return(res)
  
}

