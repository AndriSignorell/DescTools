


HodgesLehmann <- function(x, y = NULL, conf.level = NA, sides = c("two.sided","left","right"), 
                          method = c("boot"), na.rm = FALSE, ...) {
  
  #   Werner Stahel's version:
  #
  #   f.HodgesLehmann <- function(data)
  #   {
  #     ## Purpose:   Hodges-Lehmann estimate and confidence interval
  #     ## -------------------------------------------------------------------------
  #     ## Arguments:
  #     ## Remark: function changed so that CI covers >= 95%, before it was too
  #     ##         small (9/22/04)
  #     ## -------------------------------------------------------------------------
  #     ## Author: Werner Stahel, Date: 12 Aug 2002, 14:13
  #     ## Update: Beat Jaggi, Date: 22 Sept 2004
  #     .cexact <-
  #       # c(NA,NA,NA,NA,NA,21,26,33,40,47,56,65,74,84,95,107,119,131,144,158)
  #       c(NA,NA,NA,NA,NA,22,27,34,41,48,57,66,75,85,96,108,120,132,145,159)
  #     .d <- na.omit(data)
  #     .n <- length(.d)
  #     .wa <- sort(c(outer(.d,.d,"+")/2)[outer(1:.n,1:.n,"<=")])
  #     .c <- if (.n<=length(.cexact)) .n*(.n+1)/2+1-.cexact[.n] else
  #       floor(.n*(.n+1)/4-1.96*sqrt(.n*(.n+1)*(2*.n+1)/24))
  #     .r <- c(median(.wa), .wa[c(.c,.n*(.n+1)/2+1-.c)])
  #     names(.r) <- c("estimate","lower","upper")
  #     .r
  #   }
  
  if(na.rm) {
    if(is.null(y))
      x <- na.omit(x)
    else {
      ok <- complete.cases(x, y)
      x <- x[ok]
      y <- y[ok]
    }
  }
  
  if(anyNA(x) || (!is.null(y) && anyNA(y)))
    if(is.na(conf.level))
      return(NA)
  else
    return(c(est=NA,  lwr.ci=NA, upr.ci=NA))
  
  
  #  res <- wilcox.test(x,  y, conf.int = TRUE, conf.level = Coalesce(conf.level, 0.8))
  if(is.null(y)){
    res <- .Call("_DescTools_hlqest", PACKAGE = "DescTools", x)
  } else {
    res <- .Call("_DescTools_hl2qest", PACKAGE = "DescTools", x, y)
  }
  
  if(is.na(conf.level)){
    result <-  res
    names(result) <- NULL
    
  } else {
    
    sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
    if(sides!="two.sided")
      conf.level <- 1 - 2*(1-conf.level)
    
    n <- length(x)

    
    if( method=="boot"){

      # boot arguments in dots ...
      # adjusted bootstrap percentile (BCa) interval
      btype <- InDots(..., arg="type", default="bca")
      R <- InDots(..., arg="R", default=999)
      parallel <- InDots(..., arg="parallel", default="no")
      ncpus <- InDots(..., arg="ncpus", default=getOption("boot.ncpus", 1L))
      
      
      # ToDo *******************
      # *******  implement here the two sample case!! ***********
      # ToDo *******************
      
      boot.fun <- boot::boot(x, 
                             function(x, d) 
                               .Call("_DescTools_hlqest", PACKAGE="DescTools", x[d]), 
                             R=R, parallel=parallel, ncpus=ncpus)
      ci <- boot::boot.ci(boot.fun, conf=conf.level, type=btype)
      
      if(btype == "norm"){
        res <- c(est=boot.fun$t0, lci=ci[[4]][2], uci=ci[[4]][3])
      } else {
        res <- c(est=boot.fun$t0, lci=ci[[4]][4], uci=ci[[4]][5])
      }
    
    } else {
      # we'll do that later down the road
      
      # lci <- n^2/2 + qnorm((1-conf.level)/2) * sqrt(n^2 * (2*n+1)/12) - 0.5
      # uci <- n^2/2 - qnorm((1-conf.level)/2) * sqrt(n^2 * (2*n+1)/12) - 0.5
      lci <- uci <- NA
      warning("Confidence intervals not yet implemented for Hodges-Lehman-Estimator.")
      
    }
    
    if(sides=="left")        res[3] <- Inf
    else if(sides=="right")  res[2] <- -Inf

    result <- c(est=res,  lwr.ci=lci, upr.ci=uci)
  }
  
  return(result)
  
}

