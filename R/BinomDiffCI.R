

# BinomCI()
# 
# x1 <- 56; n1 <- 70; x2 <- 48; n2 <- 80
# xci <- BinomDiffCI(x1, n1, x2, n2, method=eval(formals(BinomDiffCI)$method))
# 
# alpha <- 0.05
# conf.level <- 0.95
# xci
# 

.bdci.wald <- function(x1, n1, x2, n2, alpha, correct=FALSE) {
  
  p1.hat <- x1/n1
  p2.hat <- x2/n2
  est <- p1.hat - p2.hat
  
  SE <- p1.hat * (1 - p1.hat) / n1 + p2.hat * (1 - p2.hat) / n2
  ME <- qnorm(1 - alpha/2) * sqrt(SE)
  
  if(correct)
    ME <- ME + 0.5 * (1/n1 + 1/n2)
  
  return( c(lci=est - ME, uci=est + ME) )
  
}  


.bdci.ac <- function(x1, n1, x2, n2, alpha) {
  # "ac" = Agresti-Caffo
    
  n1 <- n1+2
  n2 <- n2+2
  x1  <- x1+1
  x2  <- x2+1
  
  p1.hat <- x1/n1
  p2.hat <- x2/n2
  est <- p1.hat - p2.hat

  ME <- qnorm(1 - alpha/2) * 
             sqrt(p1.hat * (1-p1.hat) / n1 + p2.hat * (1-p2.hat) / n2)
  
  return( c( lci = est - ME, uci = est + ME) )
} 


  
.bdci.exact <- function(p1.hat, n1, p2.hat, n2, alpha) {
  # exact
  warning("exact is not yet implemented!")
  return( c( lci = NA, uci = NA) )
  
}

    
.bdci.score <- function(x1, n1, x2, n2, alpha) {
  # "score" or newcombe
  
  p1.hat <- x1/n1
  p2.hat <- x2/n2
  est <- p1.hat - p2.hat
  
  z <- qnorm(1 - alpha/2)
  
  ci1 <- BinomCI(x=x1, n=n1, conf.level=1-alpha, method="wilson")[1,]
  ci2 <- BinomCI(x=x2, n=n2, conf.level=1-alpha, method="wilson")[1,]

  lci <- est - z * sqrt( ci1["lci"] * (1-ci1["lci"])/n1 + ci2["uci"] * (1-ci2["uci"])/n2)
  uci <- est + z * sqrt( ci1["uci"] * (1-ci1["uci"])/n1 + ci2["lci"] * (1-ci2["lci"])/n2)
  
  return( c( lci = lci, uci = uci) )
  
}


.bdci.scorecc <- function(x1, n1, x2, n2, alpha) {
  # "scorecc" or newcombe_cc 

  p1.hat <- x1/n1
  p2.hat <- x2/n2
  est <- p1.hat - p2.hat

  ci1 <- BinomCI(x=x1, n=n1, conf.level=1-alpha, method="wilsoncc")[1,]
  ci2 <- BinomCI(x=x2, n=n2, conf.level=1-alpha, method="wilsoncc")[1,]

  lci <- est - sqrt((p1.hat - ci1["lci"])^2 + (ci2["uci"] - p2.hat)^2) 
  uci <- est + sqrt((ci1["uci"] - p1.hat)^2 + (p2.hat - ci2["lci"])^2) 

  return( c( lci = lci, uci = uci) )
  
}


.bdci.blj <- function(x1, n1, x2, n2, alpha) {
  # "blj"  brown-li-jeffreys
  
  p1.hat <- (x1 + 0.5) / (n1 + 1)
  p2.hat <- (x2 + 0.5) / (n2 + 1)
  est <- p1.hat - p2.hat
  
  ME <- qnorm(1 - alpha/2) * 
            sqrt(p1.hat * (1 - p1.hat)/n1 + p2.hat * (1 - p2.hat)/n2)
  
  return( c( lci = est - ME, uci = est + ME) )
  
}


.bdci.ha <- function(x1, n1, x2, n2, alpha) {
  # "ha"  Hauck-Anderson
  
  p1.hat <- x1/n1
  p2.hat <- x2/n2
  est <- p1.hat - p2.hat
  
  ME <- 1/(2 * min(n1, n2)) + 
          qnorm(1 - alpha/2) * 
            sqrt(p1.hat * (1 - p1.hat)/(n1-1) + p2.hat * (1 - p2.hat) / (n2-1))
  
  return( c( lci = est - ME, uci = est + ME) )
  
}


.bdci.mn <- function(x1, n1, x2, n2, alpha) {
  # "mn"  Miettinen-Nurminen
  z <- qchisq(1-alpha, 1)
  
  return( c(
    lci = binomdiffciMN(x1, n1, x2, n2, z, TRUE),
    uci = binomdiffciMN(x1, n1, x2, n2, z, FALSE)
  ))
  
}


.bdci.mee <- function(x1, n1, x2, n2, alpha) {
  #  "mee"  Mee, also called Farrington-Mannig

  return( c(
    lci = binomdiffciMee(x1, n1, x2, n2, alpha, TRUE),
    uci = binomdiffciMee(x1, n1, x2, n2, alpha, FALSE)
  ))
  
}



.bdci.hal <- function(x1, n1, x2, n2, alpha, correct=FALSE) {
  # "hal"  haldane 
  
  p1.hat <- x1/n1
  p2.hat <- x2/n2
  
  psi <- (p1.hat + p2.hat) / 2
  
  if(correct)
    # "jp" jeffreys-perks
    # same as haldane but with other psi
    psi <- 0.5 * ((x1 + 0.5) / (n1 + 1) + (x2 + 0.5) / (n2 + 1) )
  
  u <- (1/n1 + 1/n2) / 4
  v <- (1/n1 - 1/n2) / 4
  
  z <- qnorm(1 - alpha/2)
  
  theta <- ((p1.hat - p2.hat) + z^2 * v * (1 - 2*psi)) / (1 + z^2 * u)
  w <- z / (1+z^2*u) * sqrt(u * (4*psi*(1-psi) - (p1.hat - p2.hat)^2) + 
                              2*v*(1-2*psi) *(p1.hat - p2.hat) + 
                              4*z^2*u^2*(1-psi)*psi + z^2*v^2*(1-2*psi)^2)

  return( c( lci = theta - w, uci = theta + w) )

}


.bdci.jp <- function(x1, n1, x2, n2, alpha) {
  # "jp" jeffreys-perks
  
  # same as haldane but with other psi
  .bdci.hal(x1, n1, x2, n2, alpha, correct=TRUE)
}



.bdci.beal <- function(p1.hat, n1, p2.hat, n2, alpha, correct=FALSE) {
  # "beal" = {
  
  # experimental code only...
  # http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.633.9380&rep=rep1&type=pdf
  
  a <- p1.hat + p2.hat
  b <- p1.hat - p2.hat
  u <- ((1/n1) + (1/n2)) / 4
  v <- ((1/n1) - (1/n2)) / 4
  V <- u*((2-a)*a - b^2) + 2*v*(1-a)*b
  z <- qchisq(p=1-alpha/2, df = 1)
  A <- sqrt(z*(V + z*u^2*(2-a)*a + z*v^2*(1-a)^2))
  B <- (b + z*v*(1-a)) / (1+z*u)
  
  CI.lower <- max(-1, B - A / (1 + z*u))
  CI.upper <- min(1, B + A / (1 + z*u))
  
}





BinomDiffCI <- function(x1, n1, x2, n2, conf.level = 0.95, sides = c("two.sided","left","right"),
                        method=c("ac", "wald", "waldcc", "score", "scorecc", "mn",
                                 "mee", "blj", "ha", "hal", "jp")) {
  
  
  if(missing(sides))    sides <- match.arg(sides)
  if(missing(method))   method <- match.arg(method)
  
  
  iBinomDiffCI <- function(x1, n1, x2, n2, conf.level, sides, method) {
    
    #   .Wald #1
    #   .Wald (Corrected) #2
    #   .Exact
    #   .Exact (FM Score)
    #   .Newcombe Score #10
    #   .Newcombe Score (Corrected) #11
    #   .Farrington-Manning
    #   .Hauck-Anderson
    # http://www.jiangtanghu.com/blog/2012/09/23/statistical-notes-5-confidence-intervals-for-difference-between-independent-binomial-proportions-using-sas/
    #  Interval estimation for the difference between independent proportions: comparison of eleven methods.
    
    # https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf
    # http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.633.9380&rep=rep1&type=pdf
    
    # Newcombe (1998) (free):
    # http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.408.7354&rep=rep1&type=pdf

    
    if(sides!="two.sided")
      conf.level <- 1 - 2*(1-conf.level)
    
    alpha <- 1 - conf.level

    p1.hat <- x1/n1
    p2.hat <- x2/n2
    est <- p1.hat - p2.hat
    
    CI <- switch(method,
       "wald" =    { .bdci.wald(x1, n1, x2, n2, alpha, correct=FALSE) },
       "waldcc" =  { .bdci.wald(x1, n1, x2, n2, alpha, correct=TRUE) },
       "ac" =      { .bdci.ac(x1, n1, x2, n2, alpha)  } ,     # Agresti-Caffo
       "exact" =   { .bdci.exact(x1, n1, x2, n2, alpha) },    # exact
       "score" =   { .bdci.score(x1, n1, x2, n2, alpha) },    # Newcombe
       "scorecc" = { .bdci.scorecc(x1, n1, x2, n2, alpha) },  # Newcombe
       "mee" =     { .bdci.mee(x1, n1, x2, n2, alpha)  },     # Mee, also called Farrington-Mannig
       "blj" =     { .bdci.blj(x1, n1, x2, n2, alpha) },      # brown-li-jeffreys
       "ha" =      { .bdci.ha(x1, n1, x2, n2, alpha) },       # Hauck-Anderson
       "mn" =      { .bdci.mn(x1, n1, x2, n2, alpha)   },     # Miettinen-Nurminen
       "beal" =    { .bdci.beal(x1, n1, x2, n2, alpha) },     # Beal
       "hal" =     { .bdci.hal(x1, n1, x2, n2, alpha) },      # haldane 
       "jp" =      { .bdci.jp(x1, n1, x2, n2, alpha) }        # jeffreys-perks
    )

    ci <- c(est = est, 
            lci = max(-1, min(CI)), uci = min(1, max(CI)))
    
    if(sides=="left")
      ci[3] <- 1
    else if(sides=="right")
      ci[2] <- -1
    
    return(ci)
    
  }
  
  
  method <- match.arg(arg=method, several.ok = TRUE)
  sides <- match.arg(arg=sides, several.ok = TRUE)
  
  # Recycle arguments
  lst <- Recycle(x1=x1, n1=n1, x2=x2, n2=n2, 
                 conf.level=conf.level, sides=sides, method=method)
  
  res <- t(sapply(1:attr(lst, "maxdim"),
                  function(i) iBinomDiffCI(x1=lst$x1[i], n1=lst$n1[i], x2=lst$x2[i], n2=lst$n2[i],
                                           conf.level=lst$conf.level[i],
                                           sides=lst$sides[i],
                                           method=lst$method[i])))
  
  # get rownames
  lgn <- Recycle(x1=if(is.null(names(x1))) paste("x1", seq_along(x1), sep=".") else names(x1),
                 n1=if(is.null(names(n1))) paste("n1", seq_along(n1), sep=".") else names(n1),
                 x2=if(is.null(names(x2))) paste("x2", seq_along(x2), sep=".") else names(x2),
                 n2=if(is.null(names(n2))) paste("n2", seq_along(n2), sep=".") else names(n2),
                 conf.level=conf.level, sides=sides, method=method)
  xn <- apply(as.data.frame(lgn[sapply(lgn, function(x) length(unique(x)) != 1)]), 1, paste, collapse=":")
  
  rownames(res) <- xn
  return(res)
  
}
