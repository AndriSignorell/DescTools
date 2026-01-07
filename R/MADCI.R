


.asv.mad <- function(x, method = "TM"){
  lambda <- gld::fit.fkml(x, method = method)$lambda
  m  <- median(x)
  mad.x <- mad(x)
  fFinv <- gld::dgl(c(m - mad.x, m + mad.x, m), lambda1 = lambda)
  FFinv <- gld::pgl(c(m - mad.x, m + mad.x), lambda1 = lambda)
  A <- fFinv[1] + fFinv[2]
  C <- fFinv[1] - fFinv[2]
  B <- C^2 + 4*C*fFinv[3]*(1 - FFinv[2] - FFinv[1])
  
  (1/(4 * A^2))*(1 + B/fFinv[3]^2)
  
} 



MADCI <- function(x, 
                  conf.level = 0.95, sides = c("two.sided","left","right"), 
                  gld.method = "TM", na.rm = FALSE, ...) {
  
  if (na.rm) x <- na.omit(x)
  
  sides <- match.arg(sides, choices = c("two.sided","left","right"), 
                     several.ok = FALSE)
  
  if(sides!="two.sided")
    conf.level <- 1 - 2*(1-conf.level)
  
  
  alpha <- 1 - conf.level
  z <- qnorm(1 - alpha/2)
  
  est <- mad.x <- mad(x)
  
  n.x <- length(x)
  asv.x <- .asv.mad(x, method = gld.method)
  
  ci <- mad.x + c(-z, z) * sqrt(asv.x / n.x)
  res <- c(est, ci)
  names(res) <- c("est","lci","uci")
  
  if(sides=="left")
    res[3] <- Inf
  else if(sides=="right")
    res[2] <- -Inf
  
  return( res )
  
}



MADDiffCI <- function(x, y, 
                      conf.level = 0.95, sides = c("two.sided","left","right"), 
                      gld.method = "TM", na.rm = FALSE, ...) {
  
  if (na.rm){
    x <- na.omit(x)
    y <- na.omit(y)
  } 
  
  sides <- match.arg(sides, choices = c("two.sided","left","right"), 
                     several.ok = FALSE)
  
  if(sides!="two.sided")
    conf.level <- 1 - 2*(1-conf.level)
  
  alpha <- 1 - conf.level
  z <- qnorm(1 - alpha/2)
  
  mad.x <- mad(x)
  n.x <- length(x)
  asv.x <- .asv.mad(x, method = gld.method)
  
  mad.y <- mad(y)
  n.y <- length(y)
  asv.y <- .asv.mad(y, method = gld.method)
  
  est <- mad.x - mad.y
  ci <- est + c(-z, z) * sqrt(asv.x / n.x + asv.y / n.y)
  res <- c(est, ci)
  
  names(res) <- c("est","lci","uci")
  
  if(sides=="left")
    res[3] <- Inf
  else if(sides=="right")
    res[2] <- -Inf
  
  return( res )
  
}


MADRatioCI <- function(x, y,  
                       conf.level = 0.95, sides = c("two.sided","left","right"), 
                       gld.method = "TM", na.rm = FALSE, ...) {
  
  if (na.rm){
    x <- na.omit(x)
    y <- na.omit(y)
  } 
  
  sides <- match.arg(sides, choices = c("two.sided","left","right"), 
                     several.ok = FALSE)
  
  if(sides!="two.sided")
    conf.level <- 1 - 2*(1-conf.level)
  
  alpha <- 1 - conf.level
  z <- qnorm(1 - alpha/2)
  
  mad.x <- mad(x)
  n.x <- length(x)
  asv.x <- .asv.mad(x, method = gld.method)
  
  mad.y <- mad(y)
  n.y <- length(y)
  asv.y <- .asv.mad(y, method = gld.method)
  
  est <- (mad.x/mad.y)^2
  var.est <- 4 * est * ((1/mad.y^2) * asv.x/n.x + (est/mad.y^2) * asv.y/n.y)
  ci <- exp(log(est) + c(-z, z) * sqrt((1 / est^2) * var.est))
  
  res <- c(est, ci)
  
  names(res) <- c("est","lci","uci")
  
  if(sides=="left")
    res[3] <- Inf
  else if(sides=="right")
    res[2] <- -Inf
  
  return( res )
  
}


