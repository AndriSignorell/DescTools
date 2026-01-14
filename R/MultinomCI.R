







MultinomCI <- function(x, conf.level = 0.95, sides = c("two.sided","left","right"),
                       method = c("sisonglaz", "cplus1", "goodman", "wald", "waldcc", "wilson", "qh", "fs")) {
  
  # Code originally from 
  # Pablo J. Villacorta Iglesias <pjvi@decsai.ugr.es>\n
  # Department of Computer Science and Artificial Intelligence, University of Granada (Spain)
  
  # rewritten in R by Andri Signorell

  n <- sum(x, na.rm=TRUE)
  k <- length(x)
  p <- x/n
  
  if (missing(method)) method <- "sisonglaz"
  if(missing(sides)) sides <- "two.sided"
  
  sides <- match.arg(sides, choices = c("two.sided","left","right"), 
                     several.ok = FALSE)
  if(sides!="two.sided")
    conf.level <- 1 - 2 * (1 - conf.level)

  
  method <- match.arg(arg = method, 
                      choices = c("sisonglaz", "cplus1", "goodman", 
                                  "wald", "waldcc", "wilson", "qh", "fs"))
  
  res <- switch( method
        , "goodman" =   { .multinomci.goodman(x, n, k, conf.level) }
        , "wald" =      { .multinomci.wald(x, n, conf.level) }
        , "waldcc" =    { .multinomci.wald_cc(x, n, conf.level) }
        , "wilson" =    { .multinomci.wilson(x, n, conf.level) }
        , "fs" =        { .multinomci.fs(x, n, conf.level) }
        , "qh" =        { .multinomci.qh(x, n, k, conf.level) }
        , "sisonglaz" = { .multinomci.sisonglaz(x, n, k, conf.level) }
        , "cplus1" =    { .multinomci.cplus1(x, n, k, conf.level) }
        )
  
  if(sides=="left")
    res[, 3] <- 1
  else if(sides=="right")
    res[, 2] <- 0
  
  return(res)
}


#' @keywords internal
.moments <- function(c, lambda) {
  
  a <- lambda + c
  b <- max(lambda - c, 0)
  
  den <- diff(ppois(c(b - 1, a), lambda))
  
  r <- 1:4
  
  poisA <- ppois(a, lambda) - ppois(a - r, lambda) 
  poisB <- ppois(b - 1, lambda) - ppois(b - r - 1, lambda)
  mu <- SetNames(lambda^r * (1 - (poisA - poisB)/den), LETTERS[1:4])
  
  res <- with(as.list(mu), 
              c(A, 
                A + B - A^2, 
                C + B * (3 - 3 * A) + 
                  (A - 3 * A^2 + 2 * A^3),
                D + C * (6 - 4 * A) + 
                  B * (7 - 12 * A + 6 * A^2) + 
                  A - 4 * A^2 + 6 * A^3 - 3 * A^4, 
                den
              ))
  return(res)
  
}


#' @keywords internal
.truncpoi <- function(c, x, n, k) {
  
  m <- t(sapply(x, .moments, c=c))
  m[,4] <- m[,4] - 3*m[, 2]^2
  
  probn <- 1/(ppois(n, n) - ppois(n - 1, n))
  
  cS <- as.list(SetNames(colSums(m), LETTERS[1:5]))
  z  <- with(cS, (n - A)/sqrt(B))
  g1 <- with(cS, C/(B^(3/2)))
  g2 <- with(cS, D/(B^2))
  
  poly <- 1 + g1 * (z^3 - 3 * z)/6 + g2 * (z^4 - 6 * z^2 + 3)/24 + 
    g1^2 * (z^6 - 15 * z^4 + 45 * z^2 - 15)/72
  
  f <- poly * exp(-z^2/2)/(sqrt(2) * gamma(0.5))
  
  probx <- prod(m[, 5])
  
  return(probn * probx * f/sqrt(cS$B))
  
}


#' @keywords internal
.multinomci.goodman <- function(x, n, k, conf.level) {
  
  q.chi <- qchisq(1 - (1-conf.level)/k, df = 1)
  
  lci <- (q.chi + 2*x - sqrt(q.chi*(q.chi + 4*x*(n-x)/n))) / (2*(n+q.chi))
  uci <- (q.chi + 2*x + sqrt(q.chi*(q.chi + 4*x*(n-x)/n))) / (2*(n+q.chi))

  res <- cbind(est=x/n, lci=pmax(0, lci), uci=pmin(1, uci))
  return(res)  
} 


#' @keywords internal
.multinomci.wald <- function(x, n, conf.level) {
  
  p <- x/n  
  
  q.chi <- qchisq(conf.level, 1)
  lci <- p - sqrt(q.chi * p * (1 - p)/n)
  uci <- p + sqrt(q.chi * p * (1 - p)/n)
  
  res <- cbind(est=p, lci=pmax(0, lci), uci=pmin(1, uci))
  return(res)  
  
} 


#' @keywords internal
.multinomci.wald_cc <- function(x, n, conf.level) {
    
  p <- x/n  
  
  q.chi <- qchisq(conf.level, 1)
  lci <- p - sqrt(q.chi * p * (1 - p)/n) - 1/(2*n)
  uci <- p + sqrt(q.chi * p * (1 - p)/n) + 1/(2*n)
  
  res <- cbind(est=p, lci=pmax(0, lci), uci=pmin(1, uci))
  return(res)  
  
} 


#' @keywords internal
.multinomci.wilson <- function(x, n, conf.level) {
  
  p <- x/n  

  q.chi <- qchisq(conf.level, 1)
  lci <- (q.chi + 2*x - sqrt(q.chi^2 + 4*x*q.chi * (1 - p))) / (2*(q.chi + n))
  uci <- (q.chi + 2*x + sqrt(q.chi^2 + 4*x*q.chi * (1 - p))) / (2*(q.chi + n))
  
  res <- cbind(est=p, lci=pmax(0, lci), uci=pmin(1, uci))
  return(res)  
  
} 


#' @keywords internal
.multinomci.fs <- function(x, n, conf.level) {
    
  # references Fitzpatrick, S. and Scott, A. (1987). Quick simultaneous confidence 
  # interval for multinomial proportions. 
  # Journal of American Statistical Association 82(399): 875-878.
  
  p <- x/n  
  
  q.snorm <- qnorm(1-(1 - conf.level)/2)
  
  lci <- p - q.snorm / (2 * sqrt(n))
  uci <- p + q.snorm / (2 * sqrt(n))
  
  res <- cbind(est = p, lci = pmax(0, lci), uci = pmin(1, uci))
  return(res)  
  
} 


#' @keywords internal
.multinomci.qh <- function(x, n, k, conf.level) {
    
  # references Quesensberry, C.P. and Hurst, D.C. (1964). 
  # Large Sample Simultaneous Confidence Intervals for 
  # Multinational Proportions. Technometrics, 6: 191-195.
  
  p <- x/n  
  
  q.chi <- qchisq(conf.level, df = k-1)
  
  lci <- (q.chi + 2*x - sqrt(q.chi^2 + 4*x*q.chi*(1 - p)))/(2*(q.chi+n))
  uci <- (q.chi + 2*x + sqrt(q.chi^2 + 4*x*q.chi*(1 - p)))/(2*(q.chi+n))
  
  res <- cbind(est = p, lci = pmax(0, lci), uci = pmin(1, uci))
  return(res)  
  
} 


#' @keywords internal
.multinomci.sisonglaz <- function(x, n, k, conf.level) {

  pold <- const <- 0

  for(cc in 1:n){
    poi <- .truncpoi(cc, x, n, k)
    if(poi > conf.level && pold < conf.level) {
      const <- cc
      break
    }
    pold <- poi
  }
  
  delta <- (conf.level - pold)/(poi - pold)
  const <- const - 1

  p <- x/n  

  res <- cbind(est = p, 
               lci = pmax(0, p - const/n), 
               uci = pmin(1, p + const/n + 2*delta/n))
  return(res)  
  
}


#' @keywords internal
.multinomci.cplus1 <- function(x, n, k, conf.level) {
  
  pold <- const <- 0

  for(cc in 1:n){
    poi <- .truncpoi(cc, x, n, k)
    if(poi > conf.level && pold < conf.level) {
      const <- cc
      break
    }
    pold <- poi
  }
  
  delta <- (conf.level - pold)/(poi - pold)
  const <- const - 1
  
  p <- x/n  
  
  res <- cbind(est = p, 
               lci = pmax(0, p - const/n - 1/n), 
               uci = pmin(1, p + const/n + 1/n))
  return(res)  
  
}  


