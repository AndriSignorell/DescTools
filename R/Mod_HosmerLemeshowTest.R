


HosmerLemeshowTest <- function (fit, obs, ngr = 10, X, verbose = FALSE){
  
  # woher kommt das?? -> klaeren!
  # - > MKmisc
  
  ngr1 <- ngr
  # Hosmer-Lemeshow C statistic
  brks <- unique(quantile(fit, probs = seq(0, 1, by = 1/ngr)))
  cutfit <- cut(fit, breaks = brks, include.lowest = TRUE)
  if(length(brks) < ngr+1){
    warning("Found only ", length(brks)-1, " different groups for Hosmer-Lemesho C statistic.")
    ngr <- length(brks)-1
  }
  if(verbose){
    cat("Groups for Hosmer-Lemeshow C statistic:\n")
    print(table(cutfit))
  }
  Obs <- xtabs(cbind("0s" = 1 - obs, "1s" = obs) ~ cutfit)
  Exp <- xtabs(cbind("Os" = 1 - fit, "1s" = fit) ~ cutfit)
  chisq <- sum((Obs - Exp)^2/Exp, na.rm = TRUE)
  names(chisq) <- "X-squared"
  param <- ngr-2
  names(param) <- "df"
  P <- 1 - pchisq(chisq, param)
  
  # Hosmer-Lemeshow H statistic
  cutfit1 <- cut(fit, breaks = ngr1, include.lowest = TRUE)
  if(verbose){
    cat("Groups for Hosmer-Lemeshow H statistic:\n")
    print(table(cutfit1))
  }
  Obs1 <- xtabs(cbind(1 - obs, obs) ~ cutfit1)
  Exp1 <- xtabs(cbind(1 - fit, fit) ~ cutfit1)
  chisq1 <- sum((Obs1 - Exp1)^2/Exp1, na.rm = TRUE)
  names(chisq1) <- "X-squared"
  param1 <- ngr1-2
  names(param1) <- "df"
  P1 <- 1 - pchisq(chisq1, param1)
  dname <- paste(deparse(substitute(fit)), "and", deparse(substitute(obs)))
  C <- structure(list(statistic = chisq, parameter = param,
                      p.value = P, method = "Hosmer-Lemeshow C statistic", data.name = dname,
                      observed = Obs, expected = Exp), class = "htest")
  H <- structure(list(statistic = chisq1, parameter = param1,
                      p.value = P1, method = "Hosmer-Lemeshow H statistic", data.name = dname,
                      observed = Obs1, expected = Exp1), class = "htest")
  
  
  if(!missing(X)){
    # le Cessie-van Houwelingen-Copas-Hosmer unweighted sum of squares test for global goodness of fit
    #        X <- cbind(1, X)
    y <- obs == 1
    p <- fit
    sse <- sum((y - p)^2)
    wt <- p * (1 - p)
    d <- 1 - 2 * p
    z <- lm.wfit(X, d, wt, method = "qr")
    res <- z$residuals * sqrt(z$weights)
    sd <- sqrt(sum(res^2))
    ev <- sum(wt)
    z <- (sse - ev)/sd
    names(z) <- "z"
    P2 <- 2 * pnorm(abs(z), lower.tail = FALSE)
    stats <- c(sse, ev, sd, z, P)
    names(stats) <- c("Sum of squared errors", "Expected value|H0",
                      "SD", "Z", "P")
    gof <- structure(list(statistic = z, p.value = P2,
                          method = "le Cessie-van Houwelingen-Copas-Hosmer global goodness of fit test",
                          data.name = dname,
                          observed = sse, expected = ev), class = "htest")
    
    return(list(C = C, H = H, gof = gof))
  }
  
  list(C = C, H = H)
}


