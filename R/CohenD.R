

# CohenEffectSize <- function(x){

# (C) Antti Arppe 2007-2011
# E-mail: antti.arppe@helsinki.fi

# Cohen's Effect Size (1988)
# e0 <- matrix(,ctable.rows,ctable.cols)
# for(i in 1:ctable.rows)
# for(j in 1:ctable.cols)
# e0[i,j] <- sum.row[i]*sum.col[j]/N
# p0 <- e0/N
# p1 <- ctable/N
# effect.size <- sqrt(sum(((p1-p0)^2)/p0))
# noncentrality <- N*(effect.size^2)
# d.f=(ctable.rows-1)*(ctable.cols-1)
# beta <- pchisq(qchisq(alpha,df=d.f,lower.tail=FALSE),df=d.f,ncp=noncentrality)
# power <- 1-beta

# return(effect.size)
# }


.cohen_d_ci <- function (d, 
                         n = NULL, n2 = NULL, n1 = NULL, 
                         alpha = 0.05) {
  
  # William Revelle in psych
  
  d2t <- function (d, n = NULL, n2 = NULL, n1 = NULL) {
    
    if (is.null(n1)) {
      t <- d * sqrt(n)/2
    } else if (is.null(n2)) {
      t <- d * sqrt(n1)
    } else {
      t <- d/sqrt(1/n1 + 1/n2)
    }
    return(t)
  }
  
  t2d <- function (t, n = NULL, n2 = NULL, n1 = NULL) {
    
    if (is.null(n1)) {
      d <- 2 * t/sqrt(n)
    } else {
      if (is.null(n2)) {
        d <- t/sqrt(n1)
      } else {
        d <- t * sqrt(1/n1 + 1/n2)
      }
    }
    return(d)
  }
  
  t <- d2t(d = d, n = n, n2 = n2, n1 = n1)
  
  tail <- 1 - alpha/2
  ci <- matrix(NA, ncol = 3, nrow = length(d))
  
  for (i in 1:length(d)) {
    nmax <- max(c(n/2 + 1, n1 + 1, n1 + n2))
   
     upper <- try(t2d(uniroot(function(x) {
      suppressWarnings(pt(q = t[i], df = nmax - 2, ncp = x)) - 
        alpha/2
    }, c(min(-5, -abs(t[i]) * 10), max(5, abs(t[i]) * 10)))$root, 
    n = n[i], n2 = n2[i], n1 = n1[i]), silent = TRUE)
    if (inherits(upper, "try-error")) {
      ci[i, 3] <- NA
    }
    else {
      ci[i, 3] <- upper
    }
    ci[i, 2] <- d[i]
    
    lower.ci <- try(
      t2d(uniroot(function(x) {
            suppressWarnings( 
              pt(q = t[i], df = nmax - 2, ncp = x)
              ) - tail }, 
            c(min(-5, -abs(t[i]) * 10), 
              max(5, abs(t[i]) * 10)))$root, 
          n = n[i], n2 = n2[i], n1 = n1[i]
          ), 
          silent = TRUE)
    
    if (inherits(lower.ci, "try-error")) {
      ci[i, 1] <- NA
    }
    else {
      ci[i, 1] <- lower.ci
    }
  }
  
  colnames(ci) <- c("lower", "effect", "upper")
  rownames(ci) <- names(d)
  return(ci)
  
}


.J <- function(nx, ny) {
  (1 - 3 / ( 4 * (nx + ny) - 9))
}



CohenD <- function(x, y=NULL, 
                   conf.level = NA, sides = c("two.sided", "left", "right"), 
                   correct = FALSE, na.rm = FALSE) {
  
  if (na.rm) {
    x <- na.omit(x)
    if(!is.null(y)) y <- na.omit(y)
  }

  sides <- match.arg(sides, choices = c("two.sided","left","right"), 
                     several.ok = FALSE)
  
  if(is.null(y)){   # one sample Cohen d
    d <- mean(x) / sd(x)
    n <- length(x)
    if(!is.na(conf.level)){
      
      if(sides!="two.sided")
        conf.level <- 1 - 2*(1-conf.level)
      
      # # reference: Smithson Confidence Intervals pp. 36:
      # ci <- .nctCI(d / sqrt(n), df = n-1, conf = conf.level)
      # res <- c(d=d, lwr.ci=ci[1]/sqrt(n), upr.ci=ci[3]/sqrt(n))
      
      # changed to Revelle 2022-10-22:
      ci <- .cohen_d_ci(d = d, n = n, alpha = 1-conf.level)
      
      if(sides=="left")        ci[1] <- Inf
      else if(sides=="right")  ci[3] <- -Inf

      res <- c(est=d, lci=ci[1], uci=ci[3])
      
      
    } else {
      res <- d
    }
  } else {
    
    meanx <- mean(x)
    meany <- mean(y)
    #     ssqx <- sum((x - meanx)^2)
    #     ssqy <- sum((y - meany)^2)
    nx <- length(x)
    ny <- length(y)
    
    DF <- nx + ny - 2
    d <- (meanx - meany)
    
    .sd <- sqrt(((nx - 1) * var(x) + (ny - 1) * var(y)) / DF)
    d <- d / .sd
    
    #  if(unbiased) d <- d * gamma(DF/2)/(sqrt(DF/2) * gamma((DF - 1)/2))

    if(!is.na(conf.level)) {
      
      if(sides!="two.sided")
        conf.level <- 1 - 2*(1-conf.level)
      
      # old:
      # The Handbook of Research Synthesis and Meta-Analysis (Cooper, Hedges, & Valentine, 2009)
      ## p 238
      # ci <- d + c(-1, 1) * sqrt(((nx+ny) / (nx*ny) + .5 * d^2 / DF) * ((nx + ny)/DF)) * qt((1 - conf.level) / 2, DF)
      
      # # supposed to be better, Smithson's version:
      # ci <- .nctCI(d / sqrt(nx*ny/(nx+ny)), df = DF, conf = conf.level)
      # res <- c(d=d, lwr.ci=ci[1]/sqrt(nx*ny/(nx+ny)), upr.ci=ci[3]/sqrt(nx*ny/(nx+ny)))

      # changed to Revelle      
      ci <- .cohen_d_ci(d, n2 = nx, n1 = ny, alpha = 1-conf.level)

      if(correct){  # "Hedges' g"
        # Hedges, L. V. & Olkin, I. (1985). Statistical methods for meta-analysis. Orlando, FL: Academic Press.
        ci <- ci * .J(nx, ny)
      }
      
      if(sides=="left")        ci[1] <- Inf
      else if(sides=="right")  ci[3] <- -Inf
      
      res <- c(est=ci[2], lci=unname(ci[1]), uci=unname(ci[3]))

    } else {
      
      if(correct) d <- d * .J(nx, ny)
      
      res <- d
    }
  }
  
  ## Cohen, J. (1992). A power primer. Psychological Bulletin, 112, 155-159. Crow, E. L. (1991).
  attr(res, "magnitude") <- c("negligible","small","medium","large")[findInterval(abs(d), c(0.2, 0.5, 0.8)) + 1]
  attr(res, "sd_pooled") <- .sd
  return(res)
  
}

