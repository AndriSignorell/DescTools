

# KappaTest <- function(x, weights = c("Equal-Spacing", "Fleiss-Cohen"), conf.level = NA) {
# to do, idea is to implement a Kappa test for H0: kappa = 0 as in
# http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf, pp. 1687
#   print( "still to do...." )

# }


KappaM <- function(x, method = c("Fleiss", "Conger", "Light"), conf.level = NA) {
  
  # ratings <- as.matrix(na.omit(x))
  #
  # ns <- nrow(ratings)
  # nr <- ncol(ratings)
  #
  # # Build table
  # lev <- levels(as.factor(ratings))
  #
  # for (i in 1:ns) {
  #   frow <- factor(ratings[i,],levels=lev)
  #
  #   if (i==1)
  #     ttab <- as.numeric(table(frow))
  #   else
  #     ttab <- rbind(ttab, as.numeric(table(frow)))
  # }
  #
  # ttab <- matrix(ttab, nrow=ns)
  
  
  calc_Pe2_i <- function(M, pjr) {
    # M: N x R Matrix (factors mit gleichen levels)
    # pjr: K x R Matrix, Randverteilungen (Zeilen = Kategorien, Spalten = Rater)
    
    N <- nrow(M)
    R <- ncol(M)
    
    # Indizes: mappe jede Kategorie auf ihre Zeilennummer in pjr
    lv <- rownames(pjr)
    idx <- apply(M, 2, function(col) match(as.character(col), lv))
    # idx: N x R Matrix mit Kategorie-Indices
    
    # p_lookup[i,r,r2] = pjr[idx[i,r2], r]
    # bedeutet: nehme Randwahrscheinlichkeit von Rater r für die Kategorie,
    # die Rater r2 bei Subjekt i gewählt hat
    Pe2_i <- sapply(1:N, function(i) {
      mat <- outer(1:R, 1:R, Vectorize(function(r, r2) {
        if (r == r2) return(0)
        pjr[idx[i, r2], r]
      }))
      sum(mat) / (R * (R - 1))
    })
    
    Pe2_i
  }
  
  
  
  # we have not factors for matrices, but we need factors below...
  if(is.matrix(x))
    x <- as.data.frame(x)
  
  x <- na.omit(x)
  ns <- nrow(x)
  nr <- ncol(x)
  
  # find all levels in the data (data.frame)
  lev <- levels(factor(unlist(x)))
  levi <- seq_along(lev)
  # apply the same levels to all variables and switch to integer matrix
  xx <- do.call(cbind, lapply(x, factor, levels=lev))
  
  ttab <- apply(Abind(lapply(as.data.frame(xx), 
                             function(z) Dummy(z, method="full", 
                                               levels=levi)), 
                      along = 3),
                c(1,2), sum)
  
  agreeP <- sum((rowSums(ttab^2)-nr)/(nr*(nr-1))/ns)
  
  switch( match.arg(method, choices= c("Fleiss", "Conger", "Light"))
          
          , "Fleiss" = {
            chanceP <- sum(colSums(ttab)^2)/(ns*nr)^2
            value <- (agreeP - chanceP)/(1 - chanceP)
            
            pj <- colSums(ttab)/(ns*nr)
            qj <- 1-pj
            
            varkappa <- (2/(sum(pj*qj)^2*(ns*nr*(nr-1))))*(sum(pj*qj)^2-sum(pj*qj*(qj-pj)))
            SEkappa <- sqrt(varkappa)
            
            ci <- value + c(1,-1) * qnorm((1-conf.level)/2) * SEkappa
          }
          
          , "Conger" = {
            
            rtab <- apply(Abind(lapply(as.data.frame(t(xx)), 
                                       function(z) Dummy(z, method="full", 
                                                         levels=levi)), 
                                along = 3),
                          c(1,2), sum)
            
            rtab <- rtab/ns
            
            chanceP <- sum(colSums(ttab)^2)/(ns*nr)^2 - sum(apply(rtab, 2, var)*(nr-1)/nr)/(nr-1)
            value <- (agreeP - chanceP)/(1 - chanceP)
            
            

            
            # 1. Beobachtete Übereinstimmung pro Subjekt (Po_i)
            
            Po_i <- apply(xx, 1, function(row) {
                counts <- table(row)
                sum(counts * (counts - 1)) / (nr * (nr - 1))
              })

            ## 2. Raterspezifische Randverteilungen p_j(r)
            pjr <- lapply(1:nr, function(r) {
              tab <- table(xx[, r])
              as.numeric(tab) / ns
            })
            pjr <- do.call(cbind, pjr)   # K x R Matrix
            rownames(pjr) <- as.character(levi)
            
            ## 4. Erwartete Übereinstimmung pro Subjekt (Pe2_i)
            # chanceP_i <- numeric(ns)
            # for (i in 1:ns) {
            #   row <- xx[i, ]
            #   for (r in 1:nr) {
            #     cat_r <- as.character(row[r])
            #     for (r2 in setdiff(1:nr, r)) {
            #       chanceP_i[i] <- chanceP_i[i] + pjr[cat_r, r]
            #     }
            #   }
            #   chanceP_i[i] <- chanceP_i[i] / (nr * (nr - 1))
            # }

            chanceP_i <- calc_Pe2_i(xx, pjr)
            
            # SE for exact Kappa value
            num <- mean(((1 - chanceP) * Po_i - 2 * (1 - agreeP) * chanceP_i)^2) - 
              (agreeP * chanceP - 2 * chanceP + agreeP)^2
            varkappa <- num / ((1 - chanceP)^4 * ns)
            SEkappa <- sqrt(varkappa)
            
            ci <- value + c(1,-1) * qnorm((1-conf.level)/2) * SEkappa
            
          }
          
          , "Light" = {
            m <- DescTools::PairApply(x, DescTools::CohenKappa, symmetric=TRUE)
            value <- mean(m[upper.tri(m)])
            
            levlen <- length(lev)
            for (nri in 1:(nr - 1)) for (nrj in (nri + 1):nr) {
              for (i in 1:levlen) for (j in 1:levlen) {
                if (i != j) {
                  r1i <- sum(x[, nri] == lev[i])
                  r2j <- sum(x[, nrj] == lev[j])
                  if (!exists("dis"))
                    dis <- r1i * r2j
                  else dis <- c(dis, r1i * r2j)
                }
              }
              if (!exists("disrater"))
                disrater <- sum(dis)
              else disrater <- c(disrater, sum(dis))
              rm(dis)
            }
            B <- length(disrater) * prod(disrater)
            chanceP <- 1 - B/ns^(choose(nr, 2) * 2)
            varkappa <- chanceP/(ns * (1 - chanceP))
            SEkappa <- sqrt(varkappa)
            
            ci <- value + c(1,-1) * qnorm((1-conf.level)/2) * SEkappa
            
          }
  )
  
  
  if (is.na(conf.level)) {
    res <- value
  } else {
    res <- c("kappa"=value, lwr.ci=ci[1], upr.ci=ci[2])
  }
  return(res)
  
}

# 
# 
# 
# .KappaConger <- function(mat, conf.level = NA, verbose=0) {
# 
#   # mat: N x R Matrix (subjects x raters), kategorial (integer, factor oder char)
#   # conf.level: z.B. 0.95 für 95%-KI
# 
#   N <- nrow(mat)
#   R <- ncol(mat)
# 
#   # Kategorien festlegen
#   lv <- sort(unique(as.vector(mat)))
#   M <- apply(mat, 2, function(col) factor(col, levels = lv))
# 
#   ## 1. Beobachtete Übereinstimmung pro Subjekt (Po_i)
#   Po_i <- apply(M, 1, function(row) {
#     counts <- table(row)
#     sum(counts * (counts - 1)) / (R * (R - 1))
#   })
#   Po <- mean(Po_i)
# 
#   ## 2. Raterspezifische Randverteilungen p_j(r)
#   pjr <- lapply(1:R, function(r) {
#     tab <- table(M[, r])
#     as.numeric(tab) / N
#   })
#   pjr <- do.call(cbind, pjr)   # K x R Matrix
#   rownames(pjr) <- lv
# 
#   ## 3. Erwartete Gesamtübereinstimmung (Pe2)
#   Pe2 <- 0
#   for (r in 1:R) {
#     for (r2 in 1:R) {
#       if (r != r2) {
#         Pe2 <- Pe2 + sum(pjr[, r] * pjr[, r2])
#       }
#     }
#   }
#   Pe2 <- Pe2 / (R * (R - 1))
# 
#   ## 4. Erwartete Übereinstimmung pro Subjekt (Pe2_i)
#   Pe2_i <- numeric(N)
#   for (i in 1:N) {
#     row <- M[i, ]
#     for (r in 1:R) {
#       cat_r <- as.character(row[r])
#       for (r2 in setdiff(1:R, r)) {
#         Pe2_i[i] <- Pe2_i[i] + pjr[cat_r, r]
#       }
#     }
#     Pe2_i[i] <- Pe2_i[i] / (R * (R - 1))
#   }
# 
#   ## 5. Kappa
#   kappa <- (Po - Pe2) / (1 - Pe2)
# 
#   ## 6. Varianz & SE nach Vanbelle (2018, Gl. 8)
#   num <- mean(((1 - Pe2) * Po_i - 2 * (1 - Po) * Pe2_i)^2) -
#     (Po * Pe2 - 2 * Pe2 + Po)^2
#   varK <- num / ((1 - Pe2)^4 * N)
#   seK <- sqrt(varK)
# 
#   ## 7. Konfidenzintervall (Normalapproximation)
#   alpha <- 1 - conf.level
#   z <- qnorm(1 - alpha/2)
#   ci <- c(kappa - z * seK, kappa + z * seK)
# 
#   if(verbose == 0){
#     if(!is.na(conf.level)){
#       res <- c(est=kappa, lci=ci[1], uci=ci[2])
#     } else {
#       res <- kappa
#     }
#   } else {
# 
#     res <- list(
#         kappa = kappa,
#         SE = seK,
#         var = varK,
#         conf.level = conf.level,
#         conf.int = ci,
#         Po = Po,
#         Pe2 = Pe2
#       )
#   }
# 
#   return(res)
# 
# }
# 
# 
# 
# 
# 
# ## 5. Kappa
# kappa <- (Po -         Pe2) / (1 -     Pe2)
# value <- (agreeP - chanceP) / (1 - chanceP)
# 
# ## 6. Varianz & SE nach Vanbelle (2018, Gl. 8)
# num <- mean(((1 - chanceP) * agreeP_i - 2 * (1 - agreeP) * chanceP_i)^2) - 
#                (agreeP * chanceP - 2 * chanceP + agreeP)^2
# 
# varK <- num / ((1 - chanceP)^4 * N)
# seK <- sqrt(varK)
# 
# ## 7. Konfidenzintervall (Normalapproximation)
# alpha <- 1 - conf.level
# z <- qnorm(1 - alpha/2)
# ci <- c(kappa - z * seK, kappa + z * seK)
# 
