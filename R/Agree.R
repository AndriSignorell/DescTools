

# Percent Agreement (nominal) with designbased SE/CI after Klein/Gwet

Agree <- function(x, conf.level = 0.95, fpc = 0, verbose=0) {
  
  # x: Matrix (subjects x raters); Eintraege = Kategorien (Zahlen/Strings), NAs erlaubt
  n  <- nrow(x)
  
  # Itemweise paarweise Uebereinstimmung P_{o,i}
  poi <- apply(x, 1, function(row) {
    v <- row[!is.na(row)]
    m <- length(v)
    if (m < 2) return(NA_real_)
    tab <- table(v)
    sum(tab * (tab - 1)) / (m * (m - 1))
  })
  
  n0 <- sum(!is.na(poi))
  Po <- if (n0 > 0) mean(poi, na.rm = TRUE) else NA_real_
  
  # Subjektbeitraege kappa_i (hier = n/n0 * P_{o,i} fuer pairbare; 0 sonst)
  ki <- rep(0, n)
  if (n0 > 0) ki[!is.na(poi)] <- (n / n0) * poi[!is.na(poi)]
  
  # SE und CI (designbasiert, df = n-1)
  if (is.na(Po) || n <= 1) {
    se <- NA_real_
    ci <- c(lower = NA_real_, upper = NA_real_)
  } else {
    var_hat <- (1 - fpc) / (n * (n - 1)) * sum((ki - Po)^2)
    se <- sqrt(var_hat)
    alpha <- 1 - conf.level
    tcrit <- qt(1 - alpha/2, df = n - 1)
    ci <- c(lower = max(0, Po - tcrit * se),
            upper = min(1, Po + tcrit * se))
  }
  
  if(verbose == 0){
    if(!is.na(conf.level)) {
      res <- c(est=Po, lci=ci[1], uci=ci[2])
      
    } else {
      res <- Po
    }
  } else {
    res <- list(estimate = Po,
                se = se,
                conf.int = ci,
                n = n,
                n_pairable = n0,
                method = "Percent Agreement (design-based; Klein/Gwet)")
  }
  
  return(res)
  
  
}



# 
# Agree.default <- function(x, ..., tolerance = 0, na.rm=FALSE){
#   
#   # coercing to matrix is a good idea, as ratings should be the same type 
#   # for all the raters
#   
#   if(IsConfusionTable(x)){
#     
#     d <- sum(diag(x)) / sum(x)
#     attr(res, c("subjects")) <- sum(x)
#     attr(res, c("raters")) <- 2
#     
#   } else {
#   
#     if(inherits(x, "list"))
#       x <- do.call(cbind, x)
#     else
#       x <- as.matrix(x)
#     
#     # if matrix is character switch to factor 
#     # with all unique elements (!!) as levels
#     if(mode(x) =="character")
#       x[] <- factor(x)
#     
#     d <- sum(apply(x, 1, 
#                    function(z) diff(as.numeric(range(z, na.rm = na.rm))) <= tolerance)) 
#     
#     res <- d / nrow(x)
#     attr(res, c("subjects")) <- nrow(x)
#     attr(res, c("raters")) <- ncol(x)
#   }
#   
#   return(res)
#   
# }


# Old version, replaced 2025-08-12
# 
# Agree <- function(x, tolerance = 0, na.rm = FALSE) {
# 
#   x <- as.matrix(x)
#   if(na.rm) x <- na.omit(x)
# 
#   if(anyNA(x)) return(NA)
# 
#   ns <- nrow(x)
#   nr <- ncol(x)
# 
#   if (is.numeric(x)) {
#     rangetab <- apply(x, 1, max) - apply(x, 1, min)
#     coeff <-  sum(rangetab <= tolerance)/ns
# 
#   } else {
#     rangetab <- as.numeric(sapply(apply(x, 1, table), length))
#     coeff <- (sum(rangetab == 1)/ns)
#     tolerance <- 0
#   }
# 
#   rval <- coeff
#   attr(rval, c("subjects")) <- ns
#   attr(rval, c("raters")) <- nr
# 
#   return(rval)
# 
# }

