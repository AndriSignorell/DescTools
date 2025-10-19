



# based on Kappa from library(vcd)
# author: David Meyer
# see also: kappa in library(psych)

CohenKappa <- function (x, y = NULL, 
                        weights = c("Unweighted", "Equal-Spacing", "Fleiss-Cohen"), 
                        conf.level = NA, ...) {
  
  if (is.character(weights)) 
    weights <- match.arg(weights)
  
  if (!is.null(y) & !identical(weights, "Unweighted")) {
    # we can not ensure a reliable weighted kappa for 2 factors with different levels
    # so refuse trying it... (unweighted is no problem)
    stop("Vector interface for weighted Kappa is not supported. Provide confusion matrix.")
  }
  
  x <- NormalizeToConfusion(x=x, y=y, ...)
  
  d <- diag(x)
  n <- sum(x)
  nc <- ncol(x)
  colFreqs <- colSums(x)/n
  rowFreqs <- rowSums(x)/n
  
  kappa <- function(po, pc) {
    (po - pc)/(1 - pc)
  }
  
  std <- function(p, pc, k, W = diag(1, ncol = nc, nrow = nc)) {
              sqrt((sum(p * sweep(sweep(W, 1, W %*% colSums(p) * (1 - k)), 
                                  2, W %*% rowSums(p) * (1 - k))^2) - 
                      (k - pc * (1 - k))^2) / crossprod(1 - pc)/n)
          }
  
  if(identical(weights, "Unweighted")) {
    
    po <- sum(d)/n
    pc <- as.vector(crossprod(colFreqs, rowFreqs))
    k <- kappa(po, pc)
    s <- as.vector(std(x/n, pc, k))
    
  } else {  
    
    # some kind of weights defined
    W <- if (is.matrix(weights)) 
            weights
          
          else if (weights == "Equal-Spacing") 
            1 - abs(outer(1:nc, 1:nc, "-"))/(nc - 1)
    
          else # weights == "Fleiss-Cohen"
            1 - (abs(outer(1:nc, 1:nc, "-"))/(nc - 1))^2
    
    po <- sum(W * x)/n
    pc <- sum(W * colFreqs %o% rowFreqs)
    k <- kappa(po, pc)
    s <- as.vector(std(x/n, pc, k, W))
  }
  
  if (is.na(conf.level)) {
    res <- k
  } else {
    ci <- k + c(1, -1) * qnorm((1 - conf.level)/2) * s
    res <- c(est = k, lci = ci[1], uci = ci[2])
  }
  
  return(res)
  
}


# Use as test:
# https://online.stat.psu.edu/stat509/lesson/18/18.7

# The weighted kappa coefficient is 0.57 and the asymptotic 95% confidence 
# interval is (0.44, 0.70). This indicates that the amount of agreement 
# between the two radiologists is modest (and not as strong as the researchers 
# had hoped it would be).

# lbl<-c("Normal","Benign","Suspect","Cancer")
# m <- t(SetNames(matrix(c(21,12,0,0,
#                          4,17,1,0,
#                          3,9,15,2,
#                          0,0,0,1), nrow=4), rownames=lbl, colnames=lbl))
# 
# # matrix interface

# FmCI(CohenKappa(m, conf.level = 0.95, weights = "E"), digits=2)
# expected: 0.57 [0.44, 0.70]


# CohenKappa(m, conf.level = 0.95)
# 
# # vector interface
# with(Untable(m, colnames = c("rtr1","rtr2")), 
#      CohenKappa(rtr1, rtr2, conf.level = 0.95))
# 
# 
# # long data.frame interface
# d.wide <- AppendRowNames(Untable(m, colnames = c("rtr1","rtr2")), 
#                          "subj")
# d.long <- reshape(d.wide,
#                   varying=2:3,
#                   idvar=c("subj"),
#                   times=colnames(d.wide)[2:3],
#                   v.names="rat", timevar="rater",
#                   direction="long",
#                   new.row.names=seq(prod(dim(d.wide))))
# 
# CohenKappa(rat ~ subj | rater, data=d.long, 
#            conf.level = 0.95)
# 




