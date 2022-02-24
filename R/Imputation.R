

# =====================================================
# Function that obtains a statistic of centrality of a
# variable, given a sample of values.
# If the variable is numeric it returns de median, if it
# is a factor it returns the mode. In other cases it
# tries to convert to a factor and then returns the mode.
# =====================================================
# Luis Torgo, Jan 2009
# =====================================================

.CentralValue <- function(x, weights=NULL) {
  if (is.numeric(x)) {
    if (is.null(weights)) median(x,na.rm=TRUE)
    else if ((s <- sum(weights)) > 0) sum(x*(weights/s)) else NA
  } else {
    x <- as.factor(x)
    if (is.null(weights)) levels(x)[which.max(table(x))]
    else levels(x)[which.max(aggregate(weights, list(x), sum)[, 2])]
  }
}



# =====================================================
# Function that fills in all unknowns using the k Nearest
# Neighbors of each case with unknowns. 
# By default it uses the values of the neighbors and 
# obtains an weighted (by the distance to the case) average
# of their values to fill in the unknowns.
# If meth='median' it uses the median/most frequent value,
# instead.
# =====================================================
# Luis Torgo, Mar 2009, Nov 2011
# =====================================================

ImputeKnn <- function(data, k=10, scale=TRUE, meth='weighAvg', distData=NULL) {
  
  n <- nrow(data)  
  if (!is.null(distData)) {
    distInit <- n+1
    data <- rbind(data,distData)
  } else distInit <- 1
  N <- nrow(data)
  
  ncol <- ncol(data)
  nomAttrs <- rep(F,ncol)
  for(i in seq(ncol)) nomAttrs[i] <- is.factor(data[,i])
  nomAttrs <- which(nomAttrs)
  hasNom <- length(nomAttrs)
  contAttrs <- setdiff(seq(ncol),nomAttrs)
  
  dm <- data
  if (scale) dm[,contAttrs] <- scale(dm[,contAttrs])
  if (hasNom)
    for(i in nomAttrs) dm[,i] <- as.integer(dm[,i])
  
  dm <- as.matrix(dm)
  
  nas <- which(!complete.cases(dm))
  if (!is.null(distData)) tgt.nas <- nas[nas <= n]
  else tgt.nas <- nas
  
  if (length(tgt.nas) == 0)
    warning("No case has missing values. Stopping as there is nothing to do.")
  
  xcomplete <- dm[setdiff(distInit:N,nas),]
  if (nrow(xcomplete) < k)
    stop("Not sufficient complete cases for computing neighbors.")
  
  for (i in tgt.nas) {
    
    tgtAs <- which(is.na(dm[i,]))
    
    dist <- scale(xcomplete,dm[i,],FALSE)
    
    xnom <- setdiff(nomAttrs,tgtAs)
    if (length(xnom)) dist[,xnom] <-ifelse(dist[,xnom]>0,1,dist[,xnom])
    
    dist <- dist[,-tgtAs]
    dist <- sqrt(drop(dist^2 %*% rep(1,ncol(dist))))
    ks <- order(dist)[seq(k)]
    for(j in tgtAs)
      if (meth == 'median')
        data[i,j] <- .CentralValue(data[setdiff(distInit:N,nas),j][ks])
    else 
      data[i,j] <- .CentralValue(data[setdiff(distInit:N,nas),j][ks],exp(-dist[ks]))
  }
  
  data[1:n,]
  
}



