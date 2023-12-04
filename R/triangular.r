
# Source: EnvStats
# author: Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
# Version: 2.8.1



is_na_matrix <- function (mat, rows = TRUE) {
  
    if (!is.matrix(mat))
      stop("'mat' must be a matrix or data frame.")
    if (rows)
      return(apply(mat, 1, function(x) any(is.na(x))))
    else return(apply(mat, 2, function(x) any(is.na(x))))
  }


cbind.no.warn <- function (..., deparse.level = 1) {
  
    oldopts <- options(warn = -1)
    on.exit(options(oldopts))
    base::cbind(..., deparse.level = deparse.level)
  }



dTri <- function (x, min = 0, max = 1, mode = 1/2) {
    names.x <- names(x)
    arg.mat <- cbind.no.warn(x = as.vector(x), min = as.vector(min),
        max = as.vector(max), mode = as.vector(mode))
    na.index <- is_na_matrix(arg.mat)
    if (all(na.index))
        y <- rep(NA, nrow(arg.mat))
    else {
        y <- numeric(nrow(arg.mat))
        y[na.index] <- NA
        y.no.na <- y[!na.index]
        for (i in c("x", "min", "max", "mode")) assign(i, arg.mat[!na.index,
            i])
        if (any(is.infinite(min)) || any(is.infinite(max)))
            stop("All non-missing values of 'min' and 'max' must be finite.")
        if (any(mode <= min) || any(max <= mode))
            stop(paste("All values of 'mode' must be larger than",
                "the corresponding values of 'min', and all",
                "values of 'max' must be larger than the", "corresponding values of 'mode'."))
        mmm <- max - min
        y.no.na <- 2 * ifelse(x <= mode, (x - min)/(mmm * (mode -
            min)), (max - x)/(mmm * (max - mode)))
        y.no.na[y.no.na < 0] <- 0
        y[!na.index] <- y.no.na
    }
    if (!is.null(names.x))
        names(y) <- rep(names.x, length = length(y))
    else names(y) <- NULL
    y
}



pTri <- function (q, min = 0, max = 1, mode = 1/2) {
    names.q <- names(q)
    arg.mat <- cbind.no.warn(q = as.vector(q), min = as.vector(min),
        max = as.vector(max), mode = as.vector(mode))
    na.index <- is_na_matrix(arg.mat)
    if (all(na.index))
        p <- rep(NA, nrow(arg.mat))
    else {
        p <- numeric(nrow(arg.mat))
        p[na.index] <- NA
        p.no.na <- p[!na.index]
        for (i in c("q", "min", "max", "mode")) assign(i, arg.mat[!na.index,
            i])
        if (any(is.infinite(min)) || any(is.infinite(max)))
            stop("All non-missing values of 'min' and 'max' must be finite.")
        if (any(mode <= min) || any(max <= mode))
            stop(paste("All values of 'mode' must be larger than",
                "the corresponding values of 'min', and all",
                "values of 'max' must be larger than the", "corresponding values of 'mode'."))
        q.low <- q <= min
        p.no.na[q.low] <- 0
        q.high <- q >= max
        p.no.na[q.high] <- 1
        if (any(index <- !(q.low | q.high))) {
            for (i in c("q", "min", "max", "mode")) assign(i,
                get(i)[index])
            mmm <- max - min
            p.no.na[index] <- ifelse(q <= mode, (q - min)^2/(mmm *
                (mode - min)), 1 - ((max - q)^2/(mmm * (max -
                mode))))
        }
        p[!na.index] <- p.no.na
    }
    if (!is.null(names.q))
        names(p) <- rep(names.q, length = length(p))
    else names(p) <- NULL
    p
}



qTri <- function (p, min = 0, max = 1, mode = 1/2) {
    names.p <- names(p)
    arg.mat <- cbind.no.warn(p = as.vector(p), min = as.vector(min),
        max = as.vector(max), mode = as.vector(mode))
    na.index <- is_na_matrix(arg.mat)
    if (all(na.index))
        q <- rep(NA, nrow(arg.mat))
    else {
        q <- numeric(nrow(arg.mat))
        q[na.index] <- NA
        q.no.na <- q[!na.index]
        for (i in c("p", "min", "max", "mode")) assign(i, arg.mat[!na.index,
            i])
        if (any(p < 0) || any(p > 1))
            stop("All non-missing values of 'p' must be between 0 and 1.")
        if (any(is.infinite(min)) || any(is.infinite(max)))
            stop("All non-missing values of 'min' and 'max' must be finite.")
        if (any(mode <= min) || any(max <= mode))
            stop(paste("All values of 'mode' must be larger than",
                "the corresponding values of 'min', and all",
                "values of 'max' must be larger than the", "corresponding values of 'mode'."))
        q.no.na[p == 0] <- min[p == 0]
        q.no.na[p == 1] <- max[p == 1]
        if (any(index <- 0 < p & p < 1)) {
            for (i in c("p", "min", "max", "mode")) assign(i,
                get(i)[index])
            mmm <- max - min
            q.no.na[index] <- ifelse(p <= pTri(mode, min = min,
                max = max, mode = mode), min + sqrt(mmm * (mode -
                min) * p), max - sqrt(mmm * (max - mode) * (1 -
                p)))
        }
        q[!na.index] <- q.no.na
    }
    if (!is.null(names.p))
        names(q) <- rep(names.p, length = length(q))
    else names(q) <- NULL
    q
}


rTri <- function (n, min = 0, max = 1, mode = 1/2) {
  ln <- length(n)
  if (ln < 1)
    stop("'n' must be non-empty.")
  if (ln > 1)
    n <- ln
  else {
    if (is.na(n) || n <= 0 || n != trunc(n))
      stop("'n' must be a positive integer or vector.")
  }
  arg.mat <- cbind.no.warn(dum = rep(1, n), min = as.vector(min),
                           max = as.vector(max), mode = as.vector(mode))[, -1, drop = FALSE]
  if (n < nrow(arg.mat))
    arg.mat <- arg.mat[1:n, , drop = FALSE]
  for (i in c("min", "max", "mode")) assign(i, arg.mat[, i])
  na.index <- is_na_matrix(arg.mat)
  if (all(na.index))
    return(rep(NA, n))
  else {
    if (any(is.infinite(min)) || any(is.infinite(max)))
      stop("All non-missing values of 'min' and 'max' must be finite.")
    if (any(mode <= min) || any(max <= mode))
      stop(paste("All values of 'mode' must be larger than",
                 "the corresponding values of 'min', and all",
                 "values of 'max' must be larger than the", "corresponding values of 'mode'."))
    return(qTri(p = runif(n), min = min, max = max, mode = mode))
  }
}

