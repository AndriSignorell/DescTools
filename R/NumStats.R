

.NumStats <- function(x, ...){

    # superfast function to get most relevant set of statistics
    # for numeric values within one step

    # ATTENTION: x must not contain NAs!!
    #            (we don't want to lose time here to check for NAs)

    
    n <- length(x)
    probs <- c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)

    # the quantiles, totally analogue to the core of stats::quantile:
    index <- 1 + (n - 1) * probs

    lo <- floor(index)
    hi <- ceiling(index)

    x <- sort(x, partial = unique(c(lo, hi)))
    # WHOLE x MUST be sorted in order to get the smallest and largest values,
    # as well as the number of unique values!!!

    qs <- x[lo]
    i <- which(index > lo)
    h <- (index - lo)[i]
    qs[i] <- (1 - h) * qs[i] + h * x[hi[i]]

    names(qs) <- c("min", ".05", ".10", ".25",
                   "median", ".75", ".90", ".95", "max")

    # ... here we go, all we need so far is in qs

    # proceed with the parameteric stuff...

    # we send the SORTED vector WITHOUT NAs to the C++ function to calc
    # the power sum(s), extreme values and the mode
    # NOTE: this is highly performance relevant!
    psum <- .Call("_DescTools_n_pow_sum", PACKAGE = "DescTools", x)

    # this is method 3 in the usual functions Skew and Kurt
    skewx <- ((1 / n * psum$sum3) / (psum$sum2 / n)^1.5) * ((n - 1) / n)^(3 / 2)
    kurtx <- ((((1 / n * psum$sum4) / (psum$sum2 / n)^2) - 3) + 3) * (1 - 1 / n)^2 - 3

    # get std dev here
    varx <- psum$sum2 / (n - 1)
    sdx <- sqrt(varx)

    # get the mode
    modex <- Mode(x)

    # put together the results
    res <- list(
      n = n,
      nu = psum$unique,
      n0 = psum$zero,
      mean = psum$mean,
      meanSE = sdx / sqrt(n),
      quant = qs,
      range = unname(diff(qs[c(1, 9)])),
      meanAD = psum$sum1 / n,
      sd = sdx,
      var = varx,
      vcoef = sdx / psum$mean,
      mad = mad(x, center = qs[5]),
      IQR = unname(diff(qs[c(4, 6)])),
      skew = skewx,
      kurt = kurtx,
      small = data.frame(val  = psum$small_val,
                         freq = psum$small_freq),
      large = data.frame(val  = psum$large_val,
                         freq = psum$large_freq),
      modex = modex,
      modefreq = attr(modex, "freq")
    )

    return(res)

}




calcDesc.numeric <- function(x, n, maxrows = NULL, conf.level = 0.95,
                             include_x = TRUE, ...) {

  nstat <- .NumStats(x)

  # meanCI
  if (n > 1) {
    a <- qt(p = (1-conf.level) / 2, df = n-1) * nstat$sd / sqrt(n)
  } else {
    a <- NA
  }
  meanCI <- nstat$mean + c(1, -1) * a

  # check for remarkably frequent values in a numeric variable
  # say the most frequent value has significantly more than 5% from the total sample
  modefreq_crit <-
    binom.test(ZeroIfNA(nstat$modefreq), n = n, p = 0.05, alternative = "greater")

  if (modefreq_crit$p.value < 0.05 & nstat$nu > 12) {
    modefreq_crit <- gettextf(
      "heap(?): remarkable frequency (%s) for the mode(s) (= %s)",
      Format(modefreq_crit$estimate, fmt = "%", digits = 1),
      paste(nstat$modex, collapse = ", ")
    )
  } else {
    modefreq_crit <- NA
  }

  # we display frequencies, when unique values <=12 else we set maxrows = 0
  # which will display extreme values as high-low list
  if (is.null(maxrows)) {
    maxrows <- ifelse(nstat$nu <= 12, 12, 0)
  }

  if (maxrows > 0) {
    freq <- Freq(factor(x))
    colnames(freq)[1] <- "value"
    # use maxrows as percentage, when < 1
    if (maxrows < 1) {
      maxrows <- sum(freq[, 5] < maxrows) + 1
    }
  } else {
    freq <- NULL
  }

  # put together the results
  res <- list(
    unique = nstat$nu,
    "0s" = nstat$n0,
    mean = nstat$mean,
    meanSE = nstat$meanSE,
    conf.level = conf.level,
    meanCI = meanCI,
    quant = nstat$quant,
    range = nstat$range,
    meanAD = nstat$meanAD,
    sd = nstat$sd,
    var = nstat$var,
    vcoef = nstat$vcoef,
    mad = nstat$mad,
    IQR = nstat$IQR,
    skew = nstat$skew,
    kurt = nstat$kurt,
    small = nstat$small,
    large = nstat$large,
    mode = nstat$modex,
    modefreq_crit = modefreq_crit,
    freq = freq,
    maxrows = maxrows,
    x = if (include_x) x else NULL
  )

  return(res)
}


