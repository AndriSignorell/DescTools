


Desc <- function (x, ..., main=NULL, plotit=NULL, wrd = NULL) {

  if (is.null(wrd))
    UseMethod("Desc")

  else {
    if(!IsValidHwnd(wrd))
      warning("wrd is not a valid handle to a running Word instance.")

    else {
      if(is.null(main) && !is.recursive((x)))
        main <- deparse(substitute(x))

      z <- Desc(x, main=main, plotit=FALSE, ..., wrd=NULL)

      # only if header exists (it does not for single variables!!)
      if(!is.null(z[["_objheader"]]))
        z[["_objheader"]]["main"] <- gettextf("Describe %s (%s):", deparse(substitute(x)), class(x))

      printWrd(x=z, main=main, plotit=plotit, ..., wrd=wrd)

    }
  }

}



Desc.numeric <- function (x, main=NULL,
                          maxrows=NULL,
                          plotit=NULL, sep=NULL, digits=NULL, ...) {
  return(desc(x=x, xname= deparse(substitute(x)), main=main, digits=digits, maxrows=maxrows, plotit=plotit, sep=sep, ...))
}


Desc.integer <- function (x, main=NULL,
                          maxrows=NULL,
                          plotit=NULL, sep=NULL, digits=NULL, ...) {
  return(desc(x=x, xname= deparse(substitute(x)), main=main, digits=digits, maxrows=maxrows, plotit=plotit, sep=sep, ...))
}


Desc.factor <- function (x, main=NULL,
                         maxrows=NULL, ord=NULL,
                         plotit=NULL, sep=NULL, digits=NULL, ...) {
  return(desc(x=x, xname= deparse(substitute(x)), main=main, digits=digits, maxrows=maxrows, ord=ord, plotit=plotit, sep=sep, ...))
}

Desc.labelled <- function (x, main=NULL,
                         maxrows=NULL, ord=NULL,
                         plotit=NULL, sep=NULL, digits=NULL, ...) {
  xname <- deparse(substitute(x))
  lbl <- Label(x)
  x <- factor(x, labels=names(attr(x, "labels")))
  Label(x) <- lbl
  return(desc(x=x, xname= xname, main=main, digits=digits, maxrows=maxrows, ord=ord, plotit=plotit, sep=sep, ...))
}



Desc.ordered <- function (x, main=NULL,
                          maxrows=NULL, ord=NULL,
                          plotit=NULL, sep=NULL, digits=NULL, ...) {
  return(desc(x=x, xname= deparse(substitute(x)), main=main, digits=digits, maxrows=maxrows, ord=ord, plotit=plotit, sep=sep, ...))
}


Desc.character <- function (x, main=NULL,
                            maxrows=NULL, ord=NULL,
                            plotit=NULL, sep=NULL, digits=NULL, ...) {
  return(desc(x=x, xname= deparse(substitute(x)), main=main, digits=digits, maxrows=maxrows, ord=ord, plotit=plotit, sep=sep, ...))
}


Desc.logical <- function (x, main=NULL,
                          ord=NULL, conf.level=0.95,
                          plotit=NULL, sep=NULL, digits=NULL, ...) {
  if(is.null(ord)) ord <- "level"
  return(desc(x=x, xname= deparse(substitute(x)), main=main, digits=digits, ord=ord, conf.level=conf.level, plotit=plotit, sep=sep, ...))
}


Desc.Date <- function (x, main=NULL,
                       dprobs=NULL, mprobs=NULL,
                       plotit=NULL, sep=NULL, digits=NULL, ...) {
  return(desc(x=x, xname= deparse(substitute(x)), main=main, digits=digits, dprobs=dprobs, mprobs=mprobs, plotit=plotit, sep=sep, ...))
}


Desc.table <- function (x, main=NULL,
                        conf.level=0.95, verbose=2, rfrq="111", margins=c(1,2),
                        plotit=NULL, sep=NULL, digits=NULL, ...) {
  return(desc(x=x, xname= deparse(substitute(x)), main=main, digits=digits, conf.level=conf.level, verbose=verbose, rfrq=rfrq, margins=margins,
              plotit=plotit, sep=sep, ...))
}



Desc.default <- function (x, main=NULL, maxrows=NULL, ord=NULL, conf.level=0.95, verbose=2, rfrq="111", margins=c(1,2)
                          , dprobs=NULL, mprobs=NULL
                          , plotit=NULL, sep=NULL, digits=NULL, ...) {

  desc(x, xname= deparse(substitute(x)), main=NULL, digits=NULL, maxrows=NULL, ord=NULL, conf.level=0.95, verbose=2, rfrq="111", margins=c(1,2)
                    , dprobs=NULL, mprobs=NULL
                    , plotit=NULL, sep=NULL, ...)

}




desc <- function (x, main=NULL, xname=deparse(substitute(x)), digits=NULL, maxrows=NULL, ord=NULL
                  , conf.level=0.95, verbose=2, rfrq="111", margins=c(1,2)
                  , dprobs=NULL, mprobs=NULL
                  , plotit=NULL, sep=NULL, ...) {


  # univariate Desc

  # z <- list(xname = deparse(substitute(x)),
  #           label = attr(x, "label"))

  # we have to collapse xname here, else there are some breaks like in
  # Desc(Recode(d.pizza$driver, carp=c("Carpenter","Carter"), arm=c("Butcher","Farmer"), elselevel = "Anyone"))

  z <- list(xname = paste(StrTrim(xname), collapse=""),
            label = attr(x, "label"))

  if(any(class(x) %in% c("table","matrix"))) {
    ntot <- length(x)
    n <- sum(x)       # without NAs
    NAs <- NA         # number of NAs in a table, how to?? after all they're pairs..
  } else if(identical(class(x), "NULL")){
    ntot <- 0
    x <- NULL
    n <- 0
    NAs <- 0
  } else {
    ntot <- length(x) # total count
    x <- x[!is.na(x)]
    n <- length(x)    # now without NAs
    NAs <- ntot - n   # number of NAs
  }
  # ignore class AsIs from I(...) and keep only the rest of class(es)
  if(!is.null(x))
    class(x) <- class(x)[class(x) != "AsIs"]

  z <- c(z, main         = main,
            class        = class(x)[1],  # highest class here
            classlabel   = paste(class(x), collapse = ", "),
            length       = ntot,
            n            = n,
            NAs          = NAs,
            plotit       = plotit,
            digits       = digits,
            sep          = sep
         )

  # define order for displaying frequencies of factors, default level order for ordered factors
  # Descending frequencies for unordered factors
  if(is.null(ord)){
    if(inherits(x, "ordered") || inherits(x, "numeric") || inherits(x, "integer"))
      ord <- "level"
    else
      if(inherits(x, "factor"))
        ord <- if(nlevels(x)==2) "level" else "desc"
  }
  ord <- match.arg(arg = ord, choices = c("desc", "asc", "name", "level"))

  # define default main title
  if (is.null(main)){
    z$main <- gettextf("%s (%s)", z$xname, paste(class(x), collapse = ", "))
  }

  # if not an empty vector (or only NAs)
  if(n > 0){
    # send na stripped x to calcDesc, with n being vector length
    z <- c(z, calcDesc(x=x, n=n, digits=digits, conf.level=conf.level, ord=ord, maxrows=maxrows,
                       verbose=verbose, rfrq=rfrq, margins=margins, ... ))

    if(z$class %nin% c("numeric", "Date") && !is.null(z$unique) && !is.na(z$unique) && z$unique <= 2
       && !(z$class %in% c("factor","ordered") && z$levels>2)) {
      # escalate to logical description if only two values

      if(is.null(main))
        z$main <- gettextf("%s (%s - dichotomous)", z$xname, paste(class(x), collapse = ", "))

      if(z$class %in% c("integer")){
        z$afrq <- cbind(z$small$freq)
        rownames(z$afrq) <- z$small$val
      }
      if(z$class %in% c("factor", "ordered", "character")){
        z$afrq <- cbind(z$freq$freq)
        rownames(z$afrq) <- z$freq$level
      }

      z$rfrq <- BinomCI(z$afrq, n, conf.level = conf.level)
      z$conf.level=conf.level
      z$class <- "logical"


    }

  } else {

    z$unique <- NA
    z$noplot <- TRUE
    z$plotit <- FALSE

  }

  # why did I do that? not ok for frequencies??
  # anyway I may not overwrite digits here
  # if(is.null(digits) && !is.null(z$freq)) z$digits <- 1

  # make a list
  z <- list(z)

  class(z) <- "Desc"
  return(z)

}


Desc.data.frame <- function (x, main = NULL, plotit=NULL, enum = TRUE, sep=NULL, ...) {

  res <- Desc.list(x=x, main=main, plotit=plotit, enum=enum, sep=sep, ...)

  res[["_objheader"]][["main"]] <- gettextf("Describe %s (%s):",
                                            gsub(" +", " ", paste(deparse(substitute(x)), collapse=" ")),
                                            paste(class(x), collapse=", "))

  res[["_objheader"]][["abstract"]] <- Abstract(x)
  attr(res[["_objheader"]][["abstract"]], "main") <- res[["_objheader"]][["main"]]
  res[["_objheader"]][["str"]] <- .CaptOut(res[["_objheader"]][["abstract"]],
                                           width = getOption("width"))[-c(1:3)]

  return(res)

}


Desc.list <- function (x, main=NULL, plotit=NULL, enum = TRUE, sep=NULL, ...) {

  xname <- deparse(substitute(x))
  
  # header for the data.frame of the list

  if(is.null(names(x)))
    names(x) <- seq_along(x)
  
  # default main titles if main is left to NULL
  def.main <- is.null(main)
  if (def.main)
    main <- paste(if (enum)
      paste(seq_along(names(x)), "- "), names(x),
      sep = "")
  else
    main <- rep(main, length.out = ncol(x))

  lst <- list()
  for (i in seq_along(x)) {
    xn <- names(x)[i]
    lst[[xn]] <- Desc(x[[xn]], plotit=plotit, sep=sep, ...)[[1]]
    lst[[xn]]["xname"] <- xn
    if(def.main)
      lst[[xn]]["main"] <- gsub("x[[xn]]", main[i], lst[[xn]]["main"], fixed=TRUE)
    else
      lst[[xn]]["main"] <- main[i]

  }


  header <- list(str   = .CaptOut(
                 Str(x, list.len = Inf)),
                 xname = xname,
                 label = Label(x),
                 class = "header",
                 sep = sep,
#                 main  = gettextf("Describe %s (%s):", deparse(substitute(x)), class(x))
# we might be too late for substituting here... ?
                 main  = gettextf("Describe %s (%s):", xname, class(x))
             )
  # class(header) <- "Desc"

  lst <- append(lst, list(header), after = 0)
  names(lst)[1] <- "_objheader"
  class(lst) <- "Desc"

  return(lst)

}

Desc.formula <- function(formula, data = parent.frame(), subset, main=NULL, plotit=NULL, digits=NULL, ...) {


  mf <- match.call(expand.dots = FALSE)

  subset.expr <- mf$subset
  mf$subset <- NULL
  if (!missing(subset)) {
    s <- eval(subset.expr, data, parent.frame())
    data <- data[s,]
  }

  mm <- DescTools::ParseFormula(formula=formula, data=data)

  # don't want AsIs (will come in case of I(...)) to proceed, so just coerce to vector an back again
  # but don't use the following, as interaction names will be set to y.x instead of y:x
  # mm$lhs$mf.eval <- data.frame(lapply(mm$lhs$mf.eval, as.vector))
  # mm$rhs$mf.eval <- data.frame(lapply(mm$rhs$mf.eval, as.vector))
  for(i in which(lapply(mm$lhs$mf.eval, class) == "AsIs")) {
    mm$lhs$mf.eval[,i] <- as.vector(mm$lhs$mf.eval[,i])
  }
  for(i in which(lapply(mm$rhs$mf.eval, class) == "AsIs")) {
    mm$rhs$mf.eval[,i] <- as.vector(mm$rhs$mf.eval[,i])
  }


  lst <- list()
  for(resp in mm$lhs$vars){         # for all response variables
    for(pred in mm$rhs$vars){       # evalutate for all conditions

      y <- mm$lhs$mf.eval[, resp]
      x <- mm$rhs$mf.eval[, pred]

      if(IsDichotomous(y, na.rm=TRUE)) y <- factor(y)
      if(IsDichotomous(x, na.rm=TRUE)) x <- factor(x)

      names(y) <- resp
      names(x) <- pred

      lst[[paste(resp, pred, sep=" ~ ")]] <- calcDesc.bivar(x=y, g=x, xname=resp, gname=pred, ...)
      lst[[paste(resp, pred, sep=" ~ ")]]["plotit"] <- plotit
      lst[[paste(resp, pred, sep=" ~ ")]]["digits"] <- digits     # would not accept vectors when ["digits"] used. Why??
      # lst[[paste(resp, pred, sep=" ~ ")]]$digits <- digits      # this works

      lst[[paste(resp, pred, sep=" ~ ")]]["main"] <- if(is.null(main))
          paste(lst[[paste(resp, pred, sep=" ~ ")]]["xname"], lst[[paste(resp, pred, sep=" ~ ")]]["gname"], sep=" ~ ")
    }
  }

  if(!is.null(main)){
    main <- rep(main, length.out=length(lst))
    for(i in seq_along(lst))
      lst[[i]]["main"] <- main[i]
  }

  attr(lst, "call") <- deparse(sys.call())

  class(lst) <- "Desc"
  return(lst)

}




calcDesc           <- function(x, n, ...) {
  UseMethod("calcDesc")
}


calcDesc.default   <- function(x, n, ...) {

  if (!is.null(class(x))) {
    # cat(gettextf("\nSorry, don't know how to Desc class(es) %s (%s)!\n\n",
    #              paste(class(x), collapse = ", "), deparse(substitute(x))))
    r <- "unhandled class"

  } else {
    # cat(gettextf("\nObject %s does not exist!\n\n", deparse(substitute(x))))
    r <- "no object"

  }

  invisible(r)
}


calcDesc.numeric   <- function(x, n, maxrows = NULL, ...) {

    probs <- c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)

    # the quantiles, totally analogue to the core of stats::quantile:
    index <- 1 + (n - 1) * probs

    lo <- floor(index)
    hi <- ceiling(index)

    x <- sort(x, partial = unique(c(lo, hi)))
    # WHOLE x MUST be sorted in order to get the smallest and largest values,
    # as well as the number of unique values!!!

    # old: x <- sort(x)
    # x <- sort.int(x, method="quick")  # somewhat faster than "shell"

    qs <- x[lo]
    i <- which(index > lo)
    h <- (index - lo)[i]
    qs[i] <- (1 - h) * qs[i] + h * x[hi[i]]
    names(qs) <- c("min", ".05", ".10", ".25", "median", ".75", ".90", ".95", "max")

    # ... here we go, all we need so far is in qs

    # proceed with the parameteric stuff, we cannot calc mean faster than R, so do it here
    # meanx <- mean.default(x)      # somewhat faster than mean

    # we send the SORTED vector WITHOUT NAs to the C++ function to calc the power sum(s)
    psum <- .Call("_DescTools_n_pow_sum", PACKAGE = "DescTools", x)

    # this is method 3 in the usual functions Skew and Kurt
    skewx <- ((1/n * psum$sum3) /  (psum$sum2 / n)^1.5) * ((n - 1)/n)^(3/2)
    kurtx <- ((((1/n * psum$sum4) /  (psum$sum2 / n)^2) - 3)  + 3) * (1 - 1/n)^2 - 3

    # get std dev here
    sdx <- sqrt(psum$sum2 / (n-1))

    # get the mode
    modex <- Mode(x)
    
    # check for remarkably frequent values in a numeric variable
    # say the most frequent value has significantly more than 5% from the total sample
    modefreq_crit <- binom.test(attr(modex, "freq"), n = n, p = 0.05, alternative = "greater")
    if(modefreq_crit$p.value < 0.05)
      modefreq_crit <- gettextf("heap(?): remarkable frequency (%s) for the mode(s) (= %s)", 
                                Format(modefreq_crit$estimate, fmt="%", digits=1), paste(modex, collapse=", "))
    else
      modefreq_crit <- NA
    
    # we display frequencies, when unique values <=12 else we set maxrows = 0
    # which will display extreme values as high-low list
    if(is.null(maxrows)){
      maxrows <- ifelse(psum$unique <= 12, 12, 0)
    }

    if(maxrows > 0) {
      freq <- Freq(factor(x))
      # use maxrows as percentage, when < 1
      if (maxrows < 1)
        maxrows <- sum(freq[, 5] < maxrows) + 1

    } else {
      freq <- NULL
    }

    # put together the results
    res <- list(unique  = psum$unique,
                "0s"    = psum$zero,
                mean    = psum$mean,
                meanSE  = sdx/sqrt(n),
                quant   = qs,
                range   = unname(diff(qs[c(1,9)])),
                sd      = sdx,
                vcoef   = sdx/psum$mean,
                mad     = mad(x, center = qs[5]),
                IQR     = unname(diff(qs[c(4,6)])),
                skew    = skewx,
                kurt    = kurtx,
                small   = data.frame(val=psum$small_val, freq=psum$small_freq),
                large   = data.frame(val=psum$large_val, freq=psum$large_freq),
                mode    = modex,
                modefreq_crit= modefreq_crit,
                freq    = freq,
                maxrows = maxrows,
                x       = x
    )

    return(res)

}

calcDesc.logical   <- function(x, n, ord="level", conf.level=0.95, ...) {

  ff <- table(x)

  # how should the table be sorted, by name, level or frq? (NULL means "desc")
  switch(match.arg(ord, c("level", "desc", "asc", "name")),
         level  = {  }
         , name   = { ff <- ff[names(ff)] }
         , asc    = { ff <- sort(ff) }
         , desc   = { ff <- -sort(-ff) }
  )

  bf <- BinomCI(ff, n, conf.level = conf.level)
  rownames(bf) <- names(ff)

  res <- list(unique=length(ff),
              afrq=ff, rfrq = bf, conf.level=conf.level
  )

  return(res)
}

calcDesc.factor    <- function(x, n, maxrows = NULL, ord, ...) {

  freq <- Freq(x, ord=ord)

  if(is.null(maxrows))
    maxrows <- 12

  if (maxrows < 1)
      maxrows <- sum(freq[, 5] < maxrows) + 1

  res <- list(levels=nlevels(x),
              unique=sum(freq$freq>0), dupes=any(freq$freq>1), maxrows=maxrows,
              ord=ord, freq = freq
  )

  return(res)

}

calcDesc.character <- function(x, n, maxrows = NULL, ord, ...) {
  # simply factorize x and send to calcDesc.factor
  calcDesc.factor(x=factor(x, ordered=TRUE), n=n, ord=ord, maxrows=maxrows, ...)
}

calcDesc.Date      <- function(x, n, dprobs = NULL, mprobs=NULL, ...) {

  # time aggregation already in the definition of the variable:
  # example:     cut( x, breaks="quarter" )
  #              breaks: day, month, quarter, year

  ybreaks <- function(x, i){
    i <- StrVal(i, as.numeric=TRUE)
    as.Date(seq( from=ISOdate( as.integer(min(format(x,"%Y"), na.rm=TRUE)) %/% i * i, 1, 1)
                 , to=ISOdate( (as.integer(max(format(x,"%Y"), na.rm=TRUE))) %/% i * i + i, 1, 1)
                 , "5 years"
    ))
  }

  if(is.null(dprobs)) dprobs <- rep(1/7, 7)
  if(is.null(mprobs)) mprobs <- c(31,28,31,30,31,30,31,31,30,31,30,31) / 365

  # weekdays in your current locale, Sunday : Saturday
  dtab <- Desc(table(Weekday(x, fmt="ddd")), p=dprobs, stdres=TRUE, plotit=FALSE)[[1]]
  mtab <- Desc(table(Month(x, fmt="mmm")), p=mprobs, stdres=TRUE, plotit=FALSE)[[1]]

  tspan <- diff(range(x, na.rm=TRUE)) /15  # set na.rm=TRUE as we inform user about NAs
  hbreaks <- switch( findInterval( tspan, c(0,5,30,100,350,1250,3500,35000) )
                     , "1" = "days"
                     , "2" = "weeks"
                     , "3" = "months"
                     , "4" = "quarters"
                     , "5" = "years"
                     , "6" = "5-years"
                     , "7" = "10-years"
  )

  res <- list(
      unique       = length(unique(x))
    , highlow      = HighLow(x, nlow=4, na.last=NA)
    , dperctab     = dtab$perctab
    , d.approx.ok  = dtab$approx.ok
    , d.chisq.test = dtab$chisq.test
    , mperctab     = mtab$perctab
    , m.approx.ok  = mtab$approx.ok
    , m.chisq.test = mtab$chisq.test
    , hbreaks      = hbreaks
    , freq         = Freq(x=x, breaks=(brk <- if(hbreaks %in% c("5-years", "10-years")) {ybreaks(x, hbreaks)} else {hbreaks}))
    , x            = x
  )

  if(hbreaks %in% c("5-years", "10-years")){
     res$freq$level <- paste("[", Year(brk[-length(brk)]), ", ", Year(brk[-1]), ")", sep="")
  }

  return(res)
}


calcDesc.table     <- function(x, n, conf.level=0.95, verbose, rfrq, margins, p, digits, ...) {

  loglik.chisq <- function(r.chisq) {
    # Log-likelihood chi-squared (G2) test of independence (homogeneity)

    lhrat <- 2 * sum(r.chisq$observed * log(r.chisq$observed/r.chisq$expected), na.rm=TRUE)

    structure(list(
      statistic = structure(lhrat, .Names = "X-squared"),
      parameter = structure(r.chisq$parameter, .Names = "df"),
      p.value   = structure(pchisq(lhrat, df=r.chisq$parameter, lower.tail = FALSE), .Names = "X-squared"),
      method    = "Likelihood Ratio:",
      data.name = r.chisq$data.name), .Names = c("statistic", "parameter", "p.value", "method", "data.name"), class = "htest")
  }

  n.chisq.test <- function(tab) {

    z <- summary(tab)

    structure(list(
      statistic = structure(z$statistic, .Names = "X-squared"),
      parameter = structure(z$parameter, .Names = "df"),
      p.value   = structure(z$p.value, .Names = "X-squared"),
      method    = "Chi-squared test for independence of all factors:",
      approx.ok = z$approx.ok
    ), .Names = c("statistic", "parameter", "p.value", "method", "approx.ok"), class = "htest")
  }


  ttype <- if(identical(dim(x), c(2L,2L))) {
    "t2x2"  } else if(length(dim(x)) > 2) {
    "tndim" } else if(length(dim(x)) < 2) {
    "t1dim" } else "trxc"

  suppressWarnings(r.chisq <- if(ttype=="1dim") chisq.test(x, correct=FALSE, p=p)
                   else if(ttype=="tndim") n.chisq.test(x)
                   else chisq.test(x, correct=FALSE))

  res <- list(
    n                 = sum(x),
    dim               = dim(x),
    unique            = NULL,
    ttype             = ttype,
    verbose           = verbose,
    conf.level        = conf.level,
    chisq.test        = r.chisq,   # if(ttype=="tndim") n.chisq.test(x) else r.chisq,
    chisq.test.cont   = if(ttype %in% c("t2x2", "trxc")) suppressWarnings(chisq.test(x, correct = TRUE)) else NULL,
    loglik.chisq.test = if(ttype!="tndim") loglik.chisq(r.chisq) else NULL,
    mh.test           = if(ttype %in% c("t2x2","trxc")) MHChisqTest(x) else NULL,
    fisher.test       = if(ttype=="t2x2") fisher.test(x) else NULL,
    mcnemar.test      = if(ttype=="t2x2") mcnemar.test(x),
    or                = if(ttype=="t2x2") OddsRatio(x, conf.level = conf.level),
    relrisk1          = if(ttype=="t2x2") RelRisk(x, conf.level = conf.level, method="wald", delta=0),
    relrisk2          = if(ttype=="t2x2") RelRisk(x[,c(2,1)], conf.level = conf.level, method="wald", delta=0),
    assocs            = if(ttype %in% c("t2x2","trxc")) Assocs(x, conf.level=conf.level, verbose=verbose) else NULL,
    tab               = x,
    pfreq             = prop.table(x),
    pfreqr            = if(ttype != "t1dim") prop.table(x, 1) else NULL,
    pfreqc            = if(ttype != "t1dim") prop.table(x, 2),
    perctab           = if(ttype == "t1dim") Freq(x)
                          else if(ttype=="tndim")  NULL
                          else PercTable(x, rfrq=rfrq, margins=margins, digits=digits, ...),
    approx.ok         = if(ttype=="tndim") r.chisq$approx.ok else !(any(r.chisq$expected < 5) && is.finite(r.chisq$parameter))
  )

  return(res)

}

calcDesc.matrix    <- function(x, n, conf.level=0.95, verbose, rfrq, margins, p, digits, ...){
  calcDesc.table(x=x, n=n, conf.level=conf.level, verbose=verbose, rfrq=rfrq, margins=margins,
                 p=p, digits=digits, ...)
}


calcDesc.bivar     <- function(x, g, xname = NULL, gname = NULL, margin=FALSE, breaks=4, conf.level=0.95,
                               smooth=TRUE, test=kruskal.test, ...) {

  ok <- complete.cases(x, g)
  nv <- sum(ok)
  nx <- length(x)
  NAxs <- sum(is.na(x))
  ng <- length(g)
  NAgs <- sum(is.na(g))
  n <- max(nx, ng)

  gname <- if(is.null(gname)) deparse(substitute(g)) else gname
  xname <- if(is.null(xname)) deparse(substitute(x)) else xname

  res <- list(xname = xname, gname = gname, n = n,
              nvalid = nv, nx=nx, ng=ng, NAxs=NAxs, NAgs=NAgs,
              classx=class(x), classg=class(g), x=unname(x), g=unname(g))

  if(is.numeric(x) && is.numeric(g)) {
    res$class  <- "numnum"
    res$cor.p <- cor(x[ok], g[ok], use="all.obs")
    res$cor.s <- cor(x[ok], g[ok], method="spearman", use="all.obs")
    res$cor.k <- if(n < 5000) {
      cor(x[ok], g[ok], method="kendall", use="all.obs")
    } else { NULL }

  } else if(is.numeric(x) && !is.numeric(g)) {

    res$class  <- "numfact"
    res$mean   <- tapply(x[ok], g[ok], FUN=mean)
    res$median <- tapply(x[ok], g[ok], FUN=median)
    res$sd     <- tapply(x[ok], g[ok], FUN=sd)
    res$IQR    <- tapply(x[ok], g[ok], FUN=IQR)
    res$ns     <- tapply(x, g, FUN=function(z) sum(!is.na(z)))
    res$np     <- res$ns / res$nv
    res$NAs    <- tapply(x, g, FUN=function(z) sum(is.na(z)))
    res$Zeros  <- tapply(x[ok], g[ok], FUN=function(z) sum(z==0))
    res$nlevel <- length(res$mean)

    if(margin){
      res$mean   <- c(res$mean, "Total"=mean(x[ok]))
      res$median <- c(res$median, median(x[ok]))
      res$sd     <- c(res$sd, sd(x[ok]))
      res$IQR    <- c(res$IQR, IQR(x[ok]))
      res$ns     <- c(res$ns, sum(res$ns))
      res$np     <- c(res$np, 1)
      res$NAs    <- c(res$NAs, sum(res$NAs))
      res$Zeros  <- c(res$Zeros, sum(res$Zeros))

    }

    res$test <- tryCatch(test(x ~ g, na.action="na.omit"), error=function(e) {e})

  } else if(!is.numeric(x) && is.numeric(g)) {

    res$class  <- "factnum"
    res$mean   <- tapply(g[ok], x[ok], FUN=mean)
    res$median <- tapply(g[ok], x[ok], FUN=median)
    res$sd     <- tapply(g[ok], x[ok], FUN=sd)
    res$IQR    <- tapply(g[ok], x[ok], FUN=IQR)
    res$ns     <- tapply(g, x, FUN=function(z) sum(!is.na(z)))
    res$np     <- res$ns / res$nv
    res$NAs    <- tapply(g, x, FUN=function(z) sum(is.na(z)))
    res$Zeros  <- tapply(g[ok], x[ok], FUN=function(z) sum(z==0))
    res$nlevel <- length(res$mean)
    res$smooth <- smooth

    if(margin){
      res$mean   <- c(res$mean, "Total"=mean(g[ok]))
      res$median <- c(res$median, median(g[ok]))
      res$sd     <- c(res$sd, sd(g[ok]))
      res$IQR    <- c(res$IQR, IQR(g[ok]))
      res$ns     <- c(res$ns, sum(res$ns))
      res$np     <- c(res$np, 1)
      res$NAs    <- c(res$NAs, sum(res$NAs))
      res$Zeros  <- c(res$Zeros, sum(res$Zeros))
    }


    res$test <- tryCatch(test(g ~ x, na.action = "na.omit"), error=function(e) {e})

    res$atab <- table(unname(x[ok]), CutQ(g[ok], breaks = quantile(g[ok], probs = seq(0, 1, 1/breaks)), na.rm = TRUE))
    res$ptab <- prop.table(res$atab, 2)

    res$binci <- if(!is.na(conf.level))
      BinomCI(res$atab[2,], apply(res$atab, 2, sum), conf.level=conf.level)
    else
      NULL

  } else if(!is.numeric(x) && !is.numeric(g)) {

    res$class  <- "factfact"
    res$tab <- table(x[ok], g[ok])
    names(dimnames(res$tab)) <- c(xname, gname)

  } else {

    return(NA)
  }

  return(res)

}



.print.charmatrix <- function(x, quote=FALSE, print.gap = 2, right=TRUE, ...){

  # prints a character matrix without rownames, by default right aligned and
  # with gap = 2
  # this is used by the print.Desc routines

  rownames(x) <- rep("", nrow(x))
  print(x, quote=quote, print.gap = print.gap, right=right, ...)

}




print.Desc <- function(x, digits=NULL, plotit=NULL, nolabel=FALSE, sep=NULL, ...) {

  .print <- function(x, digits=NULL, plotit=NULL, ...) {


    digits <- Coalesce(digits, x$digits, NULL)
    if(!is.null(digits)){
      opt <- DescToolsOptions(digits=digits)
      on.exit(DescToolsOptions(opt))
    }

    plotit <- Coalesce(plotit, x$plotit, DescToolsOptions("plotit"), FALSE)

    # if(!is.null(attr(x, "call"))) {
    #   cat("\nCall:\n")
    #   cat(attr(x, "call"))
    #   cat("\n\n")
    # }

    # define the separator, "-------..." if not given
    sep <- Coalesce(sep, x$sep, paste(rep("-", (getOption("width") - 2)), collapse = ""))
    cat(sep, "\n")

    if (!identical(x$main, NA))
      cat(x$main)
    if (!is.null(x$label) && !nolabel)
      cat(" :", strwrap(x$label, indent = 2, exdent = 2), sep = "\n")
    if (!identical(x$main, NA))
      cat("\n")
    cat("\n")

    if(any(x$class %in% c("numeric", "integer", "factor", "ordered", "character",
                          "logical", "table", "matrix", "xtabs", "Date",
                          "factfact", "numnum", "numfact", "factnum"))) {

      # escalate to logical if the vector is empty
      if(x$n == 0){
        print.Desc.logical(x, digits, ...)

      } else {
        # do class dispatching by hand
        eval(parse(text=gettextf("print.Desc.%s(x, digits, ...)", x$class)))
      }

      if(plotit){
        eval(parse(text=gettextf("plot.Desc.%s(x, ...)", x$class)))

        if(getOption("debug", FALSE))
          cat(gettextf("!print.Desc!:  plot.Desc.%s(x, ...)", x$class), "\n")
      }

    } else if(identical(x$class, NULL) || identical(x$class, "NULL")) {
        cat("class is NULL, so there's nothing else to describe\n\n")

    } else if(x$class == "header") {
        print.Desc.header(x, ...)

    } else {
      print(unclass(x), ...)
    }

  }



  lapply(x, .print, digits=digits, plotit=plotit, ...)

  invisible()
}


print.Desc.header   <- function(x, digits = NULL, ...){

  cat(x[["str"]], sep="\n")
  cat("\n")

}



print.Desc.numeric  <- function(x, digits = NULL, ...) {

  nlow <- 5
  nhigh <- 5

  if(is.null(digits) && !is.null(x$digits)) digits <- x$digits
  defdigits <- is.null(digits)

  x["nperc"] <- Format(x[["n"]]/x[["length"]], fmt="%", digits=1)
  x["naperc"] <- Format(x[["NAs"]]/x[["length"]], fmt="%", digits=1)
  x["zeroperc"] <- Format(x[["0s"]]/x[["length"]], fmt="%", digits=1)

  if(x[["n"]]>1)
    a <- qt(p=.025, df=x[["n"]] - 1) * x[["meanSE"]]
  else
    a <- NA

  x["meanCI"] <- x[["mean"]] + a
  x["meanUCI"] <- x[["mean"]] - a

  x[c("length","n","NAs","unique","0s")] <- lapply(x[c("length","n","NAs","unique","0s")],
                                                    Format, fmt=Fmt("abs"))
  if(defdigits){
    # how many digits do we want to use?
    # we would use the same number as quantile does...
    out <- capture.output(x$quant)
    digits <- max(2, MaxDigits(strsplit(StrTrim(out[[2]]), split=" ")[[1]][1]))
    # for counts the quants would tipically return 0 digits, mean and ds deserve some though
    # if(digits==0) digits <- 1

  }

  x[["quant"]][] <- Format(x[["quant"]], fmt=Fmt("num", digits=digits))

  x[c("mean","meanCI","meanUCI","range","sd","vcoef","mad","IQR","skew","kurt")] <-
    lapply(x[c("mean","meanCI","meanUCI","range","sd","vcoef","mad","IQR","skew","kurt")],
           Format, fmt=Fmt("num", digits=digits))

  lst <- list(l1 = unlist(x[c("length","n","NAs","unique","0s","mean","meanCI")]),
              l2 = c("", x[["nperc"]], x[["naperc"]], "", x[["zeroperc"]], "", x[["meanUCI"]]),
              l3=x[["quant"]][-c(1,9)],
              l4=unlist(x[c("range","sd","vcoef","mad","IQR","skew","kurt")]))

  width <- max(c(unlist(lapply(lst, nchar)), unlist(lapply(lapply(lst, names), nchar))), na.rm=TRUE)
  if (x$unique == x$n)
    lst$l1["unique"] <- "= n"


  # replaced by 0.99.19
  # cat(paste(lapply(lst, .txtline, width = width, ind = "  ",
  #                  space = "  "), collapse = "\n"), "\n")
  # clarify: print.gap can be set with space, which is set here to 2 spaces
  # should we make an argument out of that?

  m <- rbind(lst$l1, lst$l2, "", names(lst$l3), lst$l3, "", names(lst$l4), lst$l4, "")
  .print.charmatrix(m)

  # we need to do that even if highlow == FALSE, as Desc.integer could need the result!!
  if(x$class == "numeric"){
    vals <- Format(c(x$small$val, x$large$val), fmt=Fmt("num", digits=digits))
  } else {
    vals <- Format(c(x$small$val, x$large$val), fmt=Fmt("abs"))
  }
  # we don't want too many digits but as well no trailing 0s by default
  if(defdigits)
    vals <- gsub("\\.0+$", "\\.0", gsub("^(\\d+\\.\\d*?[1-9])0+$", "\\1", vals, perl = TRUE))

  if(is.null(x$freq)){
    frq <- c(x$small$freq, x$large$freq)
    frqtxt <- paste(" (", Format(frq, fmt=Fmt("abs")), ")", sep = "")
    frqtxt[frq < 2] <- ""
    txt <- StrTrim(paste(vals, frqtxt, sep = ""))
    x$lowtxt <- paste(head(txt, min(length(x$small$val), nlow)), collapse = ", ")
    x$hightxt <- paste(rev(tail(txt, min(length(x$large$val), nhigh))), collapse = ", ")

    cat(paste("lowest : ", x$lowtxt, "\n", "highest: ", x$hightxt, "\n\n", sep = ""))

  } else {
    cat("\n")
    print(x$freq[1:min(nrow(x$freq), x$maxrows), ])
    if (x$maxrows < nrow(x$freq))
      cat("... etc.\n [list output truncated]\n\n")
    else cat("\n")
  }
  
  if(!is.na(x$modefreq_crit)){
    cat(x$modefreq_crit)
    cat("\n\n")
  }

}





print.Desc.logical  <- function(x, digits = NULL, ...) {

  digits <- Coalesce(digits, x$digits, NULL)

  if(!is.null(digits)){
    opt <- options(digits=digits)
    on.exit(options(opt))
  }

  m <- rbind(
    c("length","n","NAs","unique"),
    c(Format(unlist(x[c("length","n","NAs","unique")]), fmt=Fmt("abs")) ),
    c("", x["nperc"] <- Format(x[["n"]]/x[["length"]], fmt="%", digits=1),
      x["naperc"] <- Format(x[["NAs"]]/x[["length"]], fmt="%", digits=1), "")
  )
  m[] <- StrAlign(m, sep = "\\r")
  cat(paste(" ", apply(m, 1, paste, collapse= " ")), sep="\n")
  cat("\n")

  if(!is.null(x$afrq)){

    out <- cbind(
      freq=Format(x$afrq, fmt = Fmt("abs")),
      Format(x$rfrq, fmt = Fmt("per", digits=digits)))

    rownames(out) <- rownames(x$afrq)
    colnames(out) <- c("freq", "perc",
                       gettextf(c("lci%s", "uci%s"),
                                Format(x$conf.level, digits=2, leading="drop")))

    txt <- capture.output(print(StrTrim(out), quote=FALSE, right=TRUE, print.gap=2))
    cat(paste(txt[1], DescToolsOptions("footnote")[1],
              sep = ""), txt[-1], sep = "\n")
    cat(gettextf("\n%s %s%s-CI Wilson\n\n",
                 DescToolsOptions("footnote")[1], x$conf.level * 100, "%"))
  }

  if(identical(x$noplot, TRUE))
    cat(gettextf("Nothing to plot in %s\n\n", x$xname))

}


print.Desc.factor   <- function(x, digits = NULL, ...) {

  m <- rbind(
    c("length","n","NAs","unique","levels","dupes"),
    c(Format(unlist(x[c("length","n","NAs","unique","levels")]), fmt=Fmt("abs")),
      c("n","y")[x$dupes+1]),
    c("", x["nperc"] <- Format(x[["n"]]/x[["length"]], fmt="%", digits=1),
    x["naperc"] <- Format(x[["NAs"]]/x[["length"]], fmt="%", digits=1), "","","")
  )

  m[] <- StrAlign(m[], sep = "\\r")
  cat(paste(" ", apply(m, 1, paste, collapse= " ")), sep="\n")

  # digits <- Coalesce(digits, x$digits, getOption("digits"))
  digits <- Coalesce(digits, x$digits, NULL)

  x$freq <- x$freq[1:min(nrow(x$freq), x$maxrows), ]
  txt.freq <- .CaptOut(print(x$freq, digits=digits))
  cat("\n")
  cat(txt.freq, sep = "\n")

  if (x$maxrows < x$levels)
    cat("... etc.\n [list output truncated]\n\n")
  else cat("\n")

}

print.Desc.character<- function(x, digits = NULL, ...) {
  print.Desc.factor(x, digits = digits, ...)
}

print.Desc.ordered  <- function(x, digits = NULL, ...) {
  print.Desc.factor(x, digits = digits, ...)
}

print.Desc.integer  <- function(x, digits = NULL, ...) {
  print.Desc.numeric(x, digits = digits, ...)
}

print.Desc.matrix   <- function(x, digits = NULL, ...) {
  print.Desc.table(x, digits = digits, ...)
}

print.Desc.table    <- function(x, digits = NULL, ...) {

  x[c(6,8)] <- NULL

  # opt  <- options(scipen=4); on.exit(options(opt))

  if(x$ttype == "tndim") { # multdim table

    cat("Summary: \n",
        "n: ", Format(x$n, fmt=Fmt("abs")), ", ", length(x$dim), "-dim table: ", paste(x$dim, collapse=" x ")
        , "\n\n", sep="" )

    cat(gettextf("%s\n  X-squared = %s, df = %s, p-value = %s", x[["chisq.test"]][["method"]],
                 Format(x[["chisq.test"]][["statistic"]], digits=3), x[["chisq.test"]][["parameter"]],
                   Format(x[["chisq.test"]][["p.value"]], fmt="p")), "\n", sep="")
    if(!x$approx.ok)
      cat("\nWarning message:\n  Exp. counts < 5: Chi-squared approx. may be incorrect!!\n")

    cat("\n")
    print(ftable(addmargins(x$tab, c(1, length(x$dim)))))
    cat("\n")

  } else {  # <= 2-dimensional table


    if(x$ttype=="t1dim") {       # 1-dim table ****
      cat("Summary: \n",
          "n: ", Format(x$n, fmt=Fmt("abs")),
          ", rows: ", x$dim[1]
          , "\n\n", sep="" )
      cat("Pearson's Chi-squared test (1-dim uniform):\n  "
         , .CaptOut(x$chisq.test)[5], "\n\n", sep="")

      if(!x$approx.ok)
         cat("\nWarning message:\n  Exp. counts < 5: Chi-squared approx. may be incorrect!!\n\n")

      print(x$perctab)

    } else {                      # 2-dim tabl *****

      if(!is.null(attr(x, "missings")))
        missn <- paste(",", attr(x, "missings"), paste="")
      else
        missn <- ""

      cat("Summary: \n",
          "n: ", Format(x$n, fmt=Fmt("abs")),
          ", rows: ", Format(x$dim[1], fmt=Fmt("abs")),
          ", columns: ", Format(x$dim[2], fmt=Fmt("abs")),
          missn
          , "\n\n", sep="" )

      if(x$ttype=="t2x2"){

        if(x$verbose=="3"){
          cat("Pearson's Chi-squared test:\n  ", .CaptOut(x$chisq.test)[5], "\n", sep="")
        }
        cat("Pearson's Chi-squared test (cont. adj):\n  ", .CaptOut(x$chisq.test.cont)[5], "\n", sep="")
        cat("Fisher's exact test ", .CaptOut(x$fisher.test)[5], "\n", sep="")

        if(x$verbose  %in% c("2","3")){ # print only with verbosity > 1
          cat("", .CaptOut(x$mcnemar.test)[5], "\n", sep="")
        }

        if(!x$approx.ok)
          cat("\nWarning message:\n  Exp. counts < 5: Chi-squared approx. may be incorrect!!\n\n")

        if(x$verbose %in% c("2","3")){ # print only with verbosity > 1
          cat("\n")
          m <- ftable(format(rbind(
            "odds ratio    "       = x$or
            , "rel. risk (col1)  " = x$relrisk1
            , "rel. risk (col2)  " = x$relrisk2
          ), digits=3, nsmall=3))

          attr(m, "col.vars")[[1]][1] <- "estimate"
          txt <- capture.output(print(m))
          txt[1] <- paste(txt[1], DescToolsOptions("footnote")[1], sep="")
          cat(txt, sep="\n")
          cat("\n")
        }

      } else {

        # we report chisquare without cont-corr for rxc and with cont-corr for 2x2 by default
        cat("Pearson's Chi-squared test:\n  ", .CaptOut(x$chisq.test)[5], "\n", sep="")

        if(x$verbose=="3"){
          cat("Pearson's Chi-squared test (cont. adj):\n  ", .CaptOut(x$chisq.test.cont)[5], "\n", sep="")
        }

        if(x$verbose > 1){ # print only with verbosity > 1

          # Log-likelihood chi-squared (G2) test of independence (homogeneity)
          cat("Likelihood Ratio:\n  ", .CaptOut(x$loglik.chisq.test)[5], "\n", sep="")
          # Mantel-Haenszel ChiSquared (linear hypothesis)
          cat("Mantel-Haenszel Chi-squared:\n  ", .CaptOut(x$mh.test)[5], "\n", sep="")

        }

        if(!x$approx.ok)
          cat("\nWarning message:\n  Exp. counts < 5: Chi-squared approx. may be incorrect!!\n\n")

      }

      switch(x$verbose
             , "1" = { cat("\n")
             }
             , "2" = {
               cat(sprintf(
                 "\nPhi-Coefficient        %.3f\nContingency Coeff.     %.3f\nCramer's V             %.3f\n"
                 , x$assocs[1,1]
                 , x$assocs[2,1]
                 , x$assocs[3,1]
               ) )
               cat("\n")
             }
             , "3" = {
               cat("\n")
               txt <- capture.output(x$assocs)
               txt[1] <- paste(txt[1], DescToolsOptions("footnote")[1], sep="")
               cat(txt, sep="\n")
               cat("\n")
             }
      )

      # print(PercTable(x$tab, rfrq=rfrq, margins=margins, ...))
      print(x$perctab)

      if((x$verbose=="3") || (x$ttype=="t2x2"))
        cat(gettextf("\n----------\n%s %s%s conf. level\n", DescToolsOptions("footnote")[1], x$conf.level*100, "%"))

    }

    cat("\n")

  }
}

print.Desc.xtabs   <- function(x, digits = NULL, ...) {
  print.Desc.table(x, digits, ...)
}

print.Desc.Date     <- function(x, digits = NULL, ... ) {
  # time aggregation already in the definition of the variable:
  # example:     cut( x, breaks="quarter" )
  #              breaks: day, month, quarter, year

  m <- rbind(
    c("length","n","NAs","unique"),
    c(Format(unlist(x[c("length","n","NAs","unique")]), fmt=Fmt("abs")) ),
    c("", x["nperc"] <- Format(x[["n"]]/x[["length"]], fmt="%", digits=1),
      x["naperc"] <- Format(x[["NAs"]]/x[["length"]], fmt="%", digits=1), "")
  )
  m[] <- StrAlign(m, sep = "\\r")
  cat(paste(" ", apply(m, 1, paste, collapse= " ")), sep="\n")
  cat("\n")



  cat(x$highlow, "\n", sep="")

  cat("\nWeekday:\n\n")
  cat("Pearson's Chi-squared test (1-dim uniform):\n  "
      , .CaptOut(x$d.chisq.test)[5], "\n\n", sep="")

  if(!x$d.approx.ok)
    cat("\nWarning message:\n  Exp. counts < 5: Chi-squared approx. may be incorrect!!\n\n")

  print(x$dperctab)

  cat("\nMonths:\n\n")
  cat("Pearson's Chi-squared test (1-dim uniform):\n  "
      , .CaptOut(x$m.chisq.test)[5], "\n\n", sep="")

  print(x$mperctab)

  if(!x$m.approx.ok)
    cat("\nWarning message:\n  Exp. counts < 5: Chi-squared approx. may be incorrect!!\n\n")

  if(!is.null(x$hbreaks)){
    cat("\nBy", x$hbreaks, ":\n\n")
    print(x$freq)

  } else {
    cat("Warning:\n  No plausible breaks for years found!\n")
  }
  cat("\n")

}


print.Desc.factfact <- function(x, digits = NULL, ...){

  txt <- .CaptOut(Desc(x$tab, plotit=FALSE, digits=digits, ...))[-(1:2)]
  cat(txt, sep="\n")

}

print.Desc.numfact  <- function(x, digits = NULL, ...){

  cat( "Summary: \n",
       "n pairs: ", Format(x$n, fmt=Fmt("abs")),
       ", valid: ", Format(x$nvalid, fmt=Fmt("abs")),
       " (", Format(x$nvalid/x$n, fmt=Fmt("per")), ")",
       ", missings: ", Format(x$n-x$nvalid, fmt=Fmt("abs")),
       " (", Format((x$n - x$nvalid)/x$n, fmt=Fmt("per")), "),",
       " groups: ", x$nlevel
       , sep="" )
  cat("\n\n")

  digits <- Coalesce(digits, x$digits, NULL)
  if(is.null(digits)) digits <- DescToolsOptions("digits", default=3)
  digits <- rep(digits, length.out=5)

  z <- rbind(
    Format(x$mean, fmt=Fmt("num", digits=digits[1]))
    , Format(x$median, fmt=Fmt("num", digits=digits[2]))
    , Format(x$sd, fmt=Fmt("num", digits=digits[3]))
    , Format(x$IQR, fmt=Fmt("num", digits=digits[4]))
    , Format(x$ns, fmt=Fmt("abs"))
    , Format(x$np, fmt=Fmt("per", digits=digits[5]))
    , Format(x$NAs, fmt=Fmt("abs"))
    , Format(x$Zeros, fmt=Fmt("abs"))
  )
  rownames(z) <- c("mean","median","sd","IQR","n","np","NAs","0s")  # cannot use names as 0s is replaced by X.0s....
  colnames(z) <- rep("", ncol(z))
  z <- rbind(names(x$mean), z)

  z[] <- StrAlign(z, sep="\\r")
  print(z, quote=FALSE, print.gap=2, ...)

  if (inherits(x$test, "simpleError")) {
    cat(gettextf("\nError in test(x) : %s\n\n", x$test$message))
  } else {
    cat(gettextf("\n%s:\n  %s", x$test["method"],
                 .CaptOut(x$test)[5], "\n\n", sep=""))
  }

  if((x$NAgs > 0) & (length(grep("NA", x$xname)) == 0))
    cat(gettextf("\nWarning:\n  Grouping variable contains %s NAs (%s"
                 , x$NAgs, signif(x$NAgs/x$n, digits=3)*100), "%).\n", sep="")
  cat("\n")

}

print.Desc.numnum   <- function(x, digits = NULL, ...) {

  cat( "Summary: \n",
       "n pairs: ", Format(x$n, fmt=Fmt("abs")),
       ", valid: ", Format(x$nvalid, fmt=Fmt("abs")),
       " (", Format(x$nvalid/x$n, fmt=Fmt("per")), ")",
       ", missings: ", Format(x$n-x$nvalid, fmt=Fmt("abs")),
       " (", Format((x$n - x$nvalid)/x$n, fmt=Fmt("per")), ")"
       , sep="" )
  cat("\n\n")

  cat(gettextf(
    "\nPearson corr. : %s\nSpearman corr.: %s\nKendall corr. : %s\n"
    , Format(x$cor.p, fmt=Fmt("num"))
    , Format(x$cor.s, fmt=Fmt("num"))
    , if(x$nvalid < 5000){
      Format(x$cor.k, fmt=Fmt("num"))
    } else {
      "(sample too large)"
    }
  ))
}

print.Desc.factnum  <- function(x, digits = NULL, ...) {

  x$main   <- paste(x$xname, x$gname, sep=" ~ ")
  x$NAgs <- x$NAxs
  print.Desc.numfact(x, digits=digits, ...)

  cat(gettextf("\n\nProportions of %s in the quantiles of %s:\n", x$xname , x$gname))
  ptab <- x$ptab
  ptab[] <- StrAlign(Format(x$ptab, fmt=Fmt("per")))
  print(ptab, quote=FALSE, print.gap = 3, right=TRUE)
  cat("\n")
}





plot.Desc <- function(x, main = NULL, ...){

  plot.Desc.z <- function(z, main=main, ...){

    if(any(z$class %in% c("numeric","integer","character","factor","ordered","logical","Date",
                          "table","matrix","xtabs",
                          "factfact", "numnum", "factnum", "numfact")))
      eval(parse(text=gettextf("plot.Desc.%s(z, main=main, ...)", z$class)))

    else if(z$class %in% c("header")) {
      # do nothing

    } else if(z$class %in% c("palette")) {
      # eval(parse(text=gettextf("plot.%s(z, ...)", z$class)))

    } else {
      plot.Desc.default(z, main=main, ...)
    }

  }

  # dispatch for classes
  lapply(x, plot.Desc.z, main=main, ...)

  invisible()
}


plot.Desc.default   <- function(x, main=NULL, ...){
  return(gettextf("should plot %s \nbut might not be able to plot that stuff...", deparse(substitute(x))))
}


plot.Desc.numeric   <- function(x, main=NULL, args.hist = NULL, ...){

  # return the first value not being null of main, x$main, deparse(substitute(x))
  # (remind to allow NA here, for choosing no main title)
  main <- Reduce(function (x, y) ifelse(!is.null(x), x, y),
                 c(main, x$main, deparse(substitute(x))))

  if(x$maxrows == Inf) {
    args.hist <- list(type="mass")

  } else if(is.null(args.hist)){
    args.hist <- list(type=if(x$unique > 12) "hist" else "mass")
  }

  PlotFdist(x=x$x, main=main, args.hist=args.hist, ...)

}


plot.Desc.character <- function(x, main=NULL, ...){
  plot.Desc.factor(x, main=main, ...)
}

plot.Desc.factor    <- function (x, main=NULL, maxlablen = 25,
                                 type=c("bar","dot"), col=NULL, border=NULL, xlim=NULL, ecdf=TRUE, ...)  {


  # if (nlevels(factor(x)) <= 2) {
  #   plot.Desc.logical(x, main = main, ..., wrd=wrd)
  # }
  # else {

    oldpar <- par(no.readonly=TRUE);  on.exit( par(oldpar) )

    # was cex in the dots-args? parse dots.arguments
    cex <- unlist(match.call(expand.dots=FALSE)$...["cex"])
    if(is.null(cex)) cex <- par("cex")

    tab <- as.table(x$freq$freq)
    names(tab) <- x$freq[[1]]
    ptab <- as.table(x$freq$perc)
    trunc_fg <- (nrow(tab) > x$maxrows)
    if (!is.na(x$maxrows) && x$maxrows < nrow(tab)) {
      tab <- tab[1:min(nrow(tab), x$maxrows)]
      ptab <- ptab[1:min(nrow(tab), x$maxrows)]
    }

    if(max(nchar(names(tab))) > maxlablen ) names(tab) <- StrTrunc(names(tab), maxlablen)
    wtxt <- max(strwidth(names(tab), "inch"))
    wplot <- (par("pin")[1] - wtxt) / 2
    layout(matrix(c(1,2), nrow=1), widths=c(wtxt + wplot, wplot) * 2.54 )
    par(mai=c(1.2, max(strwidth(rev(names(tab)), "inch"))+.5, 0.2, .3)+.02)
    if(!is.na(x$main)) par(oma=c(0,0,3,0))


    switch(match.arg(arg = type, choices = c("bar", "dot")),
           dot = {

             if(is.null(xlim)) xlim <- range(pretty(tab)) + c(-1,1) * diff(range(pretty(tab))) * 0.04

             if(is.null(col)) col <- Pal()[1]
             if(is.null(border)) border <- "black"
             b <- barplot( rev(tab), horiz=TRUE, border=NA, col="white", las=1,
                           xlim=xlim,
                           xpd=FALSE, xlab="frequency", cex.names=cex, cex.axis=cex, cex.lab=cex, tck=-0.04)
             abline(h=b, v=0, col="grey", lty="dotted")
             segments(0, b, as.vector(rev(tab)), b)
             points( x=as.vector(rev(tab)), y=b, yaxt = "n", col=border, pch=21, bg=col, cex=1.3)
             box()

             par(mai=c(1.2, 0.1, 0.2, .3)+.02)
             b <- barplot( rev(ptab), horiz=TRUE, border=NA, col="white", las=1, names="", xlim=c(-0.04,1.04),
                           xlab="percent", cex.names=cex, cex.axis=cex, cex.lab=cex, tck=-0.04)
             abline(h=b, v=0, col="grey", lty="dotted")
             segments(0, b, as.vector(rev(ptab)), b)
             points( x=as.vector(rev(ptab)), y=b, col=border, pch=21, bg=col, cex=1.3)
             box()

           }, bar = { # type = "bar"

             if(is.null(xlim)) xlim <- range(pretty(c(0.96*min(tab), 1.04*max(tab))))

             if(is.null(col)) {
               col <- c(rep("grey80", length.out=2*nrow(tab)), rep(SetAlpha("grey80",0.4), length.out=nrow(tab)))
             } else {
               if(length(col)==1){
                 col <- c(rep(col, length.out=2*nrow(tab)), rep(SetAlpha(col,0.3), length.out=nrow(tab)))
               } else {
                 col <- rep(col, length.out=3*nrow(tab))
               }
             }
             if(is.null(border)) border <- NA
             barplot( rev(tab), horiz=TRUE, col=col[1:nrow(tab)], border=border, las=1, xlim=xlim,
                      xpd=FALSE, xlab="frequency", cex.names=cex, cex.axis=cex, cex.lab=cex, tck=-0.04)
             grid(ny=NA)

             par(mai=c(1.2, 0.15, 0.2, .3) + .02)
             if(ecdf) {
               barplot( rev(cumsum(ptab)), horiz=TRUE, col=col[(2*nrow(tab)+1):(3*nrow(tab))],
                        border=border, las=1, names="", xlim=c(0,1), xlab="percent",
                        cex.names=cex, cex.axis=cex, cex.lab=cex, tck=-0.04)
               barplot( rev(ptab), horiz=TRUE, col=col[(nrow(tab)+1):(2*nrow(tab))],
                        border=border, names="", xlab=NA, ylab=NA, add=TRUE, axes=FALSE)
             } else {
               barplot( rev(ptab), horiz=TRUE, col=col[(nrow(tab)+1):(2*nrow(tab))],
                        border=border, las=1, names="", xlim=c(0,1), xlab="percent", cex.names=cex,
                        cex.axis=cex, cex.lab=cex, tck=-0.04)
             }
             grid(ny=NA)

           })


    if(is.null(main)) main <- x$main
    if(!is.na(main))
      title(main=Coalesce(main, x$main), outer=TRUE)

    if (trunc_fg)
      text(x = par()$usr[2], y = 0.4, labels = " ...[list output truncated]  ",
           cex = 0.6, adj = c(1, 0.5))

    if(!is.null(DescToolsOptions("stamp")))
      Stamp()

  invisible()

}

plot.Desc.integer   <- function(x, main=NULL, ...) {

  # switch(as.character(cut(x$unique, breaks=c(0, 2, 12,Inf), labels=1:3))
  #        , "1" = { plot.Desc.logical(x, main=main, ...) }
  #        , "2" = { plot.Desc.factor(x, main=main, ..., type="dot")  }
  #        , "3" = { plot.Desc.numeric(x, main=main, ...) }
  # )

  if(x$unique %[]% c(0, 2)){
    plot.Desc.logical(x, main=main, ...)
  } else if(x$unique %(]% c(2, 12)| (x$maxrows > 0)){
    plot.Desc.numeric(x, main=main, args.hist=list(type="mass"), ...)
  } else {
    plot.Desc.numeric(x, main=main, ...)
  }

  invisible()

}

plot.Desc.ordered   <- function(x, main=NULL, ...){
  plot.Desc.factor(x, main=main, ...)
}

plot.Desc.logical   <- function(x, main=NULL, xlab="", col=NULL, legend=TRUE, xlim=c(0,1), confint=TRUE, ...) {

  main <- Reduce(function (x, y) ifelse(!is.null(x), x, y),
                 c(main, x$main, deparse(substitute(x))))


  if(is.null(col))
    col <- c(Pal()[1:2], "grey80","grey60","grey40")
  else
    col <- rep(col, length.out=5)

  tab <- x$afrq
  ptab <- x$rfrq[, 1]
  if(nrow(x$rfrq)>2) stop( "!plot.Desc.logical! can only display 2 levels" )
  oldpar <- par(no.readonly=TRUE);  on.exit(par(oldpar))

  par(mar=c(4.1,2.1,0,2.1))
  if(!is.na(main)) par(oma=c(0,0,3,0))

  plot(x=ptab[1], y=1, cex=0.8, xlim=xlim, yaxt="n", ylab="", type="n", bty="n", xlab=xlab, main=NA)
  segments( x0=0, x1=1, y0=1, y1=1, col="grey")
  segments( x0=c(0,1), x1=c(0,1), y0=0.8, y1=1.2, col="grey")

  # insert grid
  segments( x0=seq(0,1,0.1), x1=seq(0,1,0.1), y0=0.8, y1=1.2, col="grey", lty="dotted")
  rect(xleft=0, ybottom=0.95, xright=ptab[1], ytop=1.05, col=col[1] )     # greenyellow
  rect(xleft=ptab[1], ybottom=0.95, xright=1, ytop=1.05, col=col[2] )     # green4

  if(confint){
    ci.99 <- BinomCI(tab[1], sum(tab), conf.level=0.99)[2:3]
    ci.95 <- BinomCI(tab[1], sum(tab), conf.level=0.95)[2:3]
    ci.90 <- BinomCI(tab[1], sum(tab), conf.level=0.90)[2:3]
    rect(xleft=ci.99[1], ybottom=0.9, xright=ci.99[2], ytop=1.1, col=col[3] ) # olivedrab1
    rect(xleft=ci.95[1], ybottom=0.9, xright=ci.95[2], ytop=1.1, col=col[4] ) # olivedrab3
    rect(xleft=ci.90[1], ybottom=0.9, xright=ci.90[2], ytop=1.1, col=col[5] ) # olivedrab4
    segments( x0=ptab[1], x1=ptab[1], y0=0.7, y1=1.3)
  }

  if(legend)
    legend( x=0, y=0.75, legend=c("ci.99     ","ci.95     ","ci.90     "), box.col="white"
            , fill=col[3:5], bg="white", cex=1, ncol=3, text.width=c(0.2,0.2,0.2) )
  if(length(rownames(tab)) == 1)
    text( rownames(tab), x=ptab[1]/2, y=1.2 )
  else
    text( rownames(tab), x=c(ptab[1], ptab[1] + 1)/2, y=1.2 )

  if(!is.na(main)) title(main=main, outer=TRUE)

  if(!is.null(DescToolsOptions("stamp")))    Stamp()

  invisible()

}

plot.Desc.Date      <- function(x, main=NULL, breaks=NULL, type=c(1,2,3), ...) {

  hist.axis <- function(mids, val, breaks) {
    # define a more appropriate hist x-axis as Werner Stahel suggested

    if(identical(breaks, "days")){
      mdate <- as.Date(val, origin="1970-01-01")
      axis(side=1, at=mids, labels=Day(mdate))

      idx <- c(TRUE, diff(Month(mdate))==1)
      axis(side=1, at=mids[idx], labels=Month(mdate, "mm")[idx],  line = 1, lwd = NA)

      idx <- c(TRUE, diff(Year(mdate))==1)
      axis(side=1, at=mids[idx], labels=Year(mdate)[idx],  line = 2, lwd = NA)

    } else if(identical(breaks, "weeks")){
      mdate <- as.Date(val, origin="1970-01-01")
      axis(side=1, at=mids, labels=tolower(Month(mdate, "mm")))

      idx <- c(TRUE, diff(Month(mdate))==1)
      axis(side=1, at=mids[idx], labels=Month(mdate, "mm")[idx],  line = 1, lwd = NA)

      idx <- c(TRUE, diff(Year(mdate))==1)
      axis(side=1, at=mids[idx], labels=Year(mdate)[idx],  line = 2, lwd = NA)

    } else if(identical(breaks, "months")){
      mdate <- as.Date(val, origin="1970-01-01")
      axis(side=1, at=mids, labels=tolower(Month(mdate, "mm")))

      idx <- c(TRUE, diff(Year(mdate))==1)
      axis(side=1, at=mids[idx], labels=Year(mdate)[idx],  line = 1, lwd = NA)

    } else if(identical(breaks, "quarters")){
      mdate <- as.Date(val, origin="1970-01-01")
      axis(side=1, at=mids, labels=paste("Q", Quarter(mdate), sep="-"))

      idx <- c(TRUE, diff(Year(mdate))==1)
      axis(side=1, at=mids[idx], labels=Year(mdate)[idx], line = 1, lwd = NA)

    } else if(identical(breaks, "years")){
      mdate <- as.Date(val, origin="1970-01-01")
      axis(side=1, at=mids, labels=Year(mdate))

    } else {
      axis(side=1, at=mids, labels=x$freq$level)
    }
  }


  main <- Reduce(function (x, y) ifelse(!is.null(x), x, y),
                 c(main, x$main, deparse(substitute(x))))


  # plots exp-obs dotcharts of weekdays and months
  oldpar <- par(no.readonly=TRUE);  on.exit( par(oldpar) )

  # par(mar=c(10.1,3.1,4.1,1.1), oma=c(0,9,0,0), mfrow=c(1,1))
  par(oma=c(0,9,0,0))

  # days plot type = 1
  if(any(type == 1)){
    tab <- x$dperctab$freq
    r.chi <- x$d.chisq.test

    dotchart( as.numeric(r.chi$exp[]), xlim=range(pretty(range(c(r.chi$exp[],r.chi$obs[]))))
              , color="black", bg="white", pch=21, cex=0.8, xpd=TRUE  )
    mtext(side=2, at=7:1, line=2, names(r.chi$exp), las=1)
    points( x=as.vector(r.chi$obs), y=7:1, col="black", bg="black", pch=21, cex=1.2 )
    points( x=as.vector(r.chi$exp), y=7:1, col="black", bg="white", pch=21, cex=1.2 )

    if(!is.na(main)) title(main=gettextf("%s (a: weekday)", main))

    if(!is.null(DescToolsOptions("stamp")))
      Stamp()
  }

  if(any(type == 2)){
    r.chi <- x$m.chisq.test
    month_xlim <- range(pretty(range(c(r.chi$exp[],r.chi$obs[]))))
    dotchart( as.numeric(r.chi$exp[]), xlim=month_xlim
              , color="black", bg="white", pch=21, cex=0.8, xpd=TRUE  )
    mtext(side=2, at=12:1, line=2, names(r.chi$exp), las=1)
    points( x=as.vector(r.chi$obs), y=12:1, col="black", bg="black", pch=21, cex=1.2 )
    points( x=as.vector(r.chi$exp), y=12:1, col="black", bg="white", pch=21, cex=1.2 )

    legend(x="bottom", inset=-0.5, legend=c("expected","observed"), xpd=TRUE, ncol=2
           , pch=c(21), col=c("black","black"), bg="white", pt.bg=c("white","black"), cex=1
           , pt.cex=1, xjust=0.5, adj=c(0,0.5),  text.width=c(4,4) )

    # if(!is.na(main) & is.null(wrd)) title(main=gettextf("%s (b: month)", main))
    if(!is.na(main)) title(main=gettextf("%s (b: month)", main))

    if(!is.null(DescToolsOptions("stamp")))
      Stamp()
  }

  if(any(type == 3)){

    Mar(NULL,0)
    b <- barplot(x$freq$freq, space=0, main=NA,
                 xaxt="n", col=NA, xlab="", las=1)
    # breaks can be:  c("month","days","weeks","quarter","year")
    hist.axis(mids=b, val=x$freq$level, breaks=x$hbreaks)

    if(!is.na(main)) title(main=gettextf("%s (c: %s)", main, x$hbreaks))

    if(!is.null(DescToolsOptions("stamp")))
      Stamp()
  }

}


plot.Desc.xtabs <- function(x, main=NULL, col1 = NULL, col2 = NULL, horiz = TRUE, ...){
  plot.Desc.table(x, main, col1, col2, horiz, ...)
}



plot.Desc.table     <- function(x, main=NULL, col1 = NULL, col2 = NULL, horiz = TRUE, ...){


  opt <- DescToolsOptions(stamp=NULL)

  oldpar <- par(no.readonly=TRUE)
  on.exit(par(oldpar))

  if(is.null(main)) main <- x$main

  if(length(dim(x$tab)) == 1){
#    maxrows <- InDots(..., arg="maxrows", default = 12)
#    plot.Desc.factor(Untable(x)[,], main=main, wrd=wrd, maxrows=maxrows, col=col1)
#
#   CHECK if maxrows necessary!! ***********

    #  This is baaaaaaaaaaaaaaaaaadddddddd   *******************
    plot(Desc(Untable(x$tab)[, 1]), main=main, col=col1)
    # width <- 6
    # height <- 4

  } else if(length(dim(x$tab)) > 2){
    mosaicplot(x$tab, main=main, cex=0.8, las=1, col=col1, ... )

    # width <- 8
    # height <- 8  # dimension for 2 mosaicplots
    # par(mfrow=c(1,1))
    # par(mar=c(3.1,4.1,1.1,0.5), oma=c(0,0,ifelse(is.na(main), 0, 2),0))

  } else {

    if(is.null(col1))
      col1 <- colorRampPalette(c(Pal()[1], "white", Pal()[2]), space = "rgb")(ncol(x$tab))
    if(is.null(col2))
      col2 <- colorRampPalette(c(Pal()[1], "white", Pal()[2]), space = "rgb")(nrow(x$tab))

    if(horiz){
      # width <- 16
      # height <- 6.5  # dimension for 2 mosaicplots
      par(mfrow=c(1,2))
      par(oma=c(1.1, 2.1, ifelse(is.na(main), 0, 2.1),0))

    } else {
      # width <- 7
      # height <- 14  # dimension for 2 mosaicplots
      par(mfrow=c(2,1), xpd=TRUE)
      par(oma=c(3.1, 1.1, ifelse(is.na(main), 0, 2),0))
    }

    PlotMosaic(x$tab, main=NA, xlab=NA, ylab=NA, horiz=TRUE, cols = col1)
    PlotMosaic(x$tab, main=NA, xlab=NA, ylab=NA, horiz=FALSE, cols = col2)

    title(xlab=Coalesce(names(dimnames(x$tab))[2], "x"), outer=TRUE, line=-1)
    title(ylab=Coalesce(names(dimnames(x$tab))[1], "y"), outer=TRUE, line=0)

  }

  if(!is.na(main) && (length(dim(x$tab)) == 2))     title(main, outer=TRUE)

  options(opt)
  if(!is.null(DescToolsOptions("stamp"))) Stamp()

  # invisible(list(width=width, height=height))
  invisible()

}




plot.Desc.matrix    <- function(x, main=NULL, col1 = NULL, col2 = NULL, horiz = TRUE, ...){

  # treat matrix as table
  plot.Desc.table(x, main=main, col1=col1, col2=col2, horiz=horiz, ...)
}

plot.Desc.xtabs    <- function(x, main=NULL, col1 = NULL, col2 = NULL, horiz = TRUE, ...){

  # treat matrix as table
  plot.Desc.table(x, main=main, col1=col1, col2=col2, horiz=horiz, ...)
}

plot.Desc.factfact  <- function(x, main=NULL, col1 = NULL, col2 = NULL, horiz = TRUE, ...){
  plot.Desc.table(x, main=main, col1 = col1, col2 = col2, horiz=horiz)
}

# plot.Desc.numfact   <- function(x, main=NULL, notch=FALSE, add_ni = TRUE, ... ){
#
#   # PlotMultiDens() would maybe be nice as well
#   # or perhaps violinplot??
#
#   if(is.null(main))
#     main <- x$main
#
#   # create a new graphics window
#   par(mar=c(5, 4, 2*add_ni, 2) + .1, oma=c(0, 0, 4.1, 0))
#
#   layout(matrix(c(1,2), ncol=2, byrow=TRUE), widths=c(2,1), TRUE)
#   boxplot(x$x ~ x$g, notch=notch, type="n", xaxt="n", yaxt="n", ... )
#   grid(nx=NA, ny=NULL)
#   bx <- boxplot(x$x ~ x$g, col="white", notch=notch, add=TRUE, cex.axis=0.8, ... )
#
#   if(add_ni)
#     mtext(paste("n=", bx$n, sep=""), side=3, line=1, at=1:length(bx$n), cex=0.8)
#
#   plot.design(x$x ~ x$g, cex=0.8, xlab="", ylab="", cex.axis=0.8, main="", ... )
#   mtext( "means", side=3, line=1, cex=0.8)
#
#   title(main=main, outer=TRUE)
#
#   if(!is.null(DescToolsOptions("stamp")))  Stamp()
#
#   # reset layout
#   layout(1)
#
#   invisible()
#
# }



plot.Desc.numfact <- function(x, main=NULL, add_ni = TRUE
                              , args.boxplot= NULL
                              , col=DescToolsOptions("col")
                              , xlim=NULL, args.legend = NULL
                              , type=c("design","dens"), ...) {

  opt <- DescToolsOptions(stamp=NA)

  type <- match.arg(type)

  if(is.null(main))
    main <- x$main

  z <- split(x$x, x$g)


  if(type=="dens") {

    # Alter-Geschlechtsplot
    layout(matrix(c(1, 2), nrow = 2, byrow = TRUE), heights = c(2, 1.5)[1:2], TRUE)
    par(mar = c(0, 6.1, 1.1, 2.1), oma=c(0,0,3,0))

    b <- PlotMultiDens(z, xlim=xlim, col=col
                       , args.legend=args.legend
                       , xaxt="n", panel.first=grid(col="darkgrey")
                       , ylab="", main="", las=1, na.rm=TRUE, ...)

    par(mar = c(3.1, 6.1, 1.1, 2.1))

    # set defaults for the boxplot
    args.boxplot1 <- list(x=z,
                          frame.plot = FALSE, main = "",
                          boxwex = 0.5, horizontal = TRUE,
                          ylim=b$xlim, yaxt="n", xaxt = "n",
                          outcex = 1.3, outcol = rgb(0, 0, 0, 0.5),
                          col=SetAlpha(col, 0.6))
    if ( !is.null(args.boxplot) )
      args.boxplot1[names(args.boxplot)] <- args.boxplot
    DoCall("boxplot", args.boxplot1)

    axis(side=1)
    axis(side=2, labels=names(z), at=seq(length(z)), las=1, lwd=0)

  } else  {

    # create a new graphics window
    par(mar=c(5, 4, 2*add_ni, 2) + .1, oma=c(0, 0, 4.1, 0))

    layout(matrix(c(1,2), ncol=2, byrow=TRUE), widths=c(2,1), TRUE)

    boxplot(z, border="white", xaxt="n", yaxt="n", ... )
    # set defaults for the boxplot
    args.boxplot1 <- list(x=z,
                          frame.plot = FALSE, main = "",
                          horizontal = FALSE,
                          col="white", add=TRUE, cex.axis=0.8,
                          panel.first=grid(nx=NA, ny=NULL))
    if ( !is.null(args.boxplot) )
      args.boxplot1[names(args.boxplot)] <- args.boxplot

    args.boxplot1["panel.first"]

    bx <- DoCall("boxplot", args.boxplot1)

    if(add_ni)
      mtext(paste("n=", bx$n, sep=""), side=3, line=1, at=1:length(bx$n), cex=0.8)
    d.frm <- data.frame(x$x, x$g)
    names(d.frm) <- c(x$xname, x$gname)
    plot.design(d.frm, cex=0.8, xlab="", ylab="", cex.axis=0.8, main="")

    mtext( "means", side=3, line=1, cex=0.8)

  }

  title(main = main, outer = TRUE)

  DescToolsOptions(opt)

  if(!is.null(DescToolsOptions("stamp")))
    Stamp()

  # reset layout
  layout(1)

  invisible()

}




plot.Desc.numnum    <- function(x, main = NULL, col=SetAlpha(1, 0.3),
                                pch = NULL, cex = par("cex"), bg = par("bg"),
                                xlab= NULL, ylab= NULL, smooth = NULL, smooth.front = TRUE, ...) {

  if(is.null(main))
    main <- x$main

  plot(x=x$g, y=x$x, type="n", main=main, xlab=x$gname, ylab=x$xname, ...)
  grid()
  if(smooth.front)   # smoother should be in front of the points, this is ok, if x is long
    points(x=x$g, y=x$x, col=col, pch=pch, cex=cex, bg=bg)

  if(is.null(smooth)) {
    if(x$nvalid < 500)
      smooth <- "loess"
    else
      smooth <- "spline"
  }

  if(smooth=="loess"){
    # lines(loess(x=x$g, y=x$x, na.action = na.omit))
    lines(loess(x$x ~ x$g, na.action = na.omit))

  } else if(smooth=="spline"){
    with(na.omit(data.frame(y=x$x, x=x$g)),
       lines(smooth.spline(x=x, y=y)))
  }

  if(!smooth.front)
    points(x=x$g, y=x$x, col=col, pch=pch, cex=cex, bg=bg)

  invisible()

}


plot.Desc.factnum   <- function(x, main=NULL, col=NULL, notch=FALSE,
                             add_ni = TRUE, smooth = NULL, ...){

  if(is.null(main)) main <- x$main

  usr <- par("usr");  on.exit( par(usr) )
  mar <- c(5,4, 2*add_ni,2) + .1

  par(mar=mar, oma=c(0,0,4.1,0))
  las <- InDots(..., arg="las", default=1)
  cex <- InDots(..., arg="cex", default=par("cex"))

  layout( matrix(c(1,2), ncol=2, byrow=TRUE), widths=c(2,3), TRUE)
  boxplot(x$g ~ x$x, notch=notch, type="n", xaxt="n", yaxt="n", ... )
  grid(nx=NA, ny=NULL)
  bx <- boxplot(x$g ~ x$x, col="white", notch=notch, add=TRUE, cex.axis=cex, ... )

  if(add_ni){
    # mtext does not support string rotation: https://stat.ethz.ch/pipermail/r-help/2006-February/087775.html
    mtext( paste("n=", bx$n, sep=""), side=3, line=1, at=1:length(bx$n), cex=0.8, las=las, xpd=NA)

    # set xpd=NA to be able to write on the outer margins
    # text(y=par("usr")[4] + strheight("M"), x=1:length(bx$n), srt=ifelse(las %in% c(2,3), 90, 0), adj = 0,
    #      labels = paste("n=", bx$n, sep=""), xpd=NA, cex=cex)

  }

  if(nrow(x$ptab) < 3){

    if(!is.null(x$binci))  ylim <- range(pretty(x$binci)) else ylim <- NULL

    plot(x$ptab[2,], xaxt="n", las=1, ylab="", xlab="Quantiles of x", ylim=ylim)
    axis(side=1, at=1:10, labels=gettextf("Q%s", 1:10))
    grid()

    if(!is.null(x$binci))
      ErrBars(x$binci)

    smooth <- Coalesce(smooth, x$smooth, TRUE)

    if(ncol(x$ptab) > 6 && smooth) {
      lines(loess(p ~ x, data.frame(p=x$ptab[2,], x=1:ncol(x$ptab))))
    }
    points(x=1:ncol(x$atab), y=x$ptab[2,], pch=21, cex=1.5, bg="white", type="b")

  } else {

    if(is.null(col))
      col <- colorRampPalette(c(Pal()[1], "white", Pal()[2]), space = "rgb")(nrow(x$ptab))

    PlotMosaic(x$ptab, main=NA, xlab=NA, ylab=NA, horiz=FALSE, cols = col, cex=cex, las=las, mar=mar)

  }

  title(main=main, outer=TRUE)

  if(!is.null(DescToolsOptions("stamp")))
    Stamp()

  invisible()

}





printWrd <- function(x, main=NULL, plotit=NULL, ..., wrd=wrd){

  # x is a Desc object, wrd the handle to a word instance

  WrdPlotDesc <- function(z, wrd) {

    .plotReset <- function(){
      layout(matrix(1))
      par(
        xlog = FALSE, ylog = FALSE, adj = 0.5, ann = TRUE,
        ask = FALSE, bg = "white", bty = "o", cex = 1, cex.axis = 1,
        cex.lab = 1, cex.main = 1.2, cex.sub = 1, col = "black",
        col.axis = "black", col.lab = "black", col.main = "black",
        col.sub = "black", crt = 0, err = 0L, family = "", fg = "black",
        fig = c(0, 1, 0, 1), fin = c(12.8333333333333, 8), font = 1L,
        font.axis = 1L, font.lab = 1L, font.main = 2L, font.sub = 1L,
        #      lab = c(5L, 5L, 7L), las = 0L, lend = "round", lheight = 1,
        lab = c(5L, 5L, 7L), lend = "round", lheight = 1,
        ljoin = "round", lmitre = 10, lty = "solid", lwd = 1,
        mai = c(1.36, 1.09333, 1.093333, 0.56), mar = c(5.1, 4.1,4.1, 2.1),
        mex = 1, mfcol = c(1L, 1L), mfg = c(1L, 1L, 1L, 1L),
        mfrow = c(1L, 1L), mgp = c(3, 1, 0), mkh = 0.001, new = FALSE,
        oma = c(0, 0, 0, 0), omd = c(0, 1, 0, 1), omi = c(0, 0, 0, 0),
        pch = 1L, pin = c(11.18, 5.54666666666667),
        plt = c(0.0851948051948052, 0.956363636363636, 0.17, 0.863333333333333),
        ps = 16L, pty = "m", smo = 1, srt = 0, tck = NA_real_,
        tcl = -0.5, usr = c(0, 1, 0, 1), xaxp = c(0, 1, 5), xaxs = "r", xaxt = "s", xpd = FALSE,
        yaxp = c(0, 1, 5), yaxs = "r", yaxt = "s", ylbias = 0.2)
      #     par(
      #       xlog = FALSE, ylog = FALSE,
      #       mai = c(1.36, 1.09333, 1.093333, 0.56), mar = c(5.1, 4.1,4.1, 2.1),
      #       mex = 1, mfcol = c(1L, 1L), mfg = c(1L, 1L, 1L, 1L),
      #       mfrow = c(1L, 1L),
      #       oma = c(0, 0, 0, 0), omd = c(0, 1, 0, 1), omi = c(0, 0, 0, 0),
      #       usr = c(0, 1, 0, 1), xpd = FALSE
      #       )
    }


    .plotReset()

    if(identical(z[[1]]$noplot, TRUE)) {
      # identical as noplot will not be present in filled objects!!
      # there's nothing to plot, the variable might be empty, so just leave here

    } else {

      if(any(z[[1]]$class %in% c("factor","ordered","character") || (z[[1]]$class=="integer" && !is.null(z[[1]]$freq)))) {
        plot.Desc(z, main=NA)
        WrdPlot(width=8, height=pmin(2+3/6*nrow(z[[1]]$freq), 10), dfact=2.7, crop=c(0,0,0,0), wrd=wrd, append.cr=FALSE)

      } else if(any(z[[1]]$class %in% c("numeric","integer"))){
        plot.Desc(z, main=NA)
        WrdPlot(width=8, height=5.0, dfact=2.3, crop=c(-.2,0,0,0), wrd=wrd, append.cr=FALSE)

      } else if(any(z[[1]]$class %in% "logical")){
        plot.Desc(z, main=NA)
        WrdPlot(width=6, height=4, dfact=2.6, crop=c(-.2, 0.2,1,0), wrd=wrd, append.cr=FALSE)

      } else if(z[[1]]$class == "Date"){
        plot.Desc(z, main=NA, type=1)
        WrdPlot(width=6.5, height=5, dfact=2.5, wrd=wrd, append.cr=TRUE)
        plot.Desc(z, main=NA, type=2)
        WrdPlot(width=6.5, height=6.2, dfact=2.5, wrd=wrd, append.cr=TRUE)
        plot.Desc(z, main=NA, type=3)
        WrdPlot(width=6.5, height=4, dfact=2.5, wrd=wrd, append.cr=TRUE)

      } else if(z[[1]]$class %in% c("table","matrix", "factfact")) {
        plot.Desc(z, main=NA, horiz=z[[1]]$horiz)
        if(z[[1]]$horiz)
          WrdPlot(width=16, height=6.5, dfact=2.5, wrd=wrd, append.cr=TRUE)
        else
          WrdPlot(width=7, height=14, dfact=2.5, wrd=wrd, append.cr=TRUE)

      } else if(z[[1]]$class %in% c("numnum")) {
        plot.Desc(z, main=NA)
        WrdPlot(width=6.5, height=6.5/gold_sec_c, dfact=2.5, crop=c(0,0,0.2,0), wrd=wrd, append.cr=TRUE)

      } else if(z[[1]]$class %in% c("numfact")) {
        plot.Desc(z, main=NA)
        WrdPlot(width=15, height=7, dfact=2.2, crop=c(0,0,0.2,0), wrd=wrd, append.cr=TRUE)

      } else if(z[[1]]$class %in% c("factnum")) {
        plot.Desc(z, main=NA)
        WrdPlot(width=15, height=7, dfact=2.2, crop=c(0,0,0.2,0), wrd=wrd, append.cr=TRUE)
      }

    }
    invisible()

  }


  # start main proc  ****************

  # get fixed font
  fixedfont <- getOption("fixedfont", list(name="Consolas", size=7))

  for(i in seq_along(x)){

    # # skip object header entries
    # if(names(x[i]) == "_objheader")
    #   next

    if(x[[i]]$class == "header"){

      if(is.null(x[[i]][["abstract"]])) {

        txt <- .CaptOut(print.Desc(x[i]))[-(1:2)]
        WrdCaption(x[[i]]$main, wrd=wrd)
        ToWrd(txt=txt, wrd=wrd)
        # WrdText(txt=txt, wrd=wrd )

      } else {

        attr(x[[i]]$abstract, "main") <-  x[[i]][["main"]]
        ToWrd(x[[i]]$abstract, wrd=wrd)
      }

    } else {

      WrdCaption(x[[i]]$main, wrd=wrd)

      if(!is.null(x[[i]]$label)){
        lblfont <- InDots(..., arg="font", default=list(size=8))
        lblfont$size <- 8
        ToWrd.character(x=paste("\n", x[[i]]$label, "\n", sep=""),
                        font = lblfont, wrd=wrd)
      }


      txt <- .CaptOut(print.Desc(x[i], nolabel=TRUE))[-(1:2)]

      if(x[[i]]$class == "Date"){
        WrdTable(nrow=4, ncol=2, wrd=wrd)
        # merge cells in the first row
        wrd[["Selection"]]$MoveRight(Unit=wdConst$wdCharacter, Count=2, Extend=wdConst$wdExtend)
        wrd[["Selection"]][["Cells"]]$Merge()

        ToWrd(x=txt[1:6], font=fixedfont, wrd=wrd)
        wrd[["Selection"]]$MoveRight( wdConst$wdCell, 1, 0)
        ToWrd(x=txt[-c(1:6)],  font=fixedfont, wrd=wrd)

      } else {
        if(max(unlist(lapply(txt, nchar))) < 59){  # decide if two rows or 2 columns ist adequate
          WrdTable(nrow=1, ncol=2, wrd=wrd)
          x[[i]]$horiz <- FALSE
        } else {
          WrdTable(nrow=2, ncol=1, wrd=wrd)
          x[[i]]$horiz <- TRUE
        }

        ToWrd(x=txt, font=fixedfont, wrd=wrd)

      }

      wrd[["Selection"]]$MoveRight( wdConst$wdCell, 1, 0)

      plotit <- Coalesce(plotit, x$plotit, DescToolsOptions("plotit"), FALSE)
      if(plotit)
        WrdPlotDesc(x[i], wrd=wrd)


      wrd[["Selection"]]$EndOf( wdConst$wdTable )
      # get out of tablerange
      wrd[["Selection"]]$MoveRight( wdConst$wdCharacter, 2, 0 )
      selborder <- wrd[["Selection"]]$Borders(wdConst$wdBorderTop)
      selborder[["LineStyle"]] <- wdConst$wdLineStyleSingle
      wrd[["Selection"]]$TypeParagraph()

    }
  }

  invisible()
}




ColumnWrap <- function(x, width=NULL){

  if(is.null(width))
    width <- getOption("width") / length(x)

  width <- rep(width, length.out=length(x))

  lst <- lapply(seq_along(x), function(i) strwrap(x[[i]], width=width[i]))

  maxdim <- max(unlist(lapply(lst, length)))
  lst <- lapply(lst, function(z) c(z, rep("", maxdim-length(z))))

  do.call(cbind, lst)

}







Abstract <- function (x, sep = ", ", zero.form = ".", maxlevels = 5, trunc = TRUE, list.len = 999) {

  res <- data.frame(
    nr = 1:ncol(x),
    varname = colnames(x),
    class = unlist(lapply(x, function(z) paste(class(z), collapse = ", "))),
    label = unlist(lapply(lapply(x, Label), Coalesce, "-")),

    levels = unlist(lapply(x,
                           function(z) {
                             if (nlevels(z) > 0) {

                               maxlevels <- ifelse(is.na(maxlevels) || is.infinite(maxlevels),
                                                   nlevels(z), min(nlevels(z), maxlevels))

                               txt <- gettextf("(%s): %s", nlevels(z),
                                               paste(1:maxlevels, "-", levels(z)[1:maxlevels],
                                                     sep = "", collapse = sep))

                               if(maxlevels < nlevels(z))
                                 txt <- paste(txt, ", ...", sep="")

                               txt

                             } else {
                               ""
                             }
                           })),
    NAs = unlist(lapply(x, function(z) sum(is.na(z)))),
    stringsAsFactors = FALSE)

  res$NAs <- ifelse(res$NAs != 0,
                    paste(res$NAs, " (",
                          Format(res$NAs/dim(x)[1], fmt = "%", digits = 1), ")",
                          sep = ""), zero.form)

  rownames(res) <- NULL
  res <- res[, c("nr", "varname", "class", "NAs", "levels", "label")]
  colnames(res) <- c("Nr", "ColName", "Class", "NAs", "Levels", "Label")

  res <- res[1:min(nrow(res), list.len), ]

  attr(res, "main") <- gsub(" +", " ", paste(deparse(substitute(x)), collapse=" "))
  attr(res, "nrow") <- dim(x)[1]
  attr(res, "ncol") <- dim(x)[2]
  # complete.cases can not be constructed with lists in data.frames
  attr(res, "complete") <- ifelse(all(sapply(x, is.atomic)), sum(complete.cases(x)), NA)
  attr(res, "trunc") <- trunc

  if (!is.null(attr(x, "label")))
    attr(res, "label") <- attr(x, "label")

  class(res) <- append(class(res), "abstract", after = 0)
  # res <- AddClass(res, "abstract", after = 0)

  return(res)

}



print.abstract <- function (x, sep = NULL, width = NULL,
                            trunc = NULL, print.gap = 2, ...) {

  # check if there are labels, if there aren't, we will hide the labels column
  lbl_fg <- !all(x["Label"] == "-")

  if (is.null(width)) {
    width <- unlist(lapply(x, function(x) max(nchar(as.character(x))) +
                             1))[1:4]
    width <- c(width, rep((getOption("width") - (sum(width) + 6 * print.gap))/
                            (1 + lbl_fg), (1 + lbl_fg)))
  }


  opt <- options(max.print = 1e4)
  on.exit(options(opt))

  #   # define the separator, "-------..." if not given
  sep <- Coalesce(sep, x$sep, paste(rep("-", (getOption("width") - 2)), collapse = ""))

  cat(sep, "\n")
  cat(attr(x, "main"))

  label <- attr(x, "label")

  if (!is.null(label))
    cat(" :", strwrap(label, indent = 2, exdent = 2), sep = "\n")
  else cat("\n")

  cat(gettextf("\ndata frame:\t%s obs. of  %s variables\n\t\t%s complete cases (%s)\n\n",
               attr(x, "nrow"), attr(x, "ncol"), attr(x, "complete"), Format(attr(x, "complete")/attr(x, "nrow"), fmt="%", digits=1)))

  class(x) <- "data.frame"

  if (!lbl_fg)
    x["Label"] <- NULL

  res <- apply(x, 1, ColumnWrap, width = width)
  res <- data.frame(
    if (is.matrix(res)) {
      t(res)
    } else {
      do.call(rbind, res)
    }, stringsAsFactors = FALSE)

  colnames(res) <- colnames(x)

  if(Coalesce(trunc, attr(x, "trunc"), TRUE))
    res[,] <- sapply(1:ncol(res), function(i) StrTrunc(res[,i], maxlen = width[i]))

  res$NAs <- StrAlign(res$NAs, " ")

  print(x = res, print.gap = print.gap, right = FALSE, row.names = FALSE, ...)
  cat("\n")

}


TwoGroups <- function(x, g,
                      test=t.test, main=NULL,
                      font.txt=NULL, font.desc=NULL, wrd=NULL, ...) {

  res <- list(
    txt = Phrase(x, g, na.rm=TRUE, ...),
    desc = Desc(x ~ g, plotit=FALSE, test=test, digits=1)
  )

  plot(res$desc, type="dens", main="")

  if(is.null(wrd)){
    cat(res$txt, "\n")
    print(res$desc)

  } else {

    if(is.null(main))
      main <- gettextf("%s ~ %s", deparse(substitute(x)), deparse(substitute(g)))

    WrdCaption(main, wrd = wrd)

    ToWrd(res$txt, font=font.txt, wrd=wrd)
    ToWrd("\n", wrd=wrd)

    WrdTable(ncol=2, widths=c(5, 11), wrd=wrd)
    out <- capture.output(res$desc)[-c(1:6)]
    out <- gsub("p-val", "\n  p-val", out)
    out <- gsub("contains", "\n  contains", out)
    ToWrd(out, font=font.desc, wrd=wrd)
    wrd[["Selection"]]$MoveRight(wdConst$wdCell, 1, 0)

    WrdPlot(width = 10, height = 6.5, dfact = 2.1, crop = c(0, 0, 0.3, 0),
            wrd = wrd, append.cr = TRUE)

    wrd[["Selection"]]$EndOf( wdConst$wdTable )
    # get out of tablerange
    wrd[["Selection"]]$MoveRight( wdConst$wdCharacter, 2, 0 )
    wrd[["Selection"]]$TypeParagraph()

  }

  invisible(res)

}


# Abstract <- function(x, sep=", ", zero.form=".", maxlevels=12, trunc=TRUE) {
#
#   res <- data.frame(
#     nr = 1:ncol(x)
#     , varname=colnames(x)
#     #, type=unlist(lapply(x, typeof))
#     , class=unlist(lapply(x, function(z) paste(class(z), collapse=", ")))
#     , label= unlist(lapply(lapply(x, Label), Coalesce, "-"))
#     , levels=unlist(lapply(x, function(z) {
#       if(nlevels(z) > 0)
#         gettextf("(%s): %s", nlevels(z), paste(1:nlevels(z), "-",
#             levels(z)[1:ifelse(is.na(maxlevels) || is.infinite(maxlevels), nlevels(z), maxlevels)], sep="", collapse=sep))
#       else
#         "" }))
#     , NAs=unlist(lapply(x, function(z) sum(is.na(z))))
#     , stringsAsFactors = FALSE
#   )
#
#
#   res$NAs <- StrAlign(ifelse(res$NAs != 0,
#                 paste(res$NAs, " (", Format(res$NAs/dim(x)[1], fmt="%", digits=1), ")", sep="")
#               , zero.form)
#               , sep=" ")
#
#   rownames(res) <- NULL
#
#   res <- res[, c("nr","varname","class","NAs","levels","label")]
#   colnames(res) <- c("Nr","ColName","Class","NAs","Levels","Label")
#
#   attr(res, "main") <- deparse(substitute(x))
#   attr(res, "nrow") <- dim(x)[1]
#   attr(res, "ncol") <- dim(x)[2]
#
#   attr(res, "trunc") <- trunc
#
#   if(!is.null(attr(x, "label")))
#      attr(res, "label") <- attr(x, "label")
#
#   res <- AddClass(res, "abstract", after=0)
#   return (res)
#
# }
#
#
# print.abstract <- function (x, sep=NULL, width=NULL, trunc=NULL,  print.gap=2, ...){
#
#   # check if there are labels, if there aren't, we will hide the labels column
#   lbl_fg <- !all(x["Label"]=="-")
#
#   if(is.null(width)){
#     width <- unlist(lapply(x, function(x) max(nchar(as.character(x))) + 1))[1:4]
#     width <- c(width, rep((getOption("width") - (sum(width) + 6*print.gap))/ (1+lbl_fg), (1+lbl_fg)))
#   }
#
#
#   # define the separator, "-------..." if not given
#   sep <- Coalesce(sep, x$sep, paste(rep("-", (getOption("width") - 2)), collapse = ""))
#   cat(sep, "\n")
#
#   cat(attr(x, "main"))
#
#   label <- attr(x, "label")
#
#   if (!is.null(label))
#     cat(" :", strwrap(label, indent = 2, exdent = 2), sep = "\n")
#   else
#     cat("\n")
#
#   cat(gettextf("\ndata.frame:	%s obs. of  %s variables\n\n", attr(x, "nrow"), attr(x, "ncol")))
#
#   class(x) <- "data.frame"
#
#   if(!lbl_fg)
#     x["Label"] <- NULL
#
#   trunc <- Coalesce(trunc, attr(x, "trunc"), TRUE)
#
#   res <- apply(x, 1, ColumnWrap, width=width)
#   res <- data.frame( if(is.matrix(res)){
#             t(res)
#           } else {
#             do.call(rbind,  res)
#           } )
#   colnames(res) <- colnames(x)
#
#
#   print(x=res, print.gap=print.gap, right=FALSE, row.names=FALSE, ...)
#   cat("\n")
# }
#



#
# Desc.Lc <- function (x, main = NULL, p = c(0.8,0.9,0.95,0.99), plotit=DescToolsOptions("plotit"), ...) {
#
#   # Describe a Lorenz curve
#
#   if (is.null(main))
#     main <- gettextf("%s (%s)", deparse(substitute(x)),
#                      paste(class(x), collapse = ", "))
#   n <- length(x$p)
#
#   cat(paste(rep("-", (as.numeric(options("width")) - 2)), collapse = ""),
#       "\n")
#   if (!identical(main, NA))
#     cat(main)
#   if (!is.null(attr(x, "label")))
#     cat(" :", strwrap(attr(x, "label"), indent = 2, exdent = 2),
#         sep = "\n")
#   cat("\n\n")
#
#   d.frm <- data.frame( (1-p), sapply(p, function(p){1 - approx(x=x$p, y=x$L, xout=p)$y}))
#   colnames(d.frm) <- c("1-p","1-L(p)")
#
#   txt <- d.frm
#   txt[,2] <- round(txt[,2], 3)
#   txt <- .CaptOut(txt)
#   txt <- gsub(pattern = "0\\.", replacement = " \\.", txt)
#
#   lres <- list(l1 = list(n = n, `0s` = max(x$p[x$L==0]), Gini=x$Gini))
#
#   lfmt <- lapply(lres, lapply, .fmt)
#   width <- max(c(unlist(lapply(lfmt, nchar)), unlist(lapply(lapply(lfmt, names), nchar))))
#   cat(paste(lapply(lfmt, .txtline, width = width, ind = "  ",
#                    space = " "), collapse = "\n"), "\n")
#
#   cat(txt, sep = "\n")
#
#   if (plotit)
#     if (lres$l1$n > 0)
#       plot(x, main = main)
#
#   invisible(lres)
# }
#
#
#
#
# Desc.flags <- function(x, i=1, plotit=getOption("plotit", FALSE), ...){
#
#   cat( paste(rep("-",(as.numeric(options("width"))-2)), collapse=""), "\n" )
#   main <- InDots(..., arg="main", default = "Multiple dichotomous variables")
#   cat(main)
#   if( !is.null(attr(x,"label")) ) cat(" :", strwrap(attr(x,"label"), indent=2, exdent=2), sep="\n" )
#   cat("\n")
#
#   cat( "\nSummary: \n", "total n: ", nrow(x), "\n\n", sep="" )
#
#   d.sub <- x
#
#   flags <- do.call(rbind, lapply(d.sub, function(z) {
#     tab <- table(z)
#     data.frame(val = names(tab[i]), abs=tab[i], BinomCI(tab[i], sum(tab)))
#   }
#   ))
#   out <- data.frame( do.call(rbind,  lapply(d.sub, function(x) cbind(NAs=sum(is.na(x)), n=length(x)- sum(is.na(x)))))
#                      , flags)
#   out[,5:7] <- apply(out[,5:7],2, Format, digits=3)
#
#   print(out, quote=FALSE)
#   cat("\n")
#
#   if(plotit) PlotDesc(x)
#
# }
#
#
#
# PlotDesc.flags <- function(x, ..., wrd = NULL){
#
#   oldpar <- par(no.readonly=TRUE);  on.exit( par(oldpar) )
#
#   flags <- do.call(rbind, lapply(x, function(z) {
#     tab <- table(z)
#     BinomCI(tab[1], sum(tab))
#   }))
#   rownames(flags) <- names(x)
#
#   par(mai=c(1, max(strwidth(rev(rownames(flags)), "inch"))+2, 0.2, .3)+.02)
#   PlotDot(flags, lwd = 20, code = 0, cex=0.9,
#           lcolor = PalHelsana()["hellgruen"], pch="|", pch.cex = 2.5,
#           xlim=c(0,1), lend="butt")
#
#   if(!is.null(wrd)) WrdPlot(width=8, height=pmin(3+3/6*nrow(flags), 10),
#                             dfact=2.5, crop=c(0,0,0.2,0), wrd=wrd, append.cr=TRUE)
#   invisible()
#
# }
#
#


Desc.palette <- function(x, ...){
  print(x, ...)
  if(DescToolsOptions("plotit"))
    plot(x)
}



























