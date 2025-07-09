

# References:
# http://stackoverflow.com/questions/3443687/formatting-decimal-places-in-r
# http://my.ilstu.edu/~jhkahn/apastats.html
# https://en.wikipedia.org/wiki/Significant_figures
# http://www.originlab.com/doc/Origin-Help/Options-Dialog-NumFormat-Tab

Format <- function(x, digits = NULL, sci = NULL
                   , big.mark=NULL, ldigits = NULL
                   , zero.form = NULL, na.form = NULL
                   , fmt = NULL, align = NULL, width = NULL
                   , lang = NULL,  eps = NULL
                   , outdec = NULL, ...){
  UseMethod("Format")
}


# replaced by 0.99.26
# Format.data.frame <- function(x, digits = NULL, sci = NULL
#                           , big.mark=NULL, leading = NULL
#                           , zero.form = NULL, na.form = NULL
#                           , fmt = NULL, align = NULL, width = NULL, lang = NULL, ...){
#
#   x[] <- lapply(x, Format, digits = digits,
#                 sci = sci, big.mark = big.mark, leading = leading, zero.form = zero.form,
#                 na.form = na.form, fmt = fmt, align = align, width = width,
#                 lang = lang, ...)
#
#   class(x) <- c("Format", class(x))
#   return(x)
#
# }


Format.data.frame <- function(x, digits = NULL, sci = NULL
                              , big.mark=NULL, ldigits = NULL
                              , zero.form = NULL, na.form = NULL
                              , fmt = NULL, align = NULL, width = NULL
                              , lang = NULL, eps = NULL 
                              , outdec = NULL, ...){
  
  # organise arguments as list ...
  lst <- list(digits=digits, sci=sci, big.mark=big.mark, ldigits=ldigits,
              zero.form=zero.form, na.form=na.form, fmt=fmt, align=align,
              width=width, lang=lang, eps=eps, outdec=outdec)
  # ... in order to be able to filter NULLs
  lst <- lst[!sapply(lst, is.null)]
  # and recyle them to the number of columns
  arg <- do.call(Recycle, c(lst, list(rep(1, ncol(x)))))
  
  for(i in seq(attr(arg, "maxdim")))
    x[,i] <- Format(x[,i], digits = arg$digits[i],
                    sci = arg$sci[i], big.mark = arg$big.mark[i], ldigits = arg$ldigits[i],
                    zero.form = arg$zero.form[i],
                    na.form = arg$na.form[i], fmt = arg$fmt[i], align = arg$align[i],
                    width = arg$width[i], lang = arg$lang[i], 
                    eps= arg$eps[i], outdec=arg$outdec[i])
  
  class(x) <- c("Format", class(x))
  return(x)
  
}


Format.matrix <- function(x, digits = NULL, sci = NULL
                          , big.mark=NULL, ldigits = NULL
                          , zero.form = NULL, na.form = NULL
                          , fmt = NULL, align = NULL, width = NULL
                          , lang = NULL,  eps = NULL
                          , outdec = NULL, ...){
  
  x[,] <- Format.default(x=x, digits=digits, sci=sci, big.mark=big.mark,
                         ldigits=ldigits, zero.form=zero.form, na.form=na.form,
                         fmt=fmt, align=align, width=width, lang=lang, 
                         eps=eps, outdec=outdec, ...)
  
  class(x) <- c("Format", class(x))
  return(x)
}


Format.table <- function(x, digits = NULL, sci = NULL
                         , big.mark = NULL, ldigits = NULL
                         , zero.form = NULL, na.form = NULL
                         , fmt = NULL, align = NULL, width = NULL
                         , lang = NULL,  eps = NULL, outdec = NULL, ...){
  
  x[] <- Format.default(x=x, digits=digits, sci=sci, big.mark=big.mark,
                        ldigits=ldigits, zero.form=zero.form, na.form=na.form,
                        fmt=fmt, align=align, width=width, lang=lang, eps=eps, 
                        outdec=outdec,...)
  
  class(x) <- c("Format", class(x))
  return(x)
}


Format.ftable <- function(x, digits = NULL, sci = NULL, big.mark = NULL,
                          ldigits = NULL, zero.form = NULL, na.form = NULL,
                          fmt = NULL, align = NULL, width = NULL, lang = NULL, 
                          eps = NULL, outdec = NULL, ...){
  
  # convert ftable first to matrix, then to data.frame in order to 
  # apply recycled arguments columnwise, which is a common need
  res <- Format(as.data.frame(as.matrix(x)), digits = digits, sci = sci, big.mark = big.mark,
                ldigits = ldigits, zero.form = zero.form, na.form = na.form,
                fmt = fmt, align = align, width = width, lang = lang, 
                eps = eps, outdec=outdec, ...)
  
  x[] <- as.matrix(res)
  
  return(x)
  
}


as.CDateFmt <- function(fmt) {
  
  # fine format codes
  # http://www.autohotkey.com/docs/commands/FormatTime.htm
  
  pat <- ""
  fpat <- ""
  
  i <- 1
  # we used here:
  #       if(length(grep("\\bd{4}\\b", fmt)) > 0)
  # which found dddd only as separated string from others (\b ... blank)
  # this is not suitable for formats like yyyymmdd
  # hence this was changed to d{4}
  
  #      if(length(grep("\\bd{4}\\b", fmt)) > 0) {
  if(length(grep("d{4}", fmt)) > 0) {
    fmt <- gsub(pattern = "dddd", replacement = paste("\\\\", i, sep=""), x = fmt)
    pat <- paste(pat, "(.+)-", sep="")
    fpat <- paste(fpat, "%A-", sep="")
    i <- i+1
  }
  #      if(length(grep("\\bd{3}\\b", fmt)) > 0) {
  if(length(grep("d{3}", fmt)) > 0) {
    fmt <- gsub(pattern = "ddd", replacement = paste("\\\\", i, sep=""), x = fmt)
    pat <- paste(pat, "(.+)-", sep="")
    fpat <- paste(fpat, "%a-", sep="")
    i <- i+1
  }
  if(length(grep("d{2}", fmt)) > 0) {
    fmt <- gsub(pattern = "dd", replacement = paste("\\\\", i, sep=""), x = fmt)
    pat <- paste(pat, "(.+)-", sep="")
    fpat <- paste(fpat, "%d-", sep="")
    i <- i+1
  }
  if(length(grep("d{1}", fmt)) > 0) {
    fmt <- gsub(pattern = "d", replacement = paste("\\\\", i, sep=""), x = fmt)
    pat <- paste(pat, "0?(.+)-", sep="")
    fpat <- paste(fpat, "%e-", sep="")
    i <- i+1
  }
  if(length(grep("m{4}", fmt)) > 0) {
    fmt <- gsub(pattern = "mmmm", replacement = paste("\\\\", i, sep=""), x = fmt)
    pat <- paste(pat, "(.+)-", sep="")
    fpat <- paste(fpat, "%B-", sep="")
    i <- i+1
  }
  if(length(grep("m{3}", fmt)) > 0) {
    fmt <- gsub(pattern = "mmm", replacement = paste("\\\\", i, sep=""), x = fmt)
    pat <- paste(pat, "(.+)-", sep="")
    fpat <- paste(fpat, "%b-", sep="")
    i <- i+1
  }
  if(length(grep("m{2}", fmt)) > 0) {
    fmt <- gsub(pattern = "mm", replacement = paste("\\\\", i, sep=""), x = fmt)
    pat <- paste(pat, "(.+)-", sep="")
    fpat <- paste(fpat, "%m-", sep="")
    i <- i+1
  }
  if(length(grep("m{1}", fmt)) > 0) {
    fmt <- gsub(pattern = "m", replacement = paste("\\\\", i, sep=""), x = fmt)
    pat <- paste(pat, "0?(.+)-", sep="")
    fpat <- paste(fpat, "%m-", sep="")
    i <- i+1
  }
  if(length(grep("y{4}", fmt)) > 0) {
    fmt <- gsub(pattern = "yyyy", replacement = paste("\\\\", i, sep=""), x = fmt)
    pat <- paste(pat, "(.+)-", sep="")
    fpat <- paste(fpat, "%Y-", sep="")
    i <- i+1
  }
  if(length(grep("y{2}", fmt)) > 0) {
    fmt <- gsub(pattern = "yy", replacement = paste("\\\\", i, sep=""), x = fmt)
    pat <- paste(pat, "(.+)-", sep="")
    fpat <- paste(fpat, "%y-", sep="")
    i <- i+1
  }
  if(length(grep("y{1}", fmt)) > 0) {
    fmt <- gsub(pattern = "y", replacement = paste("\\\\", i, sep=""), x = fmt)
    pat <- paste(pat, "0?(.+)-", sep="")
    fpat <- paste(fpat, "%y-", sep="")
    i <- i+1
  }
  
  sub(pat, fmt, fpat)
  
  
}




Format.default <- function(x, digits = NULL, sci = NULL
                           , big.mark = NULL, ldigits = NULL
                           , zero.form = NULL, na.form = NULL
                           , fmt = NULL, align = NULL, width = NULL
                           , lang = NULL
                           , eps = NULL, outdec = NULL, ...){
  

  .format.pval <- function(x, eps, digits=NULL){
    # format p-values  *********************************************************
    # this is based on original code from format.pval
    
    if(is.null(digits))
      digits <- NA
    digits <- rep(digits, length.out=3)
    
    r <- character(length(is0 <- x < eps))
    if (any(!is0)) {
      rr <- x <- x[!is0]
      expo <- floor(log10(ifelse(x > 0, x, 1e-50)))
      fixp <- (expo >= -3)
      
      if (any(fixp))
        rr[fixp] <- Format(x[fixp], digits=Coalesce(digits[1], 4))
      
      if (any(!fixp))
        rr[!fixp] <- format(x[!fixp], digits=Coalesce(digits[2], 3), scientific=TRUE)
      
      r[!is0] <- rr
    }
    if (any(is0)) {
      r[is0] <- gettextf("< %s", format(eps, digits = Coalesce(digits[3], 2)))
    }
    
    return(r)
    
  }
  
  .format.stars <- function(x){
    # format significance stars  ***************************************************
    # example: Format(c(0.3, 0.08, 0.042, 0.001), fmt="*")
    
    breaks <- c(0,0.001,0.01,0.05,0.1,1)
    labels <- c("***","** ","*  ",".  ","   ")
    res <- as.character(sapply(x, cut, breaks=breaks, labels=labels, include.lowest=TRUE))
    
    return(res)
    
  }
  
  .format.pstars <- function(x, eps, digits)
    paste(.format.pval(x, eps, digits), .format.stars(x))
  
  # .leading.zero <- function(x, n, big.mark=NULL){
  #   # just add a given number of leading zeros
  #   # split at the decimal separator
  #   outdec <- getOption("OutDec")
  #   z <- strsplit(as.character(x), split=outdec, fixed = TRUE)
  #   # left side
  #   zl <- lapply(z, "[", 1)
  #   zl <- sapply(zl, function(x) sprintf(paste0("%0", n + (x<0)*1, "i"), as.numeric(x)))
  #   # right side
  #   zr <- sapply(z, "[", 2)
  #   zr <- ifelse(is.na(zr), "", paste(outdec, zr, sep=""))
  #   
  #   paste(zl, zr, sep="")
  #   
  # }
  
  .leading.zero <- function(x, n, big.mark=NULL){
    # just add a given number of leading zeros
    # split at the decimal separator
    outdec <- getOption("OutDec")
    z <- strsplit(as.character(x), split=outdec, fixed = TRUE)
    # left side
    zl <- lapply(z, "[", 1)
    zl <- sapply(zl, 
                 function(x) {
                   # remove big.marks
                   if(!is.null(big.mark))
                     x <- gsub(big.mark, "", x)
                   # append leading 0s
                   res <- sprintf(paste0("%0", n + (x<0)*1, "i"), 
                                  as.numeric(x))
                   if(!is.null(big.mark))
                     # restore big.marks
                     res <- StrRev(paste(StrChop(StrRev(res), 
                                                 len = rep(3, times=nchar(res) %/% 3 + ((nchar(res) %% 3)!=0)*1L)), collapse=big.mark))
                   return(res)
                 })
    # right side
    zr <- sapply(z, "[", 2)
    zr <- ifelse(is.na(zr), "", paste(outdec, zr, sep=""))
    
    paste(zl, zr, sep="")
    
  }
  
  
  .format.eng <- function(x, digits = NULL, ldigits = 1
                          , zero.form = NULL, na.form = NULL){
    
    s <- lapply(strsplit(format(x, scientific=TRUE), "e"), as.numeric)
    y <- unlist(lapply(s, "[[", 1))
    pwr <- unlist(lapply(s, "[", 2))
    
    return(paste(Format(y * 10^(pwr %% 3), digits=digits, ldigits=ldigits,
                        zero.form = zero.form, na.form=na.form)
                 , "e"
                 , c("-","+")[(pwr >= 0) + 1]
                 , Format(abs((pwr - (pwr %% 3))), ldigits = 2, digits=0)
                 , sep="")
    )
    
  }
  
  .format.engabb <- function(x, digits = NULL, ldigits = 1
                             , zero.form = NULL, na.form = NULL){
    
    s <- lapply(strsplit(format(x, scientific=TRUE), "e"), as.numeric)
    y <- unlist(lapply(s, "[[", 1))
    pwr <- unlist(lapply(s, "[", 2))
    
    a <- paste("1e"
               , c("-","+")[(pwr >= 0) + 1]
               , Format(abs((pwr - (pwr %% 3))), ldigits=2, digits=0)
               , sep="")
    am <- d.prefix$abbr[match(as.numeric(a), d.prefix$mult)]
    
    a[!is.na(am)] <- am[!is.na(am)]
    a[a == "1e+00"] <- ""
    
    return(paste(Format(y * 10^(pwr %% 3), digits=digits, ldigits=ldigits,
                        zero.form = zero.form, na.form=na.form)
                 , " " , a
                 , sep="")
    )
    
  }
  
  #   We accept here a fmt class to be used as user templates
  #   example:
  #
  #   fmt.int <- structure(list(
  #     digits = 5, sci = getOption("scipen"), big.mark = "",
  #     leading = NULL, zero.form = NULL, na.form = NULL,
  #     align = "left", width = NULL, txt="(%s), %s - CHF"), class="fmt"
  #   )
  #
  #   Format(7845, fmt=fmt.int)
  
  if(!is.null(InDots(..., arg = "leading", default=NULL)))
    warning("Argument 'leading' is not supported anymore, use 'ldigits' (see help)!")
  
  if(is.null(fmt)) fmt <- ""
  
  if (length(fmt) == 1) 
    if(is.character(fmt) && (fmt %in% names(DescToolsOptions("fmt")))) {
      fmt <- Fmt(fmt)
    }  
  
  if(inherits(x=fmt, what="fmt")) {
    
    # we want to offer the user the option to overrun format definitions
    # consequence is, that all defaults of the function must be set to NULL
    # as we cannot distinguish between defaults and user sets else
    
    if(!is.null(digits))        fmt$digits <- digits
    if(!is.null(sci))           fmt$sci <- sci
    if(!is.null(big.mark))      fmt$big.mark <- big.mark
    if(!is.null(ldigits))       fmt$ldigits <- ldigits
    if(!is.null(zero.form))     fmt$zero.form <- zero.form
    if(!is.null(na.form))       fmt$na.form <- na.form
    if(!is.null(align))         fmt$align <- align
    if(!is.null(width))         fmt$sci <- width
    if(!is.null(lang))          fmt$lang <- lang
    if(!is.null(eps))           fmt$eps <- eps
    if(!is.null(outdec))        fmt$outdec <- outdec
    
    return(do.call(Format, c(fmt, x=list(x))))
  }
  
  # The defined decimal character:
  # getOption("OutDec")
  
  # replaced by 0.99.26: this was not a good default, sci is easy to set
  
  # # set the defaults, if user says nothing
  # if(is.null(sci))
  #   if(is.null(digits)){
  #     # if given digits and sci NULL set sci to Inf
  #     sci <- getOption("scipen", default = 7)
  #   } else {
  #     sci <- Inf
  #   }
  
  # if sci is not set at all, the default will be 0, which leads to all numbers being
  # presented as scientific - this is definitely nonsense...
  if(is.null(sci))
    sci <- Coalesce(NAIfZero(getOption("scipen")), 7) # default
  
  sci <- rep(sci, length.out=2)
  
  if(is.null(eps))
    eps <- .Machine$double.eps
  
  if(is.null(big.mark)) big.mark <- ""
  
  if(!is.null(outdec)){
    opt <- options(OutDec = outdec)
    on.exit(options(opt))
  }
  
  if(is.null(na.form)) na.form <- NA_real_
  
  # store index of missing values in ina
  if ((has.na <- any(ina <- is.na(x))))
    x <- x[!ina]
  
  
  if(is.function(fmt)){
    
    r <- fmt(x)
    
  } else if(all(inherits(x=x, what="Date"))) {
    
    # the language is only needed for date formats, so avoid looking up the option
    # for other types
    if(is.null(lang)) lang <- DescToolsOptions("lang")
    
    if(lang=="engl"){
      loc <- Sys.getlocale("LC_TIME")
      Sys.setlocale("LC_TIME", "C")
      on.exit(Sys.setlocale("LC_TIME", loc))
    }
    
    r <- format(x, as.CDateFmt(fmt=fmt))
    
  } else if(all(class(x) %in% c("character","factor","ordered"))) {
    r <- format(x)
    
  } else if(fmt=="*"){
    r <- .format.stars(x)
    
  } else if(fmt=="p"){
    r <- .format.pval(x, eps, digits)
    
  } else if(fmt=="p*"){
    r <- .format.pstars(x, eps, digits)

  } else if(fmt=="eng"){
    r <- .format.eng(x, digits=digits, ldigits=ldigits, zero.form=zero.form, na.form=na.form)
    
  } else if(fmt=="engabb"){
    r <- .format.engabb(x, digits=digits, ldigits=ldigits, zero.form=zero.form, na.form=na.form)
    
  } else if(fmt=="e"){
    r <- formatC(x, digits = digits, width = width, format = "e",
                 big.mark=big.mark, zero.print = zero.form)
    
  } else if(fmt=="%"){
    # we use 1 digit as default here
    r <- paste(suppressWarnings(formatC(x * 100,
                                        digits = ifelse(is.null(digits), 1, digits),
                                        width = width, format = "f",
                                        big.mark=big.mark, drop0trailing = FALSE)),
               "%", sep="")
    
  } else if(fmt=="frac"){
    
    r <- as.character(MASS::fractions(x))
    
  } else {  # format else   ********************************************
    
    if(fmt != "")
      warning(gettextf("Non interpretable fmt code will be ignored.", fmt))
    
    if(identical(sci, NA)) {
      # use is.na(sci) to inhibit scientific notation
      r <- formatC(x, digits = digits, width = width, format = "f",
                   big.mark=big.mark)
    } else {
      
      # so far a numeric value, interpret negative digits
      if(!is.null(digits) && digits < 0){
        x <- round(x, digits=digits)
        digits <- 0
      }
      
      idx <- (((abs(x) > .Machine$double.eps) & (abs(x) <= 10^-sci[2])) | (abs(x) >= 10^sci[1]))
      r <- as.character(rep(NA, length(x)))
      
      # use which here instead of res[idx], because of NAs
      #   formatC is barking, classes are of no interess here, so suppress warning...
      #   what's that exactly??
      r[which(idx)] <- suppressWarnings(formatC(x[which(idx)], digits = digits, width = width, format = "e",
                                                big.mark=big.mark, drop0trailing = FALSE))
      
      #     Warning messages:
      #     1: In formatC(x[which(!idx)], digits = digits, width = width, format = "f",  :
      #                       class of 'x' was discarded
      #     formatC is barking, classes are of no interess here, so suppress warning...
      r[which(!idx)] <- suppressWarnings(formatC(x[which(!idx)], digits = digits, width = width, format = "f",
                                                 big.mark=big.mark, drop0trailing = FALSE))
    }
    
    if(!is.null(ldigits)){
      # handle leading zeros ------------------------------
      if(ldigits == 0) {
        # drop leading zeros
        r <- gsub("(?<![0-9])0+\\.", "\\.", r, perl = TRUE)
        
        # alternative:
        # res <- gsub("(-?)[^[:digit:]]0+\\.", "\\.", res)
        
        # old: mind the minus
        # res <- gsub("[^[:digit:]]0+\\.","\\.", res)
        
      } else {
        r <- .leading.zero(r, ldigits, big.mark = big.mark)
      }
    }
    
  }
  
  if(!is.null(zero.form))
    r[abs(x) < eps] <- zero.form
  
  
  if (has.na) {
    rok <- r
    r <- character(length(ina))
    r[!ina] <- rok
    r[ina] <- na.form
  }
  
  
  if(!is.null(align)){
    r <- StrAlign(r, sep = align)
  }
  
  
  class(r) <- c("Format", class(r))
  return(r)
  
}



print.Format <- function (x, quote=FALSE, ...) {
  
  class(x) <- class(x)[class(x)!="Format"]
  # print(x, quote=FALSE, right=TRUE, ...)
  NextMethod("print", quote = quote, right=TRUE, ...)
}



Fmt <- function(...){
  
  # get format templates and modify on the fly, e.g. other digits
  # x is the name of the template
  
  def <- structure(
    list(
      abs=structure(list(digits = 0, big.mark = "'"),
                    label = "Number format for counts",
                    name="abs",
                    default=TRUE, class = "fmt"),
      per=structure(list(digits = 1, fmt = "%"),
                    label = "Percentage number format",
                    name="per",
                    default=TRUE, class = "fmt"),
      num=structure(list(digits = 0, big.mark = "'"),
                    label = "Number format for floating points",
                    name="num",
                    default=TRUE, class = "fmt")
    ), name="fmt")
  
  # get a format from the fmt templates options
  res <- DescToolsOptions("fmt")
  
  # find other defined fmt in .GlobalEnv and append to list
  # found <- ls(parent.frame())[ lapply(lapply(ls(parent.frame()), function(x) gettextf("class(%s)", x)),
  #                     function(x) eval(parse(text=x))) == "fmt" ]
  # if(length(found)>0){
  #   udf <- lapply(found, function(x) eval(parse(text=x)))
  #   names(udf) <- found
  # }
  
  # collect all found formats, defaults included if not set as option
  # abs, per and num must always be available, even if not explicitly defined
  res <- c(res, def[names(def) %nin% names(res)]) #, udf)
  
  
  # get additional arguments
  dots <- list(...)
  # leave away all NULL values, these should not overwrite the defaults below
  #dots <- dots[!is.null(dots)]
  
  
  # functionality:
  # Fmt()                 return all from options
  # Fmt("abs")            return abs
  # Fmt("abs", digits=3)  return abs with updated digits
  # Fmt(c("abs","per"))   return abs and per
  
  # Fmt(nob=as.Fmt(digits=10, na.form="nodat"))  set nob
  
  
  if(length(dots)==0){
    # no arguments supplied
    # return list of defined formats
    
    # just return(res)
    
  } else {
    # some dots supplied
    # if first unnamed and the rest named, take as format name and overwrite other
    
    if(is.null(names(dots))){
      # if not names at all
      # select the requested ones by name, the unnamed ones
      fnames <- unlist(dots[is.null(names(dots))])
      res <- res[fnames]
      
      # return(res)
      
    } else {
      
      if(all(names(dots)!="")){
        # if only names (no unnamed), take name as format name and define format
        
        old <- options("DescTools")[[1]]
        opt <- old
        
        for(i in seq_along(dots))
          attr(dots[[i]], "name") <- names(dots)[[i]]
        
        opt$fmt[[names(dots)]] <- dots[[names(dots)]]
        options(DescTools=opt)
        
        # same behaviour as options
        invisible(old)
        
      } else {
        
        # select the requested ones by name, the unnamed ones
        fnames <- unlist(dots[names(dots)==""])
        res <- res[fnames]
        
        # modify additional arguments in the template definition
        for(z in names(res)){
          if(!is.null(res[[z]])){
            # use named dots, but only those which are not NULL
            idx <- names(dots) != "" & !sapply(dots[names(dots)], is.null)
            #           res[[z]][names(dots[names(dots)!=""])] <- dots[names(dots)!=""]
            res[[z]][names(dots[idx])] <- dots[idx]
          }
        }
        
        # return(res)
      }
    }
    
  }
  
  # simplify list
  if(length(res)==1) res <- res[[1]]
  
  return(res)
  
  
}


# this does not work...

# `Fmt<-` <- function (name, value){
#   opt <- options("DescTools")
#   opt$fmt[[name]] <- value
#   DescToolsOptions(opt)
# }




#
#
# # define some format templates
# .fmt_abs <- function()
#     getOption("fmt.abs", structure(list(digits=0,
#                                         big.mark="'"), class="fmt"))
# # there is an option Sys.localeconv()["thousands_sep"], but we can't change it
#

# .fmt_per <- function(digits=NULL){
#
#   # we could use getOption("digits") as default here, but this is normally not a good choice
#   # as numeric digits and percentage digits usually differ
#   res <- getOption("fmt.per", structure(list(digits=1,
#                                       fmt="%"), class="fmt"))
#   # overwrite digits if given
#   if(!is.null(digits))
#      res["digits"] <- digits
#   return(res)
# }
#

# .fmt_num <- function(digits = NULL){
#   # check if fmt is defined
#   res <- getOption("fmt.num")
#
#   # if not: use a default, based on digfix
#   if(is.null(res))
#     res <- structure(list(digits=Coalesce(digits, DescToolsOptions("digits"), 3),
#                           big.mark=Sys.localeconv()["thousands_sep"]),
#                      class="fmt")
#   else
#   # if exists overwrite digits
#     if(!is.null(digits)) res$digits <- digits
#   # what should we do, when digits are neither defined in fmt.num nor given
#   # in case the fmt.num exists?
#
#   return(res)
# }



# .fmt <- function()
#   getOption("fmt", default = list(
#     per=structure(list(digits=1, fmt="%"), name="per", label="Percentage number format", class="fmt")
#     ,	num=structure(list(digits=getOption("digfix", default=3), big.mark=Sys.localeconv()["thousands_sep"]), name="num", label="Number format for floating points", class="fmt")
#     , abs=structure(list(digits=0, big.mark=Sys.localeconv()["thousands_sep"]), name="abs", label="Number format for counts", class="fmt")
# ) )
#




print.fmt <- function(x, ...){
  
  CollapseList <- function(x){
    z <- x
    # opt <- options(useFancyQuotes=FALSE); on.exit(options(opt))
    z[unlist(lapply(z, inherits, "character"))] <- shQuote(z[unlist(lapply(z, inherits, "character"))])
    z <- paste(names(z), "=", z, sep="", collapse = ", ")
    
    return(z)
  }
  
  cat(gettextf("Format name:    %s%s\n", attr(x, "fmt_name"), 
               ifelse(identical(attr(x, "default"), TRUE), " (default)", "")),  
      gettextf("Description:   %s\n", Label(x)),
      gettextf("Definition:    %s\n", CollapseList(x)),
      gettextf("Example:       %s\n", Format(pi * 1e5, fmt=x))
  )
}



FmtCI <- function(x, template="%s [%s, %s]", ...){
  x <- Format(x, ...)
  gettextf(template, x[1], x[2], x[3])  
}


