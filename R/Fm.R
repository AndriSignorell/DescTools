


# Alternative names: Fx(), Fmt(), Frm(), Frmt()


# References:
# http://stackoverflow.com/questions/3443687/formatting-decimal-places-in-r
# http://my.ilstu.edu/~jhkahn/apastats.html
# https://en.wikipedia.org/wiki/Significant_figures
# http://www.originlab.com/doc/Origin-Help/Options-Dialog-NumFormat-Tab


# New super flexible and comprehensive format function


.is_ch_locale <- function() {
  # are CH-settings active?
  any(grepl("_CH|Switzerland", Sys.getlocale()))
}

.dec_sep <- function() gsub("1", "", format(1.1))


.thousands_sep <- function(sep="") {
  
  # try to get a default thousand's separator
  
  Coalesce(
           # take user's choice first
           getOption("thousands_sep"), 
           
           # if not there, use locale definition in R environment
           # but treat blank as "not defined"
           NAIfBlank(Sys.localeconv()["thousands_sep"]),
           
           # try to get the system's setting
           if(Sys.info()[["sysname"]] == "Windows"){
             utils::readRegistry("Control Panel\\International", hive = "HCU")$sThousand
             
           } else {
             # Sys.info()[["sysname"]] %in% c("Linux", "Darwin")
             out <- system("locale -k thousands_sep", intern = TRUE)
             sub(".*=", "", out[1])
           },
           
           # if all else fails, fallback to the given default ""
           sep)
}  



styles <- function(){
  
  # all styles found in environment
  env <- ls(envir = .GlobalEnv)
  
  # check if completely empty
  if(!identical(env, character(0))){
    
    found <- env[
      sapply(env, function(x) 
        inherits(get(x, envir = .GlobalEnv), "Style"))
    ]
    
    res_env <- SetNames(lapply(found, get), names=found)
    res_env <- lapply(res_env, function(x) {
      attr(x, "source") <- "GlobalEnv"
      x
    })
  } else {
    res_env <- NULL
  }
  
  # all styles defined in R options
  opt <- options()
  res_opt <- opt[sapply(opt, class) == "Style"]
  res_opt <- lapply(res_opt, function(x) {
    attr(x, "source") <- "options"
    x
  })
  
  # return all found styles 
  res <- append(res_env, res_opt)

  return(res)
  
}



# defined styles (defaults in options()) used by reporting functions
# abs.sty <- Coalesce(
#                Styles("abs"),
#                Style(digits=0, big.mark = .thousands_sep))
# num.sty <- Coalesce(
#                Styles("num"), 
#                Style(digits=3, big.mark = .thousands_sep))
# perc.sty <- Coalesce(
#                 Styles("perc"),
#                 Style(fmt="%", digits=1))
# pval.sty <- Coalesce(
#                 Styles("pval"),
#                 Style(fmt="pval", p_eps=3))





style <- function( x, digits = NULL, ldigits = NULL, sci = NULL
                    , big.mark=NULL, outdec = NULL
                    , na.form = NULL, zero.form = NULL
                    , fmt = NULL, p_eps = NULL
                    , width = NULL, align = NULL
                    , lang = NULL
                    , label = NULL
                    , ...){


  # following does not much more than return the non null provided arguments 
  # in a new class <style>
  
  # all function arguments, same arguments as Fm() uses
  # (for default values, we would use: a <- formals(get("Style", pos=1)))
  
  # so get all arguments from the Style() function
  a <- formalArgs(style)
  
  # remove dots name from the list
  a <- a[a %nin% c("x","label","...")]
  
  # get the values of all the arguments
  v <- sapply(a, dynGet)
  
  # get rid of NULLs and append dots again
  res <- c(v[!sapply(v, is.null)],
           unlist(match.call(expand.dots=FALSE)$...))    
  
  sty <- NA
  if(!missing(x)){
    if(inherits(x, "Style"))
      sty <- x
    else if(is.character(x)){
      if(x %in% names(styles()))
        sty <- styles()[[x]]
    }
    
    if(!identical(sty, NA)) {
      # a style with the given <name> has been found in the options
      # overwrite or append separately provided arguments
      sty[names(res)] <- res
    } else {
      warning("Style x could not be found!")
    } 
    
    res <- sty    
  }
  
  if(!is.null(label))
    Label(res) <- label
  
  class(res) <- "Style"
  return(res)
  
}




print.Style <- function(x, ...){
  
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
      gettextf("Example:       %s\n", fm(pi * 1e5, fmt=x)),
      sep = ""
  )
  if(!is.null(attr(x, "source"))){
    cat(cli::col_silver(gettextf("(Source:       %s)\n", attr(x, "source"))))
  }
  
  
}





.format.stars <- function(x, 
                          breaks=c(0,0.001,0.01,0.05,0.1,1), 
                          labels=c("***","** ","*  ",".  ","   ")){

  # format significance stars ***, **, * ... 
  # example: Fm(c(0.3, 0.08, 0.042, 0.001), fmt="*")
  
  res <- as.character(sapply(x, cut, 
                             breaks=breaks, 
                             labels=labels, include.lowest=TRUE))
  return(res)
  
}


.format.pstars <- function(x, p_eps, digits, ldigits)
  # format p-val AND stars
  paste(.format.pval(x, p_eps, digits, ldigits), .format.stars(x))



.format.pval <- function(x, p_eps=0.001, digits=3, ldigits=1){
  
  # format p-values  
  # this is based on original code from format.pval
  
  # if(is.null(digits))
  #   digits <- NA
  # 
  # digits <- rep(digits, length.out=3)

  # 1 has no digits
  is1 <- IsZero(x-1)
  # do not accept p-values outside [0,1]
  isna <- x %)(% c(0,1)
  
  r <- character(length(is0 <- x < p_eps))
  if (any(!is0)) {
    rr <- x <- x[!is0]
    expo <- floor(log10(ifelse(x > 0, x, 1e-50)))
    fixp <- (expo >= -3)
    
    if (any(fixp))
      rr[fixp] <- fm(x[fixp], digits=Coalesce(digits, 4), ldigits=ldigits)
    
    if (any(!fixp))
      rr[!fixp] <- format(x[!fixp], digits=Coalesce(digits, 3), scientific=TRUE)
    
    r[!is0] <- rr
  }
  if (any(is0)) {
    if(log10(p_eps) >= -3)
      p_eps <- fm(p_eps, digits=digits, ldigits=ldigits)
    else
      p_eps <- fm(p_eps, digits=1, fmt="e")

    r[is0] <- gettextf("< %s", p_eps)
  }
  
  r[is1] <- 1
  r[isna] <- NA
  
  return(r)
  
}



.format.eng <- function(x, digits = NULL, ldigits = 1
                        , zero.form = NULL, na.form = NULL){
  
  # engineering format, snap to powers of 10^3
  
  s <- lapply(strsplit(format(x, scientific=TRUE), "e"), as.numeric)
  y <- unlist(lapply(s, "[[", 1))
  pwr <- unlist(lapply(s, "[", 2))
  
  return(paste(fm(y * 10^(pwr %% 3), digits=digits, ldigits=ldigits,
                      zero.form = zero.form, na.form=na.form)
               , "e"
               , c("-","+")[(pwr >= 0) + 1]
               , fm(abs((pwr - (pwr %% 3))), ldigits = 2, digits=0)
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
             , fm(abs((pwr - (pwr %% 3))), ldigits=2, digits=0)
             , sep="")
  am <- d.prefix$abbr[match(as.numeric(a), d.prefix$mult)]
  
  a[!is.na(am)] <- am[!is.na(am)]
  a[a == "1e+00"] <- ""
  
  return(paste(fm(y * 10^(pwr %% 3), digits=digits, ldigits=ldigits,
                      zero.form = zero.form, na.form=na.form)
               , " " , a
               , sep="")
  )
  
}




fm <- function(x
               , digits = NULL, ldigits = NULL, sci = NULL
               , big.mark=NULL, outdec = NULL
               , na.form = NULL, zero.form = NULL
               , fmt = NULL, p_eps = NULL
               , width = NULL, align = NULL
               , lang = NULL
               , ...){
  UseMethod("fm")
}



fm.default <- function(x, digits = NULL, ldigits = NULL, sci = NULL
                       , big.mark=NULL, outdec = NULL
                       , na.form = NULL, zero.form = NULL
                       , fmt = NULL, p_eps = NULL
                       , width = NULL, align = NULL
                       , lang = NULL, ...){

  # Format a vector x
  
  # store names
  orig_names <- names(x)

  # store index of missing values in ina
  if ((has.na <- any(ina <- is.na(x))))
    x <- x[!ina]

  if(is.null(na.form)) na.form <- NA_real_
  
  # Dispatch on class of x **************
  
  if(all(inherits(x, c("Date", "POSIXct", "POSIXt")))) {
    
    # Format DATES
    
    # the language is only needed for date formats, so avoid looking up the option
    # for other types
    if(is.null(lang)) lang <- DescToolsOptions("lang")
    
    if(lang=="en"){
      loc <- Sys.getlocale("LC_TIME")
      Sys.setlocale("LC_TIME", "C")
      on.exit(Sys.setlocale("LC_TIME", loc), add = TRUE)
    }
    
    # defunct! Use Rcpp function from now on ...
    # r <- format(x, as.CDateFmt(fmt=fmt))
    
    # CharacterVector FmDateTime_cpp(
    #   SEXP x,
    #   std::string fmt,
    #   bool strict = true,
    #   std::string locale = "current"
    # )
    
    # for dates only the fmt argument is relevant
    if(inherits(fmt, "Style")) fmt <- fmt[["fmt"]]
    
    r <- formatDateTime(x=x, fmt=fmt, strict=TRUE, locale="current")

  } else if(all(class(x) %in% c("character","factor","ordered"))) {
    
    # Format any form of TEXT
    
    r <- format(x)
    # handle NAs
    if (has.na) r[ina] <- na.form
    return(r)

  } else { 
  
    # Format any NUMERIC values
    
    # store index of missing values in ina
    has.zero <- any(iz <- IsZero(x))
    
      
    # dispatch fmt *******************
    
    # fmt is a super flexible argument, it can contain
    # * a predefined style with class style
    # * a (character) name of a style defined as R option
    # * a special short cut for special format styles, currently:
    #     *       significance stars for p values
    #     p       the p value
    #     p*      both, p val and star
    #     %       a percentage value
    #     e       a scientific representation
    #     eng     engineering representation with powers of 10 in multiple of 3
    #     engabb  engineering representation with powers of 10 in multiple of 3
    # * a ISO-8601 code for dates
    # * a function

    if(!is.null(fmt)){
      
      if(is.function(fmt)){
        r <- fmt(x)
        
      } else if(inherits(x=fmt, what="Style")) {
        
        # class of fmt is: <style> 
        
        # we want to offer the user the option to overrun format definitions
        # consequence is, that all defaults of the function must be set to NULL
        # as we cannot distinguish between defaults and user sets else
        
        a <- formalArgs("fm")
        # do for all function arguments, besides x, ..., and fmt
        # (as it has the class "style" when landing here!)
        for( z in a[a %nin% c("x", "fmt", "...")]){
          # get the provided value for the argument
          value <- dynGet(z)
          # overwrite the style argument with the new value
          if( !is.null(value) ) fmt[z] <- value
        }
        
        # return the formatted values by recursive call of Fm()
        return(do.call(fm.default, c(fmt, x=list(x))))
        
      } else {
        
        # class of fmt is <character> (or something else than <function> or <style>) 
        
        # special code: *, p, *p, e, ...
        if(fmt=="*"){
          r <- .format.stars(x)
          
        } else if(fmt=="p"){
          # better use 0.001 than .Machine$double.eps as eps
          r <- .format.pval(x, Coalesce(p_eps, 1e-3), 
                            Coalesce(digits, 3), Coalesce(ldigits, 1))
          
        } else if(fmt=="p*"){
          r <- .format.pstars(x, Coalesce(p_eps, 1e-3), 
                              Coalesce(digits, 3), Coalesce(ldigits, 1))
          
        } else if(fmt=="eng"){
          r <- .format.eng(x, digits=digits, ldigits=ldigits, 
                           zero.form=zero.form, na.form=na.form)
          
        } else if(fmt=="engabb"){
          r <- .format.engabb(x, digits=digits, ldigits=ldigits, 
                              zero.form=zero.form, na.form=na.form)
          
        } else if(fmt=="e"){
          # r <- formatC(x, digits = digits, width = width, format = "e",
          #              big.mark=big.mark, zero.print = zero.form)
          r <- formatNum(x, digits = digits, sci_big = 0, sci_small = 0)
          
        } else if(fmt=="%"){
          # we use 1 digit as default here
          if(is.null(digits)) digits <- 1
          r <- paste(formatNum(x * 100, digits = digits,
                               big_mark = big.mark),
                     "%", sep="")
          
        } else if(fmt=="frac"){
          
          r <- as.character(MASS::fractions(x))
          
        } else if (fmt %in% names(styles())) {
          # so far fmt could be a character denoting the name of a style, 
          # defined either in the global environment or in the options
          r <- fm(x, fmt=styles()[[fmt]])

        } else {  # format else   ********************************************
          
          warning(gettextf("Non interpretable fmt code %s will be ignored.", fmt))
          r <- x
        }  
      } 
    } else {
      
      # fmt hat no value so proceed to basic numeric formatting
      
      # set the format defaults, if not provided ...

      CountDecimals <- function(x, digits = getOption("digits")) {
        outdec <- getOption("OutDec", ".")
        s <- formatC(x, digits = digits, format = "g")
        
        pos <- regexpr(outdec, s, fixed = TRUE)
        
        ifelse(
          pos > 0,
          nchar(s) - pos,
          0L
        )
      }

      # if sci is not set at all, the default will be 0, which leads to all numbers being
      # presented as scientific - this is definitely nonsense...
      if(is.null(sci))       sci <- Coalesce(NAIfZero(getOption("scipen")), 7) # default
      if(is.null(p_eps))     p_eps <- 1e-3
      if(is.null(big.mark))  big.mark <- Coalesce(getOption("big.mark"), "")
      if(is.null(ldigits))   ldigits <- 1
      if(is.null(digits))    digits <- max(CountDecimals(x))

      if(!is.null(outdec)) { opt <- options(OutDec = outdec)
                             on.exit(options(opt)) }

      # this is for sci big and sci small, this does not line up well with recyling rule!
      # ***** reconsider!! *****
      # sci <- rep(sci, length.out=2)
      # maybe better sci.big and sci.small (?)

      r <- formatNum(x,
                     digits = digits, ldigits=ldigits, # width = width, 
                     big_mark=big.mark, sci_big = sci, sci_small = -sci)

    }
    
    # replace zeros with required zero.form
    if(!is.null(zero.form) & has.zero)
      r[iz] <- zero.form
    
  }

  
  # the same with NAs
  if (has.na) {
    rok <- r
    r <- character(length(ina))
    r[!ina] <- rok
    r[ina] <- na.form
  }
  
  # Do the alignment
  if(!is.null(align)){
    r <- StrAlign(r, sep = align)
  }
  
  
  # restore names
  if (!is.null(orig_names)) names(r) <- orig_names
  
  class(r) <- c("Fm", class(r))
  return(r)

}


print.Fm <- function (x, quote=FALSE, ...) {
  
  class(x) <- class(x)[class(x)!="Fm"]
  # print(x, quote=FALSE, right=TRUE, ...)
  NextMethod("print", quote = quote, right=TRUE, ...)
}



fm.data.frame <- function(x, digits = NULL, ldigits = NULL, sci = NULL
                          , big.mark=NULL, outdec = NULL
                          , na.form = NULL, zero.form = NULL
                          , fmt = NULL, p_eps = NULL
                          , width = NULL, align = NULL
                          , lang = NULL, ...){
  
  # organise arguments as list ...
  lst <- list(digits=digits, sci=sci, big.mark=big.mark, ldigits=ldigits,
              zero.form=zero.form, na.form=na.form, fmt=fmt, align=align,
              width=width, lang=lang, p_eps=p_eps, outdec=outdec)
  # ... in order to be able to filter NULLs
  lst <- lst[!sapply(lst, is.null)]
  # and recyle them to the number of columns
  arg <- do.call(Recycle, c(lst, list(rep(1, ncol(x)))))
  
  for(i in seq(attr(arg, "maxdim")))
    x[,i] <- fm(x[,i], digits = arg$digits[i],
                    sci = arg$sci[i], big.mark = arg$big.mark[i], ldigits = arg$ldigits[i],
                    zero.form = arg$zero.form[i],
                    na.form = arg$na.form[i], fmt = arg$fmt[i], align = arg$align[i],
                    width = arg$width[i], lang = arg$lang[i], 
                    p_eps= arg$p_eps[i], outdec=arg$outdec[i])
  
  class(x) <- c("Fm", class(x))
  return(x)
  
}


fm.matrix <- function(x, digits = NULL, ldigits = NULL, sci = NULL
                      , big.mark=NULL, outdec = NULL
                      , na.form = NULL, zero.form = NULL
                      , fmt = NULL, p_eps = NULL
                      , width = NULL, align = NULL
                      , lang = NULL, ...){
  
  x[,] <- fm.default(x=x, digits=digits, sci=sci, big.mark=big.mark,
                         ldigits=ldigits, zero.form=zero.form, na.form=na.form,
                         fmt=fmt, align=align, width=width, lang=lang, 
                         p_eps=p_eps, outdec=outdec, ...)
  
  class(x) <- c("Fm", class(x))
  return(x)
}


fm.table <- function(x, digits = NULL, ldigits = NULL, sci = NULL
                     , big.mark=NULL, outdec = NULL
                     , na.form = NULL, zero.form = NULL
                     , fmt = NULL, p_eps = NULL
                     , width = NULL, align = NULL
                     , lang = NULL, ...){
  
  x[] <- fm.default(x=x, digits=digits, sci=sci, big.mark=big.mark,
                        ldigits=ldigits, zero.form=zero.form, na.form=na.form,
                        fmt=fmt, align=align, width=width, lang=lang, p_eps=p_eps, 
                        outdec=outdec,...)
  
  class(x) <- c("Fm", class(x))
  return(x)
}


fm.ftable <- function(x, digits = NULL, ldigits = NULL, sci = NULL
                      , big.mark=NULL, outdec = NULL
                      , na.form = NULL, zero.form = NULL
                      , fmt = NULL, p_eps = NULL
                      , width = NULL, align = NULL
                      , lang = NULL, ...){
  
  # convert ftable first to matrix, then to data.frame in order to 
  # apply recycled arguments columnwise, which is a common need
  res <- fm(as.data.frame(as.matrix(x)), digits = digits, sci = sci, big.mark = big.mark,
                ldigits = ldigits, zero.form = zero.form, na.form = na.form,
                fmt = fmt, align = align, width = width, lang = lang, 
                p_eps = p_eps, outdec=outdec, ...)
  
  x[] <- as.matrix(res)
  
  return(x)
  
}



fmCI <- function(x, template=NULL, ...){

  x <- fm(x, ...)
  n <- length(x)
  
  if (!n %in% c(2, 3))
    stop("x must have length 2 (lci, uci) or 3 (estimate, lci, uci)")
  
  if (is.null(template)) {
    template <- switch(
      n,
      "%s [%s, %s]",   # n = 3
      "[%s, %s]"       # n = 2
    )
  }
  
  do.call(gettextf, c(list(template), as.list(x)))

}



