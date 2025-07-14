


# Alternative names: Fx(), Fmt(), Frm(), Frmt()


# References:
# http://stackoverflow.com/questions/3443687/formatting-decimal-places-in-r
# http://my.ilstu.edu/~jhkahn/apastats.html
# https://en.wikipedia.org/wiki/Significant_figures
# http://www.originlab.com/doc/Origin-Help/Options-Dialog-NumFormat-Tab


# New super flexible and comprehensive format function


.is_ch_locale <- function() {
  # sind CH-Einstellungen aktiv?
  any(grepl("_CH|Switzerland", Sys.getlocale()))
}

.dec_sep <- function() sub("1", "", format(1.1))


.OptStyles <- function(){
  # return all styles defined in R options
  opt <- options()
  return(opt[sapply(opt, class) == "style"])
}



.format.stars <- function(x, 
                          breaks=c(0,0.001,0.01,0.05,0.1,1), 
                          labels=c("***","** ","*  ",".  ","   ")){
  # format significance stars  ***************************************************
  # example: Fm(c(0.3, 0.08, 0.042, 0.001), fmt="*")
  
  res <- as.character(sapply(x, cut, 
                             breaks=breaks, 
                             labels=labels, include.lowest=TRUE))
  return(res)
  
}


.format.pstars <- function(x, eps, digits)
  paste(.format.pval(x, eps, digits), .format.stars(x))



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
      rr[fixp] <- Fm(x[fixp], digits=Coalesce(digits[1], 4))
    
    if (any(!fixp))
      rr[!fixp] <- format(x[!fixp], digits=Coalesce(digits[2], 3), scientific=TRUE)
    
    r[!is0] <- rr
  }
  if (any(is0)) {
    r[is0] <- gettextf("< %s", format(eps, digits = Coalesce(digits[3], 2)))
  }
  
  return(r)
  
}



.format.eng <- function(x, digits = NULL, ldigits = 1
                        , zero.form = NULL, na.form = NULL){
  
  s <- lapply(strsplit(format(x, scientific=TRUE), "e"), as.numeric)
  y <- unlist(lapply(s, "[[", 1))
  pwr <- unlist(lapply(s, "[", 2))
  
  return(paste(Fm(y * 10^(pwr %% 3), digits=digits, ldigits=ldigits,
                      zero.form = zero.form, na.form=na.form)
               , "e"
               , c("-","+")[(pwr >= 0) + 1]
               , Fm(abs((pwr - (pwr %% 3))), ldigits = 2, digits=0)
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
             , Fm(abs((pwr - (pwr %% 3))), ldigits=2, digits=0)
             , sep="")
  am <- d.prefix$abbr[match(as.numeric(a), d.prefix$mult)]
  
  a[!is.na(am)] <- am[!is.na(am)]
  a[a == "1e+00"] <- ""
  
  return(paste(Fm(y * 10^(pwr %% 3), digits=digits, ldigits=ldigits,
                      zero.form = zero.form, na.form=na.form)
               , " " , a
               , sep="")
  )
  
}



Style <- function(  name = NULL  
                  , digits = NULL, sci = NULL
                  , big.mark=NULL, ldigits = NULL
                  , zero.form = NULL, na.form = NULL
                  , fmt = NULL, align = NULL, width = NULL
                  , lang = NULL,  eps = NULL
                  , outdec = NULL 
                  , label = NULL, ...){
  
  
  # if a name ist provided, look for the name in the DescTools options
  if(!is.null(name)){
    
    # get all defined styles in the options, and select the required one
    sty <- .OptStyles()[[name]]
    
    if(is.null(sty)){
      warning(gettextf("Style %s could not be found in options.", name))
      return(NA)
    }
  }
    
  
  # following does not much more than return the non null provided arguments 
  # in a new class <style>
  
  # all function arguments, same arguments as Fm() uses
  # for default values use: a <- formals(get("Style", pos=1))
  a <- formalArgs(Style)
  # remove dots name from the list
  a <- a[a %nin% c("name", "label","...")]
  # get the values of all the arguments
  v <- sapply(a, dynGet)
  # get rid of NULLs and append dots
  res <- c(v[!sapply(v, is.null)],
           unlist(match.call(expand.dots=FALSE)$...))    
  
  if(!is.null(name)){
    # a style with the given <name> has been found in the options
    # overwrite or append separately provided arguments
    sty[names(res)] <- res
    res <- sty
  }

  if(!is.null(label))
    Label(res) <- label
  
  class(res) <- "style"
  return(res)
  
}




print.style <- function(x, ...){
  
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
      gettextf("Example:       %s\n", Fm(pi * 1e5, fmt=x))
  )
}






Fm <- function(x, digits = NULL, sci = NULL
               , big.mark=NULL, ldigits = NULL
               , zero.form = NULL, na.form = NULL
               , fmt = NULL, align = NULL, width = NULL
               , lang = NULL,  eps = NULL
               , outdec = NULL, ...){
  UseMethod("Fm")
}



Fm.default <- function(x, digits = NULL, sci = NULL
                        , big.mark=NULL, ldigits = NULL
                        , zero.form = NULL, na.form = NULL
                        , fmt = NULL, align = NULL, width = NULL
                        , lang = NULL,  eps = NULL
                        , outdec = NULL, ...){


  # store index of missing values in ina
  if ((has.na <- any(ina <- is.na(x))))
    x <- x[!ina]


  # Dispatch on class of x **************
  
  if(all(inherits(x=x, what="Date"))) {
    
    # Format DATES
    
    # the language is only needed for date formats, so avoid looking up the option
    # for other types
    if(is.null(lang)) lang <- DescToolsOptions("lang")
    
    if(lang=="engl"){
      loc <- Sys.getlocale("LC_TIME")
      Sys.setlocale("LC_TIME", "C")
      on.exit(Sys.setlocale("LC_TIME", loc), add = TRUE)
    }
    
    r <- format(x, as.CDateFmt(fmt=fmt))
    
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
        
      } else if(inherits(x=fmt, what="style")) {
        
        # class of fmt is: <style> 
        
        # we want to offer the user the option to overrun format definitions
        # consequence is, that all defaults of the function must be set to NULL
        # as we cannot distinguish between defaults and user sets else
        
        a <- formalArgs("Fm")
        # do for all function arguments, besides x, ..., and fmt
        # (as it has the class "style" when landing here!)
        for( z in a[a %nin% c("x", "fmt", "...")]){
          # get the provided value for the argument
          value <- dynGet(z)
          # overwrite the style argument with the new value
          if( !is.null(value) ) fmt[z] <- value
        }
        
        # return the formatted values by recursive call of Fm()
        return(do.call(Fm.default, c(fmt, x=list(x))))
        
      } else {
        
        # class of fmt is <character> (or something else than <function> or <style>) 
        
        # special code: *, p, *p, e, ...
        if(fmt=="*"){
          r <- .format.stars(x)
          
        } else if(fmt=="p"){
          r <- .format.pval(x, Coalesce(eps, .Machine$double.eps), digits)
          
        } else if(fmt=="p*"){
          r <- .format.pstars(x, Coalesce(eps, .Machine$double.eps), digits)
          
        } else if(fmt=="eng"){
          r <- .format.eng(x, digits=digits, ldigits=ldigits, 
                           zero.form=zero.form, na.form=na.form)
          
        } else if(fmt=="engabb"){
          r <- .format.engabb(x, digits=digits, ldigits=ldigits, 
                              zero.form=zero.form, na.form=na.form)
          
        } else if(fmt=="e"){
          # r <- formatC(x, digits = digits, width = width, format = "e",
          #              big.mark=big.mark, zero.print = zero.form)
          r <- formatNum(x, digits = digits, sci_big = 0)
          
        } else if(fmt=="%"){
          # we use 1 digit as default here
          if(is.null(digits)) digits <- 1
          r <- paste(formatNum(x * 100, digits = digits,
                               big_mark = big.mark),
                     "%", sep="")
          
        } else if(fmt=="frac"){
          
          r <- as.character(MASS::fractions(x))
          
        } else {  # format else   ********************************************
          
            warning(gettextf("Non interpretable fmt code %s will be ignored.", fmt))
          
        }  
      } 
    } else {
      
      # fmt hat no value so proceed to basic numeric formatting
      
      # set the format defaults, if not provided ...
      
      # if sci is not set at all, the default will be 0, which leads to all numbers being
      # presented as scientific - this is definitely nonsense...
      if(is.null(sci))       sci <- Coalesce(NAIfZero(getOption("scipen")), 7) # default
      if(is.null(eps))       eps <- .Machine$double.eps
      if(is.null(big.mark))  big.mark <- Coalesce(getOption("big.mark"), "")
      if(is.null(na.form))   na.form <- NA_real_
      
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
  
  
  class(r) <- c("Fm", class(r))
  return(r)

}


print.Fm <- function (x, quote=FALSE, ...) {
  
  class(x) <- class(x)[class(x)!="Fm"]
  # print(x, quote=FALSE, right=TRUE, ...)
  NextMethod("print", quote = quote, right=TRUE, ...)
}



Fm.data.frame <- function(x, digits = NULL, sci = NULL
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
    x[,i] <- Fm(x[,i], digits = arg$digits[i],
                    sci = arg$sci[i], big.mark = arg$big.mark[i], ldigits = arg$ldigits[i],
                    zero.form = arg$zero.form[i],
                    na.form = arg$na.form[i], fmt = arg$fmt[i], align = arg$align[i],
                    width = arg$width[i], lang = arg$lang[i], 
                    eps= arg$eps[i], outdec=arg$outdec[i])
  
  class(x) <- c("Fm", class(x))
  return(x)
  
}


Fm.matrix <- function(x, digits = NULL, sci = NULL
                          , big.mark=NULL, ldigits = NULL
                          , zero.form = NULL, na.form = NULL
                          , fmt = NULL, align = NULL, width = NULL
                          , lang = NULL,  eps = NULL
                          , outdec = NULL, ...){
  
  x[,] <- Fm.default(x=x, digits=digits, sci=sci, big.mark=big.mark,
                         ldigits=ldigits, zero.form=zero.form, na.form=na.form,
                         fmt=fmt, align=align, width=width, lang=lang, 
                         eps=eps, outdec=outdec, ...)
  
  class(x) <- c("Fm", class(x))
  return(x)
}


Fm.table <- function(x, digits = NULL, sci = NULL
                         , big.mark = NULL, ldigits = NULL
                         , zero.form = NULL, na.form = NULL
                         , fmt = NULL, align = NULL, width = NULL
                         , lang = NULL,  eps = NULL, outdec = NULL, ...){
  
  x[] <- Fm.default(x=x, digits=digits, sci=sci, big.mark=big.mark,
                        ldigits=ldigits, zero.form=zero.form, na.form=na.form,
                        fmt=fmt, align=align, width=width, lang=lang, eps=eps, 
                        outdec=outdec,...)
  
  class(x) <- c("Fm", class(x))
  return(x)
}


Fm.ftable <- function(x, digits = NULL, sci = NULL, big.mark = NULL,
                          ldigits = NULL, zero.form = NULL, na.form = NULL,
                          fmt = NULL, align = NULL, width = NULL, lang = NULL, 
                          eps = NULL, outdec = NULL, ...){
  
  # convert ftable first to matrix, then to data.frame in order to 
  # apply recycled arguments columnwise, which is a common need
  res <- Fm(as.data.frame(as.matrix(x)), digits = digits, sci = sci, big.mark = big.mark,
                ldigits = ldigits, zero.form = zero.form, na.form = na.form,
                fmt = fmt, align = align, width = width, lang = lang, 
                eps = eps, outdec=outdec, ...)
  
  x[] <- as.matrix(res)
  
  return(x)
  
}



FmCI <- function(x, template="%s [%s, %s]", ...){
  x <- Fm(x, ...)
  gettextf(template, x[1], x[2], x[3])  
}


