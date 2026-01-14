


DescToolsOptions <- function (..., default = NULL, reset = FALSE) {
  
    # .Deprecated("setDescToolsOption")
  
    
    .Simplify <- function(x)
      if(is.list(x) && length(x)==1L)
        x[[1L]]
    else
      x
    
    # all system defaults
    def <- list(
      col       = c(DescTools::hblue, DescTools::hred,  DescTools::horange),
      digits    = 3,
      fixedfont = structure(list(name = "Consolas", size = 7), class = "font"),
      fmt       = structure(list(
        abs = structure(list(digits = 0, big.mark = "'"), .Names = c("digits", "big.mark"),
                        name = "abs", label = "Number format for counts",
                        default = TRUE, class = "fmt"),
        per = structure(list(digits = 1, fmt = "%"), .Names = c("digits", "fmt"),
                        name = "per", label = "Percentage number format",
                        default = TRUE, class = "fmt"),
        num = structure(list(digits = 3, big.mark = "'"), .Names = c("digits", "big.mark"),
                        name = "num", label = "Number format for floats",
                        default = TRUE, class = "fmt")), name = "fmt"),
      footnote  = c("'", "\"", "\"\""),
      lang      = "en",
      plotit    = TRUE,
      stamp     = expression(gettextf("%s/%s", Sys.getenv("USERNAME"),
                                      Format(Today(), fmt = "yyyy-mm-dd"))),
      lastWrd=NULL,
      lastXL=NULL,
      lastPP=NULL
    )
    
    
    # potentionally evaluate dots
    dots <- lapply(list(...), function(x) {
      if (is.symbol(x))
        eval(substitute(x, env = parent.frame()))
      else
        x
    })
    # reduce length[[1]] list to a list n (exclude single named argument)
    if(length(dots)==1L && is.list(dots) &&
       !(length(dots)==1 && !is.null(names(dots))))
      dots <- dots[[1]]
    
    # refuse to work with several options and defaults
    if (length(dots) > 1L && !is.null(default))
      stop("defaults can only be used with single options")
    
    # ignore anything else, set the defaults and return old values
    if (reset == TRUE)
      invisible(options(DescTools = def))
    
    # flag these values as defaults, not before they are potentially reset
    # do not set on lastXYZ options (can't set attribute on NULL values)
    for(i in seq_along(def)[-c(9:11)])
      attr(def[[i]], "default") <- TRUE
    
    
    opt <- getOption("DescTools")
    # store such as to return as result
    old <- opt
    # take defaults and overwrite found entries in options
    def[names(opt)] <- opt
    opt <- def
    
    # no names were given, so just return all options
    if (length(dots) == 0) {
      return(opt)
      
    } else {
      # entries were supplied, now check if there were named entries
      # dots is then a list with length 1
      if (is.null(names(dots))) {
        # if no names, check default and return either the value
        # or if this does not exist, the default
        if (!is.null(default))
          # a default is given, so get old option value and replace with user default
          # when it's NULL
          # note: in old are the original option values (no system defaults)
          return(.Simplify(ifelse(is.null(old[[dots]]), default, old[[dots]])))
        
        else
          # no defaults given, so return options, evt. sys defaults
          # reduce list to value, if length 1
          return(.Simplify(opt[unlist(dots)]))
        
      } else {
        # there are named values, so these are to be stored
        # restore old options in opt (no defaults should be stored)
        opt <- old
        if (is.null(opt))
          opt <- list()
        
        opt[names(dots)] <- dots
        # store full option set
        options(DescTools = opt)
        # return only the new set variables
        old <- old[names(dots)]
        
      }
    }
    
    invisible(old)

}


setDescToolsOption <- function(...) {
  opts <- list(...)
  stopifnot(length(opts) > 0)
  names(opts) <- paste0("DescTools.", names(opts))
  options(opts)
  invisible(NULL)
}


# internal getOption wrapper for DescTools options
.getOption <- function(name, default = NULL) {
  getOption(paste0("DescTools.", name), default)
}


