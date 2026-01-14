

.onLoad <- function(libname, pkgname) {

  # cat("message from .onLoad via cat\n")

  # this does not work:!!!
  #
  # options(lastWord = NULL)
  # options(lastPP = NULL)

  # fmt <- getOption("fmt", default = list(
  #     per = structure(list(digits=1, fmt="%"), name="per", label="Percentage number format", class="fmt")
  #   , abs = structure(list(digits=0, big.mark="'"), name="abs", label="Number format for counts", class="fmt")
  #   ,	num = structure(list(digits=3, big.mark="'"), name="num", label="Number format for floating points", class="fmt")
  # ) )
  #
  # # assign("fmt", fmt, dtls.glob)
  #
  # # print(fmt)
  #
  # assign("fmt", fmt, envir = .GlobalEnv)
  # assign("fmt", fmt, envir = as.environment("Package:DescTools"))

  # does not work
  # .env <- new.env()
  # attach(.env)
  # assign(x = "fmt", value = fmt, envir = .env)

  # presetting DescTools options not already defined by the user
  op <- options()
  pkg.op <- list(
    
    DescTools.col       = c("#8296C4","#9A0941","#F08100","#FED037",
                            "#CAB790","#B3BA12","#666666"),
    DescTools.digits    = 3,
    DescTools.fixedfont = structure(list(name = "Consolas", size = 7), 
                                    class = "Font"),
    DescTools.footnote  = c("\u00B9","\u00B2","\u00B3","\u2074",
                            "\u2075","\u2076","\u2077","\u2078","\u2079"), 
    DescTools.lang      = "en",
    DescTools.plotit    = TRUE,
    DescTools.stamp     = expression(gettextf("%s / %s", Sys.getenv("USERNAME"),
                                              Format(Today(), fmt = "yyyy-mm-dd"))),
    DescTools.linesep   = cli::col_yellow("\u2500"),
    
    DescTools.lastWrd   = NULL,
    DescTools.lastXL    = NULL,
    DescTools.lastPP    = NULL,
    
    abs.sty   = structure(list(digits = 0, big.mark = "",
                                         label = "Number format for counts"), 
                                    class = "Style"),
    per.sty   = structure(list(digits = 1, fmt = "%",
                                         name = "per", label = "Percentage number format"),
                                    class = "Style"),
    num.sty   = structure(list(digits = 3, big.mark = "",
                                         label = "Number format for numeric values"), 
                                    class = "Style"),
    pval.sty   = structure(list(fmt="p", eps=1e-3,
                                          label = "Number format for p-values"),
                                     class = "Style")
  )
  
  toset <- !(names(pkg.op) %in% names(op))
  if (any(toset)) options(pkg.op[toset])

  # packageStartupMessage("Hello all!", " ", domain = "DescTools", appendLF = FALSE)
  invisible()
}

.DescToolsEnv <- new.env(parent = emptyenv())


