

TwoGroups <- function(x, ..., plotit=TRUE){
  UseMethod("TwoGroups")
}  


TwoGroups.default <- function(x, g, main=NULL, vname=NULL, ..., plotit=TRUE){
  
  if (is.null(main)) {
    if(is.null(vname))
      main <- gettextf("%s ~ %s", deparse(substitute(x)), 
                       deparse(substitute(g)))
    else
      main <- gettextf("%s ~ %s", vname[1], vname[2])
    
  }
  
  res <- list(txt = Phrase(x = x, g = g, na.rm = TRUE, xname = vname[1]), 
              desc = Desc(x ~ g, data.frame(x, g), plotit = FALSE, test=t.test, 
                          digits = 1, main=main),
              main=main, plotit=plotit)
  
  class(res) <- "TwoGroups"
  
  return(res)
  
}


plot.TwoGroups <- function(x, main=NULL, ...){
  main <- Coalesce(main, x$main)
  plot(x$desc, type = "dens", main=main, ...)
}


print.TwoGroups <- function(x, ..., plotit=NULL){
  if(!is.na(x$main))
     cat(x$main, "\n\n")
  
  cat(x$txt, "\n\n")
  
  print(x$desc)
  
  if(Coalesce(plotit, x$plotit, DescToolsOptions("plotit"), FALSE))
    plot(x)
}


TwoGroups.formula <- function (formula, data, subset, na.action, ...) {
  
  if (missing(formula) || (length(formula) != 3L)) 
    stop("'formula' missing or incorrect")
  
  if (length(attr(terms(formula[-2L]), "term.labels")) != 1L) 
    stop("'formula' missing or incorrect")
  
  m <- match.call(expand.dots = FALSE)
  
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame())
  DNAME <- paste(names(mf), collapse = " by ")
  vname <- names(mf)
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  
  g <- factor(mf[[-response]])
  if (nlevels(g) != 2L) 
    stop("grouping factor must have exactly 2 levels")
  
  # DATA <- split(mf[[response]], g)
  x <- mf[[response]]
  
  y <- TwoGroups(x = x, g = g, vname=vname, ...)
  
  # if (length(y$estimate) == 2L) {
  #   names(y$estimate) <- paste("mean in group", levels(g))
  #   names(y$null.value) <- paste("difference in means between", 
  #                                paste("group", levels(g), collapse = " and "))
  
  y$data.name <- DNAME
  y
  
}



ToWrd.TwoGroups <- function(x, font = NULL, ..., 
                            wrd = DescToolsOptions("lastWord")) {
  
  if(!is.na(x$main))
    WrdCaption(x$main, wrd = wrd)
  
  font <- rep(font, times=2)[1:2]
  # font[1] is font.txt, font[2] font.desc
  
  ToWrd(x$txt, font = font[1], wrd = wrd)
  ToWrd("\n", wrd = wrd)
  WrdTable(ncol = 2, widths = c(5, 11), wrd = wrd)
  out <- capture.output(x$desc)[-c(1:6)]
  out <- gsub("p-val", "\n  p-val", out)
  out <- gsub("contains", "\n  contains", out)
  ToWrd(out, font = font[2], wrd = wrd)
  wrd[["Selection"]]$MoveRight(wdConst$wdCell, 1, 0)
  WrdPlot(width = 10, height = 6.5, dfact = 2.1, 
          crop = c(0, 0, 0.3, 0), wrd = wrd, append.cr = TRUE)
  
  wrd[["Selection"]]$EndOf(wdConst$wdTable)
  wrd[["Selection"]]$MoveRight(wdConst$wdCharacter, 2, 0)
  wrd[["Selection"]]$TypeParagraph()
  
}



# **************************************
# Old version
# **************************************

# TwoGroups <- function(x, g,
#                       test = t.test, main = NULL,
#                       font.txt = NULL, font.desc = NULL, wrd = NULL, ...) {
#   res <- list(
#     txt = Phrase(x, g, na.rm = TRUE, ...),
#     desc = Desc(x ~ g, plotit = FALSE, test = test, digits = 1)
#   )
# 
#   plot(res$desc, type = "dens", main = "")
# 
#   if (is.null(wrd)) {
#     cat(res$txt, "\n")
#     print(res$desc)
#   } else {
#     if (is.null(main)) {
#       main <- gettextf(
#         "%s ~ %s",
#         deparse(substitute(x)), deparse(substitute(g))
#       )
#     }
# 
#     WrdCaption(main, wrd = wrd)
# 
#     ToWrd(res$txt, font = font.txt, wrd = wrd)
#     ToWrd("\n", wrd = wrd)
# 
#     WrdTable(ncol = 2, widths = c(5, 11), wrd = wrd)
#     out <- capture.output(res$desc)[-c(1:6)]
#     out <- gsub("p-val", "\n  p-val", out)
#     out <- gsub("contains", "\n  contains", out)
#     ToWrd(out, font = font.desc, wrd = wrd)
#     wrd[["Selection"]]$MoveRight(wdConst$wdCell, 1, 0)
# 
#     WrdPlot(
#       width = 10, height = 6.5, dfact = 2.1, crop = c(0, 0, 0.3, 0),
#       wrd = wrd, append.cr = TRUE
#     )
# 
#     wrd[["Selection"]]$EndOf(wdConst$wdTable)
#     # get out of tablerange
#     wrd[["Selection"]]$MoveRight(wdConst$wdCharacter, 2, 0)
#     wrd[["Selection"]]$TypeParagraph()
#   }
# 
#   invisible(res)
# }




