

## GUI-Elements: select variables by dialog, FileOpen, DescDlg, ObjectBrowse ====


.InitDlg <- function(width, height, x=NULL, y=NULL, resizex=FALSE, 
                     resizey=FALSE, main="Dialog", ico="R"){

  top <- tcltk::tktoplevel()

  # Alternative for Windows:
  # if(Sys.info()["sysname"]=="Windows") {
  #   res <- system("wmic path Win32_VideoController get CurrentVerticalResolution,CurrentHorizontalResolution /format:value", intern = TRUE)
  #   res <- as.integer(StrExtract(grep("Cur", res, val=TRUE), "[0-9]+"))
  #   if(is.null(x)) x <- round(res[1]/2 - 50)
  #   if(is.null(y)) y <- round(res[2]/2 - 25)
  # }

  # if(is.null(x)) x <- as.integer(tcltk::tkwinfo("screenwidth", top))/2 - 50
  # if(is.null(y)) y <- as.integer(tcltk::tkwinfo("screenheight", top))/2 - 25

  if(is.null(x)) x <- round((as.integer(tcltk::tkwinfo("screenwidth", top)) - width)/2)
  if(is.null(y)) y <- round((as.integer(tcltk::tkwinfo("screenheight", top)) - height)/2)

  geom <- gettextf("%sx%s+%s+%s", width, height, x, y)
  tcltk::tkwm.geometry(top, geom)
  tcltk::tkwm.title(top, main)
  tcltk::tkwm.resizable(top, resizex, resizey)
  # alternative:
  # tcltk::tkwm.iconbitmap(top, file.path(find.package("DescTools"), "extdata", paste(ico, "ico", sep=".")))
  #    
  tcltk::tkwm.iconbitmap(top, gettextf("%s/%s.ico", .getDescToolsPath(), ico))
  
  return(top)

}



PasswordDlg <- function() {

  requireNamespace("tcltk", quietly = FALSE)

  e1 <- environment()
  pw <- character()

  tfpw <- tcltk::tclVar("")

  OnOK <- function() {
    assign("pw", tcltk::tclvalue(tfpw), envir = e1)
    tcltk::tkdestroy(root)
  }

  # do not update screen
  tcltk::tclServiceMode(on = FALSE)
  # create window
  root <- .InitDlg(205, 110, resizex=FALSE, resizey=FALSE, main="Login", ico="key")

  # define widgets
  content <- tcltk::tkframe(root, padx=10, pady=10)
  tfEntrPW <- tcltk::tkentry(content, width="30", textvariable=tfpw, show="*" )
  tfButOK <- tcltk::tkbutton(content,text="OK",command=OnOK, width=6)
  tfButCanc <- tcltk::tkbutton(content, text="Cancel", width=7,
                               command=function() tcltk::tkdestroy(root))

  # build GUI
  tcltk::tkgrid(content, column=0, row=0)
  tcltk::tkgrid(tcltk::tklabel(content, text="Enter Password"), column=0, row=0,
                columnspan=3, sticky="w")
  tcltk::tkgrid(tfEntrPW, column=0, row=1, columnspan=3, pady=10)
  tcltk::tkgrid(tfButOK, column=0, row=2, ipadx=15, sticky="w")
  tcltk::tkgrid(tfButCanc, column=2, row=2, ipadx=5, sticky="e")

  # binding event-handler
  tcltk::tkbind(tfEntrPW, "<Return>", OnOK)

  tcltk::tkfocus(tfEntrPW)
  tcltk::tclServiceMode(on = TRUE)

  tcltk::tcl("wm", "attributes", root, topmost=TRUE)

  tcltk::tkwait.window(root)

  return(pw)

}





# SaveAsDlg <- function(x, filename){
#   if(missing(filename))
#     filename <- file.choose()
#   if(! is.na(filename)) save(list=deparse(substitute(x)), file = filename)
#   else
#     warning("No filename supplied")
# }
#

#
# FileOpenCmd <- function(fmt=NULL) {
#
#   fn <- file.choose()
#   # fn <- tcltk::tclvalue(tcltk::tkgetOpenFile())
#
#   op <- options(useFancyQuotes = FALSE)
#   # switch from backslash to slash
#   fn <- gsub("\\\\", "/", fn)
#
#   # parse the filename into path, filename, filextension
#   fnamelong <- rev(unlist(strsplit(fn, "/")))[1]
#   ext <- rev(unlist(strsplit( fnamelong, "\\.")))[1]
#   fname <- substr(fnamelong, 1, nchar(fnamelong) - nchar(ext) - 1)
#   path <- substr(fn, 1, nchar(fn) - nchar(fname) - nchar(ext) - 1)
#
#
#   if(is.null(fmt)) {
#     if(ext %in% c("rda", "RData"))
#       fmt <- 3
#     else if(ext %in% c("dat", "csv"))
#       fmt <- 2
#     else
#       fmt <- 1
#   }
#
#
#   # read.table text:
#   if(fmt == 1) {
#     fmt <- "\"%path%%fname%.%ext%\""
#
#   } else if( fmt == 2) {
#     fmt="d.%fname% <- read.table(file = \"%path%%fname%.%ext%\", header = TRUE, sep = \";\", na.strings = c(\"NA\",\"NULL\"), strip.white = TRUE)"
#
#   } else if( fmt == 3) {
#     fmt="load(file = \"%path%%fname%.%ext%\")"
#
#   }
#
#
#   rcmd <- gsub("%fname%", fname, gsub("%ext%", ext, gsub( "%path%", path, fmt)))
#
#   # utils::writeClipboard(rcmd)
#   .ToClipboard(rcmd)
#
#   options(op)
#
#   invisible(rcmd)
#
# }



