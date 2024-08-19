

#' Display Compact Abstract of a Data Frame
#' 
#' Compactly display the content and structure of a `data.frame`, including
#' variable labels. `str()` is optimized for lists and its output is
#' relatively technical, when it comes to e.g. attributes. `summary()` on
#' the other side already calculates some basic statistics. 
#' 
#' The levels of a factor and describing variable labels (as created by
#' [Label()]) will be wrapped within the columns.
#' 
#' The first 4 columns are printed with the needed fix width, the last 2
#' (Levels and Labels) are wrapped within the column. The width is calculated
#' depending on the width of the screen as given by `getOption("width")`.
#' 
#' `ToWord` has an interface for the class `abstract`.
#' 
#' @name Abstract
#' @rdname Abstract
#' 
#' @param x a `data.frame` to be described
#' @param sep the separator for concatenating the levels of a factor
#' @param zero.form a symbol to be used, when a variable has zero NAs.
#' @param maxlevels (integer, `Inf`) Max. number of factor levels to display.
#'        Default is 5. Set this to `Inf`, if all levels are needed.
#' @param trunc logical, defining if level names exceeding the column with
#'        should be truncated. Default is `TRUE`.
#' 
#' @param list.len numeric; maximum number of list elements to display.
#' 
#' @return an object of class `abstract`, essentially a character matrix
#' with 5 or 6 columns containing:
#' 
#' 1. a column number (`Nr`),
#' 2. the name of the column (`ColName`),
#' 3. the column class (`Class`),
#' 4. the number of NAs (`NAs`),
#' 5. the levels if the variable is a factor (`Levels`), 
#' 6. (if there are any) descriptive labels for the column (`Labels`).
#' 
#' @author Andri Signorell <andri@@signorell.net>
#' 
#' @concept Desc
#' @family Statistical summary functions
#' @seealso [utils::str()], [base::summary()], [ColumnWrap()], [Desc()]
#' 
#' 
#' @keywords print
#' @examples
#' 
#' d.mydata <- d.pizza
#' # let's use some labels
#' Label(d.mydata) <- "Lorem ipsum dolor sit amet, consetetur sadipscing elitr,
#' sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat,
#' sed diam voluptua. At vero eos et accusam."
#' 
#' Label(d.mydata$temperature) <- "Amet, consetetur sadipscing elitr, sed diam nonumy "
#' 
#' Abstract(d.mydata)
#' 



Abstract <- function(x, sep = ", ", zero.form = ".", maxlevels = 5,
                     trunc = TRUE, list.len = 999) {
  
  shortclass <- function(x) {
    z <- unlist(lapply(x, function(z) paste(class(z), collapse = ", ")))
    res <- tolower(substr(z, 1, 3))
    # z <- c("integer", "date", "numeric", "factor", "logical", "ordered") 
    return(res)
  }
  
  
  res <- data.frame(
    nr = 1:ncol(x),
    class = shortclass(x),
    varname = colnames(x),
    label = unlist(lapply(lapply(x, Label), Coalesce, "-")),
    levels = unlist(lapply(
      x,
      function(z) {
        if (nlevels(z) > 0) {
          maxlevels <- ifelse(is.na(maxlevels) || is.infinite(maxlevels),
                              nlevels(z), min(nlevels(z), maxlevels)
          )
          
          txt <- gettextf(
            "(%s): %s", nlevels(z),
            paste(1:maxlevels, "-", levels(z)[1:maxlevels],
                  sep = "", collapse = sep
            )
          )
          
          if (maxlevels < nlevels(z)) {
            txt <- paste(txt, ", ...", sep = "")
          }
          
          txt
        } else {
          ""
        }
      }
    )),
    NAs = unlist(lapply(x, function(z) sum(is.na(z)))),
    stringsAsFactors = FALSE
  )
  
  res$NAs <- ifelse(res$NAs != 0,
                    paste(res$NAs, " (",
                          Format(res$NAs / dim(x)[1], fmt = "%", digits = 1), ")",
                          sep = ""
                    ), zero.form
  )
  
  rownames(res) <- NULL
  res <- res[, c("nr", "class", "varname", "NAs", "levels", "label")]
  colnames(res) <- c("Nr", "Class", "ColName", "NAs", "Levels", "Label")
  
  res <- res[1:min(nrow(res), list.len), ]
  
  attr(res, "main") <-
    gsub(" +", " ", paste(deparse(substitute(x)), collapse = " "))
  attr(res, "nrow") <- dim(x)[1]
  attr(res, "ncol") <- dim(x)[2]
  # complete.cases can not be constructed with lists in data.frames
  attr(res, "complete") <-
    ifelse(all(sapply(x, is.atomic)), sum(complete.cases(x)), NA)
  attr(res, "trunc") <- trunc
  
  if (!is.null(attr(x, "label"))) {
    attr(res, "label") <- attr(x, "label")
  }
  
  class(res) <- append(class(res), "abstract", after = 0)
  # res <- AddClass(res, "abstract", after = 0)
  
  return(res)
}




#' @rdname Abstract
#' @export
#' 
#' @param width Console width. If `NULL`, defaults to 
#'        [options("width")][base::options()].
#' @param print.gap (integer) Number of spaces between columns.
#' @param ... Further arguments to `print` method.


print.abstract <- function(x, sep = NULL, width = NULL,
                           trunc = NULL, print.gap = 2, ...) {
  # check if there are labels, if there aren't, we will hide the labels column
  lbl_fg <- !all(x["Label"] == "-")
  
  if (is.null(width)) {
    width <- unlist(lapply(x, function(x) {
      max(nchar(as.character(x))) +
        1
    }))[1:4]
    width <-
      c(width, rep((getOption("width") - (sum(width) + 6 * print.gap)) /
                     (1 + lbl_fg), (1 + lbl_fg)))
  }
  
  
  opt <- options(max.print = 1e4)
  on.exit(options(opt))
  
  cat(.LineSep(sep, x), "\n")
  cat(attr(x, "main"))
  
  label <- attr(x, "label")
  
  if (!is.null(label)) {
    cat(" :", strwrap(label, indent = 2, exdent = 2), sep = "\n")
  } else {
    cat("\n")
  }
  
  cat(gettextf(
    "\ndata frame:\t%s obs. of  %s variables\n\t\t%s complete cases (%s)\n\n",
    attr(x, "nrow"), attr(x, "ncol"), attr(x, "complete"),
    Format(attr(x, "complete") / attr(x, "nrow"), fmt = "%", digits = 1)
  ))
  
  class(x) <- "data.frame"
  
  if (!lbl_fg) {
    x["Label"] <- NULL
  }
  
  res <- apply(x, 1, ColumnWrap, width = width)
  res <- data.frame(
    if (is.matrix(res)) {
      t(res)
    } else {
      do.call(rbind, res)
    },
    stringsAsFactors = FALSE
  )
  
  colnames(res) <- colnames(x)
  
  if (Coalesce(trunc, attr(x, "trunc"), TRUE)) {
    res[, ] <- sapply(
      1:ncol(res),
      function(i) StrTrunc(res[, i], maxlen = width[i])
    )
  }
  
  res$NAs <- StrAlign(res$NAs, " ")
  
  print(x = res, print.gap = print.gap, right = FALSE, row.names = FALSE, ...)
  cat("\n")
}



