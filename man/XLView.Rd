\name{XLView}
\alias{XLView}
\alias{XLKill}
\alias{ToXL}
\alias{ToXL.data.frame}
\alias{ToXL.matrix}
\alias{ToXL.default}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Use MS-Excel as Viewer for a Data.Frame
%%  ~~function to do ... ~~
}
\description{
\code{XLView} can be used to view and edit a data.frame directly in MS-Excel, resp. to create a new data.frame in MS-Excel.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
XLView(x, col.names = TRUE, row.names = FALSE, na = "", 
       preserveStrings = FALSE, sep = ";")

ToXL(x, at, ..., xl=DescToolsOptions("lastXL"))
\method{ToXL}{data.frame}(x, at, ..., xl=DescToolsOptions("lastXL"))
\method{ToXL}{matrix}(x, at, ..., xl=DescToolsOptions("lastXL"))
\method{ToXL}{default}(x, at, byrow = FALSE, ..., xl=DescToolsOptions("lastXL"))

XLKill()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ is a data.frame to be transferred to MS-Excel. If data is missing a new file will be created.
%%     ~~Describe \code{data} here~~
}
  \item{row.names}{ either a logical value indicating whether the row names of x are to be written along with x, or
  a character vector of row names to be written.}
  \item{col.names}{either a logical value indicating whether the column names of x are to be written
  along with x, or a character vector of column names to be written.
  See the section on 'CSV files' \code{\link{write.table}} for the meaning of \code{col.names = NA}. }

\item{na}{the string to use for missing values in the data.}
\item{preserveStrings}{logical, will preserve strings from being converted to numerics when imported in MS-Excel. See details. Default is \code{FALSE}.}
\item{sep}{the field separator string used for export of the object. Values within each row of x are separated by this string.}

\item{at}{can be a range adress as character (e.g. \code{"A1"}), a vector of 2 integers (e.g \code{c(1,1)}) or a cell object as it is returned by \code{xl$Cells(1,1)}, denominating the left upper cell, where the data.frame will be placed in the MS-Excel sheet.}
\item{byrow}{logical, defines if the vector should be inserted by row or by column (default).}
\item{xl}{the pointer to a MS-Excel instance. An new instance can be created with \code{GetNewXL()}, returning the appropriate handle. A handle to an already running instance is returned by \code{GetCurrXL()}.
  Default is the last created pointer stored in \code{DescToolsOptions("lastXL")}.}
\item{\dots}{further arguments are not used.}
}
\details{
The data.frame will be exported in CSV format and then imported in MS-Excel. When importing data, MS-Excel will potentially change characters to numeric values. If this seems undesirable (maybe we're loosing leading zeros) then you should enclose the text in quotes and preset a =.
x <- \code{gettextf('="\%s"', x)} would do the trick.   \cr\cr
Take care: Changes to the data made in MS-Excel will NOT automatically be updated in the original data.frame.
The user will have to read the csv-file into R again.
See examples how to get this done.\cr

\code{ToXL()} is used to export data frames or vectors directly to MS-Excel, without export the data to a csv-file and import it on the XL side. So it it possible to export several data.frames into one Workbook and edit the tables after ones needs.


\code{XLKill} will kill a running XL instance (which might be invisible). Background is the fact, that the simple XL$quit() command
would not terminate a running XL task, but only set it invisible (observe the TaskManager). This ghost version may sometimes confuse XLView and hinder to create a new instance. In such cases you have to do the garbage collection...
}
\value{the name/path of the temporary file edited in MS-Excel.
}
\author{
Andri Signorell <andri@signorell.net>, \code{ToXL()} is based on code of Duncan Temple Lang <duncan@r-project.org>
}
\note{The function works only in Windows and requires \bold{RDCOMClient} to be installed (see: Additional_repositories in DESCRIPTION of the package).
%%  ~~further notes~~
}


\seealso{\code{\link{GetNewXL}}, \code{\link{XLGetRange}}, \code{\link{XLGetWorkbook}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
# Windows-specific example
XLView(d.diamonds)

# edit an existing data.frame in MS-Excel, make changes and save there, return the filename
fn <- XLView(d.diamonds)
# read the changed file and store in new data.frame
d.frm <- read.table(fn, header=TRUE, quote="", sep=";")

# Create a new file, edit it in MS-Excel...
fn <- XLView()
# ... and read it into a data.frame when in R again
d.set <- read.table(fn, header=TRUE, quote="", sep=";")

# Export a ftable object, quite elegant...
XLView(format(ftable(Titanic), quote=FALSE), row.names = FALSE, col.names = FALSE)


# Export a data.frame directly to XL, combined with subsequent formatting

xl <- GetNewXL()
owb <- xl[["Workbooks"]]$Add()
sheet <- xl$Sheets()$Add()
sheet[["name"]] <- "pizza"

ToXL(d.pizza[1:10, 1:10], xl$Cells(1,1))

obj <- xl$Cells()$CurrentRegion()
obj[["VerticalAlignment"]] <- xlConst$xlTop

row <- xl$Cells()$CurrentRegion()$rows(1)
# does not work:   row$font()[["bold"]] <- TRUE
# works:
obj <- row$font()
obj[["bold"]] <- TRUE

obj <- row$borders(xlConst$xlEdgeBottom)
obj[["linestyle"]] <- xlConst$xlContinuous

cols <- xl$Cells()$CurrentRegion()$columns(1)
cols[["HorizontalAlignment"]] <- xlConst$xlLeft

xl$Cells()$CurrentRegion()[["EntireColumn"]]$AutoFit()
cols <- xl$Cells()$CurrentRegion()$columns(4)
cols[["WrapText"]] <- TRUE
cols[["ColumnWidth"]] <- 80
xl$Cells()$CurrentRegion()[["EntireRow"]]$AutoFit()

sheet <- xl$Sheets()$Add()
sheet[["name"]] <- "whisky"
ToXL(d.whisky[1:10, 1:10], xl$Cells(1,1))}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

\concept{MS-Office}
