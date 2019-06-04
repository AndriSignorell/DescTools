\name{PercTable}
\alias{PercTable}
\alias{PercTable.default}
\alias{PercTable.table}
\alias{PercTable.formula}
\alias{PercTable.matrix}
\alias{Margins}
\alias{print.PercTable}


\title{Percentage Table
}
\description{Prints a 2-way contingency table along with percentages, marginal, and conditional distributions.
All the frequencies are nested into one single table.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{

\method{PercTable}{default}(x, y = NULL, \dots)

\method{PercTable}{table}(tab, row.vars = NULL, col.vars = NULL, justify = "right",
          freq = TRUE, rfrq = "100", expected = FALSE, residuals = FALSE,
          stdres = FALSE, margins = NULL, digits = NULL, \dots)

\method{PercTable}{formula}(formula, data, subset, na.action, \dots)

\method{print}{PercTable}(x, vsep = NULL, ...)

Margins(tab, ...)

}

\arguments{
  \item{x, y}{objects which can be interpreted as factors (including character strings).
   x and y will be tabulated via \code{table(x, y)}.\cr
   If x is a matrix, it will be coerced to a table via as.table(x).
%%     ~~Describe \code{x} here~~
}
  \item{tab}{a r x c-contingency table
%%     ~~Describe \code{tab} here~~
}
  \item{row.vars}{a vector of row variables (see Details).
%%     ~~Describe \code{row.vars} here~~
}
  \item{col.vars}{a vector of column variables (see Details). If this is left to \code{NULL} the table structure will be preserved.
%%     ~~Describe \code{col.vars} here~~
}
  \item{justify}{either \code{"left"} or \code{"right"} for defining the alignment of the table cells.
%%     ~~Describe \code{col.vars} here~~
}
  \item{freq}{boolean. Should absolute frequencies be included? Defaults to TRUE.
%%     ~~Describe \code{freq} here~~
}
  \item{rfrq}{a string with 3 characters, each of them being 1 or 0. The first position means total percentages,
  the second means row percentages and the third column percentages. "011" produces a table output with row and column percentages.
%%     ~~Describe \code{rfrq} here~~
}
  \item{expected}{the expected counts under the null hypothesis.
}
  \item{residuals}{the Pearson residuals, (observed - expected) / sqrt(expected).
}
  \item{stdres}{standardized residuals, (observed - expected) / sqrt(V), where V is the residual cell variance
    (for the case where x is a matrix, n * p * (1 - p) otherwise).
}
  \item{margins}{a vector, consisting out of 1 and/or 2. Defines the margin sums to be included.
  1 stands for row margins, 2 for column margins, c(1,2) for both. Default is \code{NULL} (none).
%%     ~~Describe \code{margins} here~~
}
\item{digits}{integer. With how many digits shoud the relative frequencies be formatted? Default can be set by \code{DescToolsOptions(digits=x)}. }

   \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} will be tabled versus rhs (\code{table(lhs, rhs)}).}
  \item{data}{an optional matrix or data frame (or similar: see \code{\link{model.frame}}) containing the variables in the formula \code{formula}.
    By default the variables are taken from \code{environment(formula)}.}
  \item{subset}{an optional vector specifying a subset of observations to be used.}
  \item{na.action}{a function which indicates what should happen when the data contain NAs. Defaults to \code{getOption("na.action")}.}

  \item{vsep}{logical, defining if an empty row should be introduced between the table rows. Default is FALSE, if only a table with one single description (either frequencies or percents) should be returned and \code{TRUE} in any other case.
%%     ~~Describe \code{digits} here~~
}
  \item{\dots}{the dots are passed from \code{PercTable.default()} to \code{PercTable.table()} and from \code{Margins} to the function \code{\link{Freq}}.
%%     ~~Describe \code{\dots} here~~
}
}
\details{PercTable prints a 2-dimensional table. The absolute and relative frequencies are nested into one flat table by means of \code{ftable}.
\code{row.vars}, resp. \code{col.vars}  can be used to define the structure of the table. \code{row.vars} can either be the names of
the dimensions (included percentages are named \code{"idx"}) or numbers (1:3, where 1 is the first dimension of the table,
2 the second and 3 the percentages). \cr
Use \code{Sort()} if you want to have your table sorted by rows.\cr\cr
The style in which numbers are formatted is selected by
\code{\link{Fmt}()} from the DescTools options.
Absolute frequencies will use \code{Fmt("abs")} and \code{Fmt("per")} will do it for the percentages. The options can be changed with \code{Fmt(abs=as.fmt(...))} which is basically a \code{"fmt"}-object containing any format information used in \code{\link{Format}}.

\code{Margins()} returns a list containing all the one dimensional margin tables of a n-dimensional table along the given dimensions. It uses \code{\link{margin.table}()} for all the dimensions and adds the appropriate percentages.

%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns an object of class \code{"ftable"}.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\references{
  Agresti, Alan (2007) \emph{Introduction to categorical data analysis}. NY: John Wiley and Sons, Section 2.4.5\cr
}

\author{
Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}


\seealso{
\code{\link{Freq}}, \code{\link{table}}, \code{\link{ftable}}, \code{\link{prop.table}}, \code{\link{addmargins}}, \code{\link{DescToolsOptions}}, \code{\link{Fmt}}\cr
There are similar functions in package \pkg{sfsmisc} \code{\link[sfsmisc]{printTable2}} and package \pkg{vcd} \code{\link[vcd]{table2d_summary}}, both
lacking some of the flexibility we needed here. \cr

}
\examples{
tab <- table(driver=d.pizza$driver, area=d.pizza$area)

PercTable(tab=tab, col.vars=2)

PercTable(tab=tab, col.vars=2, margins=c(1,2))
PercTable(tab=tab, col.vars=2, margins=2)
PercTable(tab=tab, col.vars=2, margins=1)
PercTable(tab=tab, col.vars=2, margins=NULL)

PercTable(tab=tab, col.vars=2, rfrq="000")

# just the percentages without absolute values
PercTable(tab=tab, col.vars=2, rfrq="110", freq=FALSE)

# just the row percentages in percent format (pfmt = TRUE)
PercTable(tab, freq= FALSE, rfrq="010", pfmt=TRUE, digits=1)

# just the expected frequencies and the standard residuals
PercTable(tab=tab, rfrq="000", expected = TRUE, stdres = TRUE)


# rearrange output such that freq are inserted as columns instead of rows
PercTable(tab=tab, col.vars=c(3,2), rfrq="111")

# putting the areas in rows
PercTable(tab=tab, col.vars=c(3,1), rfrq="100", margins=c(1,2))

# formula interface with subset
PercTable(driver ~ area, data=d.pizza, subset=wine_delivered==0)

# sort the table by rows, order first column (Zurich), then third, then row.names (0)
PercTable(tab=Sort(tab, ord=c(1,3,0)))

# reverse the row variables, so that absolute frequencies and percents
# are not nested together
PercTable(tab, row.vars=c(3, 1))

# the vector interface
PercTable(x=d.pizza$driver, y=d.pizza$area)
PercTable(x=d.pizza$driver, y=d.pizza$area, margins=c(1,2), rfrq="000", useNA="ifany")

# one dimensional x falls back to the function Freq()
PercTable(x=d.pizza$driver)

# the margin tables
Margins(Titanic)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{multivariate}



