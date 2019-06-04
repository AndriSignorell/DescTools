\name{Abstract}
\alias{Abstract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Display Compact Abstract of a Data Frame
%%  ~~function to do ... ~~
}
\description{Compactly display the content and structure of a data.frame, including variable labels. \code{str} is optimised for lists and its output is relatively technical, when it comes to e.g. attributes. \code{summary} on the other side already calculates some basic statistics.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Abstract(x, sep = ", ", zero.form = ".", maxlevels = 5, trunc = TRUE, list.len = 999)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a \code{data.frame} to be described
%%     ~~Describe \code{x} here~~
}
  \item{sep}{the separator for concatenating the levels of a factor
%%     ~~Describe \code{sep} here~~
}
  \item{zero.form}{a symbol to be used, when a variable has zero NAs.
%%     ~~Describe \code{zero.form} here~~
}
\item{maxlevels}{integer, defining how many factor levels are to be displayed. Default is 12. Set this to Inf, if all levels are needed.}
\item{trunc}{logical, defining if level names excceeding the column with should be truncated. Default ist \code{TRUE}.}

\item{list.len}{numeric; maximum number of list elements to display.}

}

\details{
The levels of a factor and describing variable labels (as created by \code{\link{Label}}) will be wrapped within the columns.

The first 4 columns are printed with the needed fix width, the last 2 (Levels and Labels) are wrapped within the column. The width is calculated depending on the width of the screen as given by \code{getOption("width")}.

ToWord has an interface for the class \code{abstract}.

}

\value{an object of class \code{abstract}, essentially a character matrix with 5 or 6 columns
containing a sequential nr (Nr), the name of the column (ColName), the class (Class), the number of NAs (NAs), the levels if the variable is a factor (Levels) and - if there are any - descriptive labels for the column (Labels).
. }


\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{str}}, \code{\link{summary}}, \code{\link{ColumnWrap}}
}
\examples{
d.mydata <- d.pizza
# let's use some labels
Label(d.mydata) <- "Lorem ipsum dolor sit amet, consetetur sadipscing elitr,
sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat,
sed diam voluptua. At vero eos et accusam."

Label(d.mydata$temperature) <- "Amet, consetetur sadipscing elitr, sed diam nonumy "

Abstract(d.mydata)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }% use one of  RShowDoc("KEYWORDS")

