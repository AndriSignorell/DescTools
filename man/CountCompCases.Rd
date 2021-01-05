\name{CountCompCases}
\alias{CountCompCases}
\alias{print.CountCompCases}


\title{Count Complete Cases
%%  ~~function to do ... ~~
}
\description{Return for each variable of a data frame the number of missing values and the complete cases to be expected if this variable would be omitted.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CountCompCases(x)

\method{print}{CountCompCases}(x, digits=1, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a data.frame containg the data.}
  \item{digits}{the number of digits to be used when printing the results.}
  \item{\dots}{the dots are not further used.}
}
\value{A list with three elements. The first gives the number of rows, the second the number of complete cases for the whole data frame.
The third element \code{tab} contains the data for the single variables.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{
\code{\link{PlotMiss}}, \code{\link{CompleteColumns}}, \code{\link{complete.cases}}, \code{\link{is.na}}, \code{\link{na.omit}}
}
\examples{
CountCompCases(d.pizza)
}
