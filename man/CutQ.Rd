% $Id: CutQ.Rd 1433 2010-05-01 22:03:03Z warnes $
%
\name{CutQ}
\alias{CutQ}

\title{Create a Factor Variable Using the Quantiles of a Continuous Variable}
\description{
  Create a factor variable using the quantiles of a continous variable.
}
\usage{
CutQ(x, breaks = quantile(x, seq(0, 1, by = 0.25), na.rm = TRUE), 
     labels = NULL, na.rm = FALSE, ...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{ continous variable. }
  \item{breaks}{ the breaks for creating groups. By default the quartiles will be used, say \code{quantile} \code{seq(0, 1, by = 0.25)} quantiles. See \code{\link{quantile}} for details. If breaks is given as a single integer it is interpreted as the intended number of groups, e.g. \code{breaks=10} will return x cut in deciles.}
  \item{labels}{labels for the levels of the resulting category. By default, labels are defined as \code{Q1}, \code{Q2} to the length of breaks - 1. The parameter ist passed to \code{\link{cut}}, so if \code{labels} are set to \code{FALSE}, simple integer codes are returned instead of a factor.
%%     ~~Describe \code{labels} here~~
}

\item{na.rm}{ Boolean indicating whether missing values should be
    removed when computing quantiles.  Defaults to TRUE.}
  \item{\dots}{ Optional arguments passed to \code{\link{cut}}. }
}
\details{

  This function uses \code{\link{quantile}} to obtain the specified
  quantiles of \code{x}, then calls \code{\link{cut}} to create a factor
  variable using the intervals specified by these quantiles.

  It properly handles cases where more than one quantile obtains the
  same value, as in the second example below.  Note that in this case,
  there will be fewer generated factor levels than the specified number
  of quantile intervals.
}
\value{
  Factor variable with one level for each quantile interval given by \code{q}.
}

\author{Gregory R. Warnes <greg@warnes.net>, some slight modifications Andri Signorell <andri@signorell.net>}

\seealso{ \code{\link{cut}}, \code{\link{quantile}} }

\examples{
# create example data
\testonly{set.seed(1234)}
x <- rnorm(1000)

# cut into quartiles
quartiles <- CutQ(x)
table(quartiles)

# cut into deciles
deciles <- CutQ(x, breaks=10, labels=NULL)
table(deciles)

# show handling of 'tied' quantiles.
x <- round(x)  # discretize to create ties
stem(x)        # display the ties
deciles <- CutQ(x, breaks=10)

table(deciles) # note that there are only 5 groups (not 10) 
               # due to duplicates
}

\keyword{manip}
\keyword{univar}

