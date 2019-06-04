\name{\%overlaps\%}
\alias{\%overlaps\%}
\alias{Interval}
\alias{Overlap}

\title{Determines If And How Extensively Two Date Ranges Overlap
%%  ~~function to do ... ~~
}
\description{\%overlaps\% determines if two date ranges overlap at all and returns a logical value.
Interval returns the number of days of the overlapping part of the
two date periods. Inspired by the eponymous SQL-functions.
}
\usage{
x \%overlaps\% y

Overlap(x, y)

Interval(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{range 1, vector of 2 numeric values or matrix with 2 columns, the first defining the left point the second the right point of the range.
}
  \item{y}{range 2, vector of 2 numeric values or matrix with 2 columns, the first defining the left point the second the right point of the range.
}
}
\details{
\code{\%overlaps\%} returns \code{TRUE} or \code{FALSE} depending on if the two ranges overlap. The function \code{Overlap} returns the range of the overlapping region as numeric value. This will be 0, if the ranges do not overlap. \cr
\code{Interval} returns the width of the empty space between 2 ranges. Again this will be 0 if the ranges overlap.

To handle overlapping ranges there are 4 cases to consider:
\preformatted{
range a:     |--------------|
range b:  |-----|
range c:              |--------|
range d:           |-----|
          1  2  3  4  5  6  7  8}

Ranges a and b overlap, the function \code{Overlap} will return the absolute value of the overlapping region (which will be 3 - 2 = 1 in this case). The result will be the same for \code{Overlap(a, b)} and \code{Overlap(b, a)}.\cr \code{Interval} will have a direction. Ranges b and c do not overlap, \code{Overlap} will return 0, \code{\%overlaps\%} FALSE. \code{Interval} will return 2 for the case \code{Interval(a, b)} and -2 for \code{Interval(b, a)}.

This functions can be of value, if one has to decide, whether confidence intervals overlap or not.

}

\value{ returns a logical vector (match or not for each element of x). \cr
Interval and Overlap return a numeric vector.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{similar operators: \code{\link{Between}}, \code{\link{\%like\%}} \cr
for calculating the overlapping time: \code{\link{difftime}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
as.Date(c("2012-01-03", "2012-02-03")) \%overlaps\%
  as.Date(c("2012-03-01", "2012-03-03"))
as.Date(c("2012-01-03", "2012-02-03")) \%overlaps\%
  as.Date(c("2012-01-15", "2012-01-21"))

Interval(as.Date(c("2012-01-03", "2012-02-03")), as.Date(c("2012-03-01", "2012-03-03")))


# both ranges are recyled if necessary
as.Date("2012-01-03") \%overlaps\% as.Date(c("2012-03-01", "2012-03-03"))

# works with numerics as well
c(1, 18) \%overlaps\% c(10, 45)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
