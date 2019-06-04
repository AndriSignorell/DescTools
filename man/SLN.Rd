\name{Depreciation}
\alias{SLN}
\alias{SYD}
\alias{DB}
\alias{Depreciation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Several Methods of Depreciation of an Asset
%%  ~~function to do ... ~~
}
\description{Return the depreciation of an asset for a specified period using different methods. \code{SLN} returns the straight-line depreciation
\code{DB} uses the fixed-declining balance method
and \code{SYD} returns the sum-of-years' digits depreciation.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SLN(cost, salvage, life)
DB(cost, salvage, life, period = 1:life)
SYD(cost, salvage, life, period = 1:life)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cost}{initial cost of the asset.
%%     ~~Describe \code{cost} here~~
}
  \item{salvage}{value at the end of the depreciation (sometimes called the salvage value of the asset).
%%     ~~Describe \code{salvage} here~~
}
  \item{life}{number of periods over which the asset is depreciated (sometimes called the useful life of the asset).
%%     ~~Describe \code{life} here~~
}
\item{period}{period for which you want to calculate the depreciation. Period must use the same units as life.}
}
\value{val
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{NPV}()}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# depreciation allowance for each year
SLN(cost = 50000, salvage = 10000, life = 5)
DB(cost = 50000, salvage = 10000, life = 5)

50000 - c(0, cumsum(SYD(cost = 50000, salvage = 10000, life = 5)))


}
