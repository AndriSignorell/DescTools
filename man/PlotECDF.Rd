\name{PlotECDF}
\alias{PlotECDF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Empirical Cumulative Distribution Function
%%  ~~function to do ... ~~
}
\description{Faster alternative for plotting the empirical cumulative distribution function (ecdf).
The function offers the option to construct the ecdf on the base of a histogram, which makes sense, when x is large. So the plot process is much faster, without loosing much precision in the details.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotECDF(x, breaks = NULL, col = Pal()[1], ylab = "", 
         lwd = 2, xlab = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric vector of the observations for ecdf.
%%     ~~Describe \code{x} here~~
}
  \item{breaks}{will be passed directly to \code{\link{hist}}. If left to \code{NULL}, no histogram will be used.
%%     ~~Describe \code{breaks} here~~
}
  \item{col}{color of the line.
%%     ~~Describe \code{col} here~~
}
  \item{ylab}{label for the y-axis.
%%     ~~Describe \code{ylab} here~~
}
  \item{lwd}{line width.
%%     ~~Describe \code{lwd} here~~
}
  \item{xlab}{label for the x-axis.
%%     ~~Describe \code{xlab} here~~
}
  \item{\dots}{arguments to be passed to subsequent functions.
%%     ~~Describe \code{\dots} here~~
}
}
\details{The stats function \code{\link{plot.ecdf}} is fine for vectors that are not too large. However for n ~ 1e7 we would observe a dramatic performance breakdown (possibly in combination with the use of \code{\link{do.call}}).

\code{PlotECDF} is designed as alternative for quicker plotting the ecdf for larger vectors. If \code{breaks} are provided as argument, a histogram with that number of breaks will be calculated and the ecdf will use those frequencies instead of respecting every single point. \cr Note that a plot will rarely need more than ~1'000 points on x to have a sufficient resolution on usual terms. \code{\link{PlotFdist}} will also use this number of breaks by default.
%%  ~~ If necessary, more details than the description above ~~
}
\value{no value returned, use \code{\link{plot.ecdf}} if any results are required.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{plot.ecdf}}, \code{\link{PlotFdist}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
PlotECDF(d.pizza$temperature)

# make large vector
x <- rnorm(n=1e7)

# plot only 1000 points instead of 1e7
PlotECDF(x, breaks=1000)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot}
