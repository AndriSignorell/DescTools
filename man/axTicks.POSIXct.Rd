\name{axTicks.POSIXct}
\alias{axTicks.POSIXct}
\alias{axTicks.Date}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute Axis Tickmark Locations (For POSIXct Axis)
%%  ~~function to do ... ~~
}
\description{Compute pretty tickmark locations, the same way as R does internally. By default, gives the at values which axis.POSIXct(side, x) would use.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
axTicks.POSIXct(side, x, at, format, labels = TRUE, ...)

axTicks.Date(side = 1, x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{side}{See \link{axis}.
%%     ~~Describe \code{side} here~~
}
  \item{x, at}{A date-time or date object.
%%     ~~Describe \code{x} here~~
}
  \item{format}{See strptime.
%%     ~~Describe \code{format} here~~
}
  \item{labels}{Either a logical value specifying whether annotations are to be made at the tickmarks, or a vector of character strings to be placed at the tickpoints.
%%     ~~Describe \code{labels} here~~
}
  \item{\dots}{Further arguments to be passed from or to other methods.
%%     ~~Describe \code{\dots} here~~
}
}
\details{\link{axTicks} has no implementation for POSIXct axis. This function fills the gap.
%%  ~~ If necessary, more details than the description above ~~
}
\value{numeric vector of coordinate values at which axis tickmarks can be drawn.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net> simply copying R-Core code
%%  ~~who you are~~
}

\seealso{\code{\link{axTicks}}, \code{\link{axis.POSIXct}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
with(beaver1, {
  time <- strptime(paste(1990, day, time \%/\% 100, time \%\% 100),
                   "\%Y \%j \%H \%M")
  plot(time, temp, type = "l") # axis at 4-hour intervals.
  # now label every hour on the time axis
  plot(time, temp, type = "l", xaxt = "n")
  r <- as.POSIXct(round(range(time), "hours"))
  axis.POSIXct(1, at = seq(r[1], r[2], by = "hour"), format = "\%H")
  # place the grid
  abline(v=axTicks.POSIXct(1, at = seq(r[1], r[2], by = "hour"), format = "\%H"),
         col="grey", lty="dotted")
})

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }
\keyword{ chron }
