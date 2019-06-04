\name{Canvas}
\alias{Canvas}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Canvas for Geometric Plotting
%%  ~~function to do ... ~~
}
\description{This is just a wrapper for creating an empty plot with suitable defaults for plotting geometric shapes.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Canvas(xlim = NULL, ylim = xlim, main = NULL, xpd = par("xpd"),
       mar=c(5.1,5.1,5.1,5.1), asp = 1, bg = par("bg"), usrbg = "white", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xlim, ylim}{the xlims and ylims for the plot. Default is c(-1, 1).
%%     ~~Describe \code{xlim} here~~
}
  \item{xpd}{expand drawing area, defaults to \code{par("xpd")}.
%%     ~~Describe \code{xlim} here~~
}
  \item{main}{the main title on top of the plot.
%%     ~~Describe \code{xlim} here~~
}
  \item{mar}{set margins. Defaults to c(5.1,5.1,5.1,5.1).
%%     ~~Describe \code{xlim} here~~
}
  \item{asp}{numeric, giving the aspect ratio y/x. (See \code{\link{plot.window}} for details. Default is 1.
%%     ~~Describe \code{xlim} here~~
}
  \item{bg}{the background color of the plot, defaults to par("bg"), which usually will be "white".
%%     ~~Describe \code{xlim} here~~
}
  \item{usrbg}{the color of the user space of the plot, defaults to "white".
%%     ~~Describe \code{xlim} here~~
}
  \item{\dots}{additional arguments are passed to the \code{plot()} command.
%%     ~~Describe \code{xlim} here~~
}

}
\details{The plot is created with these settings:\cr
\code{asp = 1, xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a list of all the previous values of the parameters changed (returned invisibly)
}

\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\examples{
Canvas(7)
text(0, 0, "Hello world!", cex=5)
}
\keyword{ hplot }
