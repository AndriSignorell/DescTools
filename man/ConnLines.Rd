\name{ConnLines}
\alias{ConnLines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add Connection Lines to a Barplot
%%  ~~function to do ... ~~
}
\description{Add connection lines to a stacked barplot (beside = \code{TRUE} is not supported). The function expects exactly the same arguments, that were
used to create the barplot.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ConnLines(..., col = 1, lwd = 1, lty = "solid", xalign = c("mar", "mid"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{the arguments used to create the barplot. (The dots are sent directly to barplot).
%%     ~~Describe \code{\dots} here~~
}
  \item{col}{the line color of the connection lines. Defaults to black.
%%     ~~Describe \code{lcol} here~~
}
  \item{lwd}{the line width for the connection lines. Default is 1.
%%     ~~Describe \code{lwd} here~~
}
  \item{lty}{the line type for the connection lines. Line types can either be specified as an integer
  (0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash) or as one of the
  character strings \code{"blank"}, \code{"solid"}, \code{"dashed"}, \code{"dotted"}, \code{"dotdash"}, \code{"longdash"}, or \code{"twodash"}.
  Default is \code{"solid"}.
}

\item{xalign}{defines where the lines should be aligned to on the x-axis. Can be set
either to the margins of the bars ("mar" which is the default) or to "mid". The latter will lead the connecting lines to the middle of the bars.}
%%     ~~Describe \code{lty} here~~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{BarText}}, \code{\link{barplot}} 
}
\examples{
tab <- with(
  subset(d.pizza, driver \%in\% c("Carpenter", "Miller", "Farmer", "Butcher")),
  table(factor(driver), Weekday(date, "dd", stringsAsFactor=TRUE))
)
tab

barplot(tab, beside=FALSE, space=1.2)
ConnLines(tab, beside=FALSE, space=1.2, col="grey50", lwd=1, lty=2)

barplot(tab, beside=FALSE, space=1.2, horiz=TRUE)
ConnLines(tab, beside=FALSE, space=1.2, horiz=TRUE, col="grey50", lwd=1, lty=2)


cols <- Pal("Helsana")[1:4]
b <- barplot(tab, beside=FALSE, horiz=FALSE, col=cols)
ConnLines(tab, beside=FALSE, horiz=FALSE, col="grey50", lwd=1, lty=2)

# set some labels
BarText(tab, b, 
        labels=Format(tab, zero.form = "", digits=0), pos = "mid", 
        col=(matrix(rep(TextContrastColor(cols), each=ncol(tab)),
                    nrow=nrow(tab), byrow=FALSE )))

# align to the middle of the bars
barplot(tab, beside=FALSE, space=1.2)
ConnLines(tab, beside=FALSE, space=1.2, col="grey50", lwd=1, lty=2, method="mid")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }
