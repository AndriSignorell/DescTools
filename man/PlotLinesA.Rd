\name{PlotLinesA}
\alias{PlotLinesA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Lines
%%  ~~function to do ... ~~
}
\description{Plot the columns of one matrix against the columns of another. Adds a legend on the right at the endpoints of lines.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotLinesA(x, y, col = 1:5, lty = 1, lwd = 1, lend = par("lend"),
           xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, xaxt = NULL, yaxt = NULL,
           cex = 1, args.legend = NULL, main = NULL, grid = TRUE, mar = NULL,
           pch = NA, pch.col = par("fg"), pch.bg = par("bg"), pch.cex = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{vectors or matrices of data for plotting. The number of rows should match. If one of them are missing, the other is taken as y and an x vector of \code{1:n} is used. Missing values (\code{NAs}) are allowed.
%%     ~~Describe \code{x} here~~
}
  \item{col}{vector of colors. Colors are used cyclically.
%%     ~~Describe \code{col} here~~
}
  \item{lty, lwd, lend}{vector of line types, widths, and end styles. The first element is for the first column, the second element for the second column, etc., even if lines are not plotted for all columns. Line types will be used cyclically until all plots are drawn.
%%     ~~Describe \code{lty} here~~
}
  \item{xlab, ylab}{titles for x and y axes, as in \link{plot}.
%%     ~~Describe \code{xlab} here~~
}
  \item{xlim, ylim}{ranges of x and y axes, as in \link{plot}.
%%     ~~Describe \code{xlim} here~~
}
\item{xaxt, yaxt}{a character which specifies the x axis type. Specifying "\code{n}" suppresses plotting of the axis. The standard value is "\code{s}", any value other than "\code{n}" implies plotting.}
  \item{cex}{character expansion factor relative to current \code{par("cex")}.
%%     ~~Describe \code{xlim} here~~
}
\item{args.legend}{list of additional arguments for the legend; names of the list are used as argument names. If set to NA, the legend will be suppressed. See details.
}

  \item{main}{an overall title for the plot.
%%     ~~Describe \code{main} here~~
}
  \item{grid}{logical adds an nx by ny rectangular grid to an existing plot.
%%     ~~Describe \code{grid} here~~
}
  \item{mar}{the margins of the plot.
%%     ~~Describe \code{mar} here~~
}
  \item{pch}{character string or vector of 1-characters or integers for plotting characters, see \code{\link{points}}. The first character is the plotting-character for the first plot, the second for the second, etc. The default is the digits (1 through 9, 0) then the lowercase and uppercase letters. If no points should be drawn set this argument to \code{NA} (this is the default).
}
  \item{pch.col}{vector of colors for the points. Colors are used cyclically. Ignored if \code{pch = NA}.
}
  \item{pch.bg}{vector of background (fill) colors for the open plot symbols given by \code{pch = 21:25} as in \code{\link{points}}. The default is set to \code{par("bg")}. Ignored if \code{pch = NA}.
}
  \item{pch.cex}{vector of character expansion sizes, used cyclically. This works as a multiple of \code{\link{par}("cex")}. Default is 1.0. Ignored if \code{pch = NA}.
}
\item{\dots}{the dots are sent to \code{\link{matplot}}}.

}
\details{This function is rather a template, than a function. It wraps \code{\link{matplot}} to generate a lines plot and adds a rather sophisticated legend on the right side, while calculating appropriate margins. A grid option is included (as \code{panel.first} does not work in matplot).

As in matplot, the first column of x is plotted against the first column of y, the second column of x against the second column of y, etc. If one matrix has fewer columns, plotting will cycle back through the columns again. (In particular, either x or y may be a vector, against which all columns of the other argument will be plotted.)

The legend can be controlled by following arguments:

\preformatted{list(line = c(1, 1), width = 1, y = SpreadOut(unlist(last), 
     mindist = 1.2 * strheight("M") * par("cex")), 
     labels = names(last), cex = par("cex"), col = col[ord], 
     lwd = lwd[ord], lty = lty[ord])}
     
All arguments are recycled.
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{matplot}}, \code{\link{par}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
m <- matrix(c(3,4,5,1,5,4,2,6,2), nrow = 3,
            dimnames = list(dose = c("A","B","C"),
                            age  = c("2010","2011","2012")))

PlotLinesA(m, col=c(Pal("Helsana")), main="Dose ~ age_grp", lwd=3, ylim=c(1, 10))


# example from MASS
shoes <- list(
  A = c(13.2, 8.2, 10.9, 14.3, 10.7, 6.6, 9.5, 10.8, 8.8, 13.3),
  B = c(14, 8.8, 11.2, 14.2, 11.8, 6.4, 9.8, 11.3, 9.3, 13.6))

PlotLinesA(do.call(rbind, shoes), xlim=c(0.75,2.25), col=1, main="shoes",
           pch=21, pch.bg="white", pch.col=1, pch.cex=1.5)

# let's define some arbitrary x-coordinates
PlotLinesA(x=c(1,2,6,8,15), y=VADeaths)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
