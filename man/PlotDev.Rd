\name{PlotDev}
\alias{PlotDev}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot to Device
%%  ~~function to do ... ~~
}
\description{Plot to a graphics device.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotDev(fn, type = c("tif", "pdf", "eps", "bmp", "png", "jpg"),
        width = NULL, height = NULL, units="cm", res = 300, open = TRUE,
        compression = "lzw", expr, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fn}{Filename
%%     ~~Describe \code{fn} here~~
}
  \item{type}{one out of "tif", "pdf", "eps", "bmp", "png", "jpg"
%%     ~~Describe \code{type} here~~
}
  \item{width}{the width of the graphic in cm.
%%     ~~Describe \code{w} here~~
}
  \item{height}{the height of the graphic in cm.
%%     ~~Describe \code{h} here~~
}
\item{units}{The units in which \code{height} and \code{width} are given. Can be \code{px} (pixels), \code{in} (inches), \code{cm} (the default) or \code{mm}.

}
  \item{res}{the resolution
%%     ~~Describe \code{res} here~~
}
  \item{open}{logical. Should the graphic be opened after having being created?
%%     ~~Describe \code{open} here~~
}
\item{compression}{the type of compression to be used (tif only).}

  \item{expr}{the plot expression
%%     ~~Describe \code{expr} here~~
}
\item{\dots}{the dots are sent to the specified device function.}
}
\details{A simple convenience wrapper for producing TIF-Files for publication.
%%  ~~ If necessary, more details than the description above ~~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{tiff}}, \code{\link{pdf}}, \code{\link{postscript}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
PlotDev(fn="bar", type="tiff", expr=
  barplot(1:5, col=Pal("Helsana"))
)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{"device"}% use one of  RShowDoc("KEYWORDS")
