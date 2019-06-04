\name{Stamp}
\alias{Stamp}
\title{Date/Time/Directory Stamp the Current Plot}
\description{
Stamp the current plot in the extreme lower right
corner. A free text or expression can be defined as
text to the stamp.
}
\usage{
Stamp(txt = NULL, las = par("las"), cex = 0.6)
}
\arguments{
  \item{txt}{an optional single text string. If it is not given, the function will look for a defined option named \code{stamp}. If not found the current date will be taken as text. If the stamp option is defined as expression the function will evaluate it. This can be used to define dynamic texts.}
  \item{las}{numeric in {1,3}, defining direction of the text. 1 means horizontal, 3 vertical. Default is taken from \code{par("las")}.}
  \item{cex}{numeric \bold{c}haracter \bold{ex}pansion factor; multiplied by \code{par("cex")} yields the final character size. Defaults to 0.6.}
}
\details{
The text can be freely defined as option. If user and date should be included by default, the following option using an expression will help:
\preformatted{DescToolsOptions(stamp=expression(gettextf('\%s/\%s',
          Sys.getenv('USERNAME'), Format(Today(), fmt='yyyy-mm-dd') )))}

  For \R results may not be satisfactory if \code{par(mfrow=)} is in effect.
}
\seealso{\code{\link{text}}}

\author{Frank E Harrell Jr <f.harrell@vanderbilt.edu>\cr
with some amendments by Andri Signorell <andri@signorell.net>}
\examples{
plot(1:20)
Stamp()
}
\keyword{aplot}
