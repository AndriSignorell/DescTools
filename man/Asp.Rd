\name{Asp}
\alias{Asp}
\docType{data}
\title{Get Aspect Ratio of the Current Plot
%%   ~~ data name/kind ... ~~
}
\description{Returns the aspect ratio of the current plot in user coordinates.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{Asp()}

\details{The aspect ratio of the plot is calculated as

\preformatted{
w   <- par("pin")[1] / diff(par("usr")[1:2])
h   <- par("pin")[2] / diff(par("usr")[3:4])
asp <- w/h
}
}
\author{Andri Signorell <andri@signorell.net>}

\examples{
Asp()
}
\keyword{aplot}
