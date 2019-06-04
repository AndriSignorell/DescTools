\name{d.diamonds}
\alias{d.diamonds}
\docType{data}
\title{ Data diamonds
%%   ~~ data name/kind ... ~~
}
\description{As I suppose, an artificial dataset 
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(d.diamonds)}
\format{
  A data frame with 440 observations on the following 10 variables.
  \describe{
    \item{\code{index}}{a numeric vector}
    \item{\code{carat}}{a numeric vector}
    \item{\code{colour}}{a factor with levels \code{D} \code{E} \code{F} \code{G} \code{H} \code{I} \code{J} \code{K} \code{L}}
    \item{\code{clarity}}{an ordered factor with levels \code{I2} < \code{I1} < \code{SI3} < \code{SI2} < \code{SI1} < \code{VS2} < \code{VS1} < \code{VVS2} < \code{VVS1}}
    \item{\code{cut}}{an ordered factor with levels \code{F} < \code{G} < \code{V} < \code{X} < \code{I}}
    \item{\code{certification}}{a factor with levels \code{AGS} \code{DOW} \code{EGL} \code{GIA} \code{IGI}}
    \item{\code{polish}}{an ordered factor with levels \code{F} < \code{G} < \code{V} < \code{X} < \code{I}}
    \item{\code{symmetry}}{an ordered factor with levels \code{F} < \code{G} < \code{V} < \code{X} < \code{I}}
    \item{\code{price}}{a numeric vector}
    \item{\code{wholesaler}}{a factor with levels \code{A} \code{B} \code{C}}
  }
}
\details{
P Poor
F Fair
G Good
V Very good
X Excellent
I Ideal


}
\source{somewhere from the net... }

\examples{
data(d.diamonds)
str(d.diamonds)
}
\keyword{datasets}
