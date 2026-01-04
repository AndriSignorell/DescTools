\name{FmCI}
\alias{FmCI}
\title{
Format Confidence Intervals
}
\description{
Format confidence intervals using a flexible template.
}
\usage{
FmCI(x, template = NULL, ...)
}
\arguments{
  \item{x}{the numerical values, given in the order in which they are 
  used in the template.
}
  \item{template}{character string as template for the desired format. 
  \%s are the placeholders for the numerical values. 
  Default is \code{"\%s [\%s, \%s]"} for <est> [<lci>, <uci>].
}
  \item{\dots}{the dots are passed on to the \code{\link{Fm}()} function.
}
}
\value{a formatted string
}
\author{Andri Signorell <andri@signorell.net>
}
\seealso{
\code{\link{Fm}}
}
\examples{
x <- c(est=2.1, lci=1.5, uci=3.8)

# default template
FmCI(x)
# user defined template (note the double percent code)
FmCI(x, template="\%s (95\%\%-CI \%s-\%s)", digits=1)

# in higher datastructures
mci <- tapply(warpbreaks$breaks, warpbreaks$wool, MeanCI)
sapply(mci, FmCI, digits=1)
}
