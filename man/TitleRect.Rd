\name{TitleRect}
\alias{TitleRect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Boxed Annotation
%%  ~~function to do ... ~~
}
\description{The function can be used to add a title to a plot surrounded by a rectangular box. This is useful for plotting several plots in narrow distances.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
TitleRect(label, bg = "grey", border = 1, col = "black", xjust = 0.5,
          line = 2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{label}{the main title
}
  \item{bg}{the background color of the box.
}
  \item{border}{the border color of the box
}
  \item{col}{the font color of the title
}
  \item{xjust}{the x-justification of the text. This can be \code{c(0, 0.5, 1)} for left, middle- and right alignement.
}
\item{line}{on which MARgin line, starting at 0 counting outwards}
  \item{\dots}{the dots are passed to the \code{\link{text}} function, which can be used to change font and similar arguments.
}
}
\value{nothing is returned
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{title}}
}
\examples{
plot(pressure)
TitleRect("pressure")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }% __ONLY ONE__ keyword per line
