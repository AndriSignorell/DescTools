\name{InDots}
\alias{InDots}

\title{Is a Specific Argument in the Dots-Arguments?
}
\description{Returns the value of a specific named argument if it was comprised in the dots or a default value, if it wasn't.
}
\usage{
InDots(..., arg, default)
}

\arguments{
  \item{\dots}{the dots arguments to be checked.
}
  \item{arg}{the name of argument to test for.
}
  \item{default}{the default value to return, if the argument \code{arg} does not exist in the dots.
}
}
\value{the value of the argument, if it exists else the specified default value.
}
\author{Andri Signorell <andri@signorell.net>}

\keyword{ manip }
