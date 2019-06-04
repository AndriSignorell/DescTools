\name{InDots}
\alias{InDots}

\title{Is a Specific Argument in the Dots-Arguments?
}
\description{Returns \code{TRUE} if a specific named argument was given in the dots.
}
\usage{
InDots(..., arg, default)
}

\arguments{
  \item{\dots}{the dots arguments of the function whose arguments are to be checked.
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
