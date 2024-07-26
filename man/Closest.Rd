\name{Closest}
\alias{Closest}

\title{Find the Closest Value }

\description{Find the closest value(s) of a number in a vector x. Multiple values will be reported, if the differences are the same or if there are duplicates of the same value.}
\usage{
Closest(x, a, which = FALSE, na.rm = FALSE)
}
\arguments{
  \item{x}{the vector to be searched in
}
  \item{a}{the reference value
}
\item{which}{a logical value defining if the index position or the value should be returned. By default will the value be returned.}
  \item{na.rm}{a logical value indicating whether \code{NA} values should be stripped before the computation proceeds.
}
}
\value{the value or index in x which is closest to a }
\author{Andri Signorell <andri@signorell.net> }
\seealso{ \code{\link{which}}  }
\examples{

# basic
set.seed(8)
x <- runif(10) * 10
Closest(x, 3.1)
sort(x)

y <- sample(10, size=10, replace=TRUE)
# multiple observations of the same closest value 
Closest(y, a=6)
# get the relevant positions
Closest(y, a=6, which=TRUE)

# two different values having the same distance
Closest(c(2, 3, 4, 5), a=3.5)

# vectorize "a"
Closest(c(2, 3, 4, 5), a=c(3.1, 3.9))

# vectorize "which"
Closest(c(2, 3, 4, 5), a=3.1, which=c(FALSE, TRUE))

# vectorize both
Closest(c(2, 3, 4, 5), a=c(3.1, 3.9), which=c(FALSE, TRUE))

}
\keyword{ logic }
