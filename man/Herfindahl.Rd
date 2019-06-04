\name{Herfindahl}
\alias{Herfindahl}
\alias{Rosenbluth}
\title{Concentration Measures}
\description{
Computes the concentration within a vector according to the
specified concentration measure. }
\usage{
Herfindahl(x, n = rep(1, length(x)), parameter = 1, na.rm = FALSE)
Rosenbluth(x, n = rep(1, length(x)), na.rm = FALSE)
}
\arguments{
 \item{x}{a vector containing non-negative elements}
\item{n}{a vector of frequencies (weights), must be same length as x.}
\item{parameter}{parameter of the concentration measure (if set to \code{NULL}
  the default parameter of the respective measure is used)}
\item{na.rm}{logical. Should missing values be removed? Defaults to FALSE.}
}

\note{
The same measure is usually known as the Simpson index in ecology, and as the Herfindahl index or the Herfindahl-Hirschman index (HHI) in economics.
}
\value{
  the value of the concentration measure
}
\note{ These functions were previously published as \code{conc()} in the  \pkg{ineq} package and have been
integrated here without logical changes. \code{NA} and weights support were added.
}

\references{Cowell, F. A. (2000) Measurement of Inequality, in Atkinson, A. B., Bourguignon, F.  \emph{Handbook of Income Distribution}. (Eds) Amsterdam

Cowell, F. A. (1995) \emph{Measuring Inequality}. Prentice Hall/Harvester Wheatshef

Hall, M., Tidemann, N. (1967) \emph{Measures of Concentration}, JASA 62, 162-168.
}

\author{Achim Zeileis <achim.zeileis@r-project.org>}

\seealso{See \code{\link{Gini}}, \code{\link{Atkinson}} and \code{\link[ineq]{ineq}()} for additional inequality measures}

\examples{
# generate vector (of sales)
x <- c(541, 1463, 2445, 3438, 4437, 5401, 6392, 8304, 11904, 22261)

# compute Herfindahl coefficient with parameter 1
Herfindahl(x)

# compute coefficient of Hall/Tiedemann/Rosenbluth
Rosenbluth(x)

# Some more examples
Herfindahl(c(261,29,33,15,39,28,95,5,6,28,69,8,105,38,15))
Herfindahl(c(783,121,112,70,201,153,425,19,37,126,325,51,442,193,41))
}
\keyword{univar}

