\name{Atkinson}
\alias{Atkinson}
\title{Atkinson Index - A Measure of Inequality. }
\description{
The Atkinson index is an inequality measure and is useful in determining which end of the distribution contributed most to the observed inequality. }
\usage{
Atkinson(x, n = rep(1, length(x)), parameter = 0.5, na.rm = FALSE)
}

\arguments{
 \item{x}{a vector containing at least non-negative elements.}
 \item{n}{a vector of frequencies, must be same length as x.}
\item{parameter}{parameter of the inequality measure (if set to \code{NULL}
  the default parameter of the respective measure is used).}
 \item{na.rm}{logical. Should missing values be removed? Defaults to FALSE.
%%     ~~Describe \code{na.rm} here~~
}

  }
\value{
  the value of the Akinson Index.
}
\note{
This function was previously published as \code{ineq()} in the  \pkg{ineq} package and has been
integrated here without logical changes, but with some extensions for \code{NA}-handling and the use of weights.
}

\references{
Cowell, F. A. (2000) Measurement of Inequality in Atkinson, A. B. / Bourguignon, F. (Eds): \emph{Handbook of Income Distribution}. Amsterdam.

Cowell, F. A. (1995) \emph{Measuring Inequality} Harvester Wheatshef: Prentice Hall.

Marshall, Olkin (1979) \emph{Inequalities: Theory of Majorization and Its
Applications}. New York: Academic Press.

Atkinson, A. B. (1970): On the Measurment of Inequality, \emph{Journal of Economic Theory}, Vol. 2(3), pp. 244-263.
}

\author{Achim Zeileis <Achim.Zeileis@R-project.org>}

\seealso{See \code{\link{Herfindahl}}, \code{\link{Rosenbluth}} for concentration measures and
\code{\link[ineq]{ineq}()} in the package \pkg{ineq} for additional inequality measures}

\examples{
# generate vector (of incomes)
x <- c(541, 1463, 2445, 3438, 4437, 5401, 6392, 8304, 11904, 22261)

# compute Atkinson coefficient with parameter=1
Atkinson(x, parameter=1)
}
\keyword{univar}

