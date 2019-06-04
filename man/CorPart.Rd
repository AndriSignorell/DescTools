\name{CorPart}
\alias{CorPart}

\title{Find the Correlations for a Set x of Variables With Set y Removed}
\description{A straightforward application of matrix algebra to remove the effect of the variables in the y set from the x set.
Input may be either a data matrix or a correlation matrix.  Variables in x and y are specified by location.
}
\usage{
CorPart(m, x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{a data or correlation matrix.}
  \item{x}{the variable numbers associated with the X set. }
  \item{y}{the variable numbers associated with the Y set.}

}
\details{
It is sometimes convenient to partial the effect of a number of variables (e.g., sex, age, education)
out of the correlations of another set of variables.  This could be done laboriously by finding the residuals of
various multiple correlations, and then correlating these residuals.  The matrix algebra alternative is to do it directly.
}
\value{The matrix of partial correlations.
}
\references{ Revelle, W. \emph{An introduction to psychometric theory with applications in R} Springer. \cr (working draft available at  \url{http://personality-project.org/r/book/}
}
\author{ William Revelle }
\seealso{\code{\link{cor}}}
\examples{
# example from Bortz, J. (1993) Statistik fuer Sozialwissenschaftler, Springer, pp. 413

abstr <- c(9,11,13,13,14,9,10,11,10,8,13,7,9,13,14)
coord <- c(8,12,14,13,14,8,9,12,8,9,14,7,10,12,12)
age <- c(6,8,9,9,10,7,8,9,8,7,10,6,10,10,9)

# calculate the correlation of abstr and coord, after without the effect of the age
CorPart(cbind(abstr, coord, age), 1:2, 3)

# by correlation matrix m
m <- cor(cbind(abstr, coord, age))
CorPart(m, 1:2, 3)

# ... which would be the same as:
lm1 <- lm(abstr ~ age)
lm2 <- lm(coord ~ age)

cor(resid(lm1), resid(lm2))
}
\keyword{multivariate}

