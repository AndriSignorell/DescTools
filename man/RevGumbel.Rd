
\name{pRevGumbel}
\alias{dRevGumbel}
\alias{pRevGumbel}
\alias{qRevGumbel}
\alias{qRevGumbelExp}
\alias{rRevGumbel}
\title{"Reverse" Gumbel Distribution Functions}
\description{
  Density, distribution function, quantile function and random
  generation for the \dQuote{Reverse} Gumbel distribution with
  parameters \code{location} and \code{scale}.
}
\usage{
dRevGumbel (x, location = 0, scale = 1)
pRevGumbel (q, location = 0, scale = 1)
qRevGumbel (p, location = 0, scale = 1)
rRevGumbel (n, location = 0, scale = 1)

qRevGumbelExp(p)

}
\arguments{
  \item{x, q}{numeric vector of abscissa (or quantile) values at which
    to evaluate the density or distribution function.}
  \item{p}{numeric vector of probabilities at which to evaluate the
    quantile function.}
  \item{location}{location of the distribution}
  \item{scale}{scale (\eqn{> 0}) of the distribution.}
  \item{n}{number of random variates, i.e., \code{\link{length}} of
    resulting vector of \code{rRevGumbel(..)}.}
}
%% \details{
%% }
\value{
  a numeric vector, of the same length as \code{x}, \code{q}, or
  \code{p} for the first three functions, and of length \code{n} for
  \code{rRevGumbel()}.
}
%% \references{
%% }
\author{Werner Stahel; partly inspired by package \pkg{VGAM}.
  Martin Maechler for numeric cosmetic.
}
\seealso{
  the \code{\link{Weibull}} distribution functions in \R's \pkg{stats} package.
}
\examples{
curve(pRevGumbel(x, scale= 1/2), -3,2, n=1001, col=1, lwd=2,
      main = "RevGumbel(x, scale = 1/2)")
abline(h=0:1, v = 0, lty=3, col = "gray30")
curve(dRevGumbel(x, scale= 1/2),       n=1001, add=TRUE,
      col = (col.d <- adjustcolor(2, 0.5)), lwd=3)
legend("left", c("cdf","pdf"), col=c("black", col.d), lwd=2:3, bty="n")

med <- qRevGumbel(0.5, scale=1/2)
cat("The median is:",  format(med),"\n")
}
\keyword{distribution}
