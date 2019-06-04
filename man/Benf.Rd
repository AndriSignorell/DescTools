\name{Benford}
\alias{Benford}
\alias{dBenf}
\alias{pBenf}
\alias{qBenf}
\alias{rBenf}
\title{ Benford's Distribution }
\description{
  Density, distribution function,
  quantile function,
  and random generation
  for Benford's distribution.

}
\usage{
dBenf(x, ndigits = 1, log = FALSE)
pBenf(q, ndigits = 1, log.p = FALSE)
qBenf(p, ndigits = 1)
rBenf(n, ndigits = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, q}{
   Vector of quantiles.
   See \code{ndigits}.


  }
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. A single positive integer.
  Else if \code{length(n) > 1} then the length is
          taken to be the number required.


  }
  \item{ndigits}{
  Number of leading digits, either 1 or 2.
  If 1 then the support of the distribution is \{1,\ldots,9\}, else
  \{10,\ldots,99\}.


  }
  \item{log, log.p}{
  Logical.
  If \code{log.p = TRUE} then all probabilities \code{p} are
  given as \code{log(p)}.


  }

}
\details{

Benford's Law (aka \emph{the significant-digit law}) is the
empirical observation that in many naturally occuring tables of
numerical data, the leading significant (nonzero) digit
is not uniformly distributed in \eqn{\{1,2,\ldots,9\}}{1:9}.
Instead, the leading significant digit (\eqn{=D}, say)
obeys the law
  \deqn{P(D=d) = \log_{10} \left( 1 + \frac1d \right)}{%
        P(D=d) =  log10(1 + 1/d)}
for \eqn{d=1,\ldots,9}.
This means
the probability the first significant digit is 1 is
approximately \eqn{0.301}, etc.


Benford's Law was apparently first discovered in 1881
by astronomer/mathematician
S. Newcombe. It started by the observation
that the pages of a book of logarithms were dirtiest at the
beginning and progressively cleaner throughout.
In 1938, a General Electric physicist called F. Benford
rediscovered the law on this same observation. Over
several years he collected data from different sources
as different as atomic weights, baseball statistics,
numerical data from \emph{Reader's Digest},
and drainage areas of rivers.


Applications of Benford's Law has been as diverse as
to the area of
fraud detection in accounting  and the design computers.


}
\value{
  \code{dBenf} gives the density,
  \code{pBenf} gives the distribution function, and
  \code{qBenf} gives the quantile function, and
  \code{rBenf} generates random deviates.


}
\source{ These functions were previously published as \code{dbenf()} etc. in the  \pkg{VGAM} package and have been
integrated here without logical changes. 
}

\references{ 

Benford, F. (1938)
The Law of Anomalous Numbers.
\emph{Proceedings of the American Philosophical Society},
\bold{78}, 551--572.

Newcomb, S. (1881)
Note on the Frequency of Use of the Different Digits in Natural Numbers.
\emph{American Journal of Mathematics},
\bold{4}, 39--40.

}
\author{ T. W. Yee }
%\note{ 
%  Currently only the leading digit is handled.
%  The first two leading digits would be the next simple extension.
%
%}

%\seealso{
%    \code{\link{logff}}.
%}
\examples{
dBenf(x <- c(0:10, NA, NaN, -Inf, Inf))
pBenf(x)

\dontrun{
xx <- 1:9
barplot(dBenf(xx), col = "lightblue", las = 1, xlab = "Leading digit",
        ylab = "Probability", names.arg = as.character(xx),
        main = paste("Benford's distribution",  sep = ""))

hist(rBenf(n = 1000), border = "blue", prob = TRUE,
     main = "1000 random variates from Benford's distribution",
     xlab = "Leading digit", sub="Red is the true probability",
     breaks = 0:9 + 0.5, ylim = c(0, 0.35), xlim = c(0, 10.0))
lines(xx, dBenf(xx), col = "red", type = "h")
points(xx, dBenf(xx), col = "red")
}
}
\keyword{distribution}
