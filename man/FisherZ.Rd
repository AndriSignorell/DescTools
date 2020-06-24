\name{FisherZ}
\alias{FisherZ}
\alias{FisherZInv}
\alias{CorCI}


\title{Fisher-Transformation for Correlation to z-Score}

\description{Convert a correlation to a z score or z to r using the Fisher transformation or find the confidence intervals for a specified correlation.}
\usage{
FisherZ(rho)
FisherZInv(z)
CorCI(rho, n, conf.level = 0.95, alternative = c("two.sided", "less", "greater"))

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rho}{the Pearson's correlation coefficient}
  \item{z}{a Fisher z transformed value}
  \item{n}{sample size used for calculating the confidence intervals}

  \item{alternative}{is a character string, one of \code{"greater"},
  \code{"less"}, or \code{"two.sided"}, or the initial letter of each,
  indicating the specification of the alternative hypothesis.
  \code{"greater"} corresponds to positive association, \code{"less"} to negative association.}

  \item{conf.level}{confidence level for the returned confidence
  interval, restricted to lie between zero and one.}

}

\details{The  sampling distribution of  Pearson's r is not normally distributed. Fisher developed a transformation now called "Fisher's z-transformation" that converts Pearson's r to the normally distributed variable z. The formula for the transformation is:

\deqn{z_r = tanh^{-1}(r) = \frac{1}{2}log\left ( \frac{1+r}{1-r}\right )}
}

\value{z value corresponding to r (in FisherZ) \cr
r corresponding to z (in FisherZInv) \cr
rho, lower and upper confidence intervals (CorCI) \cr
}

\seealso{\code{\link{cor.test}}}

\author{William Revelle <revelle@northwestern.edu>, \cr
slight modifications Andri Signorell <andri@signorell.net> based on R-Core code
}

\examples{
cors <- seq(-.9, .9, .1)

zs <- FisherZ(cors)
rs <- FisherZInv(zs)
round(zs, 2)
n <- 30
r <- seq(0, .9, .1)
rc <- t(sapply(r, CorCI, n=n))
t <- r * sqrt(n-2) / sqrt(1-r^2)
p <- (1 - pt(t, n-2)) / 2

r.rc <- data.frame(r=r, z=FisherZ(r), lower=rc[,2], upper=rc[,3], t=t, p=p)

round(r.rc,2)
}

\keyword{ multivariate }% at least one, from doc/KEYWORDS
\keyword{ models }% __ONLY ONE__ keyword per line
