\name{Triangular}
\alias{Triangular}
\alias{dTri}
\alias{pTri}
\alias{qTri}
\alias{rTri}
\title{
  The Triangular Distribution
}
\description{
  Density, distribution function, quantile function, and random generation 
  for the triangular distribution with parameters \code{min}, \code{max}, 
  and \code{mode}.
}
\usage{
  dTri(x, min = 0, max = 1, mode = 1/2)
  pTri(q, min = 0, max = 1, mode = 1/2)
  qTri(p, min = 0, max = 1, mode = 1/2)
  rTri(n, min = 0, max = 1, mode = 1/2)
}
\arguments{
  \item{x}{
  vector of quantiles.  Missing values (\code{NA}s) are allowed.
}
  \item{q}{
  vector of quantiles.  Missing values (\code{NA}s) are allowed.
}
  \item{p}{
  vector of probabilities between 0 and 1.  Missing values (\code{NA}s) are allowed.
}
  \item{n}{
  sample size.  If \code{length(n)} is larger than 1, then \code{length(n)} 
  random values are returned.
}
  \item{min}{
  vector of minimum values of the distribution of the random variable.  
  The default value is \code{min=0}.
}
  \item{max}{
  vector of maximum values of the random variable.  
  The default value is \code{max=1}.
}
  \item{mode}{
  vector of modes of the random variable.  
  The default value is \code{mode=1/2}.
}
}
\details{
  Let \eqn{X} be a triangular random variable with parameters \code{min=}\eqn{a}, 
  \code{max=}\eqn{b}, and \code{mode=}\eqn{c}.


  \emph{Probability Density and Cumulative Distribution Function} \cr
  The density function of \eqn{X} is given by:
  \tabular{lll}{
    \eqn{f(x; a, b, c) =}  \tab  \eqn{\frac{2(x-a)}{(b-a)(c-a)}}  \tab for \eqn{a \le x \le c} \cr
                           \tab  \eqn{\frac{2(b-x)}{(b-a)(b-c)}}  \tab for \eqn{c \le x \le b} \cr
  }
  where \eqn{a < c < b}.

  The cumulative distribution function of \eqn{X} is given by:
  \tabular{lll}{
    \eqn{F(x; a, b, c) =}  \tab  \eqn{\frac{(x-a)^2}{(b-a)(c-a)}}  \tab for \eqn{a \le x \le c} \cr
                           \tab  \eqn{1 - \frac{(b-x)^2}{(b-a)(b-c)}}  \tab for \eqn{c \le x \le b} \cr
  }
  where \eqn{a < c < b}.

  \emph{Quantiles} \cr
  The \eqn{p^th} quantile of \eqn{X} is given by:
  \tabular{lll}{
    \eqn{x_p =}  \tab  \eqn{a + \sqrt{(b-a)(c-a)p}}    \tab for \eqn{0 \le p \le F(c)} \cr
                 \tab  \eqn{b - \sqrt{(b-a)(b-c)(1-p}} \tab for \eqn{F(c) \le p \le 1} \cr
  }
  where \eqn{0 \le p \le 1}.

  \emph{Random Numbers} \cr
  Random numbers are generated using the inverse transformation method:
  \deqn{x = F^{-1}(u)}
  where \eqn{u} is a random deviate from a uniform \eqn{[0, 1]} distribution. 

  \emph{Mean and Variance} \cr
  The mean and variance of \eqn{X} are given by:
  \deqn{E(X) = \frac{a + b + c}{3}}
  \deqn{Var(X) = \frac{a^2 + b^2 + c^2 - ab - ac - bc}{18}}
}
\value{
  \code{dTri} gives the density, \code{pTri} gives the distribution function, 
  \code{qTri} gives the quantile function, and \code{rTri} generates random 
  deviates. 
}
\references{
  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995). 
  \emph{Continuous Univariate Distributions, Volume 2}. 
  Second Edition. John Wiley and Sons, New York.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The triangular distribution is so named because of the shape of its probability 
  density function.  The average of two independent identically distributed 
  uniform random variables with parameters \code{min=}\eqn{\alpha} and 
  \code{max=}\eqn{\beta} has a triangular distribution with parameters 
  \code{min=}\eqn{\alpha}, \code{max=}\eqn{\beta}, and 
  \code{mode=}\eqn{(\beta-\alpha)/2}.

  The triangular distribution is sometimes used as an input distribution in 
  probability risk assessment.
}
\seealso{
  \link[stats:Uniform]{Uniform},   
  Probability Distributions and Random Numbers.
}
\examples{
  # Density of a triangular distribution with parameters 
  # min=10, max=15, and mode=12, evaluated at 12, 13 and 14: 

  dTri(12:14, 10, 15, 12) 
  #[1] 0.4000000 0.2666667 0.1333333

  #----------

  # The cdf of a triangular distribution with parameters 
  # min=2, max=7, and mode=5, evaluated at 3, 4, and 5: 

  pTri(3:5, 2, 7, 5) 
  #[1] 0.06666667 0.26666667 0.60000000

  #----------

  # The 25'th percentile of a triangular distribution with parameters 
  # min=1, max=4, and mode=3: 

  qTri(0.25, 1, 4, 3) 
  #[1] 2.224745

  #----------

  # A random sample of 4 numbers from a triangular distribution with 
  # parameters min=3 , max=20, and mode=12. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(10) 
  rTri(4, 3, 20, 12) 
  #[1] 11.811593  9.850955 11.081885 13.539496
}
\keyword{ distribution }
\keyword{ datagen }
