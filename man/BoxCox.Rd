\name{BoxCox}
\alias{BoxCox}
\alias{BoxCoxInv}
\title{Box Cox Transformation}
\usage{BoxCox(x, lambda)
BoxCoxInv(x, lambda)
}
\arguments{
\item{x}{a numeric vector }
\item{lambda}{transformation parameter}
}
\description{\code{BoxCox()} returns a transformation of the input variable using a Box-Cox transformation.\cr
\code{BoxCoxInv()} reverses the transformation.
}
\details{The Box-Cox transformation is given by

% \deqn{\theta(h) =
%   \left\{\begin{array}{ll}
%      u_\beta \left(\mu + \frac{\nu(h)}{\Gamma(1 - \xi)}
%   \right),&\xi \neq 0\\
%      \exp\left(\frac{\nu(h)}{\sigma}\right),&\xi = 0
%   \end{array}\right.
% }{u_beta (mu + nu(h) / Gamma(1 - \xi)), if \xi < 1,
% exp(nu(h)/sigma), otherwise}

\deqn{f_\lambda(x) =
  \left\{\begin{array}{ll}
     \frac{x^\lambda - 1}{\lambda} &\textup{for }\lambda \neq 0\\
     log(x) &\textup{for }\lambda = 0
  \end{array}\right.
}{f(x;\lambda)=(x^\lambda - 1)/\lambda, if \lambda not 0,
log(x), otherwise}


}
\value{a numeric vector of the same length as x.
}
\references{Box, G. E. P. and Cox, D. R. (1964) An analysis of transformations. \emph{JRSS B} \bold{26} 211--246.
}
\seealso{Use \code{\link{BoxCoxLambda}} or \code{\link[MASS]{boxcox}} in \code{library(MASS)} to find optimal lambda values. }
\note{These two functions are borrowed from \code{library(forecast)}.}
\author{Rob J Hyndman <rob.hyndman@monash.edu>}

\examples{
# example by Greg Snow
x <- rlnorm(500, 3, 2)

par(mfrow=c(2,2))
qqnorm(x, main="Lognormal")
qqnorm(BoxCox(x, 1/2), main="BoxCox(lambda=0.5)")
qqnorm(BoxCox(x, 0), main="BoxCox(lambda=0)")

PlotFdist(BoxCox(x, 0))

bx <- BoxCox(x, lambda = BoxCoxLambda(x) )
}
\keyword{univar}


% Better representation like (https://en.wikibooks.org/wiki/LaTeX/Mathematics)
% \[ f(n) =
%   \begin{cases}
%     n/2       & \quad \text{if } n \text{ is even}\\
%     -(n+1)/2  & \quad \text{if } n \text{ is odd}\\
%   \end{cases}
% \]

% \deqn{f_\lambda(x) =\frac{x^\lambda - 1}{\lambda}}

