\name{VonNeumannTest}
\alias{VonNeumannTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Von Neumann's Successive Difference Test
%%  ~~function to do ... ~~
}
\description{A popular statistic to test for independence is the von Neumann ratio.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
VonNeumannTest(x, alternative = c("two.sided", "less", "greater"), unbiased = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector containing the observations}
  \item{alternative}{a character string specifying the alternative hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}. You can specify just the initial letter.
}
\item{unbiased}{logical. In order for VN to be an unbiased estimate of the true population value, the calculated value is multiplied by \eqn{n/(n-1)}{n/(n-1)}. Default is TRUE.
%%     ~~Describe \code{unbiased} here~~
}
}
\details{
The VN test statistic is in the unbiased case
\deqn{VN=\frac{\sum_{i=1}^{n-1}(x_i-x_{i+1})^2 \cdot n}{\sum_{i=1}^{n}\left(x_i-\bar{x}\right)^2 \cdot (n-1)} }{VN=\sum(x_i-x_{i+1})^2 / \sum(x_i-mean(x)^2 * n/n-1}
It is known that \eqn{(VN-\mu)/\sigma} is asymptotically standard normal, where \eqn{\mu=\frac{2n}{n-1}}{\mu=2n/(n-1)} and \eqn{\sigma^2=4\cdot n^2 \frac{(n-2)}{(n+1)(n-1)^3}}{\sigma^2=[4*n^2 * (n-2)]/[(n+1)(n-1)^3]}.

The VN test statistic is in the original (biased) case
\deqn{VN=\frac{\sum_{i=1}^{n-1}(x_i-x_{i+1})^2}{\sum_{i=1}^{n}\left(x_i-\bar{x}\right)^2}}{VN=\sum(x_i-x_{i+1})^2 / \sum(x_i-mean(x)^2}
The test statistic \eqn{(VN-2)/\sigma} is asymptotically standard normal, where \eqn{\sigma^2=\frac{4\cdot(n-2)}{(n+1)(n-1)}}{\sigma^2=[4*(n-2)]/[(n+1)(n-1)]}.


Missing values are silently removed.
}
\value{
A list with class "htest" containing the components:
\item{statistic}{the value of the VN statistic and the normalized statistic test.}
\item{parameter, n}{the size of the data, after the remotion of consecutive duplicate values.}
\item{p.value}{the p-value of the test.}
\item{alternative}{a character string describing the alternative hypothesis.}
\item{method}{a character string indicating the test performed.}
\item{data.name}{a character string giving the name of the data.}
}
\references{
von Neumann, J. (1941) Distribution of the ratio of the mean square successive difference to the variance.
\emph{Annals of Mathematical Statistics} \bold{12}, 367-395.
}
\author{Andri Signorell <andri@signorell.net>}

\seealso{\code{\link{BartelsRankTest}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
VonNeumannTest(d.pizza$temperature)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }% __ONLY ONE__ keyword per line
