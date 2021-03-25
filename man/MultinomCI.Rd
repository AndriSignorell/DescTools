\name{MultinomCI}
\alias{MultinomCI}

\title{Confidence Intervals for Multinomial Proportions}

\description{
Confidence intervals for multinomial proportions are often approximated by single binomial confidence intervals, which might in practice often yield satisfying results, but is properly speaking not correct. This function calculates simultaneous confidence intervals for multinomial proportions either according to the methods of Sison and Glaz, Goodman, Wald, Wald with continuity correction or Wilson.
}
\author{Pablo J. Villacorta Iglesias <pjvi@decsai.ugr.es>\cr
Department of Computer Science and Artificial Intelligence, University of Granada (Spain)
(Sison-Glaz)

Andri Signorell <andri@signorell.net>
(Goodman, Wald, Wilson)
}
\usage{
MultinomCI(x, conf.level = 0.95, sides = c("two.sided", "left", "right"),
           method = c("sisonglaz", "cplus1", "goodman", "wald", "waldcc", "wilson"))
}
\arguments{
\item{x}{
A vector of positive integers representing the number of occurrences of each class. The total number of samples equals the sum of such elements.
}
  \item{conf.level}{ confidence level, defaults to 0.95. }

\item{sides}{a character string specifying the side of the confidence interval, must be one of \code{"two.sided"} (default),
\code{"left"} or \code{"right"}. You can specify just the initial letter. \code{"left"} would be analogue to a hypothesis of
\code{"greater"} in a \code{t.test}.}

  \item{method}{ character string specifing which method to use; can be one out of
  \code{"sisonglaz"}, \code{"cplus1"}, \code{"goodman"},  \code{"wald"},  \code{"waldcc"}, \code{"wilson"}.
  Method can be abbreviated. See details. Defaults to \code{"sisonglaz"}. }


}
\details{
Given a vector of observations with the number of samples falling in each class of a multinomial distribution,
builds the simultaneous confidence intervals for the multinomial probabilities according to the method proposed by the mentioned authors.
The R code for Sison and Glaz (1995) has been translated from thes SAS code written by May and Johnson (2000).\cr
Some approaches for the confidence intervals can potentially yield negative results or values beyond 1. These would be reset such as not to exceed the range of [0, 1].
}
\value{
A matrix with 3 columns:
  \item{est}{estimate}
  \item{lwr.ci}{lower bound of the confidence interval}
  \item{upr.ci}{upper bound of the confidence interval}

The number of rows correspond to the dimension of x.

}
\references{
Sison, C.P and Glaz, J. (1995) Simultaneous confidence intervals and sample size determination
for multinomial proportions. \emph{Journal of the American Statistical Association}, 90:366-369.

Glaz, J., Sison, C.P. (1999) Simultaneous confidence intervals for multinomial proportions.
\emph{Journal of Statistical Planning and Inference} 82:251-262.

May, W.L., Johnson, W.D.(2000) Constructing two-sided simultaneous confidence intervals for
multinomial proportions for small counts in a large number of cells. \emph{Journal of Statistical Software} 5(6) .
Paper and code available at \url{https://www.jstatsoft.org/v05/i06}.

Goodman, L. A. (1965) On Simultaneous Confidence Intervals for Multinomial
Proportions \emph{Technometrics}, 7, 247-254.

Wald, A. Tests of statistical hypotheses concerning several parameters when the number of observations is large, \emph{Trans. Am. Math. Soc.} 54 (1943) 426-482.

Wilson, E. B. Probable inference, the law of succession and statistical inference, \emph{J.Am. Stat. Assoc.} 22 (1927) 209-212.

}

\examples{
# Multinomial distribution with 3 classes, from which a sample of 79 elements
# were drawn: 23 of them belong to the first class, 12 to the
# second class and 44 to the third class. Punctual estimations
# of the probabilities from this sample would be 23/79, 12/79
# and 44/79 but we want to build 95% simultaneous confidence intervals
# for the true probabilities

MultinomCI(c(23, 12, 44), conf.level=0.95)


x <- c(35, 74, 22, 69)

MultinomCI(x, method="goodman")
MultinomCI(x, method="sisonglaz")
MultinomCI(x, method="cplus1")
MultinomCI(x, method="wald")
MultinomCI(x, method="waldcc")
MultinomCI(x, method="wilson")

# compare to
BinomCI(x, n=sum(x))
}

\keyword{univar}
