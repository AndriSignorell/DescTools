\name{WoolfTest}
\alias{WoolfTest}
\title{Woolf Test For Homogeneity in 2x2xk Tables}
\description{
  Test for homogeneity on \eqn{2 \times 2 \times k}{2 x 2 x k} tables
  over strata (i.e., whether the log odds ratios are the same in all
  strata).
}
\usage{
WoolfTest(x)
}
\arguments{
  \item{x}{a \eqn{2 \times 2 \times k}{2 x 2 x k} table, where the last dimension refers to the strata. }
}
\value{
  A list of class \code{"htest"} containing the following
  components:
  \item{statistic}{the chi-squared test statistic.}
  \item{parameter}{degrees of freedom of the approximate chi-squared
    distribution of the test statistic.}
  \item{p.value}{\eqn{p}-value for the test.}
  \item{method}{a character string indicating the type of test
    performed.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{observed}{the observed counts.}
  \item{expected}{the expected counts under the null hypothesis.}
}
\seealso{
  \code{\link{mantelhaen.test}}, \code{\link{BreslowDayTest}}
}
\note{ This function was previously published as \code{woolf_test()} in the  \pkg{vcd} package and has been integrated here without logical changes.
}

\author{David Meyer, Achim Zeileis, Kurt Hornik, Michael Friendly}
\references{
Woolf, B. 1955: On estimating the relation between blood group and
disease. \emph{Ann. Human Genet.} (London) \bold{19}, 251-253.
}
\examples{
migraine <- xtabs(freq ~ .,
            cbind(expand.grid(treatment=c("active","placebo"),
                               response=c("better","same"),
                               gender=c("female","male")),
                  freq=c(16,5,11,20,12,7,16,19))
            )

WoolfTest(migraine)
}
\keyword{htest}
