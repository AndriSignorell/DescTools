\name{BreslowDayTest}
\alias{BreslowDayTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Breslow-Day Test for Homogeneity of the Odds Ratios
%%  ~~function to do ... ~~
}
\description{ Calculates the Breslow-Day test of homogeneity for a
  \eqn{2 \times 2 \times k}{2 x 2 x k} table, in order to investigate if
  all \eqn{k} strata have the same OR.
If OR is not given, the Mantel-Haenszel estimate is used.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BreslowDayTest(x, OR = NA, correct = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a \eqn{2 \times 2 \times k}{2 x 2 x k} table.
%%     ~~Describe \code{x} here~~
}
  \item{OR}{the odds ratio to be tested against. If left undefined (default) the Mantel-Haenszel estimate will be used.
%%     ~~Describe \code{OR} here~~
}
  \item{correct}{If TRUE, the Breslow-Day test with Tarone's adjustment is computed, which subtracts an adjustment factor to make the resulting statistic asymptotically chi-square.
%%     ~~Describe \code{printORi.s} here~~
}
}

\details{
For the Breslow-Day test to be valid, the sample size should be relatively large in each stratum, and at least 80\% of the expected cell counts should be greater than 5. Note that this is a stricter sample size requirement than the requirement for the Cochran-Mantel-Haenszel test for  tables, in that each stratum sample size (not just the overall sample size) must be relatively large. Even when the Breslow-Day test is valid, it might not be very powerful against certain alternatives, as discussed in Breslow and Day (1980).

Alternatively, it might be better to cast the entire inference problem
into the setting of a logistic regression model. Here, the underlying
question of the Breslow-Day test can be answered by investigating whether an
interaction term with the strata variable is necessary (e.g. using a
likelihood ratio test using the \code{anova} function).
}

\references{
Breslow, N. E., N. E. Day (1980) The Analysis of Case-Control Studies \emph{Statistical Methods in Cancer Research: Vol. 1}. Lyon, France, IARC Scientific Publications.

Tarone, R.E. (1985) On heterogeneity tests based on efficient scores, \emph{Biometrika}, 72, pp. 91-95.

Jones, M. P., O'Gorman, T. W., Lemka, J. H., and Woolson, R. F. (1989) A Monte Carlo Investigation of Homogeneity Tests of the Odds Ratio Under Various Sample Size Configurations \emph{Biometrics}, 45, 171-181
\cr

Breslow, N. E. (1996) Statistics in Epidemiology: The Case-Control Study \emph{Journal of the American Statistical Association}, 91, 14-26.

%% ~put references to the literature/web site here ~
}
\author{Michael Hoehle <hoehle@math.su.se>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{WoolfTest}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
migraine <- xtabs(freq ~ .,
            cbind(expand.grid(treatment=c("active", "placebo"),
                              response =c("better", "same"),
                              gender   =c("female", "male")),
                  freq=c(16, 5, 11, 20, 12, 7, 16, 19))
            )

# get rid of gender
tab <- xtabs(Freq ~ treatment + response, migraine)
Desc(tab)

# only the women
female <- migraine[,, 1]
Desc(female)

# .. and the men
male <- migraine[,, 2]
Desc(male)

BreslowDayTest(migraine)
BreslowDayTest(migraine, correct = TRUE)


salary <- array(
      c(38, 12, 102, 141, 12, 9, 136, 383),
      dim=c(2, 2, 2),
      dimnames=list(exposure=c("exposed", "not"),
                    disease =c("case", "control"),
                    salary  =c("<1000", ">=1000"))
                    )

# common odds ratio = 4.028269
BreslowDayTest(salary, OR = 4.02)
}
\keyword{htest}
