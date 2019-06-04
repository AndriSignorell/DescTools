\name{PostHocTest}
\alias{PostHocTest}
\alias{PostHocTest.aov}
\alias{PostHocTest.table}
\alias{PostHocTest.matrix}
\alias{print.PostHocTest}
\alias{plot.PostHocTest}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Post-Hoc Tests
%%  ~~function to do ... ~~
}
\description{A convenience wrapper for computing post-hoc test after having calculated an ANOVA.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PostHocTest(x, ...)

\method{PostHocTest}{aov}(x, which = NULL,
            method = c("hsd", "bonferroni", "lsd", "scheffe", "newmankeuls", "duncan"),
            conf.level = 0.95, ordered = FALSE, ...)

\method{PostHocTest}{table}(x, method = c("none", "fdr", "BH", "BY", "bonferroni",
                          "holm", "hochberg", "hommel"),
            conf.level = 0.95, ...)


\method{print}{PostHocTest}(x, digits = getOption("digits", 3), ...)
\method{plot}{PostHocTest}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an aov object.
%%     ~~Describe \code{x} here~~
}
  \item{method}{one of \code{"hsd"}, \code{"bonf"}, \code{"lsd"}, \code{"scheffe"}, \code{"newmankeuls"}, defining the method for the pairwise comparisons.\cr For the post hoc test of tables the methods of \code{\link{p.adjust}} can be supplied. See the detail there.
%%     ~~Describe \code{method} here~~
}
  \item{which}{a character vector listing terms in the fitted model for which the intervals should be calculated. Defaults to all the terms.
%%     ~~Describe \code{method} here~~
}
  \item{conf.level}{a numeric value between zero and one giving the family-wise confidence level to use.
  If this is set to NA, just a matrix with the p-values will be returned.
%%     ~~Describe \code{x} here~~
}
  \item{ordered}{a logical value indicating if the levels of the factor should be ordered according to increasing average in the sample before taking differences. If ordered is \code{TRUE} then the calculated differences in the means will all be positive. The significant differences will be those for which the lower end point is positive. \cr
  This argument will be ignored if method is not either \code{hsd} or \code{newmankeuls}.
}
  \item{digits}{controls the number of fixed digits to print.
}

  \item{\dots}{further arguments, not used so far.
%%     ~~Describe \code{\dots} here~~
}
}
\details{The function is designed to consolidate a couple of post-hoc tests with the same interface for
input and output.

Choosing Tests\cr
Different Post Hoc tests use different methods to control FW and PE. Some tests are very conservative. Conservative tests go to great lengths to prevent the user from committing a Type I error.  They use more stringent criterion for determining significance. Many of these tests become more and more stringent as the number of groups increases (directly limiting the FW and PE error rate). Although these tests buy you protection against Type I error, it comes at a cost. As the tests become more stringent, you loose Power (1-B).  More Liberal tests, buy you Power but the cost is an increased chance of Type I error.  There is no set rule for determining which test to use, but different researchers have offered some guidelines for choosing. Mostly it is an issue of pragmatics and whether the number of comparisons exceeds K-1.

Fisher's LSD\cr
The Fisher LSD (Least Significant Different) sets Alpha Level per comparison. Alpha = .05 for every comparison. df = df error (i.e. df within).
This test is the most liberal of all Post Hoc tests. The critical t for significance is unaffected by the number of groups.
This test is appropriate when you have 3 means to compare. In general the alpha is held at .05 because of the criterion that you can't look at LSD's unless the Anova is significant.
This test is generally not considered appropriate if you have more than 3 means unless there is reason to believe that there is no more than one true Null Hypothesis hidden in the means.

Dunn's (Bonferroni)\cr
Dunn's t-test is sometimes referred to as the Bonferroni t because it used the Bonferroni PE correction procedure in determining the critical value for significance. In general, this test should be used when the number of comparisons you are making exceeds the number of degrees of freedom you have between groups (e.g. K-1). This test sets alpha per experiment; Alpha = (.05)/c for every comparison. df = df error (c = number of comparisons (K(K-1))/2)
This test is extremely conservative and rapidly reduces power as the number of comparisons being made increase.

Newman-Keuls\cr
Newman-Keuls is a step down procedure that is not as conservative as Dunn's t test. First, the means of the groups are ordered (ascending or descending) and then the largest and smallest means are tested for significant differences. If those means are different, then test smallest with next largest, until you reach a test that is not significant. Once you reach that point then you can only test differences between means that exceed the difference between the means that were found to be non-significant.
Newman-Keuls is perhaps one of the most common Post Hoc test, but it is a rather controversial test. The major problem with this test is that when there is more than one true Null Hypothesis in a set of means it will overestimate they FW error rate.
In general we would use this when the number of comparisons we are making is larger than K-1 and we don't want to be as conservative as the Dunn's test is.

Tukey's HSD\cr
Tukey HSD (Honestly Significant Difference) is essentially like the Newman-Keul, but the tests between each mean are compared to the critical value that is set for the test of the means that are furthest apart (rmax e.g. if there are 5 means we use the critical value determined for the test of X1 and X5).
This Method corrects for the problem found in the Newman-Keuls where the FW is inflated when there is more than one True Null Hypothesis in a set of means. It buys protection against Type I error, but again at the cost of Power.
It tends to be the most common test and preferred test because it is very conservative with respect to Type I error when the Null hypothesis is true. In general, HSD is preferred when you will make all the possible comparisons between a large set of means (Six or more means).

Scheffe\cr
The Scheffe Test is designed to protect against a Type I error when all possible complex and simple comparisons are made. That is we are not just looking the possible combinations of comparisons between pairs of means. We are also looking at the possible combinations of comparisons between groups of means. Thus Scheffe is the most conservative of all tests.
Because this test does give us the capacity to look at complex comparisons, it essentially uses the same statistic as the Linear Contrasts tests. However, Scheffe uses a different critical value (or at least it makes an adjustment to the critical value of F).
This test has less power than the HSD when you are making Pairwise (simple) comparisons, but it has more power than HSD when you are making Complex comparisons.
In general, only use this when you want to make many Post Hoc complex comparisons (e.g. more than K-1).


Tables\cr
For tables pairwise chi-square test can be performed, either without correction or with correction for multiple testing following the logic in \code{\link{p.adjust}}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{an object of type "PostHocTest", which will either be \cr
A) a list of data.frames containing the mean difference, lower ci, upper ci and the p-value,
if a conf.level was defined (something else than NA) or \cr
B) a list of matrices with the p-values, if conf.level has been set to NA.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{TukeyHSD}}, \code{\link{aov}},  \code{\link{pairwise.t.test}},
\code{\link{ScheffeTest}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
PostHocTest(aov(breaks ~ tension, data = warpbreaks), method = "lsd")
PostHocTest(aov(breaks ~ tension, data = warpbreaks), method = "hsd")
PostHocTest(aov(breaks ~ tension, data = warpbreaks), method = "scheffe")

r.aov <- aov(breaks ~ tension, data = warpbreaks)

# compare p-values:
round(cbind(
    lsd= PostHocTest(r.aov, method="lsd")$tension[,"pval"]
  , bonf=PostHocTest(r.aov, method="bonf")$tension[,"pval"]
), 4)

# only p-values by setting conf.level to NA
PostHocTest(aov(breaks ~ tension, data = warpbreaks), method = "hsd",
            conf.level=NA)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
