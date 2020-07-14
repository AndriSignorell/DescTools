\name{BinomDiffCI}
\alias{BinomDiffCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Confidence Interval for a Difference of Binomials
%%  ~~function to do ... ~~
}
\description{Several confidence intervals for the difference between proportions are available, but they can produce markedly different results. Traditional approaches, such as the Wald interval do not perform well unless the sample size is large. Better intervals are available. These include the Agresti/Caffo method (2000), Newcombe Score method (1998) and a more computing intensive ones as by Miettinen and Nurminen (1985) or Mee (1984).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BinomDiffCI(x1, n1, x2, n2, conf.level = 0.95, sides = c("two.sided","left","right"),
            method = c("wald", "waldcc", "ac", "score", "scorecc", "mn",
                       "mee", "blj", "ha", "hal", "jp"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{ number of successes for the first group. }
  \item{n1}{ number of trials for the first group. }
  \item{x2}{ number of successes for the second group. }
  \item{n2}{ number of trials for the second group. }
  \item{conf.level}{ confidence level, defaults to 0.95. }
  \item{sides}{a character string specifying the side of the confidence interval, must be one of \code{"two.sided"} (default),
\code{"left"} or \code{"right"}. You can specify just the initial letter. \code{"left"} would be analogue to a hypothesis of
\code{"greater"} in a \code{t.test}.}


  \item{method}{ one of \code{"wald"}, \code{"waldcc"}, \code{"ac"}, \code{"score"}, \code{"scorecc"}, \code{"mn"}, \code{"mee"}, \code{"blj"}, \code{"ha"}, \code{"hal"}, \code{"jp"}.
%%     ~~Describe \code{method} here~~
}
}

%% use https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf for formulas one day...

\details{
All arguments are being recycled.

We estimate the difference between proportions using the sample proportions:
 \deqn{\hat{\delta} =\hat{p}_1 - \hat{p}_2 = \frac{x_1}{n_1} - \frac{x_2}{n_2}}

The traditional \bold{Wald } confidence interval for the difference of two proportions \eqn{\delta} is based on the asymptotic normal distribution of \eqn{\hat{\delta}}.

  The \bold{Corrected Wald} interval uses a continuity correction included in the test statistic. The continuity correction is subtracted from the numerator of the test statistic if the numerator is greater than zero; otherwise, the continuity correction is added to the numerator. The value of the continuity correction is (1/n1 + 1/n2)/2.

  The \bold{Agresti-Caffo} (code \code{"ac"}) is equal to the Wald interval with the adjustment according to Agresti, Caffo (2000) for difference in proportions and independent samples. It adds 1 to x1 and x2 and adds 2 to n1 and n2 and performs surpringly well.

\bold{Newcombe} (code \code{"scorecc"}) proposed a confidence interval for the difference based on the Wilson score confidence interval for a single proportion. A variant uses a continuity correction for the Wilson interval (code \code{"scorecc"}).

\bold{Miettinen and Nurminen} showed that the restricted maximum likelihood estimates for p1 and p2 can
be obtained by solving a cubic equation and gave unique closed-form expressions for them. The Miettinen-Nurminen confidence interval is returned with code \code{"mn"}.

The \bold{Mee} (code \code{"mee"}) interval proposed by Mee (1984) and Farrington-Manning (1990) is using the same maximum likelihood estimators as Miettinen-Nurminen but with another correcting factor.

The \bold{Brown, Li's Jeffreys} (code \code{"blj"}) interval was proposed by Brown, Li's Jeffreys (2005).

The \bold{Hauck-Anderson} (code \code{"ha"}) interval was proposed by Hauck-Anderson (1986).

The \bold{Haldane} (code \code{"hal"}) interval is described in Newcombe (1998) and so is
the \bold{Jeffreys-Perks} (code \code{"jp"}).

Some approaches for the confidence intervals can potentially yield negative results or values beyond [-1, 1]. These would be reset such as not to exceed the range of [-1, 1].

Which of the methods to use is currently still the subject of lively discussion and has not yet been conclusively clarified. See e.g. Fagerland (2011).


%%  ~~ If necessary, more details than the description above ~~
}
\value{
  A vector with 3 elements for estimate, lower confidence intervall and upper for the upper one.
 }
\references{
Agresti, A, Caffo, B (2000) Simple and effective confidence intervals for proportions and difference of proportions result from adding two successes and two failures. \emph{The American Statistician} 54 (4), 280-288.

Beal, S L (1987) Asymptotic Confidence Intervals for the Difference Between Two Binomial Parameters for Use with Small Samples;  \emph{Biometrics}, 43, 941-950.

Brown L, Li X (2005) Confidence intervals for two sample binomial distribution, \emph{Journal of Statistical Planning and Inference}, 130(1), 359-375.

Hauck WW, Anderson S. (1986) A comparison of large-sample confidence interval methods for the difference of two binomial probabilities \emph{The American Statistician} 40(4): 318-322.

Farrington, C. P. and Manning, G. (1990) Test Statistics and Sample Size Formulae for Comparative Binomial Trials with Null Hypothesis of Non-zero Risk Difference or Non-unity Relative Risk \emph{Statistics in Medicine}, 9, 1447-1454.

Mee RW (1984) Confidence bounds for the difference between two probabilities, \emph{Biometrics} 40:1175-1176 .

Miettinen OS, Nurminen M. (1985) Comparative analysis of two rates. \emph{Statistics in Medicine} 4, 213-226.

Newcombe, R G (1998). Interval Estimation for the Difference Between Independent Proportions: Comparison of Eleven Methods. \emph{Statistics in Medicine}, 17, 873--890.

Fagerland M W, Lydersen S and Laake P (2011) Recommended confidence intervals for two independent binomial proportions, \emph{Statistical Methods in Medical Research} 0(0) 1-31

}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{BinomCI}},  \code{\link{MultinomCI}},  \code{\link{binom.test}},
\code{\link{prop.test}}, \code{\link{BinomRatioCI}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x1 <- 56; n1 <- 70; x2 <- 48; n2 <- 80
xci <- BinomDiffCI(x1, n1, x2, n2, method=c("wald", "waldcc", "ac", "score",
            "scorecc", "mn", "mee", "blj", "ha"))
Format(xci[,-1], digits=4)

x1 <- 9; n1 <- 10; x2 <- 3; n2 <- 10
yci <- BinomDiffCI(x1, n1, x2, n2, method=c("wald", "waldcc", "ac", "score",
            "scorecc", "mn", "mee", "blj", "ha"))
Format(yci[, -1], digits=4)

# https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf, page 9
SetNames(round(
  BinomDiffCI(56, 70, 48, 80, 
              method=c("wald", "waldcc", "hal", 
                       "jp", "mee",
                       "mn", "score", "scorecc", 
                       "ha", "ac", "blj"))[,-1], 4),
  rownames=c("1. Wald, no CC", "2. Wald, CC", "3. Haldane", "4. Jeffreys-Perks",
             "5. Mee", "6. Miettinen-Nurminen", "10. Score, no CC", "11. Score, CC",
             "12. Hauck-Andersen", "13. Agresti-Caffo", "16. Brown-Li"))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{category}
