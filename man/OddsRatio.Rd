\name{OddsRatio}
\alias{OddsRatio}
\alias{OddsRatio.glm}
\alias{OddsRatio.multinom}
\alias{OddsRatio.zeroinfl}
\alias{OddsRatio.default}

\title{Odds Ratio Estimation and Confidence Intervals}

\description{
  Calculates odds ratio by unconditional maximum likelihood estimation (\code{wald}),
  conditional maximum likelihood estimation (\code{mle}) or median-unbiased estimation (\code{midp}).
  Confidence intervals are calculated using normal approximation (\code{wald}) and exact methods
  (\code{midp}, \code{mle}).
}

\usage{
OddsRatio(x, conf.level = NULL, ...)

\method{OddsRatio}{glm}(x, conf.level = NULL, digits = 3, use.profile = TRUE, ...)

\method{OddsRatio}{multinom}(x, conf.level = NULL, digits = 3, ...)

\method{OddsRatio}{zeroinfl}(x, conf.level = NULL, digits = 3, ...)

\method{OddsRatio}{default}(x, conf.level = NULL, y = NULL, method = c("wald", "mle", "midp"),
          interval = c(0, 1000), ...)
}

\arguments{

  \item{x}{a vector or a \eqn{2 \times 2}{2 x 2} numeric matrix, resp. table.
%%     ~~Describe \code{x} here~~
}
  \item{y}{NULL (default) or a vector with compatible dimensions to \code{x}. If y is provided, \code{table(x, y, \dots)} will be calculated.
%%     ~~Describe \code{y} here~~
}
\item{digits}{the number of fixed digits to be used for printing the odds ratios.}
  \item{method}{
    method for calculating odds ratio and confidence intervals. Can be one out of
    "\code{wald}", "\code{mle}", "\code{midp}". Default is \code{"wald"} (not because it is the best, but
    because it is the most commonly used.)
  }
    \item{conf.level}{confidence level. Default is \code{NA} for tables and numeric vectors, meaning no confidence intervals will be reported. 0.95 is used as default for models.}
  \item{interval}{
    interval for the function \code{\link{uniroot}} that finds the
    odds ratio median-unbiased estimate and \code{midp} exact confidence
    interval.
  }
\item{use.profile}{logical. Defines if profile approach should be used, which normally is a good choice. Calculating profile can however take ages for large datasets and not be necessary there. So we can fallback to normal confidence intervals. }
   \item{\dots}{further arguments are passed to the function \code{\link{table}}, allowing i.e. to set \code{useNA}. This refers only to the vector interface.
%%     ~~Describe \code{\dots} here~~
}

}

\details{

  If a \eqn{2 \times 2}{2 x 2} table is provided the following table structure is preferred:
  \preformatted{
                    disease=0   disease=1
    exposed=0 (ref)    n00         n01
    exposed=1          n10         n11
  }
  however, for odds ratios the following table is
  equivalent:
  \preformatted{
                    disease=1   disease=0
    exposed=1          n11         n10
    exposed=0          n01         n00
  }
  If the table to be  provided to this function is not in the
  preferred form, the function \code{\link{Rev}()} can be used to "reverse" the table rows, resp.
  -columns. Reversing columns or rows (but not both) will lead to the inverse of the odds ratio.

%  If you are providing categorical variables (factors
%  or character vectors), the first level of the "exposure" variable is
%  treated as the reference. However, you can set the reference of a
%  factor using the \code{\link[stats]{relevel}} function.

%  Likewise, each row of the rx2 table is compared to the exposure
%  reference level and test of independence two-sided p values are
%  calculated using mid-p exact, Fisher's Exact, Monte Carlo simulation,
%  and the chi-square test.

In case of zero entries, 0.5 will be added to the table.
}


\value{
 a single numeric value if conf.level is set to \code{NA}\cr
 a numeric vector with 3 elements for estimate, lower and upper confidence interval if conf.level is provided
}

\references{
  Kenneth J. Rothman and Sander Greenland (1998): \emph{Modern Epidemiology},
  Lippincott-Raven Publishers

  Kenneth J. Rothman (2002): \emph{Epidemiology: An Introduction}, Oxford
  University Press

  Nicolas P. Jewell (2004): \emph{Statistics for Epidemiology}, 1st Edition,
  2004, Chapman & Hall, pp. 73-81

  Agresti, Alan (2013) \emph{Categorical Data Analysis}. NY: John Wiley and Sons, Chapt. 3.1.1

}

\author{Andri Signorell <andri@signorell.net>, strongly based on code from Tomas Aragon, <aragon@berkeley.edu>}

\seealso{
  \code{\link{RelRisk}}
}

\examples{
# Case-control study assessing whether exposure to tap water
#   is associated with cryptosporidiosis among AIDS patients

tab <- matrix(c(2, 29, 35, 64, 12, 6), 3, 2, byrow=TRUE)
dimnames(tab) <- list("Tap water exposure" = c("Lowest", "Intermediate", "Highest"),
                      "Outcome" = c("Case", "Control"))
tab <- Rev(tab, margin=2)

OddsRatio(tab[1:2,])
OddsRatio(tab[c(1,3),])

OddsRatio(tab[1:2,], method="mle")
OddsRatio(tab[1:2,], method="midp")
OddsRatio(tab[1:2,], method="wald", conf.level=0.95)

# in case of zeros consider using glm for calculating OR
dp <- data.frame (a=c(20, 7, 0, 0), b=c(0, 0, 0, 12), t=c(1, 0, 1, 0))
fit <- glm(cbind(a, b) ~ t, data=dp, family=binomial)

exp(coef(fit))

# calculation of log oddsratios in a 2x2xk table
migraine <- xtabs(freq ~ .,
                  cbind(expand.grid(treatment=c("active","placebo"),
                                    response=c("better","same"),
                                    gender=c("female","male")),
                        freq=c(16,5,11,20,12,7,16,19))
)

log(apply(migraine, 3, OddsRatio))

# OddsRatio table for logistic regression models
r.glm <- glm(type ~ ., data=MASS::Pima.tr2, family=binomial)
OddsRatio(r.glm)

plot(OddsRatio(r.glm), xlim=c(0.5, 2), main="OddsRatio - glm", pch=NA,
     lblcolor=hred, args.errbars=list(col=horange, pch=21, col.pch=hblue,
     bg.pch=hyellow, cex.pch=1.5))
}

\keyword{models}
\keyword{multivar}
