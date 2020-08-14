\name{ORToRelRisk}
\alias{ORToRelRisk}
\alias{ORToRelRisk.default}
\alias{ORToRelRisk.OddsRatio}
\title{ Transform Odds Ratio to Relative Risk }
\description{The odds ratio is a common measure when comparing two groups in
terms of an outcome that is either present or absent. As the odds ratio is in general poorly understood, odds ratios are often discussed in terms of risks, relying on the approximation, that odds ratio and relative risk are about the same when the outcome is rare. However the relative risk also depends on the risk of the baseline group and if the outcome is not rare there can be large differences between both measures and the odds ratio may substantially overestimate the relative risk. In fact, the same odds ratio could imply a very different relative risk for subgroups of the population with different baseline risks.

The present function transforms a given odds-ratio (OR) to the respective relative risk (RR) either for simple odds ratios but also for odds ratios resulting from a logistic model.
}


\usage{
ORToRelRisk(...)

\method{ORToRelRisk}{OddsRatio}(x, \dots )
\method{ORToRelRisk}{default}(or, p0, \dots)
}
\arguments{
  \item{x}{the odds ratios of a logistic model as returned by \code{\link{OddsRatio}()}}
  \item{or}{ numeric vector, containing odds-ratios. }
  \item{p0}{ numeric vector, incidence of the outcome of interest in the
             nonexposed group ("baseline risk").}
  \item{\dots}{further arguments, are not used here.}           
}
\details{
  The function transforms a given odds-ratio (or) to the respective
  relative risk (rr). It can also be used to transform the limits
  of confidence intervals.

The formula for converting an odds ratio to a relative risk is
\deqn{rr = \frac{or}{1 - p_0 + p_0 \cdot or}}{rr = or / (1 - p_0 + p_0 * or)}

where \eqn{p_0} is the baseline risk.


For transformation of odds ratios resulting from a logit model, we use the formula of Zhang and Yu (1998).
}
\value{
  relative risk.
}
\references{
  Zhang, J. and Yu, K. F. (1998). What's the relative risk? A method of correcting the odds ratio in cohort
  studies of common outcomes. \emph{JAMA}, \bold{280}(19):1690-1691.

Grant, R. L. (2014) Converting an odds ratio to a range of plausible relative
risks for better communication of research findings. BMJ 2014;348:f7450 doi: 10.1136/bmj.f7450
}
\author{ Matthias Kohl <matthias.kohl@stamats.de>}
\examples{
(heart <- as.table(matrix(c(11, 2, 4, 6), nrow=2,
                          dimnames = list(Exposure = c("High", "Low"), 
                                          Response = c("Yes", "No")))))
RelRisk(heart)
# calculated as (11/15)/(2/8)

OddsRatio(heart)
# calculated as (11/4)/(2/6)

ORToRelRisk(OddsRatio(heart), p0 = 2/8)
# Relative risk = odds ratio / (1 - p0 + (p0 * odds ratio))
# where p0 is the baseline risk


## single OR to RR
ORToRelRisk(14.1, 0.05)

## OR and 95% confidence interval
ORToRelRisk(c(14.1, 7.8, 27.5), 0.05)

## Logistic OR and 95% confidence interval
logisticOR <- rbind(c(14.1, 7.8, 27.5),
                    c(8.7, 5.5, 14.3),
                    c(27.4, 17.2, 45.8),
                    c(4.5, 2.7, 7.8),
                    c(0.25, 0.17, 0.37),
                    c(0.09, 0.05, 0.14))
colnames(logisticOR) <- c("OR", "2.5\%", "97.5\%")
rownames(logisticOR) <- c("7.4", "4.2", "3.0", "2.0", "0.37", "0.14")
logisticOR

## p0
p0 <- c(0.05, 0.12, 0.32, 0.27, 0.40, 0.40)

## Compute corrected RR
## helper function
ORToRelRisk.mat <- function(or, p0){
  res <- matrix(NA, nrow = nrow(or), ncol = ncol(or))
  for(i in seq_len(nrow(or)))
    res[i,] <- ORToRelRisk(or[i,], p0[i])
  dimnames(res) <- dimnames(or)
  res
}
RR <- ORToRelRisk.mat(logisticOR, p0)
round(RR, 2)

## Results are not completely identical to Zhang and Yu (1998)
## what probably is caused by the fact that the logistic OR values
## provided in the table are rounded and not true values.
}
\keyword{univar}