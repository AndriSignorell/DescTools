\name{CCC}

\alias{CCC}

\title{
Concordance Correlation Coefficient
}

\description{
Calculates Lin's concordance correlation coefficient for agreement on a continuous measure.
}

\usage{
CCC(x, y, ci = "z-transform", conf.level = 0.95, na.rm = FALSE)
}

\arguments{
  \item{x}{a vector, representing the first set of measurements.}
  \item{y}{a vector, representing the second set of measurements.}
  \item{ci}{a character string, indicating the method to be used. Options are \code{z-transform} or \code{asymptotic}.}
  \item{conf.level}{magnitude of the returned confidence interval. Must be a single number between 0 and 1.}
    \item{na.rm}{
  logical, indicating whether \code{NA} values should be stripped before the computation proceeds. If set to \code{TRUE} only the complete cases of the ratings will be used. Defaults to \code{FALSE}.
  %%     ~~Describe \code{na.rm} here~~
}

}

\details{
Computes Lin's (1989, 2000) concordance correlation coefficient for agreement on a continuous measure obtained by two methods. The concordance correlation coefficient combines measures of both precision and accuracy to determine how far the observed data deviate from the line of perfect concordance (that is, the line at 45 degrees on a square scatter plot). Lin's coefficient increases in value as a function of the nearness of the data's reduced major axis to the line of perfect concordance (the accuracy of the data) and of the tightness of the data about its reduced major axis (the precision of the data).

Both \code{x} and \code{y} values need to be present for a measurement pair to be included in the analysis. If either or both values are missing (i.e. coded \code{NA}) then the measurement pair is deleted before analysis.
}

\value{
A list containing the following:
  \item{rho.c}{the concordance correlation coefficient.}
  \item{s.shift}{the scale shift.}
  \item{l.shift}{the location shift.}
  \item{C.b}{a bias correction factor that measures how far the best-fit line deviates from a line at 45 degrees. No deviation from the 45 degree line occurs when C.b = 1. See Lin (1989, page 258).}
  \item{blalt}{a data frame with two columns: \code{mean} the mean of each pair of measurements, \code{delta} vector \code{y} minus vector \code{x}.}
%%  \item{nmissing}{a count of the number of measurement pairs ignored due to missingness.}
}

\references{
Bland J, Altman D (1986). Statistical methods for assessing agreement between two methods of clinical measurement. \emph{The Lancet} 327: 307 - 310.

Bradley E, Blackwood L (1989). Comparing paired data: a simultaneous test for means and variances. \emph{American Statistician} 43: 234 - 235.

Dunn G (2004). \emph{Statistical Evaluation of Measurement Errors: Design and Analysis of Reliability Studies}. London: Arnold.

Hsu C (1940). On samples from a normal bivariate population. \emph{Annals of Mathematical Statistics} 11: 410 - 426.

Krippendorff K (1970). Bivariate agreement coefficients for reliability of data. In: Borgatta E, Bohrnstedt G (eds)
\emph{Sociological Methodology}. San Francisco: Jossey-Bass, pp. 139 - 150.

Lin L (1989). A concordance correlation coefficient to evaluate reproducibility. \emph{Biometrics} 45: 255 - 268.

Lin L (2000). A note on the concordance correlation coefficient. \emph{Biometrics} 56: 324 - 325.

Pitman E (1939). A note on normal correlation. \emph{Biometrika} 31: 9 - 12.

Reynolds M, Gregoire T (1991). Comment on Bradley and Blackwood. \emph{American Statistician} 45: 163 - 164.

Snedecor G, Cochran W (1989). \emph{Statistical Methods}. Ames: Iowa State University Press.
}

\author{Mark Stevenson <mark.stevenson1@unimelb.edu.au>}


\seealso{
  \code{\link{ICC}}, \code{\link{KendallW}}
}

\examples{
## Concordance correlation plot:
set.seed(seed = 1234)
method1 <- rnorm(n = 100, mean = 0, sd = 1)
method2 <- method1 + runif(n = 100, min = 0, max = 1)

## Introduce some missing values:
method1[50] <- NA
method2[75] <- NA

tmp.ccc <- CCC(method1, method2, ci = "z-transform",
   conf.level = 0.95)

lab <- paste("CCC: ", round(tmp.ccc$rho.c[,1], digits = 2), " (95\% CI ", 
   round(tmp.ccc$rho.c[,2], digits = 2), " - ",
   round(tmp.ccc$rho.c[,3], digits = 2), ")", sep = "")
z <- lm(method2 ~ method1)

par(pty = "s")
plot(method1, method2, xlim = c(0, 5), ylim = c(0,5), xlab = "Method 1", 
   ylab = "Method 2", pch = 16)
abline(a = 0, b = 1, lty = 2)
abline(z, lty = 1)
legend(x = "topleft", legend = c("Line of perfect concordance", 
   "Reduced major axis"), lty = c(2,1), lwd = c(1,1), bty = "n")
text(x = 1.55, y = 3.8, labels = lab)

## Bland and Altman plot (Figure 2 from Bland and Altman 1986):
x <- c(494,395,516,434,476,557,413,442,650,433,417,656,267,
   478,178,423,427)

y <- c(512,430,520,428,500,600,364,380,658,445,432,626,260,
   477,259,350,451)

tmp.ccc <- CCC(x, y, ci = "z-transform", conf.level = 0.95)
tmp.mean <- mean(tmp.ccc$blalt$delta)
tmp.sd <- sqrt(var(tmp.ccc$blalt$delta))

plot(tmp.ccc$blalt$mean, tmp.ccc$blalt$delta, pch = 16, 
   xlab = "Average PEFR by two meters (L/min)", 
   ylab = "Difference in PEFR (L/min)", xlim = c(0,800), 
   ylim = c(-140,140)) 
abline(h = tmp.mean, lty = 1, col = "gray")
abline(h = tmp.mean - (2 * tmp.sd), lty = 2, col = "gray")
abline(h = tmp.mean + (2 * tmp.sd), lty = 2, col = "gray")
legend(x = "topleft", legend = c("Mean difference", 
   "Mean difference +/ 2SD"), lty = c(1,2), bty = "n")
legend(x = 0, y = 125, legend = c("Difference"), pch = 16, 
    bty = "n")
}

\keyword{univar}
