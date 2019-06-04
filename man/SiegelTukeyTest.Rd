\name{SiegelTukeyTest}
\alias{SiegelTukeyTest}
\alias{SiegelTukeyRank}
\alias{SiegelTukeyTest.default}
\alias{SiegelTukeyTest.formula}
\title{Siegel-Tukey Test For Equality In Variability
}
\description{
Non-parametric Siegel-Tukey test for equality in variability.
The null hypothesis is that the variability of x is equal between two
groups. A rejection of the null hypothesis indicates that variability differs between
the two groups. \code{SiegelTukeyRank} returns the ranks, calculated after Siegel Tukey logic.
}
\usage{
SiegelTukeyTest(x, ...)

\method{SiegelTukeyTest}{default}(x, y, adjust.median = FALSE,
                alternative = c("two.sided", "less", "greater"),
                mu = 0, exact = NULL, correct = TRUE, conf.int = FALSE,
                conf.level = 0.95, \dots)

\method{SiegelTukeyTest}{formula}(formula, data, subset, na.action, \dots)


SiegelTukeyRank(x, g, drop.median = TRUE)

}
\arguments{
  \item{x, y}{numeric vector of data values. Non-finite (e.g. infinite or missing) values will be omitted.
}
\item{g}{a vector or factor object giving the group for the corresponding elements of x.
}
\item{adjust.median}{Should between-group differences in medians be leveled
    before performing the test? In certain cases, the Siegel-Tukey test is
    susceptible to median differences and may indicate significant differences
    in variability that, in reality, stem from differences in medians. Default is \code{FALSE}.

}
  \item{alternative}{a character string specifying the alternative hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}. You can specify just the initial letter.
}
  \item{mu}{a number specifying an optional parameter used to form the null hypothesis. See Details.}

  \item{exact}{a logical indicating whether an exact p-value should be computed. This is passed directly to \code{\link{wilcox.test}}.
}
  \item{correct}{a logical indicating whether to apply continuity correction in the normal approximation for the p-value.
}
  \item{conf.int}{a logical indicating whether a confidence interval should be computed.
}
  \item{conf.level}{confidence level of the interval.}
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the data values and rhs the corresponding groups.}
  \item{data}{an optional matrix or data frame (or similar: see \code{\link{model.frame}}) containing the variables in the formula \code{formula}.
    By default the variables are taken from \code{environment(formula)}.}
  \item{subset}{an optional vector specifying a subset of observations to be used.}
  \item{na.action}{a function which indicates what should happen when the data contain NAs. Defaults to \code{getOption("na.action")}.}


\item{drop.median}{logical, defining whether the median of the combined samples should be left out, ensuring that there's an even number of elements (which is a requirement of the Siegel-Tukey test). Defaults to \code{TRUE}.
}
  \item{\dots}{further arguments to be passed to or from methods.}

}
\details{
The Siegel-Tukey test has relatively low power and may, under certain
conditions, indicate significance due to differences in medians rather than
differences in variabilities (consider using the argument \code{adjust.median}).
Consider also using \code{\link{mood.test}} or \code{\link{ansari.test}}.

}

\value{
A list of class \code{htest}, containing the following components:
  \item{statistic}{ Siegel-Tukey test (Wilcoxon test on tie-adjusted Siegel-Tukey ranks, after the median adjustment if specified).}
  \item{p.value}{   the p-value for the test}
  \item{null.value}{is the value of the median specified by the null hypothesis. This
                    equals the input argument \code{mu}. }
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{method}{  the type of test applied}
  \item{data.name}{a character string giving the names of the data.}
}


\references{
Siegel, S., Tukey, J. W. (1960): A nonparametric sum of ranks procedure for relative spread in unpaired samples.
\emph{Journal of the American Statistical Association}.

Sheskin, D. J. (2004): \emph{Handbook of parametric and nonparametric statistical procedures} 3rd edition. Chapman and Hall/CRC. Boca Raton, FL.
%% ~put references to the literature/web site here ~
}
\author{Daniel Malter, Tal Galili <tal.galili@gmail.com>, Andri Signorell <andri@signorell.net>\cr
published on: \url{http://www.r-statistics.com/2010/02/siegel-tukey-a-non-parametric-test-for-equality-in-variability-r-code/}
}

\seealso{ \code{\link{mood.test}}, \code{\link{ansari.test}}, \code{\link{wilcox.test}},  \code{\link{LeveneTest}}

}
\examples{
# Duller, S. 183
x <- c(12, 13, 29, 30)
y <- c(15, 17, 18, 24, 25, 26)
SiegelTukeyTest(x, y)
SiegelTukeyTest(x, y, alternative="greater")

# Duller, S. 323
old <- c(870,930,935,1045,1050,1052,1055)
new <- c(932,970,980,1001,1009,1030,1032,1040,1046)
SiegelTukeyTest(old, new, alternative = "greater")
# compare to the recommended alternatives
mood.test(old, new, alternative="greater")
ansari.test(old, new, alternative="greater")

# Bortz, S. 250
x <- c(26.3,26.5,26.8,27.0,27.0,27.2,27.3,27.3,27.4,27.5,27.6,27.8,27.9)
id <- c(2,2,2,1,2,2,1,2,2,1,1,1,2)-1
SiegelTukeyTest(x ~ id)


# Sachs, Angewandte Statistik, 12. Auflage, 2007, S. 314
A <- c(10.1,7.3,12.6,2.4,6.1,8.5,8.8,9.4,10.1,9.8)
B <- c(15.3,3.6,16.5,2.9,3.3,4.2,4.9,7.3,11.7,13.1)
SiegelTukeyTest(A, B)



### 1
x <- c(4,4,5,5,6,6)
y <- c(0,0,1,9,10,10)
SiegelTukeyTest(x, y)

### 2
# example for a non equal number of cases:
x <- c(4,4,5,5,6,6)
y <- c(0,0,1,9,10)
SiegelTukeyTest(x, y)

### 3
x <- c(33, 62, 84, 85, 88, 93, 97, 4, 16, 48, 51, 66, 98)
id <- c(0,0,0,0,0,0,0,1,1,1,1,1,1)
SiegelTukeyTest(x ~ id)

### 4
x <- c(177,200,227,230,232,268,272,297,47,105,126,142,158,172,197,220,225,230,262,270)
id <- c(rep(0,8),rep(1,12))
SiegelTukeyTest(x ~ id, adjust.median=TRUE)

### 5
x <- c(33,62,84,85,88,93,97)
y <- c(4,16,48,51,66,98)
SiegelTukeyTest(x, y)

### 6
x <- c(0,0,1,4,4,5,5,6,6,9,10,10)
id <- c(0,0,0,1,1,1,1,1,1,0,0,0)
SiegelTukeyTest(x ~ id)

### 7
x <- c(85,106,96, 105, 104, 108, 86)
id <- c(0,0,1,1,1,1,1)
SiegelTukeyTest(x ~ id)
}
\keyword{ htest }
