\name{FindCorr}
\alias{FindCorr}
\title{Determine Highly Correlated Variables}
\description{
This function searches through a correlation matrix and returns a vector of integers
corresponding to columns to remove to reduce pair-wise correlations.
}
\usage{
FindCorr(x, cutoff = .90, verbose = FALSE)
}

\arguments{
  \item{x}{A correlation matrix}
  \item{cutoff}{A numeric value for the pair-wise absolute correlation cutoff}
  \item{verbose}{A boolean for printing the details}
}
\details{
   The absolute values of pair-wise correlations are considered. If two variables have a high correlation,
   the function looks at the mean absolute correlation of each variable and removes the variable with the
   largest mean absolute correlation.

   There are several function in the \pkg{subselect} package (\code{\link[subselect:eleaps]{leaps}}, \code{\link[subselect:genetic]{genetic}}, \code{\link[subselect:anneal]{anneal}}) that can also be used
   to accomplish the same goal.
}
\value{
  A vector of indices denoting the columns to remove. If no correlations meet the criteria, \code{numeric(0)} is returned.
}
\references{
  Max Kuhn. Contributions from Jed Wing, Steve Weston, Andre Williams, Chris Keefer,
  Allan Engelhardt, Tony Cooper, Zachary Mayer and the R Core Team (2014). caret:
  Classification and Regression Training. R package version 6.0-35.
  \url{https://cran.r-project.org/package=caret}
}
\author{Original R code by Dong Li, modified by Max Kuhn}

\seealso{\code{\link[subselect:eleaps]{leaps}}, \code{\link[subselect:genetic]{genetic}}, \code{\link[subselect:anneal]{anneal}}}

\examples{
corrMatrix <- diag(rep(1, 5))
corrMatrix[2, 3] <- corrMatrix[3, 2] <- .7
corrMatrix[5, 3] <- corrMatrix[3, 5] <- -.7
corrMatrix[4, 1] <- corrMatrix[1, 4] <- -.67

corrDF <- expand.grid(row = 1:5, col = 1:5)
corrDF$correlation <- as.vector(corrMatrix)
PlotCorr(xtabs(correlation ~ ., corrDF), las=1, border="grey")

FindCorr(corrMatrix, cutoff = .65, verbose = TRUE)

FindCorr(corrMatrix, cutoff = .99, verbose = TRUE)

# d.pizza example
m <- cor(data.frame(lapply(d.pizza, as.numeric)), use="pairwise.complete.obs")
FindCorr(m, verbose = TRUE)
m[, FindCorr(m)]
}
\keyword{manip}
