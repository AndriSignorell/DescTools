\name{Strata}
\alias{Strata}
\title{Stratified Sampling}
\description{Stratified sampling with equal/unequal probabilities.}

\usage{
Strata(x, stratanames = NULL, size,
       method = c("srswor", "srswr", "poisson", "systematic"),
       pik, description = FALSE)}

\arguments{
\item{x}{a data frame or a matrix; its number of rows is n, the population size.}

\item{stratanames}{vector of stratification variables.}

\item{size}{vector of stratum sample sizes (in the order in which the strata are given in the input
data set).}

\item{method}{method to select units; implemented are: a) simple random
sampling without replacement (\code{"srswor"}), b) simple random sampling with replacement (\code{"srswr"}),
c) Poisson sampling (\code{"poisson"}), d) systematic sampling (\code{"systematic"}) (default is \code{"srswor"}).}

\item{pik}{vector of inclusion probabilities or auxiliary information used to compute them;
this argument is only used for unequal probability sampling (Poisson and systematic). If an
auxiliary information is provided, the function uses the inclusionprobabilities function for
computing these probabilities. If the method is "srswr" and the sample size is larger than
the population size, this vector is normalized to one.}

\item{description}{a message is printed if its value is TRUE; the message gives the number
of selected units and the number of the units in the population.
By default, the value is FALSE.}
}

\value{
The function produces an object, which contains the following information:
\item{id}{the identifier of the selected units.}
\item{stratum}{the unit stratum.}
\item{prob}{the final unit inclusion probability.}
}

\author{Andri Signorell <andri@signorell.net>
\cr rewritten based on the ideas of Yves Tille <yves.tille@unine.ch> and Alina Matei <alina.matei@unine.ch>}

\seealso{
\code{\link{sample}} }

\examples{
# Example from An and Watts (New SAS procedures for Analysis of Sample Survey Data)
# generates artificial data (a 235X3 matrix with 3 columns: state, region, income).
# the variable "state" has 2 categories ('nc' and 'sc').
# the variable "region" has 3 categories (1, 2 and 3).
# the sampling frame is stratified by region within state.
# the income variable is randomly generated

m <- rbind(matrix(rep("nc",165), 165, 1, byrow=TRUE),
           matrix(rep("sc", 70), 70, 1, byrow=TRUE))
m <- cbind.data.frame(m, c(rep(1, 100), rep(2,50), rep(3,15),
                      rep(1, 30), rep(2, 40)), 1000 * runif(235))
names(m) <- c("state", "region", "income")

# computes the population stratum sizes
table(m$region, m$state)

# not run
#     nc  sc
#  1 100  30
#  2  50  40
#  3  15   0
# there are 5 cells with non-zero values
# one draws 5 samples (1 sample in each stratum)
# the sample stratum sizes are 10,5,10,4,6, respectively
# the method is 'srswor' (equal probability, without replacement)

s <- Strata(m, c("region", "state"), size=c(10, 5, 10, 4, 6), method="srswor")

# extracts the observed data
data.frame(income=m[s$id, "income"], s)

# see the result using a contigency table
table(s$region, s$state)


# The same data as in Example 1
# the method is 'systematic' (unequal probability, without replacement)
# the selection probabilities are computed using the variable 'income'
s <- Strata(m,c("region", "state"), size=c(10, 5, 10, 4, 6),
            method="systematic", pik=m$income)

# extracts the observed data
data.frame(income=m[s$id, "income"], s)

# see the result using a contigency table
table(s$region, s$state)
}
\keyword{survey}
