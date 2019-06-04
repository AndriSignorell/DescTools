\name{CochranQTest}
\alias{CochranQTest}
\alias{CochranQTest.default}
\alias{CochranQTest.formula}
\title{
Cochran's Q test
}
\description{
Perform the Cochran's Q test for unreplicated randomized block design experiments with a 
binary response variable and paired data. 
}
\usage{
CochranQTest(y, ...)

\method{CochranQTest}{default}(y, groups, blocks, ...)

\method{CochranQTest}{formula}(formula, data, subset, na.action, ...)

}
\arguments{
  \item{y}{
  either a numeric vector of data values, or a data matrix.
}
  \item{groups}{
a vector giving the group for the corresponding elements of y if this is a vector; ignored if y is a matrix. If not a factor object, it is coerced to one.
}
  \item{blocks}{
a vector giving the block for the corresponding elements of y if this is a vector; ignored if y is a matrix. If not a factor object, it is coerced to one.}
  \item{formula}{
a formula of the form \code{a ~ b | c}, where a, b and c give the data values and corresponding groups and blocks, respectively.}
  \item{data}{
an optional matrix or data frame (or similar: see model.frame) containing the variables in the formula formula. By default the variables are taken from environment(formula).}
  \item{subset}{
an optional vector specifying a subset of observations to be used.
}
  \item{na.action}{
a function which indicates what should happen when the data contain NAs. Defaults to getOption("na.action").}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{
CochranQTest can be used for analyzing unreplicated complete block designs (i.e., there is exactly one 
binary observation in y for each combination of levels of groups and blocks) where the normality assumption may be violated.

The null hypothesis is that apart from an effect of blocks, the location parameter of y is the same in each of the groups.

If y is a matrix, groups and blocks are obtained from the column and row indices, respectively. 
NA's are not allowed in groups or blocks; if y contains NA's, corresponding blocks are removed.

Note that Cochran's Q Test is analogue to the Friedman test with 0, 1 coded response. This is used here for a simple implementation.
}

\value{

A list with class \code{htest} containing the following components:

\item{statistic}{the value of Cochran's chi-squared statistic.}
\item{parameter}{the degrees of freedom of the approximate chi-squared distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{method}{the character string "Cochran's Q-Test".}
\item{data.name}{a character string giving the names of the data.}
}

\author{
Andri Signorell <andri@signorell.net>
}

\examples{
# example in: 
# http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
# pp. S. 1824

# use expand.grid, xtabs and Untable to create the dataset
d.frm <- Untable(xtabs(c(6,2,2,6,16,4,4,6) ~ ., 
    expand.grid(rep(list(c("F","U")), times=3))), 
    colnames = LETTERS[1:3])

# rearrange to long shape    
d.long <- reshape(d.frm, varying=1:3, times=names(d.frm)[c(1:3)], 
                  v.names="resp", direction="long")


# after having done the hard work of data organisation, performing the test is a piece of cake....
CochranQTest(resp ~ time | id, data=d.long)
}




