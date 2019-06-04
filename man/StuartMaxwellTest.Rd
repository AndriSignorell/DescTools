\name{StuartMaxwellTest}
\alias{StuartMaxwellTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Stuart-Maxwell Marginal Homogeneity Test
%%  ~~function to do ... ~~
}
\description{This function computes the marginal homogeneity test for a \eqn{k \times  k}{k x k} matrix of assignments of objects to k categories or an \eqn{n \times 2 \times k}{n x 2} matrix of category scores for n data objects by two raters. The statistic is distributed as chi-square with k-1 degrees of freedom. \cr
It can be viewed as an extention of McNemar test to \eqn{k \times k}{k x k} table. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StuartMaxwellTest(x, y = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{either a 2-way contingency table in matrix form, or a factor object.
%%     ~~Describe \code{x} here~~
}
  \item{y}{a factor object; ignored if x is a matrix.
%%     ~~Describe \code{y} here~~
}
}
\details{The null is that the probabilities of being classified into cells [i,j] and [j,i] are the same.

If x is a matrix, it is taken as a two-dimensional contingency table, and hence its entries should be nonnegative integers. Otherwise, both x and y must be vectors or factors of the same length. Incomplete cases are removed, vectors are coerced into factors, and the contingency table is computed from these.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the test statistic.}
  \item{parameter}{the degrees of freedom.}
  \item{p.value}{the p-value of the test.}
  \item{method}{a character string indicating what type of test was
    performed.} 
  \item{data.name}{a character string giving the name of the data.}
}
\references{
Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley & Sons, pp 86 ff.
%% ~put references to the literature/web site here ~
}
\author{Andri Signorell <andri@signorell.net>, based on Code from Jim Lemon
%%  ~~who you are~~
}

\seealso{\code{\link{mcnemar.test}}, \code{\link{chisq.test}}, \code{\link{MHChisqTest}},
\code{\link{BreslowDayTest}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
hyp <- as.table(matrix(c(20,3,0,10,30,5,5,15,40), nrow=3))
StuartMaxwellTest(hyp)

# Source: http://www.john-uebersax.com/stat/mcnemar.htm#stuart
mc <- as.table(matrix(c(
         732, 1524, 1575, 1577, 1602, 837, 1554, 1437, 
         1672, 1600, 841, 1363, 1385, 1484, 1524, 791), nrow=4))

StuartMaxwellTest(mc)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
