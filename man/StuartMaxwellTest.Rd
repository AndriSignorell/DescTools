\name{StuartMaxwellTest}
\alias{StuartMaxwellTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Stuart-Maxwell Marginal Homogeneity Test
%%  ~~function to do ... ~~
}
\description{This function computes the marginal homogeneity test for a \eqn{k \times  k}{k x k} matrix of assignments of objects to \code{k} categories or two vectors \code{x}, \code{y} of category scores for \code{n} data objects by two raters. The statistic is distributed as \eqn{\chi^2}{chi-square} with \code{k-1} degrees of freedom. \cr
It can be viewed as an extension of the McNemar test to \eqn{k \times k}{k x k} table. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StuartMaxwellTest(x, y = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{either a 2-way \eqn{k \times  k}{k x k} contingency table in matrix form, or a factor.
%%     ~~Describe \code{x} here~~
}
  \item{y}{a factor with the same levels as x; ignored if x is a matrix.
%%     ~~Describe \code{y} here~~
}
}
\details{The null is that the probabilities of being classified into cells [i, j] and [j, i] are the same.

If \code{x} is a matrix, it is taken as a two-dimensional contingency table, and hence its entries should be nonnegative integers. Otherwise, both x and y must be vectors or factors of the same length and with the same levels. \cr
Incomplete cases are removed, vectors are coerced into factors, and the contingency table is computed from these.

If there is perfect agreement for any category k, that category must be omitted in order to invert matrix S. 

If for any category \code{k}, all frequencies in row \code{k} and column \code{k} are 0, except possibly for the main diagonal element (e.g., for perfect agreement for category \code{k}, in such cases also the corresponding row and column marginal frequencies would be equal), then the category is not included in the test and should be ignored, say the Stuart-Maxwell test is performed with respect to the remaining categories only. The degree of freedom \code{df} in this case can still be considered \code{k - 1}, where \code{k} is the number of original categories; this treats omitted categories as if they were included but contributed 0 to the value of \eqn{\chi^2}{Chi-square} - a reasonable view since such categories have equal row and column marginals. (See: \url{https://www.john-uebersax.com/stat/mcnemar.htm#stuart})
           
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
Stuart, A (1955) A test for homogeneity of the marginal distributions in a two-way classification. \emph{Biometrika}, 42, 412-416.

Maxwell, A.E. (1970) Comparing the classification of subjects by two independent judges. \emph{British Journal of Psychiatry}, 116, 651-655.

Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley & Sons, pp 86 ff.

%% ~put references to the literature/web site here ~
}
\author{Andri Signorell <andri@signorell.net>, based on Code from Jim Lemon
%%  ~~who you are~~
}

\seealso{
\code{\link{BhapkarTest}} for a more powerful alternative to the Stuart-Maxwell test

\code{\link{mcnemar.test}}, \code{\link{chisq.test}}, \code{\link{MHChisqTest}},
\code{\link{BreslowDayTest}}

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Source: https://john-uebersax.com-us.com/stat/mcnemar.htm#stuart
hyp <- as.table(matrix(c(20,3,0,10,30,5,5,15,40), nrow=3))
StuartMaxwellTest(hyp)

# same as defined with two vectors
d.hyp <- Untable(hyp)
StuartMaxwellTest(x=d.hyp[,1], y=d.hyp[,2])


mc <- as.table(matrix(c(
         732, 1524, 1575, 1577, 1602, 837, 1554, 1437, 
         1672, 1600, 841, 1363, 1385, 1484, 1524, 791), nrow=4))

StuartMaxwellTest(mc)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
\concept{ irr }
