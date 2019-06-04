\name{PageTest}
\alias{PageTest}
\alias{PageTest.default}
\alias{PageTest.formula}
\title{Exact Page Test for Ordered Alternatives}
\description{
  Performs a Page test for ordered alternatives using an exact algorithm by Stefan Wellek (1989) 
  with unreplicated blocked data.
}
\usage{
PageTest(y, \dots)
  
\method{PageTest}{default}(y, groups, blocks, \dots)
  
\method{PageTest}{formula}(formula, data, subset, na.action, \dots)
}

\arguments{
  \item{y}{either a numeric vector of data values, or a data matrix.}
  \item{groups}{a vector giving the group for the corresponding
                elements of \code{y} if this is a vector;  ignored if \code{y}
                is a matrix.  If not a factor object, it is coerced to one.}
  \item{blocks}{a vector giving the block for the corresponding
                elements of \code{y} if this is a vector;  ignored if \code{y}
                is a matrix.  If not a factor object, it is coerced to one.}
  \item{formula}{a formula of the form \code{a ~ b | c}, where \code{a},
                 \code{b} and \code{c} give the data values and corresponding groups
                 and blocks, respectively.}
  \item{data}{an optional matrix or data frame (or similar: see
                                                \code{\link{model.frame}}) containing the variables in the
              formula \code{formula}.  By default the variables are taken from
              \code{environment(formula)}.}
  \item{subset}{an optional vector specifying a subset of observations
                to be used.}
  \item{na.action}{a function which indicates what should happen when
                   the data contain \code{NA}s.  Defaults to
                   \code{getOption("na.action")}.}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{
  \code{PageTest} can be used for analyzing unreplicated complete
  block designs (i.e., there is exactly one observation in \code{y}
                 for each combination of levels of \code{groups} and \code{blocks})
  where the normality assumption may be violated.
  
  The null hypothesis is that apart from an effect of \code{blocks},
  the location parameter of \code{y} is the same in each of the
  \code{groups}.\cr
  The implemented alternative is, that the location parameter will be monotonly greater along the groups, \cr
  \eqn{H_{A}: \theta_{1} \le \theta_{2} \le \theta_{3}} ... (where at least one inequality is strict).\cr
  If the other direction is required, the order of the groups has to be reversed.
  \cr\cr
  The Page test for ordered alternatives is slightly more powerful than
  the Friedman analysis of variance by ranks.
  
  If \code{y} is a matrix, \code{groups} and \code{blocks} are
  obtained from the column and row indices, respectively.  \code{NA}'s
  are not allowed in \code{groups} or \code{blocks};  if \code{y}
  contains \code{NA}'s, corresponding blocks are removed.
  
  For small values of k (methods) or N (data objects), \samp{PageTest} 
  will calculate the exact p-values. 
  For \samp{k, N > 15, Inf}, a normal 
  approximation is returned. Only one of these values will be returned.
}
\value{
  A list with class \code{"htest"} containing the following components:
    \item{statistic}{the L-statistic with names attribute  \dQuote{L}.}
  \item{p.value}{the p-value of the test.}
  \item{method}{the character string \code{"Page test for ordered alternatives"}.}
  \item{data.name}{a character string giving the names of the data.}
}
\seealso{
  \code{\link{friedman.test}}
}
\references{
Page, E. (1963): Ordered hypotheses for multiple treatments: A significance test for linear ranks. \emph{Journal of the American Statistical Association}, 58, 216-230.

Siegel, S. & Castellan, N. J. Jr. (1988): \emph{Nonparametric
 statistics for the behavioral sciences}. Boston, MA: McGraw-Hill.
 
Wellek, S. (1989): Computing exact p-values in Page's nonparametric test against trend. \emph{Biometrie und Informatik in Medizin und Biologie 20}, 163-170 
}
\note{Special thanks to Prof. S. Wellek for porting old GAUSS code to R. }

\author{Stefan Wellek <stefan.wellek@zi-mannheim.de> (exact p-values), Andri Signorell <andri@signorell.net> (interface) (strongly based on R-Core code) } 

\examples{
 # Craig's data from Siegel & Castellan, p 186
 soa.mat <- matrix(c(.797,.873,.888,.923,.942,.956,
  .794,.772,.908,.982,.946,.913,
  .838,.801,.853,.951,.883,.837,
  .815,.801,.747,.859,.887,.902), nrow=4, byrow=TRUE)
 PageTest(soa.mat)
 

# Duller, pg. 236 
pers <- matrix(c(
1, 72, 72, 71.5, 69, 70, 69.5, 68, 68, 67, 68,
2, 83, 81, 81, 82, 82.5, 81, 79, 80.5, 80, 81,
3, 95, 92, 91.5, 89, 89, 90.5, 89, 89, 88, 88,
4, 71, 72, 71, 70.5, 70, 71, 71, 70, 69.5, 69,
5, 79, 79, 78.5, 77, 77.5, 78, 77.5, 76, 76.5, 76,
6, 80, 78.5, 78, 77, 77.5, 77, 76, 76, 75.5, 75.5
), nrow=6, byrow=TRUE) 

colnames(pers) <- c("person", paste("week",1:10))

# Alternative: week10 < week9 < week8 ... 
PageTest(pers[, 11:2])


# Sachs, pg. 464

pers <- matrix(c(
  3,2,1,4,
  4,2,3,1,
  4,1,2,3,
  4,2,3,1,
  3,2,1,4,
  4,1,2,3,
  4,3,2,1,
  3,1,2,4,
  3,1,4,2), 
  nrow=9, byrow=TRUE, dimnames=list(1:9, LETTERS[1:4]))  

# Alternative: B < C < D < A
PageTest(pers[, c("B","C","D","A")])


# long shape and formula interface
plng <- data.frame(expand.grid(1:9, c("B","C","D","A")), 
                   as.vector(pers[, c("B","C","D","A")]))
colnames(plng) <- c("block","group","x")

PageTest(plng$x, plng$group, plng$block)

PageTest(x ~ group | block, data = plng)



score <- matrix(c(
  3,4,6,9,
  4,3,7,8,
  3,4,4,6,
  5,6,8,9,
  4,4,9,9,
  6,7,11,10
  ), nrow=6, byrow=TRUE) 

PageTest(score)
}

\keyword{htest}
