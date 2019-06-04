\name{LehmacherTest}
\alias{LehmacherTest}
\alias{print.mtest}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Lehmacher's Test for Marginal Homogenity
%%  ~~function to do ... ~~
}
\description{Performs Lehmacher's chi-squared test for marginal homogenity in a symmetric two-dimensional contingency table.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
LehmacherTest(x, y = NULL)

\method{print}{mtest}(x, digits = 4L, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{either a two-dimensional contingency table in matrix form, or a factor object.
%%     ~~Describe \code{x} here~~
}
  \item{y}{a factor object; ignored if x is a matrix.
%%     ~~Describe \code{y} here~~
}
\item{digits}{a non-null value for digits specifies the minimum number of significant digits to be printed in values. See details in \code{\link{print.default}}.}
\item{\dots}{further arguments to be passed to or from other methods. They are ignored in this function.}
}
\details{The null is that the probabilities of being classified into cells [i,j] and [j,i] are the same.

If x is a matrix, it is taken as a two-dimensional contingency table, and hence its entries should be nonnegative integers. Otherwise, both x and y must be vectors or factors of the same length. Incomplete cases are removed, vectors are coerced into factors, and the contingency table is computed from these.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  A list with class \code{"mtest"} containing the following components:
  \item{statistic}{a vector with the value of the test statistics.}
  \item{parameter}{the degrees of freedom, which is always 1 in LehmacherTest.}
  \item{p.value}{a vector with the p-values of the single tests.}
  \item{p.value.corr}{a vector with the "hochberg" adjusted p-values of the single tests. (See \code{\link{p.adjust}})}
  \item{method}{a character string indicating what type of test was
    performed.} 
  \item{data.name}{a character string giving the name of the data.}

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Lehmacher, W. (1980) Simultaneous sign tests for marginal homogeneity of square contingency tables
\emph{Biometrical Journal}, Volume 22, Issue 8,  pages 795-798

%% ~put references to the literature/web site here ~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{mcnemar.test}} (resp. BowkerTest for a CxC-matrix), \code{\link{StuartMaxwellTest}}, \code{\link{WoolfTest}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- matrix(c(400,40,20,10, 
              50,300,60,20, 
              10,40,120,5, 
              5,90,50,80), nrow=4, byrow=TRUE)
              
LehmacherTest(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
