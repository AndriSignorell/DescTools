\name{BhapkarTest}
\alias{BhapkarTest}
\docType{data}
\title{Bhapkar Marginal Homogeneity Test 
%%   ~~ data name/kind ... ~~
}
\description{Bhapkar (1966) tested marginal homogeneity by exploiting the asymptotic normality of marginal proportion, and this test is also called Bhapkar's test. The idea of constructing test statistic is similar to the
one of generalized McNemar's test statistic used in \code{\link{StuartMaxwellTest}}, and the major difference lies in the calculation of elements in
variance-covariance matrix. 
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{
BhapkarTest(x, y = NULL)
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

\details{Although the Bhapkar and Stuart-Maxwell tests are asymptotically equivalent (Keefe, 1982). Generally,
the Bhapkar (1966) test is a more powerful alternative to the Stuart-Maxwell test. With a large N, both
will produce the same chi-squared value. As the Bhapkar test is more powerful, it is preferred.

%%  ~~ If necessary, more details than the __description__ above ~~
}
\references{
Bhapkar V.P. (1966). A note on the equivalence of two test criteria for hypotheses in categorical data.
\emph{Journal of the American Statistical Association}, 61: 228-235.

Ireland C.T., Ku H.H., and Kullback S. (1969). Symmetry and marginal homogeneity of an r x r
contingency table. \emph{Journal of the American Statistical Association}, 64: 1323-1341.
%%  ~~ possibly secondary sources and usages ~~
}

\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{StuartMaxwellTest}}, \code{\link{mcnemar.test}}, \code{\link{chisq.test}}, \code{\link{MHChisqTest}},
\code{\link{BreslowDayTest}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
# Source: http://www.john-uebersax.com/stat/mcnemar.htm#stuart
mc <- as.table(matrix(c(20,3,0,10,30,5,5,15,40), nrow=3))

BhapkarTest(mc)
}
\keyword{ htest }