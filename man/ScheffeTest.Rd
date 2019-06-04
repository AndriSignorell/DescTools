\name{ScheffeTest}
\alias{ScheffeTest}
\alias{ScheffeTest.default}
\alias{ScheffeTest.aov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Scheffe Test for Pairwise and Otherwise Comparisons
%%  ~~function to do ... ~~
}
\description{
Scheffe's method applies to the set of estimates of all possible contrasts among the factor level means, not just the pairwise differences considered by Tukey's method. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ScheffeTest(x, ...) 

\method{ScheffeTest}{aov}(x, which = NULL, contrasts = NULL, 
            conf.level = 0.95, ...)
\method{ScheffeTest}{default}(x, g = NULL, which = NULL, 
            contrasts = NULL, conf.level = 0.95, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{either a fitted model object, usually an \code{\link{aov}} fit, when g is left to \code{NULL} or a response variable to be evalutated by g (which mustn't be \code{NULL} then).
%%     ~~Describe \code{x} here~~
}
  \item{g}{the grouping variable.
%%     ~~Describe \code{g} here~~
}
  \item{which}{character vector listing terms in the fitted model for which the intervals should be calculated. Defaults to all the terms.
%%     ~~Describe \code{which} here~~
}
  \item{contrasts}{a \eqn{r \times c}{r x c} matrix containing the contrasts to be computed, while \code{r} is the number of factor levels and \code{c} the number of contrasts. Each column must contain a full contrast ("sum") adding up to 0. Note that the argument \code{which} must be defined, when non default contrasts are used.
  Default value of \code{contrasts} is \code{NULL}. In this case all pairwise contrasts will be reported.
%%     ~~Describe \code{contrasts} here~~
}
  \item{conf.level}{numeric value between zero and one giving the confidence level to use.
  If this is set to NA, just a matrix with the p-values will be returned.
%%     ~~Describe \code{conf.level} here~~
}
  \item{\dots}{further arguments, currently not used.
%%     ~~Describe \code{\dots} here~~
}
}
%%\details{The function is build to mimic SPSS-Output.
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{A list of classes \code{c("PostHocTest")}, with one component for each term requested in \code{which}. Each component is a matrix with columns \code{diff} giving the difference in the observed means, \code{lwr.ci} giving the lower end point of the interval, \code{upr.ci} giving the upper end point and \code{pval} giving the p-value after adjustment for the multiple comparisons.

There are print and plot methods for class \code{"PostHocTest"}. The plot method does not accept \code{xlab}, \code{ylab} or \code{main} arguments and creates its own values for each plot.
}
\references{
Robert O. Kuehl, Steel R. (2000) \emph{Design of experiments}. Duxbury

Steel R.G.D., Torrie J.H., Dickey, D.A. (1997) \emph{Principles and Procedures of Statistics, A Biometrical Approach}. McGraw-Hill
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{pairwise.t.test}}, \code{\link{TukeyHSD}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
fm1 <- aov(breaks ~ wool + tension, data = warpbreaks)

ScheffeTest(x=fm1)
ScheffeTest(x=fm1, which="tension")

TukeyHSD(fm1)

# some special contrasts
y <- c(7,33,26,27,21,6,14,19,6,11,11,18,14,18,19,14,9,12,6,
       24,7,10,1,10,42,25,8,28,30,22,17,32,28,6,1,15,9,15,
       2,37,13,18,23,1,3,4,6,2)
group <- factor(c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,
       3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6))

r.aov <- aov(y ~ group)

ScheffeTest(r.aov, contrasts=matrix( c(1,-0.5,-0.5,0,0,0,
                                       0,0,0,1,-0.5,-0.5), ncol=2) )

# just p-values:
ScheffeTest(r.aov, conf.level=NA)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
