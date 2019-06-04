\name{DunnettTest}
\alias{DunnettTest}
\alias{DunnettTest.default}
\alias{DunnettTest.formula}

\title{Dunnett's Test for Comparing Several Treatments With a Control}
\description{
  Performs Dunnett's test for comparing several treatments with a control.
}
\usage{
DunnettTest(x, \dots)

\method{DunnettTest}{default}(x, g, control = NULL, conf.level = 0.95, \dots)

\method{DunnettTest}{formula}(formula, data, subset, na.action, \dots)

}
\arguments{
  \item{x}{a numeric vector of data values, or a list of numeric data
    vectors.}
  \item{g}{a vector or factor object giving the group for the
    corresponding elements of \code{x}.  Ignored if \code{x} is a
    list.}
  \item{control}{the level of the control group against which the others should be tested. If there are multiple levels the calculation will be performed for every one. } 
    \item{conf.level}{confidence level of the interval.
%%     ~~Describe \code{conf.level} here~~
}

  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs}
    gives the data values and \code{rhs} the corresponding groups.}
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
  \code{DunnettTest} does the post hoc pairwise multiple comparisons procedure.
    
  If \code{x} is a list, its elements are taken as the samples to be
  compared, and hence have to be numeric data vectors.  In this case,
  \code{g} is ignored, and one can simply use \code{DunnettTest(x)}
  to perform the test.  If the samples are not yet contained in a
  list, use \code{DunnettTest(list(x, ...))}.
  
  Otherwise, \code{x} must be a numeric data vector, and \code{g} must
  be a vector or factor object of the same length as \code{x} giving
  the group for the corresponding elements of \code{x}.
}
\value{A list of class \code{c("PostHocTest")}, containing one matrix named after the control with columns \code{diff} giving the difference in the observed means, \code{lwr.ci} giving the lower end point of the interval, \code{upr.ci} giving the upper end point and \code{pval} giving the p-value after adjustment for the multiple comparisons.

There are print and plot methods for class \code{"PostHocTest"}. The plot method does not accept \code{xlab}, \code{ylab} or \code{main} arguments and creates its own values for each plot.
}

\author{Andri Signorell <andri@signorell.net>, the interface is based on R-Core code}

\references{
Dunnett C. W. (1955) A multiple comparison procedure for comparing several treatments with a control, \emph{Journal of the American Statistical Association}, 50:1096-1121.

}
\seealso{
  \code{\link{PostHocTest}}
}
\examples{
## Hollander & Wolfe (1973), 116.
## Mucociliary efficiency from the rate of removal of dust in normal
##  subjects, subjects with obstructive airway disease, and subjects
##  with asbestosis.
x <- c(2.9, 3.0, 2.5, 2.6, 3.2) # normal subjects
y <- c(3.8, 2.7, 4.0, 2.4)      # with obstructive airway disease
z <- c(2.8, 3.4, 3.7, 2.2, 2.0) # with asbestosis

DunnettTest(list(x, y, z))

## Equivalently,
x <- c(x, y, z)
g <- factor(rep(1:3, c(5, 4, 5)),
            labels = c("Normal subjects",
                       "Subjects with obstructive airway disease",
                       "Subjects with asbestosis"))

DunnettTest(x, g)

## Formula interface
boxplot(Ozone ~ Month, data = airquality)
DunnettTest(Ozone ~ Month, data = airquality)

DunnettTest(Ozone ~ Month, data = airquality, control="8", conf.level=0.9)
}
\keyword{htest}
