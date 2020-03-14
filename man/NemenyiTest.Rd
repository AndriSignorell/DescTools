\name{NemenyiTest}
\alias{NemenyiTest}
\alias{NemenyiTest.default}
\alias{NemenyiTest.formula}
%- Also NEED an '\alias' for EACH other topic documented here.


\title{Nemenyi Test
%%  ~~function to do ... ~~
}
\description{Performs Nemenyi's test of multiple comparisons.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
NemenyiTest(x, ...)

\method{NemenyiTest}{default}(x, g, dist = c("tukey", "chisq"), out.list = TRUE, ...)

\method{NemenyiTest}{formula}(formula, data, subset, na.action, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector of data values, or a list of numeric data
    vectors.}
  \item{g}{a vector or factor object giving the group for the
    corresponding elements of \code{x}.  Ignored if \code{x} is a
    list.}

  \item{dist}{the distribution used for the test. Can be \code{tukey} (default) or \code{chisq}.}
  \item{out.list}{logical, defining if the output should be organized in listform.}

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
%% ToDo!! Tell when to use this test. References needed!
Nemenyi proposed a test based on rank sums and the application of the family-wise error method to control Type I error inflation, if multiple comparisons are done. The Tukey and Kramer approach uses mean rank sums and can be employed for equally as well as unequally sized samples without ties.

%%  ~~ If necessary, more details than the description above ~~
}

\value{
A list of class \code{htest}, containing the following components:
  \item{statistic}{ Nemenyi test}
  \item{p.value}{   the p-value for the test}
  \item{null.value}{is the value of the median specified by the null hypothesis. This
                    equals the input argument \code{mu}. }
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{method}{  the type of test applied}
  \item{data.name}{a character string giving the names of the data.}
}

\references{
Nemenyi, P. B. (1963) \emph{Distribution-Free Multiple Comparisons} New York, State University of New York, Downstate Medical Center

Hollander, M., Wolfe, D.A. (1999) \emph{Nonparametric Statistical Methods} New York, Wiley, pp. 787

Friedman, M. (1937) The use of ranks to avoid the assumption of normality implicit in the analysis of variance  \emph{Journal of the American Statistical Association}, 32:675-701

Friedman, M. (1940) A comparison of alternative tests of significance for the problem of m rankings \emph{Annals of Mathematical Statistics}, 11:86-92

}
\author{
Andri Signorell <andri@signorell.net>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{DunnTest}}, \code{\link{ConoverTest}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Hollander & Wolfe (1973), 116.
## Mucociliary efficiency from the rate of removal of dust in normal
##  subjects, subjects with obstructive airway disease, and subjects
##  with asbestosis.
x <- c(2.9, 3.0, 2.5, 2.6, 3.2) # normal subjects
y <- c(3.8, 2.7, 4.0, 2.4)      # with obstructive airway disease
z <- c(2.8, 3.4, 3.7, 2.2, 2.0) # with asbestosis

NemenyiTest(list(x, y, z))

## Equivalently,
x <- c(x, y, z)
g <- factor(rep(1:3, c(5, 4, 5)),
            labels = c("Normal subjects",
                       "Subjects with obstructive airway disease",
                       "Subjects with asbestosis"))

NemenyiTest(x, g)

## Formula interface.
boxplot(Ozone ~ Month, data = airquality)
NemenyiTest(Ozone ~ Month, data = airquality)

# Hedderich & Sachs, 2012, p. 555
d.frm <- data.frame(x=c(28,30,33,35,38,41, 36,39,40,43,45,50, 44,45,47,49,53,54),
                    g=c(rep(LETTERS[1:3], each=6)), stringsAsFactors=TRUE)

NemenyiTest(x~g, d.frm)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
