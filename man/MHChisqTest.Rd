\name{MHChisqTest}
\alias{MHChisqTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Mantel-Haenszel Chi-Square Test
%%  ~~function to do ... ~~
}
\description{The Mantel-Haenszel chi-square statistic tests the alternative hypothesis that there is a linear association between the row variable and the column variable. Both variables must lie on an ordinal scale.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MHChisqTest(x, srow = 1:nrow(x), scol = 1:ncol(x))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a frequency table or a matrix. 
%%     ~~Describe \code{tab} here~~
}
  \item{srow}{scores for the row variable, defaults to 1:nrow.
%%     ~~Describe \code{srow} here~~
}
  \item{scol}{scores for the colummn variable, defaults to 1:ncol.
%%     ~~Describe \code{scol} here~~
}
}
\details{The statistic is computed as \eqn{ Q_{MH} = (n-1) \cdot r^2}, where \eqn{r^2} is the Pearson correlation between the row variable and the column variable. The Mantel-Haenszel chi-square statistic use the scores specified by srow and scol.
Under the null hypothesis of no association, \eqn{Q_{MH}} has an asymptotic chi-square distribution with one degree of freedom. 

%%  ~~ If necessary, more details than the description above ~~
}
\value{
  A list with class \code{"htest"} containing the following
  components:
  \item{statistic}{the value the Mantel-Haenszel chi-squared test statistic.}
  \item{parameter}{the degrees of freedom of the approximate
    chi-squared distribution of the test statistic.}
  \item{p.value}{the p-value for the test.}
  \item{method}{a character string indicating the type of test
    performed.}
  \item{data.name}{a character string giving the name(s) of the data.}
}
\references{
Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley & Sons, pp 86 ff.
%% ~put references to the literature/web site here ~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{chisq.test}}, 
for calculating correlation of a table: \code{\link[boot]{corr}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## A r x c table  Agresti (2002, p. 57) Job Satisfaction
Job <- matrix(c(1,2,1,0, 3,3,6,1, 10,10,14,9, 6,7,12,11), 4, 4,
              dimnames = list(income = c("< 15k", "15-25k", "25-40k", "> 40k"),
                              satisfaction = c("VeryD", "LittleD", "ModerateS", "VeryS"))
       )

MHChisqTest(Job, srow=c(7.5,20,32.5,60))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }% __ONLY ONE__ keyword per line
