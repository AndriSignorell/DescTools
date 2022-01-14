\name{CochranArmitageTest}
\alias{CochranArmitageTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cochran-Armitage Test for Trend
%%  ~~function to do ... ~~
}
\description{Perform a Cochran Armitage test for trend in binomial proportions across the levels of a single variable. This test is appropriate only when one variable has two levels and the other variable is ordinal. The two-level variable represents the response, and the other represents an explanatory variable with ordered levels.
The null hypothesis is the hypothesis of no trend, which means that the binomial proportion is the same for all levels of the explanatory variable.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CochranArmitageTest(x, alternative = c("two.sided", "one.sided"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a frequency table or a matrix.
%%     ~~Describe \code{x} here~~
}
  \item{alternative}{a character string specifying the alternative hypothesis, must be one of \code{"two.sided"} (default), \code{"one.sided"}. You can specify just the initial letter.
%%     ~~Describe \code{alternative} here~~
}
}

\value{A list of class \code{htest}, containing the following components:
  \item{statistic}{ the z-statistic of the test.}
  \item{parameter}{ the dimension of the table.}
  \item{p.value}{   the p-value for the test.}
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{method}{the character string \dQuote{Cochran-Armitage test for trend}.}
  \item{data.name}{a character string giving the names of the data.}

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley & Sons

%% ~put references to the literature/web site here ~
}
\author{Andri Signorell <andri@signorell.net> strongly based on code from
Eric Lecoutre <lecoutre@stat.ucl.ac.be>\cr
\url{https://stat.ethz.ch/pipermail/r-help/2005-July/076371.html}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{prop.trend.test}}

\url{https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.5/procstat/procstat_freq_details76.htm}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# http://www.lexjansen.com/pharmasug/2007/sp/sp05.pdf, pp. 4
dose <- matrix(c(10,9,10,7, 0,1,0,3), byrow=TRUE, nrow=2, dimnames=list(resp=0:1, dose=0:3))
Desc(dose)

CochranArmitageTest(dose)
CochranArmitageTest(dose, alternative="one.sided")


# not exactly the same as in package coin:
# independence_test(tumor ~ dose, data = lungtumor, teststat = "quad")
lungtumor <- data.frame(dose = rep(c(0, 1, 2), c(40, 50, 48)),
                        tumor = c(rep(c(0, 1), c(38, 2)),
                                  rep(c(0, 1), c(43, 7)),
                                  rep(c(0, 1), c(33, 15))))
tab <- table(lungtumor$dose, lungtumor$tumor)
CochranArmitageTest(tab)

# but similar to
prop.trend.test(tab[,1], apply(tab,1, sum))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
