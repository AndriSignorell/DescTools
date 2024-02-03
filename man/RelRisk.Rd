\name{RelRisk}
\alias{RelRisk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Relative Risk
%%  ~~function to do ... ~~
}
\description{Computes the relative risk and its confidence intervals. 
Confidence intervals are calculated using normal approximation (\code{"wald"}), (\code{"score"}) or by 
using odds ratio (\code{"use.or"}) 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RelRisk(x, y = NULL, conf.level = NA, 
        method = c("score", "wald", "use.or"), delta = 0.5, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector or a 2x2 numeric matrix, resp. table.
}
  \item{y}{\code{NULL} (default) or a vector with compatible dimensions to \code{x}. If y is provided, \code{table(x, y, ...)} will be calculated.
}
  \item{conf.level}{confidence level. Default is \code{NA}, meaning no confidence intervals will be
  reported.
%%     ~~Describe \code{conf.level} here~~
}
  \item{method}{method for calculating the relative risk and the confidence intervals. Can be one out of
    \code{"score"}, \code{"wald"}, \code{"use.or"}. Default is \code{"score"}.
%%     ~~Describe \code{method} here~~
}
  \item{delta}{small constant to be added to the numerator for calculating the log risk ratio (Wald method). Usual choice is 0.5 although there does not seem to be any 
  theory behind this. (Dewey, M. 2006)
%%     ~~Describe \code{delta} here~~
}
  \item{\dots}{further arguments are passed to the function \code{\link{table}}, allowing i.e. to set \code{useNA}.
  }

}
\details{
  Best is to always put the outcome variable (disease yes/no) in the columns and the exposure variable in the rows. In other words, put the dependent variable – the one that describes the problem under study – in the columns. And put the independent variable – the factor assumed to cause the problem – in the rows. (Gerritsen, 2010)
  
  According to this, the function expects the following table structure:
  \preformatted{
                     diseased=1   diseased=0
    exposed=1 (ref)     n00          n01
    exposed=0           n10          n11	
  }

The relative risk is then calculated as:

\preformatted{       (exposed & diseased) / exposed
rr = ----------------------------------
     (unexposed & diseased) / unexposed
}

  If the table to be used is not in the
  required shape, use the function \code{\link{Rev}()} and/or \code{\link{t}()} to reverse rows, columns, or both, resp. to transpose the table. 
}
\value{
If \code{conf.level} is not \code{NA} then the result will be
  a vector with 3 elements for estimate, lower confidence intervall and upper for the upper one.
  Else the relative risk will be reported as a single value. 

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
  Rothman, K. J. and Greenland, S. (1998) \emph{Modern Epidemiology}.
  Lippincott-Raven Publishers

  Rothman, K. J. (2002) \emph{Epidemiology: An Introduction}. Oxford
  University Press
  
  Jewell, N. P. (2004) \emph{Statistics for Epidemiology}. 1st Edition,
  2004, Chapman & Hall, pp. 73-81

  Selvin, S. (1998) \emph{Modern Applied Biostatistical Methods Using
  S-Plus}. 1st Edition, Oxford University Press
  
  Gerritsen, A (2010) \url{https://www.theanalysisfactor.com/cross-tabulation-in-cohort-and-case-control-studies/}

} 

\author{Andri Signorell <andri@signorell.net>, based on code of Yongyi Min and Michael Dewey
%%  ~~who you are~~
}

\seealso{ \code{\link{OddsRatio}} 
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
m <- matrix(c(78,50,1422,950), 
            nrow=2, 
            dimnames = list(water=c("cont", "clean"), 
                            diarrhea=c("yes", "no")))

RelRisk(m, conf.level = 0.95)


mm <- cbind(c(9,20),c(41,29))
mm
 
RelRisk(t(mm), conf.level=0.95)
RelRisk(t(mm), conf.level=0.95, method="wald")
RelRisk(t(mm), conf.level=0.95, method="use.or")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }

