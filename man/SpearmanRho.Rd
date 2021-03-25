\name{SpearmanRho}
\alias{SpearmanRho}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Spearman Rank Correlation
%%  ~~function to do ... ~~
}
\description{Calculate Spearman correlation coefficient and its confidence interval. In addition to the base R function \code{\link{cor}()}, frequency tables are also accepted as arguments (i.e. actually weights are used).

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SpearmanRho(x, y = NULL, use = c("everything", "all.obs", "complete.obs", 
            "na.or.complete","pairwise.complete.obs"), 
            conf.level = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector, an ordered factor, matrix or data frame. An ordered factor will be coerced to numeric.
%%     ~~Describe \code{x} here~~
}
  \item{y}{\code{NULL} (default) or a vector, an ordered factor, matrix or data frame with compatible dimensions to x. 
  An ordered factor will be coerced to numeric.
%%     ~~Describe \code{y} here~~
}
\item{use}{
an optional character string giving a method for computing covariances in the presence of missing values. 
This must be (an abbreviation of) one of the strings \code{"everything"}, \code{"all.obs"}, \code{"complete.obs"}, 
\code{"na.or.complete"}, or \code{"pairwise.complete.obs"}.
}
  \item{conf.level}{confidence level of the interval. If set to \code{NA} (which is the default) no confidence interval will be calculated. 
%%     ~~Describe \code{conf.level} here~~
}
}
\details{The function calculates Spearman's rho statistic by means of \code{cor(..., method="spearman")} when two variables \code{x} and \code{y} are supplied. If a frequency table is provided an implementation based on SAS documentation is used.\cr
The confidence intervals are calculated via z-Transformation.\cr

 
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Either a single numeric value, if no confidence interval is required, \cr
or a vector with 3 elements for estimate, lower and upper confidence intervall. 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Conover W. J. (1999) \emph{Practical Nonparametric Statistics (3rd edition)}. Wiley
%% ~put references to the literature/web site here ~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{cor}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
pain <- as.table(matrix(c(26,  6, 26, 7, 23, 
                           9, 18, 14, 9, 23), 
                           ncol=5, byrow=TRUE, 
        dimnames=list(adverse=c("no", "yes"), dose=1:5)))

SpearmanRho(pain)

SpearmanRho(pain, conf.level=0.95)
  
# must be the same as
with(Untable(pain), 
     SpearmanRho(adverse, dose, conf.level=0.95))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivar }
