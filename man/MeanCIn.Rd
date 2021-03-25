\name{MeanCIn}
\alias{MeanCIn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sample Size for a Given Width of a Confidence Interval for a Mean
%%  ~~function to do ... ~~
}
\description{Returns the required sample size to obtain a given width of a confidence interval for the sample mean. The function uses \code{\link{uniroot}()} to find a numeric solution. The t distribution is used.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MeanCIn(ci, sd, interval = c(2, 100000), conf.level = 0.95, 
        norm = FALSE, tol = .Machine$double.eps^0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ci}{the left and right bound of the interval, which is presumed to be symmetric.
%%     ~~Describe \code{ci} here~~
}
  \item{sd}{the standard deviation of the sample.
%%     ~~Describe \code{sd} here~~
}
  \item{interval}{the interval for the sample size to be searched into, (default is c(2, 100000)).
%%     ~~Describe \code{interval} here~~
}
  \item{conf.level}{ confidence level, defaults to \code{0.95}. }

  \item{norm}{logical, determining if the t- or normaldistribution should be used.
%%     ~~Describe \code{norm} here~~
}
  \item{tol}{the desired accuracy (convergence tolerance).
%%     ~~Describe \code{tol} here~~
}
}
\details{The required sample sizes for a specific width of confidence interval for the mean depends recursively on the sample size, as the samplesize defines the degrees of freedom in the t-distribution. Although in most practical cases it will be sufficient to use the normal distribution, we might be interested in exact results.
%%  ~~ If necessary, more details than the description above ~~
}
\value{a numeric value 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{BinomCIn}()}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
MeanCIn(ci=c(25, 27), sd=5) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{ confidence interval }
% \concept{ ~cpt2 }
% Use only one concept per line.
