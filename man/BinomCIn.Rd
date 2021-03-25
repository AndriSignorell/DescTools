\name{BinomCIn}
\alias{BinomCIn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sample Size for a Given Width of a Binomial Confidence Interval
%%  ~~function to do ... ~~
}
\description{Returns the necessary sample size to achieve a given width of a binomial confidence interval, as calculated by \code{\link{BinomCI}()}. The function uses \code{\link{uniroot}()} to find a numeric solution. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BinomCIn(p = 0.5, width, interval = c(1, 100000), 
         conf.level = 0.95, sides = "two.sided", method = "wilson")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{probability for success, defaults to \code{0.5}.
%%     ~~Describe \code{p} here~~
}
  \item{width}{the width of the confidence interval
%%     ~~Describe \code{width} here~~
}
  \item{interval}{a vector containing the end-points of the interval to be searched for the root. The defaults are set to \code{c(1, 100000)}.
%%     ~~Describe \code{interval} here~~
}
  \item{conf.level}{ confidence level, defaults to \code{0.95}. }
  \item{sides}{a character string specifying the side of the confidence interval, must be one of \code{"two.sided"} (default),
\code{"left"} or \code{"right"}. You can specify just the initial letter. \code{"left"} would be analogue to a hypothesis of
\code{"greater"} in a \code{t.test}.}

  \item{method}{ character string specifing which method to use; this can be one out of:
  \code{"wald"}, \code{"wilson"}, \code{"wilsoncc"}, \code{"agresti-coull"}, \code{"jeffreys"},
  \code{"modified wilson"}, \code{"modified jeffreys"}, \code{"clopper-pearson"},
  \code{"arcsine"}, \code{"logit"}, \code{"witting"} or \code{"pratt"}. Defaults to \code{"wilson"}.
  Abbreviation of method are accepted. See details in \code{\link{BinomCI}()}. }
}
\details{The required sample sizes for a specific width of confidence interval depends on the proportion in the population. This value might be unknown right from the start when a study is planned.
In such cases the sample size needed for a given level of accuracy can be estimated using the worst case percentage which is p=50\%. When a better estimate is available you can you can use it to get a smaller interval.
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
}

\seealso{\code{\link{BinomCI}()}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
BinomCIn(p=0.1, width=0.05, method="pratt")
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
