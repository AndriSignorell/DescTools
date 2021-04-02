\name{QuantileCI}
\alias{QuantileCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Confidence Interval for Any Quantile
%%  ~~function to do ... ~~
}
\description{Calculates the confidence interval for any quantile. Although bootstrapping might be a good approach for getting senisble confidence intervals there's sometimes need to have a nonparameteric alternative. This function offers one.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
QuantileCI(x, probs=seq(0, 1, .25), conf.level = 0.95, 
           sides = c("two.sided", "left", "right"),
           na.rm = FALSE, method = c("exact", "boot"), R = 999)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.
}
\item{probs}{numeric vector of probabilities with values in \emph{[0,1]}. (Values up to \code{2e-14} outside that range are accepted and moved to the nearby endpoint.)}
  \item{conf.level}{confidence level of the interval
}
\item{sides}{a character string specifying the side of the confidence interval, must be one of \code{"two.sided"} (default), \code{"left"} or \code{"right"} (abbreviations allowed). \cr\code{"left"} would be analogue to a \code{"greater"} hypothesis in a \code{t.test}.}

  \item{na.rm}{logical. Should missing values be removed? Defaults to \code{FALSE}.
}
  \item{method}{defining the type of interval that should be calculated (one out of \code{"exact"}, \code{"boot"}). Default is \code{"exact"}. See Details.}

\item{R}{The number of bootstrap replicates. Usually this will be a single positive integer. See
\code{\link{boot.ci}} for details.
}
}
\details{
The \code{"exact"} method corresponds to the way the confidence interval for the median is calculated in SAS. \cr
The boot confidence interval type is calculated by means of \code{\link{boot.ci}} with default type \code{"basic"}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{if probs was of length 1
a numeric vector with 3 elements:
  \item{est}{est}
  \item{lwr.ci}{lower bound of the confidence interval}
  \item{upr.ci}{upper bound of the confidence interval}
  
or, if probs was a vector, a matrix with 3 columns consisting of estimate, lower ci, upper ci
  \code{est, lwr.ci, upr.ci}


%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net> based on code of W Huber on StackExchange
%%  ~~who you are~~
}
\seealso{ \code{\link{Quantile}},  \code{\link{quantile}},  \code{\link{MedianCI}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
QuantileCI(d.pizza$price, probs=0.25, na.rm=TRUE)

QuantileCI(d.pizza$price, na.rm=TRUE)
QuantileCI(d.pizza$price, conf.level=0.99, na.rm=TRUE)

# multiple probs
QuantileCI(1:100, method="exact" , probs = c(0.25, 0.75, .80, 0.95))
QuantileCI(1:100, method="boot" , probs = c(0.25, 0.75, .80, 0.95))
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
