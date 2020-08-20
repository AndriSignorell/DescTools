\name{MADCI}
\alias{MADCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Confidence Intervals for Median Absolute Deviations
%%  ~~function to do ... ~~
}
\description{A function for the median absolute deviation is included in base R, \code{\link{mad}}, but there's no function for calculating confidence intervals. Arachchige/Prendergast introduce interval estimators of the MAD to make reliable inferences for dispersion for a single population and ratios and differences of MADs for comparing two populations. 
}
\usage{
MADCI(x, y = NULL, two.samp.diff = TRUE, gld.est = "TM", 
      conf.level = 0.95, sides = c("two.sided","left","right"), 
      na.rm = FALSE, ...)      
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{x}{a (non-empty) numeric vector of data values.
}
  \item{y}{a second (non-empty) numeric vector of data values.
%%     ~~Describe \code{y} here~~
}
 \item{two.samp.diff}{logical, defining if the confidence intervals for a difference of x and y should be calculated.
%%     ~~Describe \code{two.samp.diff} here~~
}
  \item{gld.est}{type of gld estimator.
%%     ~~Describe \code{gld.est} here~~
}
  \item{conf.level}{confidence level of the interval
}
\item{sides}{a character string specifying the side of the confidence interval, must be one of \code{"two.sided"} (default), \code{"left"} or \code{"right"}. You can specify just the initial letter. \code{"left"} would be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.}

  \item{na.rm}{logical. Should missing values be removed? Defaults to \code{FALSE}.
}
\item{\dots}{further arguments, not used here}
 }
\value{a numeric vector with 3 elements:
  \item{mad}{median absolute deviation}
  \item{lwr.ci}{lower bound of the confidence interval}
  \item{upr.ci}{upper bound of the confidence interval}
}

\references{
Arachchige Chandima N. P. G., Prendergast Luke A. (2019) Confidence intervals for median absolute deviations, arXiv:1910.00229 [math.ST]
%% ~put references to the literature/web site here ~
}
\author{Arachchige Chandima N. P. G., Prendergast Luke A., 
 Andri Signorell <andri@signorell.net> (only interface)
%%  ~~who you are~~
}

\seealso{\code{\link{mad}}, \code{\link{MAD}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- rlnorm(100)
y <- rlnorm(200, meanlog=1.2)

MADCI(x)                           # single sample

MADCI(x, y)                        # two sample difference
MADCI(x, y, two.samp.diff = FALSE) # two sample squared ratio 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
