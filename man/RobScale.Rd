\name{RobScale}
\alias{RobScale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Robust Scaling With Median and Mad
%%  ~~function to do ... ~~
}
\description{\code{RobScale} is a wrapper function for robust standardization, using \code{\link{median}}
and \code{\link{mad}} instead of \code{\link{mean}} and \code{\link{sd}}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RobScale(x, center = TRUE, scale = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric matrix(like object).

%%     ~~Describe \code{x} here~~
}
  \item{center}{a logical value defining whether x should be centered by the median. 
   Centering is done by subtracting the column medians (omitting NAs) of x from their corresponding columns.
   If center is FALSE, no centering is done.
%%     ~~Describe \code{center} here~~
}
  \item{scale}{a logical value defining whether x should be scaled by the mad.
  Scaling is done by dividing the (centered) columns of x by their mad. 
  If scale is FALSE, no scaling is done. 

%%     ~~Describe \code{scale} here~~
}
}
\value{
the centered, scaled matrix. 
The numeric centering and scalings used (if any) are returned as attributes "scaled:center" and "scaled:scale" 

}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{scale}, \code{sweep}
}
\examples{
x <- d.pizza$temperature
plot(x=seq_along(x), y=RobScale(x), xlim=c(0,100))
points(x=seq_along(x), y=scale(x), col="red" )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }
\keyword{ robust }

