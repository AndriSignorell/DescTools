\name{LinScale}
\alias{LinScale}

\title{Linear Scaling
}
\description{This will scale the numeric vector \code{x} linearly from an old scale between \code{low} and \code{high} to a new one between \code{newlow} and \code{newhigh}.
}
\usage{
LinScale(x, low = NULL, high = NULL, newlow = 0, newhigh = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric matrix(like object).

}
  \item{low}{numeric. The minimum value of the scale, defaults to min(x).
  This is calculated columnwise by default; defined \code{low} or \code{high} arguments
  will be recycled if necessary.
}
  \item{high}{numeric. The maximum value of the scale, defaults to max(x).
  This is calculated columnwise by default; when a maxval is entered, it will be recycled.
}
  \item{newlow}{numeric. The minimum value of the new scale, defaults to 0, resulting in a 0-1 scale for x. \code{newlow} is recycled if necessary.
}
  \item{newhigh}{numeric. The maximum value of the scale, defaults to 1.
  \code{newhigh} is recycled if necessary.
}
}
\details{Hmm, hardly worth coding...
}
\value{The centered and scaled matrix. The numeric centering and scalings used (if any) are returned as attributes "\code{scaled:center}" and "\code{scaled:scale}"
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{ \code{\link{scale}},  \code{\link{RobScale}}, \code{\link{sweep}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# transform the temperature from Celsius to Fahrenheit
LinScale(d.pizza[1:20, "temperature"], 0, 100, -17.8, 37.8 )

# and the price from Dollar to Euro
LinScale(d.pizza[1:20, "price"], 0, 1, 0, 0.76)

# together
LinScale(d.pizza[1:20, c("temperature", "price")],
  0, c(100, 1), c(-17.8, 0), c(37.8, 0.76) )


\dontrun{
par(mfrow=c(3,1), mar=c(0,5,0,3), oma=c(5,0,5,0))
plot(LinScale(d.frm[,1]), ylim=c(-2,2), xaxt="n", ylab="LinScale")
plot(RobScale(d.frm[,1]), ylim=c(-2,2), xaxt="n", ylab="RobScale")
plot(scale(d.frm[,1]), ylim=c(-2,2), ylab="scale")
title("Compare scales", outer = TRUE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }
