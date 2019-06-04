\name{PlotQQ}
\alias{PlotQQ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{QQ-Plot for Any Distribution
%%  ~~function to do ... ~~
}
\description{Create a QQ-plot for a variable of any distribution.
The assumed underlying distribution can be defined as a function of f(p), including all required parameters. Confidence bands are provided by default.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotQQ(x, qdist=qnorm, main = NULL, xlab = NULL, ylab = NULL, datax = FALSE, add = FALSE,
       args.qqline = NULL, conf.level = 0.95, args.cband = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the data sample
%%     ~~Describe \code{x} here~~
}
  \item{qdist}{the quantile function of the assumed distribution. Can either be given as simple function name or defined as own function using the required arguments. Default is \code{qnorm()}. See examples.

%%     ~~Describe \code{qdist} here~~
}
  \item{main}{the main title for the plot. This will be "Q-Q-Plot" by default
%%     ~~Describe \code{main} here~~
}
  \item{xlab}{the xlab for the plot
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{the ylab for the plot
%%     ~~Describe \code{ylab} here~~
}
\item{datax}{logical. Should data values be on the x-axis? Default is \code{FALSE}.}
\item{add}{logical specifying if the points should be added to an already existing plot; defaults to \code{FALSE}.}
  \item{args.qqline}{arguments for the qqline. This will be estimated
   as a line through the 25\% and 75\% quantiles by default, which is the same procedure as \code{\link{qqline}()} does for normal
   distribution (instead of set it to {abline(a = 0, b = 1))}. The quantiles can however be overwritten by setting the argument \code{probs} to some user defined values. Also the method for calculating the quantiles can be defined (default is 7, see \code{\link{quantile}}). The line defaults are set to \code{col = par("fg")}, \code{lwd = par("lwd")} and \code{lty = par("lty")}.
   No line will be plotted if \code{args.qqline} is set to \code{NA}.
}
  \item{conf.level}{confidence level for the confidence interval. Set this to \code{NA}, if no confidence band should be plotted.
  Default is \code{0.95}. The confidence intervals are calculated pointwise method based on a Kolmogorov-Smirnov distribution.
%%     ~~Describe \code{type} here~~
}
  \item{args.cband}{list of arguments for the confidence band, such as color or border (see \code{\link{DrawBand}}).
%%     ~~Describe \code{type} here~~
}

  \item{\dots}{the dots are passed to the plot function.
%%     ~~Describe \code{\dots} here~~
}
}
\details{The function generates a sequence of points between 0 and 1 and transforms those
into quantiles by means of the defined assumed distribution.
%%  ~~ If necessary, more details than the description above ~~
}
\note{ The code is inspired by the tip 10.22 "Creating other Quantile-Quantile plots" from R Cookbook and based on R-Core code from the function \code{qqline}. The calculation of confidence bands are rewritten based on an algorithm published in the package \code{BoutrosLab.plotting.general}.}
\references{Teetor, P. (2011) \emph{R Cookbook}. O'Reilly, pp. 254-255.}

\author{Andri Signorell <andri@signorell.net>, Ying Wu <Ying.Wu@stevens.edu>
%%  ~~who you are~~
}

\seealso{\code{\link{qqnorm}}, \code{\link{qqline}}, \code{\link{qqplot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
y <- rexp(100, 1/10)
PlotQQ(y, function(p) qexp(p, rate=1/10))

w <- rweibull(100, shape=2)
PlotQQ(w, qdist = function(p) qweibull(p, shape=4))

z <- rchisq(100, df=5)
PlotQQ(z, function(p) qchisq(p, df=5),
       args.qqline=list(col=2, probs=c(0.1, 0.6)),
       main=expression("Q-Q plot for" ~~ {chi^2}[nu == 3]))
abline(0,1)

# add 5 random sets
for(i in 1:5){
  z <- rchisq(100, df=5)
  PlotQQ(z, function(p) qchisq(p, df=5), add=TRUE, args.qqline = NA,
         col="grey", lty="dotted")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}
\keyword{ hplot }

