\name{PlotProbDist}
\alias{PlotProbDist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Probability Distribution
%%  ~~function to do ... ~~
}
\description{Produce a plot from a probability distribution with shaded areas. This is often needed in theory texts for classes in statistics. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotProbDist(breaks, FUN, 
             blab = NULL, main = "", xlim = NULL, col = NULL, density = 7, 
             alab = LETTERS[1:(length(breaks) - 1)], 
             alab_x = NULL, alab_y = NULL, ylab = "density", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{breaks}{a numeric vector containing the breaks of different areas. The start and end must not be infinity.
%%     ~~Describe \code{breaks} here~~
}
  \item{FUN}{the (typically) distribution function
%%     ~~Describe \code{fn} here~~
}
  \item{blab}{text for labelling the breaks
%%     ~~Describe \code{blab} here~~
}
  \item{main}{main title for the plot
%%     ~~Describe \code{main} here~~
}
  \item{xlim}{the x-limits for the plot
%%     ~~Describe \code{xlim} here~~
}
  \item{col}{the color for the shaded areas
%%     ~~Describe \code{col} here~~
}
  \item{density}{the density for the shaded areas
%%     ~~Describe \code{density} here~~
}
  \item{alab}{the labels for areas
%%     ~~Describe \code{alab} here~~
}
  \item{alab_x}{the x-coord for the area labels
%%     ~~Describe \code{alab_x} here~~
}
  \item{alab_y}{the y-coord for the area labels, if left to default they will be placed in the middle of the plot
%%     ~~Describe \code{alab_y} here~~
}
\item{ylab}{the label for they y-axis}
\item{\dots}{further parameters passed to internally used function \code{\link{curve}()}}
}
\details{The function sets up a two-step plot procedure based on curve() and Shade() with additional labelling for convenience.
%%  ~~ If necessary, more details than the description above ~~
}
\value{nothing returned
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{Shade}}, \code{\link{curve}}, \code{\link{polygon}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# plot t-distribution
PlotProbDist(breaks=c(-6, -2.3, 1.5, 6), 
             function(x) dt(x, df=8), 
             blab=c("A","B"), xlim=c(-4,4), alab=NA,
             main="t-Distribution (df=8)",
             col=c(hred, hblue, horange), density=c(20, 7))

# Normal
PlotProbDist(breaks=c(-10, -1, 12), 
             function(x) dnorm(x, mean=2, sd=2), 
             blab="A", xlim=c(-7,10),
             main="Normal-Distribution N(2,2)",
             col=c(hred, hblue), density=c(20, 7))

# same for Chi-square
PlotProbDist(breaks=c(0, 15, 35), 
             function(x) dchisq(x, df=8), 
             blab="B", xlim=c(0, 30),
             main=expression(paste(chi^2-Distribution, " (df=8)")),
             col=c(hblue, hred), density=c(0, 20))
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
