\name{PlotMiss}
\alias{PlotMiss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Missing Data
%%  ~~function to do ... ~~
}
\description{Takes a data frame and displays the location of missing data. The missings can be clustered and be displayed together.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotMiss(x, col = hred, bg = SetAlpha(hecru, 0.3), clust = FALSE,
         main = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a data.frame to be analysed.
%%     ~~Describe \code{x} here~~
}
  \item{col}{the colour of the missings.
%%     ~~Describe \code{col} here~~
}
  \item{bg}{the background colour of the plot.
%%     ~~Describe \code{bg} here~~
}
  \item{clust}{logical, defining if the missings should be clustered. Default is \code{FALSE}.
%%     ~~Describe \code{clust} here~~
}
  \item{main}{the main title.
%%     ~~Describe \code{main} here~~
}
  \item{\dots}{the dots are passed to \code{\link{plot}}.
%%     ~~Describe \code{main} here~~
}
}
\details{A graphical display of the position of the missings can be help to detect dependencies or patterns within the missings.
%%  ~~ If necessary, more details than the description above ~~
}
\value{if clust is set to TRUE, the new order will be returned invisibly.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>, following an idea of Henk Harmsen <henk@carbonmetrics.com>
%%  ~~who you are~~
}

\seealso{\code{\link{hclust}}, \code{\link{CountCompCases}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
PlotMiss(d.pizza, main="Missing pizza data")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot}
