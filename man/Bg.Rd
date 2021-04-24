\name{Bg}
\alias{Bg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Background of a Plot
%%  ~~function to do ... ~~
}
\description{Paints the background of the plot, using either the figure region, the plot region or both. It can sometimes be cumbersome to elaborate the coordinates and base R does not provide a simple function for that.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Bg(col = "grey", region = c("plot", "figure"), border = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col}{the color of the background, if two colors are provided, the first is used for the plot region and the second for the figure region.
%%     ~~Describe \code{col} here~~
}
  \item{region}{either \code{"plot"} or \code{"figure"}
%%     ~~Describe \code{region} here~~
}
  \item{border}{color for rectangle border(s). Default is \code{NA} for no borders.
%%     ~~Describe \code{border} here~~
}
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{rect}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# use two different colors for the figure region and the plot region
plot(x = rnorm(100), col="blue", cex=1.2, pch=16,
     panel.first={Bg(c("red", "lightyellow"))
                  grid()})
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
