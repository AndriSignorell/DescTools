\name{ABCCoords}
\alias{ABCCoords}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Coordinates for "bottomright", etc.
%%  ~~function to do ... ~~
}
\description{Return the xy.coordinates for the literal positions "bottomright", etc. as used to place legends.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ABCCoords(x = "topleft", region = "figure", cex = NULL, linset = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{one out of \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"},
                        \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"}, \code{"center"}
%%     ~~Describe \code{x} here~~
}
  \item{region}{one out of \code{plot} or  \code{figure}
}
  \item{cex}{the character extension for the text.
%%     ~~Describe \code{cex} here~~
}
  \item{linset}{line inset in lines of text.
%%     ~~Describe \code{linset} here~~
}
  \item{\dots}{the dots are passed to the \code{strwidth()} and \code{strheight()} functions in case there where more specific text formats.
%%     ~~Describe \code{\dots} here~~
}
}
\details{The same logic as for the legend can be useful for placing texts, too.
This function returns the coordinates for the text, which can be used in the specific text functions.
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
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{text}}, \code{\link{BoxedText}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
plot(x = rnorm(10), type="n", xlab="", ylab="")
# note that plot.new() has to be called before we can grab the geometry
ABCCoords("bottomleft")

lapply(c("bottomleft", "left"), ABCCoords)

plot(x = rnorm(10), type="n", xlab="", ylab="")
text(x=(xy <- ABCCoords("bottomleft", region = "plot"))$xy, 
     labels = "My Maybe Long Text", adj = xy$adj, xpd=NA)

text(x=(xy <- ABCCoords("topleft", region = "figure"))$xy, 
     labels = "My Maybe Long Text", adj = xy$adj, xpd=NA)

plot(x = rnorm(10), type="n", xlab="", ylab="")
sapply(c("topleft", "top", "topright", "left", "center", "right", "bottomleft", "bottom", "bottomright"), 
       function(x) 
         text(x=(xy <- ABCCoords(x, region = "plot", linset=1))$xy, 
              labels = "MyMarginText", adj = xy$adj, xpd=NA)
)


plot(x = rnorm(100), type="n", xlab="", ylab="",
     panel.first={Bg(c("red", "lightyellow"))
             grid()})
xy <- ABCCoords("topleft", region = "plot")
par(xpd=NA)
BoxedText(x=xy$xy$x, y=xy$xy$y, xpad = 1, ypad = 1,
          labels = "My Maybe Long Text", adj = xy$adj, col=SetAlpha("green", 0.8))
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
