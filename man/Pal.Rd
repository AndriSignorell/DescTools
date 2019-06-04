\name{DescTools Palettes}
\alias{Pal}
\alias{plot.palette}

\alias{hblue}
\alias{hred}
\alias{horange}
\alias{hgreen}
\alias{hyellow}
\alias{hecru}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Some Custom Palettes
%%  ~~function to do ... ~~
}
\description{Some more custom palettes.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Pal(pal, n = 100, alpha = 1)

\method{plot}{palette}(x, cex = 3, ...)

hred
horange
hyellow
hecru
hblue
hgreen

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pal}{name or number of the palette. One of
  \code{RedToBlack (1)}, \code{RedBlackGreen (2)}, \code{SteeblueWhite (3)}, \code{RedWhiteGreen (4)}, \code{RedWhiteBlue0 (5)}, \code{RedWhiteBlue1 (6)}, \code{RedWhiteBlue2 (7)}, \code{RedWhiteBlue3 (8)}, \code{Helsana (9)}, \code{Tibco (10)}, \code{RedGreen1 (11)}, \code{Spring (12)}, \code{Soap (13)}, \code{Maiden (14)}, \code{Dark (15)}, \code{Accent (16)}, \code{Pastel (17)}, \code{Fragile (18)}, \code{Big (19)}, \code{Long (20)}, \code{Night (21)}, \code{Dawn (22)}, \code{Noon (23)}, \code{Light (24)}

%%     ~~Describe \code{pal} here~~
}
  \item{n}{integer, number of colors for the palette.
%%     ~~Describe \code{n} here~~
}
 \item{alpha}{the alpha value to be added. This can be any value from 0 (fully transparent) to 1 (opaque). \code{NA} is interpreted so as to delete a potential alpha channel. Default is 0.5.
%%     ~~Describe \code{alpha} here~~
}

\item{x}{a palette to be plotted.}
\item{cex}{extension for the color squares. Defaults to 3.}
\item{\dots}{further arguments passed to the function.}
}
\details{hred, horange, hyellow, hecru, hblue and hgreen are constants, pointing to the according color from the palette Pal("Helsana").

%% Use \code{options(palette=Pal("Dark"))} to set the default palette used throughout DescTools to \code{"Dark"}.
}

\value{a vector of colors
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{colorRampPalette}}
}
\examples{
Canvas(c(0,1))
ColorLegend(x=0, y=1, width=0.1, col=Pal(1, n=50))
ColorLegend(x=0.15, y=1, width=0.1, col=Pal(2, n=50))
ColorLegend(x=0.3, y=1, width=0.1, col=Pal(3, n=50))
ColorLegend(x=0.45, y=1, width=0.1, col=Pal(4, n=50))
ColorLegend(x=0.6, y=1, width=0.1, col=Pal(5, n=50))
ColorLegend(x=0.75, y=1, width=0.1, col=Pal(6, n=50))
ColorLegend(x=0.9, y=1, width=0.1, col=Pal(7))
ColorLegend(x=1.05, y=1, width=0.1, col=Pal(8))

text(1:8, y=1.05, x=seq(0,1.05,.15)+.05)
title(main="DescTools palettes")

par(mfrow=c(4,2), mar=c(1,1,2,1))
barplot(1:9, col=Pal("Tibco"), axes=FALSE, main="Palette 'Tibco'" )

barplot(1:7, col=Pal("Helsana"), axes=FALSE, main="Palette 'Helsana'" )
barplot(1:7, col=SetAlpha(Pal("Helsana")[c("ecru","hellgruen","hellblau")], 0.6),
        axes=FALSE, main="Palette 'Helsana' (Alpha)" )

barplot(1:10, col=Pal("RedToBlack", 10), axes=FALSE, main="Palette 'RedToBlack'" )
barplot(1:10, col=Pal("RedBlackGreen", 10), axes=FALSE, main="Palette 'RedGreenGreen'" )
barplot(1:10, col=Pal("SteeblueWhite", 10), axes=FALSE, main="Palette 'SteeblueWhite'" )
barplot(1:10, col=Pal("RedWhiteGreen", 10), axes=FALSE, main="Palette 'RedWhiteGreen'" )


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ color}
