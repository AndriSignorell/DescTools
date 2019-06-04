\name{PlotTreemap}
\alias{PlotTreemap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a Treemap
%%  ~~function to do ... ~~
}
\description{
Creates a treemap where rectangular regions of different size, color,
  and groupings visualize the elements.
}
\usage{
PlotTreemap(x, grp = NULL, labels = NULL, cex = 1, text.col = "black",
            col = rainbow(length(x)), labels.grp = NULL, cex.grp = 3,
            text.col.grp = "black", border.grp = "grey50",
            lwd.grp = 5, main = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector storing the values to be used to calculate the
    areas of rectangles.
%%     ~~Describe \code{x} here~~
}
  \item{grp}{a vector specifying the group (i.e. country, sector,
    etc.) to which each element belongs.
%%     ~~Describe \code{grp} here~~
}
  \item{labels}{a vector specifying the labels.
%%     ~~Describe \code{labels} here~~
}
  \item{cex}{the character extension for the area labels. Default is 1.
%%     ~~Describe \code{cex} here~~
}
  \item{text.col}{the text color of the area labels. Default is "black".
%%     ~~Describe \code{text.col} here~~
}
  \item{col}{ a vector storing the values to be used to calculate the
    color of rectangles.
%%     ~~Describe \code{col} here~~
}
  \item{labels.grp}{a character vector specifying the labels for the groups.
%%     ~~Describe \code{labels.grp} here~~
}
  \item{cex.grp}{the character extension for the group labels. Default is 3.
%%     ~~Describe \code{cex.grp} here~~
}
  \item{text.col.grp}{the text color of the group labels. Default is "black".
%%     ~~Describe \code{text.col.grp} here~~
}
  \item{border.grp}{the border color for the group rectangles. Default is "grey50". Set this to \code{NA} if no special border
  is desired.
%%     ~~Describe \code{border.grp} here~~
}
  \item{lwd.grp}{the linewidth of the group borders. Default is 5.
%%     ~~Describe \code{lwd.grp} here~~
}
  \item{main}{a title for the plot.
%%     ~~Describe \code{main} here~~
}
}
\details{A treemap is a two-dimensional visualization for quickly analyzing large, hierarchical data sets. Treemaps are unique among visualizations because they provide users with the ability to see both a high level overview of data as well as fine-grained details. Users can find outliers, notice trends, and perform comparisons using treemaps.
Each data element contained in a treemap is represented with a rectangle, or a cell. Treemap cell arrangement, size, and color are each mapped to an attribute of that element. Treemap cells can be grouped by common attributes. Within a group, larger cells are placed towards the bottom left, and smaller cells are placed at the top right.
%%  ~~ If necessary, more details than the description above ~~
}
\value{returns a list with groupwise organized midpoints in x and y for the rectangles within a group and for the groups themselves.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>, strongly based on code from Jeff Enos \email{jeff@kanecap.com}
%%  ~~who you are~~
}

\seealso{\code{\link{PlotCirc}}, \code{\link{mosaicplot}}, \code{\link{barplot}}
}

\examples{
set.seed(1789)
N <- 20
area <- rlnorm(N)

PlotTreemap(x=sort(area, decreasing=TRUE), labels=letters[1:20], col=Pal("RedToBlack", 20))


grp <- sample(x=1:3, size=20, replace=TRUE, prob=c(0.2,0.3,0.5))

z <- Sort(data.frame(area=area, grp=grp), c("grp","area"), decreasing=c(FALSE,TRUE))
z$col <- SetAlpha(c("steelblue","green","yellow")[z$grp],
                  unlist(lapply(split(z$area, z$grp),
                  function(...) LinScale(..., newlow=0.1, newhigh=0.6))))

PlotTreemap(x=z$area, grp=z$grp, labels=letters[1:20], col=z$col)


b <- PlotTreemap(x=z$area, grp=z$grp, labels=letters[1:20], labels.grp=NA,
                 col=z$col, main="Treemap")

# the function returns the midpoints of the areas
# extract the group midpoints from b
mid <- do.call(rbind, lapply(lapply(b, "[", 1), data.frame))

# and draw some visible text
BoxedText( x=mid$grp.x, y=mid$grp.y, labels=LETTERS[1:3], cex=3, border=NA,
  col=SetAlpha("white",0.7) )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
