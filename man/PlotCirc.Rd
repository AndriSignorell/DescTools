\name{PlotCirc}
\alias{PlotCirc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Circular Plot
}
\description{This visualising scheme represents the unidirectional relationship between the rows and the columns of a contingency table.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotCirc(tab, acol = rainbow(sum(dim(tab))), aborder = "darkgrey",
         rcol = SetAlpha(acol[1:nrow(tab)], 0.5), rborder = "darkgrey",
         gap = 5, main = "", labels = NULL, cex.lab = 1.0, las = 1,
         adj = NULL, dist = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{a table to be visualised.
%%     ~~Describe \code{tab} here~~
}
  \item{acol}{the colors for the peripheral annuli.
%%     ~~Describe \code{acol} here~~
}
  \item{aborder}{the border colors for the peripheral annuli.
%%     ~~Describe \code{acol} here~~
}
  \item{rcol}{the colors for the ribbons.
%%     ~~Describe \code{rcol} here~~
}
  \item{rborder}{the border colors for the ribbons.
%%     ~~Describe \code{rcol} here~~
}
  \item{gap}{the gap between the entities in degrees.
%%     ~~Describe \code{gap} here~~
}
  \item{main}{the main title, defaults to "".
%%     ~~Describe \code{main} here~~
}
  \item{labels}{the labels. Defaults to the column names and rownames of the table.
%%     ~~Describe \code{labels} here~~
}
 \item{las}{alignment of the labels, 1 means horizontal, 2 radial and 3 vertical.
%%     ~~Describe \code{labels} here~~
}
 \item{adj}{adjustments for the labels. (Left: 0, Right: 1, Mid: 0.5)
%%     ~~Describe \code{labels} here~~
}
\item{dist}{gives the distance of the labels from the outer circle. Default is 2.}
\item{cex.lab}{the character extension for the labels.
%%     ~~Describe \code{cex.lab} here~~
}
}
\details{
The visual scheme of representing relationships can be applied to a table, given the observation that a table cell is a relationship (with a value) between a row and column. By representing the row and columns as segments along the circle, the information in the corresponding cell can be encoded as a link between the segments.
In general, the cell represents a unidirectional relationship (e.g. row->column) - in this relationship the role of the segments is not interchangeable (e.g. (row,col) and (col,row) are different cells). To identify the role of the segment, as a row or column, the ribbon is made to terminate at the row segment but slightly away from the column segment. In this way, for a given ribbon, it is easy to identify which segment is the row and which is the column.
}
\value{the calculated points for the labels, which can be used to place userdefined labels.}

\references{
Inspired by \url{https://circos.ca/presentations/articles/vis_tables1/}
%% ~put references to the literature/web site here ~
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{PlotPolar}}
}
\examples{
tab <- matrix(c(2,5,8,3,10,12,5,7,15), nrow=3, byrow=FALSE)
dimnames(tab) <- list(c("A","B","C"), c("D","E","F"))
tab


PlotCirc( tab,
  acol = c("dodgerblue","seagreen2","limegreen","olivedrab2","goldenrod2","tomato2"),
  rcol = SetAlpha(c("red","orange","olivedrab1"), 0.5)
)

tab <- table(d.pizza$weekday, d.pizza$operator)
par(mfrow=c(1,2))
PlotCirc(tab, main="weekday ~ operator")
PlotCirc(t(tab), main="operator ~ weekday")
}
\keyword{hplot }
