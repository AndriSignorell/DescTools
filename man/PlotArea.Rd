\name{PlotArea}
\alias{PlotArea}
\alias{PlotArea.default}
\alias{PlotArea.formula}

\title{Create an Area Plot
}
\description{
Produce a stacked area plot, or add polygons to an existing plot.
}
\usage{
\method{PlotArea}{default}(x, y = NULL, prop = FALSE, add = FALSE, xlab = NULL,
         ylab = NULL, col = NULL, frame.plot = FALSE, \dots)

\method{PlotArea}{formula}(formula, data, subset, na.action, \dots)
}

%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{x}{numeric vector of x values, or if \code{y=NULL} a numeric
    vector of y values. Can also be a 1-dimensional table (x values in
    names, y values in array), matrix or 2-dimensional table (x values
    in row names and y values in columns), a data frame (x values in
    first column and y values in subsequent columns), or a time-series
    object of class \code{ts/mts}.
}
  \item{y}{numeric vector of y values, or a matrix containing y values
    in columns.
}
  \item{prop}{whether data should be plotted as proportions, so stacked
    areas equal 1.
%%     ~~Describe \code{prop} here~~
}
  \item{add}{
  whether polygons should be added to an existing plot.
}
  \item{xlab}{label for x axis.
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{label for y axis.
%%     ~~Describe \code{ylab} here~~
}
  \item{col}{fill color of polygon(s). The default is a vector of gray
    colors.
%%     ~~Describe \code{col} here~~
}
  \item{frame.plot}{
  a logical indicating whether a box should be drawn around the plot.
}
  \item{formula}{a \code{\link{formula}}, such as \code{y ~ x} or
    \code{cbind(y1, y2) ~ x}, specifying x and y values. A dot on the
    left-hand side, \code{formula = . ~ x}, means all variables except
    the one specified on the right-hand side.
}
  \item{data}{a data frame (or list) from which the variables in
    \code{formula} should be taken.
}
  \item{subset}{
  an optional vector specifying a subset of observations
    to be used.
}
  \item{na.action}{a function which indicates what should happen when the data contain \code{NA} values. Defaults to \code{getOption("na.action")}.}

  \item{\dots}{further arguments are passed to \code{matplot} and
    \code{polygon}.
}

}
\value{
Matrix of cumulative sums that was used for plotting.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
\url{https://r.789695.n4.nabble.com/areaplot-td2255121.html}
%% ~put references to the literature/web site here ~
}
\author{
Arni Magnusson <arnima@hafro.is>
}

\seealso{
\code{\link{barplot}}, \code{\link{polygon}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# PlotArea with stapled areas
tab <- table( d.pizza$date, d.pizza$driver )
PlotArea(x=as.Date(rownames(tab)), y=tab, xaxt="n", xlab="Date", ylab="Pizzas delivered" )

# add x-axis and some text labels
xrng <- pretty(range(as.Date(rownames(tab))))
axis(side=1, at=xrng, labels=xrng)
text( x=min(d.pizza$date + .5, na.rm=TRUE), y=cumsum(tab[2,])-2.5, label=levels(d.pizza$driver),
  adj=c(0,0.5), col=TextContrastColor(gray.colors(7)))


# formula
PlotArea(Armed.Forces~Year, data=longley)
PlotArea(cbind(Armed.Forces,Unemployed)~Year, data=longley)

# add=TRUE
plot(1940:1970, 500*runif(31), ylim=c(0,500))
PlotArea(Armed.Forces~Year, data=longley, add=TRUE)

# matrix
PlotArea(WorldPhones)
PlotArea(WorldPhones, prop=TRUE, col=rainbow(10))

# table
PlotArea(table(d.pizza$weekday))
PlotArea(table(d.pizza$weekday, d.pizza$driver))

# ts/mts
PlotArea(austres)
PlotArea(Seatbelts[,c("drivers","front","rear")],
         ylab="Killed or seriously injured")
abline(v=1983+1/12, lty=3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
