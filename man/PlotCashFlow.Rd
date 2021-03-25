\name{PlotCashFlow}
\alias{PlotCashFlow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cash Flow Plot
}
\description{A cash flow plot is a plot used in finance and allows you to graphically depict the timing of the cash flows as well as their nature as either inflows or outflows. An "up" arrow represents money received and a "down" arrow money paid out.
%%  ~~ A concise1111 (1-5 lines) description of what the function does. ~~
}
\usage{
PlotCashFlow(x, y, xlim = NULL, labels = y, mar = NULL, 
             cex.per = par("cex"), cex.tck = par("cex") * 0.8, 
             cex.cash = par("cex"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{time period of the cashflows (in and out)
%%     ~~Describe \code{x} here~~
}
  \item{y}{amount of the cashflows
%%     ~~Describe \code{y} here~~
}
  \item{xlim}{range of the x-axis, defaults to \code{range(x)}.
%%     ~~Describe \code{xlim} here~~
}
  \item{labels}{the labels of the cashflows will be printed outside the arrows.
%%     ~~Describe \code{labels} here~~
}
\item{mar}{a vector with 4 elements, defining the margins for the plot}
\item{cex.per}{the character extension for the period labels}
\item{cex.tck}{character extension for the ticklabels, tipically years}
\item{cex.cash}{the character extension for the labels of the cashflows}

}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\seealso{\code{\link{NPV}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
PlotCashFlow(x=c(6:9, 13:15), y=-c(rep(40, 4), rep(50,3)),
             xlim=c(6,17), labels=c(rep(40, 4), rep(50,3)))

PlotCashFlow(x=c(6,8,9,12,17), y=c(10,30,40,50,70))
}
