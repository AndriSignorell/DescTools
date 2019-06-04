\name{PlotTernary}
\alias{PlotTernary}
\title{Ternary or Triangular Plots}
\description{
  \code{PlotTernary} plots in a triangle the values of three variables. Useful for mixtures
	(chemistry etc.).
}
\usage{
PlotTernary(x, y = NULL, z = NULL, args.grid = NULL, lbl = NULL, main = "",...)
}
\arguments{
  \item{x}{vector of first variable. Will be placed on top of the triangle.}
  \item{y}{vector of second variable (the right corner).}
  \item{z}{vector of third variable (on the left corner).}
  \item{args.grid}{list of additional arguments for the grid. Set this argument to \code{NA} if no grid should be drawn.  The usual color and linetype will be used.}
  \item{main}{overall title for the plot. }
  \item{lbl}{the labels for the corner points. Default to the names of x, y, z.}
  \item{\dots}{the dots are sent to \code{\link{points}}}
}
\references{
  J. Aitchison (1986)
  \emph{The Statistical Analysis of Compositional Data.}
  Chapman and Hall, p.360.

  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}

\seealso{example in \code{\link[MASS]{Skye}}}
\author{Andri Signorell <andri@signorell.net> based on example code by W. N. Venables and B. D. Ripley mentioned
}

\examples{
# some random data in three variables
c1 <- runif(25)
c2 <- runif(25)
c3 <- runif(25)

# basic plot
par(mfrow=c(1, 2))
PlotTernary(c1, c2, c3, args.grid=NA)

\dontrun{
# plot with different symbols and a grid using a dataset from MASS
data(Skye, package="MASS")

PlotTernary(Skye[c(1,3,2)], pch=15, col=hred, main="Skye",
            lbl=c("A Sodium", "F Iron", "M Magnesium"))
}
}
\keyword{hplot}
