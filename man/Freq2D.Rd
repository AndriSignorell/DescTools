\name{Freq2D}
\alias{Freq2D}
\alias{Freq2D.default}
\alias{Freq2D.formula}
\title{Bivariate (Two-Dimensional) Frequency Distribution}
\description{
  Calculate a frequency distribution for two continuous variables.
}
\usage{
Freq2D(x, \dots)

\method{Freq2D}{formula}(formula, data, subset, \dots)

\method{Freq2D}{default}(x, y, n=20, pad=0, dnn=NULL, \dots)
}
\arguments{
  \item{x}{a vector of x values, or a data frame whose first two columns
    contain the x and y values.}
  \item{y}{a vector of y values.}
  \item{formula}{a \code{\link{formula}}, such as \code{y~x}.}
  \item{data}{a \code{data.frame}, \code{matrix}, or \code{list} from
    which the variables in \code{formula} should be taken.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used.}
  \item{n}{the desired number of bins for the output, a scalar or a
    vector of length 2.}
  \item{pad}{number of rows and columns to add to each margin,
    containing only zeros.}
  \item{dnn}{the names to be given to the dimensions in the result.}
  \item{\dots}{named arguments to be passed to the default method.}
}
\details{
  The exact number of bins is determined by the
  \code{\link{pretty}} function, based on the value of \code{n}.

  Padding the margins with zeros can be helpful for subsequent analysis,
  such as smoothing.

  The \code{print} logical flag only has an effect when \code{layout=1}.
}
\value{
  The \code{layout} argument specifies one of the following formats for
  the binned frequency output:

  \enumerate{
    \item{\code{matrix} that is easy to read, aligned like a
      scatterplot.}
    \item{\code{list} with three elements (x, y, matrix) that can be
      passed to various plotting functions.}
    \item{\code{data.frame} with three columns (x, y, frequency) that
      can be analyzed further.}
  }
}
\author{Arni Magnusson <arnima@hafro.is>}

\seealso{
  \code{\link{cut}}, \code{\link{table}}, and \code{\link{print.table}}
  are the basic underlying functions.\cr
  \code{\link{Freq}}, \code{\link{PercTable}} 
}

\examples{
Freq2D(quakes$long, quakes$lat, dnn="")
Freq2D(lat ~ long, quakes, n=c(10, 20), pad=1)

# range(Freq2D(saithe, print=FALSE))

# Layout, plot
# Freq2D(saithe, layout=2)
# Freq2D(saithe, layout=3)
# contour(Freq2D(saithe, layout=2))
# lattice::contourplot(Freq ~ Bio + HR, Freq2D(saithe,layout=3))
}
% Graphics
\keyword{dplot}
% Basics
\keyword{manip}
% Statistics
\keyword{distribution}
\keyword{multivariate}
