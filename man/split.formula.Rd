
\name{Split}
\alias{Split}
\alias{Split.default}
\alias{Split.formula}
\title{Divide Data into Groups}

\description{
Divides data into groups defined by a grouping factor.
For the formula method, the left-hand side of the formula is split
according to one or more grouping variables on the right-hand side.
}

\usage{
Split(x, ...)

\method{Split}{default}(x, f, drop = FALSE, ...)
\method{Split}{formula}(formula, data, subset, na.action, drop = FALSE, ...)
}
\arguments{
 \item{x}{a vector or data object to be split.}

  \item{f}{a factor or list defining the grouping structure.}

\item{formula}{
a formula of the form \code{lhs ~ rhs} where \code{lhs} is a
  variable giving the data values and \code{rhs }
  defines the grouping, or a list of such factors 
  in which case their interaction is used for the grouping.
}

\item{data}{
  an optional matrix or data frame (or similar: see \code{\link{model.frame}}) 
  containing the variables in the formula. 
  By default the variables are taken from \code{environment(formula)}.
}

\item{subset}{	
an optional vector specifying a subset of observations to be used.
}

\item{na.action}{
a function which indicates what should happen when the data contain \code{NA}s.
}

\item{drop}{logical indicating if levels that do not occur should 
be dropped (if \code{f} is a \code{factor} or a list).
Defaults to \code{FALSE}.}

\item{\dots}{additional arguments to be passed to \code{\link{split}}.}
}

\details{
The formula interface constructs a model frame using
\code{\link[stats]{model.frame}} and supports optional
\code{subset} and \code{na.action} arguments.
}
\seealso{\link{split}}

\author{Andri Signorell <andri@signorell.net>}

\examples{
Split(extra ~ group, data = sleep)

# more variables are taken as interaction
Split(breaks ~ wool + tension, warpbreaks)

# subset
Split(breaks ~ wool, warpbreaks, subset = tension=="L")

}
\keyword{ category }
