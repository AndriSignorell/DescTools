\name{split.formula}
\alias{split.formula}
\title{Formula Interface for Split
}
\description{Implementation of a simple formula interface for the 
\code{\link{split}} function. Idea is to split the \code{rhs} 
of the formula by the \code{lhs}.
}
\usage{
\method{split}{formula} (formula, data, subset, na.action, drop = FALSE, ...)

}
\arguments{

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

\item{\dots}{other arguments to be passed to \code{\link{split}}.}
}

\author{Andri Signorell <andri@signorell.net>}

\seealso{\code{\link{split}}  }
\examples{
split(extra ~ group, data = sleep)

# more variables are taken as interaction
split(breaks ~ wool + tension, warpbreaks)

# subset
split(breaks ~ wool, warpbreaks, subset = tension=="L")

}
\keyword{ category }
