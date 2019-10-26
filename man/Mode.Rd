\name{Mode}
\alias{Mode}
\title{Mode, Most Frequent Value(s)
}
\description{
Calculates the mode, the most frequent value, of a variable x. This makes mostly sense for qualitative data.  
}
\usage{
Mode(x, na.rm = FALSE)
}
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.
}
  \item{na.rm}{logical. Should missing values be removed? Defaults to FALSE.
}
}
\value{Returns the most frequent value. If there are more than one, all of them are returned in a vector. 
}
\author{
Andri Signorell <andri@signorell.net>
}

\note{Consider using \code{density(x)$x[which.max(density(x)$y)]} for quantitative data or alternatively use \code{hist()}.\cr
Another interesting idea:\cr
\preformatted{
peak <- optimize(function(x, model) predict(model, data.frame(x = x)),
                 c(min(x), max(x)),
                 maximum = TRUE,
                 model = y.loess) 
            
points(peak$maximum, peak$objective, pch=FILLED.CIRCLE <- 19) 
}
}

\seealso{ \code{\link{mean}},  \code{\link{median}}
}

\examples{
data(d.pizza)
Mode(d.pizza$driver)

# use sapply for evaluating data.frames (resp. apply for matrices)
sapply(d.pizza[,c("driver","temperature","date")], Mode, na.rm=TRUE)
}

\keyword{ univar }

