\name{Mode}
\alias{Mode}
\title{Mode, Most Frequent Value(s)
}
\description{
Calculate the mode, the most frequent value, of a variable x. This makes mostly sense for qualitative data, at most for x being an integer vector.
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
\value{Returns the most frequent value. If there are more than one, all of them will be returned in a vector.}

\references{https://stackoverflow.com/questions/55212746/rcpp-fast-statistical-mode-function-with-vector-input-of-any-type/
https://stackoverflow.com/a/55213471/8416610
}
\author{
Andri Signorell <andri@signorell.net>, great Rcpp part by Joseph Wood and Ralf Stubner
}

\note{Consider using \kbd{density(x)$x[which.max(density(x)$y)]} for quantitative data or alternatively use \kbd{hist()}.\cr
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
# normal mode
Mode(c(0:5, 5))

Mode(5)
Mode(NA)
Mode(c(NA, NA))
Mode(c(NA, 0:5))
Mode(c(NA, 0:5), na.rm=TRUE)
Mode(c(NA, 0:5, 5), na.rm=TRUE)

# returns all encountered modes, if several exist
Mode(c(0:5, 4, 5, 6))


data(d.pizza)
Mode(d.pizza$driver)

# use sapply for evaluating data.frames (resp. apply for matrices)
sapply(d.pizza[,c("driver","temperature","date")], Mode, na.rm=TRUE)
}

\keyword{ univar }

