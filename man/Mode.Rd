\name{Mode}
\alias{Mode}
\title{Mode, Most Frequent Value(s)
}
\description{
Calculate the mode, the most frequent value, of a numeric or character vector x. 
}
\usage{
Mode(x, na.rm = FALSE)
}
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.
}
  \item{na.rm}{logical. Should missing values be removed? Defaults to \code{FALSE}.
}
}
\details{The mode is usually useful for qualitative data, sometimes still for an integer vector. For numerical vectors, it is not so much the central tendency property of the mode that is interesting as the information about conspicuous accumulation points, which  sometimes can indicate data errors. In \code{Desc()} it is integrated in the numeric description to draw the analyst's attention to strikingly high frequencies of a single value as soon as they exceed a certain treshold. (In a numeric vector we would in general rather expect low numbers of tied values, or we should be aware of the process properties that generates them.)

The handling of \code{NA} values follows the standards of the package. As soon as a single \code{NA} value occurs, \code{NA} is returned as result. This approach can sometimes be conservative when calculating the mode. The mode could be determined unambiguously in cases where the number of missing values is small enough that - regardless of what value they have - they cannot alter the sample mode. The modal frequency could then be determined within a lower and upper range. In the example of \code{x=c(1,1,1,1,2,2,NA)} we know that the mode of x is 1 regardless of what the true value is for the one missing value; and we know that the modal frequency must be between 4 and 5. However this is not implemented in the function and further considerations in this direction are left to the user here.

The mode is elsewhere often calculated in a crude and wasteful way by tabulating the frequency for all elements of the vector and returning the most frequent one. This function uses a sophisticated data structure in C++ and is limited to determining the most frequent element only. Therefore it is orders of magnitude faster than other implementations, especially for large numeric vectors with large numbers of distinct values. 

You might furthermore consider using \code{density(x)$x[which.max(density(x)$y)]} for quantitative data or alternatively use \code{hist()}.\cr
Another interesting idea for a more robust estimation of the mode:\cr
\preformatted{
peak <- optimize(function(x, model) predict(model, data.frame(x = x)),
                 c(min(x), max(x)),
                 maximum = TRUE,
                 model = y.loess) 
            
points(peak$maximum, peak$objective, pch=FILLED.CIRCLE <- 19) 
}
}
\value{The most frequent value as number or character, depending of \code{class(x)}. If there is more than one, all are returned in a vector.\cr
The modal frequency is attached as attribute named \code{"freq"}.}

\references{https://stackoverflow.com/questions/55212746/rcpp-fast-statistical-mode-function-with-vector-input-of-any-type/
https://stackoverflow.com/a/55213471/8416610
}
\author{
Andri Signorell <andri@signorell.net>, great Rcpp part by Joseph Wood and Ralf Stubner
}

\seealso{ \code{\link{Mean}},  \code{\link{Median}}
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

Mode(d.pizza$driver)
Mode(d.pizza$driver, na.rm=TRUE)
Mode(as.character(d.pizza$driver), na.rm=TRUE)

# use sapply for evaluating data.frames (resp. apply for matrices)
sapply(d.pizza[,c("driver", "temperature", "date")], Mode, na.rm=TRUE)
}

\keyword{ univar }

