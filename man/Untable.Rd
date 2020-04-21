\name{Untable}
\alias{Untable}
\alias{Untable.default}
\alias{Untable.data.frame}
\title{Recover Original Data From Contingency Table}

\description{Recreates the data.frame out of a contingency table x.
}
\usage{
Untable(x, ...)

\method{Untable}{data.frame}(x, freq = "Freq", rownames = NULL, ...)

\method{Untable}{default}(x, dimnames = NULL, type = NULL, rownames = NULL, colnames = NULL, ...)

}

\arguments{
  \item{x}{a numeric vector, a matrix, a table or a data.frame. If x is a vector, a matrix or a table it is  interpreted as frequencies which are to be inflated to the original list. \cr If x is a data.frame it is interpreted as a table in frequency form (containing one or more factors and a frequency variable).
%%     ~~Describe \code{x} here~~
}
\item{dimnames}{the dimension names of x to be used for expanding. Can be used to expand a weight vector to its original values. If set to \code{NULL} (default) the dimnames of x will be used.}
\item{type}{defines the data type generated. This allows to directly define factors or ordered factors, but also numeric values. See examples.
}
  \item{rownames}{A names vector for the rownames of the resulting data.frame. If set to \code{NULL} (default) the 
  names will be defined according to the table's dimnames.
}
  \item{colnames}{A names vector for the colnames of the resulting data.frame. If set to \code{NULL} (default) the 
  names will be defined according to the table's dimnames.
}
\item{freq}{character, the name of the frequency variable in case x is a data.frame.}

\item{\dots}{further arguments passed to or from functions (not used here).}
}

\details{For x being a vector this reduces to \code{rep(..., n)} with n as vector (which is not supported by \code{rep()}). \code{NA}s in the table will be treated as 0 without raising an error.
}

\value{a data.frame with the detailed data 
(even if x was a 1-dimensional table)
}

\author{Andri Signorell <andri@signorell.net>
}

\seealso{ \code{\link{expand.grid}}, \code{\link{rep}}, \code{\link{gl}}, \code{\link{xtabs}}
}
\examples{
d.titanic <- Untable(Titanic)
str(d.titanic)

# ... not the same as:
data.frame(Titanic)


tab <- table(set1=sample(letters[1:5], size=40, replace=TRUE), 
             set2=sample(letters[11:15], size=40, replace=TRUE))
Untable(tab)


# return a numeric vector by setting type and coerce to a vector by [,]
Untable(c(6,2,2), type="as.numeric")[,]


# how to produce the original list based on frequencies, given as a data.frame
d.freq <- data.frame(xtabs(Freq ~ Sex + Survived, data=Titanic))

# a data list with each individual
d.data <- Untable( xtabs(c(1364, 126, 367, 344) ~ ., 
             expand.grid(levels(d.freq$Sex),levels(d.freq$Survived)))) 
head(d.data)

# expand a weights vector
Untable(c(1,4,5), dimnames=list(c("Zurich","Berlin","London")))

# and the same with a numeric vector 
Untable(c(1,4,5), dimnames=list(c(5,10,15)), type="as.numeric")[,]
# ... which again is nothing else than
rep(times=c(1,4,5), x=c(5,10,15))

# the data.frame interface
d.freq <- data.frame(f1=c("A","A","B","B"), f2=c("C","D","C","D"), Freq=c(1,2,3,4))
Untable(d.freq)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
