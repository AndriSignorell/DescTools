\name{\%c\%}
\alias{\%c\%}
\alias{Concatenate Strings}

\title{Concatenates Two Strings Without Any Separator
}
\description{\%c\% is just a short operator implementation for paste(x, y, separator="").
}
\usage{
x \%c\% y

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{first string
}
  \item{y}{second string, which will be pasted behind the first one.
}
}
\value{ returns the concatenation as string.
}
\details{R-Core does not consider it a good idea to use + as an operator not being commutative. So we use c here.
\cr
See the discussion:
\url{https://stat.ethz.ch/pipermail/r-devel/2006-August/039013.html}\cr
and \url{https://stackoverflow.com/questions/1319698/why-doesnt-operate-on-characters-in-r?lq=1}\cr

Still the paste syntax seems sometimes clumsy in daily life and so \%c\% might spare some keys.
}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{\code{\link{Between}}, \code{\link{\%like\%}}
}
\examples{
"foo" \%c\% "bar"

# works with numerics as well
345 \%c\% 457
}
\keyword{ manip }
