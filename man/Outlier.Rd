\name{Outlier}
\alias{Outlier}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Outlier
%%  ~~function to do ... ~~
}
\description{Return outliers following Tukey's boxplot definition.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Outlier(x, method = c("boxplot"), value = TRUE,na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.
}
  \item{method}{the method to be used. So far only Tukey's boxplot rule is implemented.
%%     ~~Describe \code{method} here~~
}
  \item{value}{logical. If \code{FALSE}, a vector containing the (integer) indices of the outliers is returned, and if \code{TRUE} (default), a vector containing the matching elements themselves is returned.}

  \item{na.rm}{logical. Should missing values be removed? Defaults to \code{FALSE}.
}
}
\details{Outlier detection is a tricky problem and should be handled with care. We implement only Tukey's boxplot rule as a rough idea of spotting extreme values.

%%  ~~ If necessary, more details than the description above ~~
}
\value{the values of x lying outside the whiskers in a boxplot or the indices of them
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{boxplot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Outlier(d.pizza$temperature, na.rm=TRUE)

# it's the same as the result from boxplot
sort(d.pizza$temperature[Outlier(d.pizza$temperature, value=FALSE, na.rm=TRUE)])
b <- boxplot(d.pizza$temperature, plot=FALSE)
sort(b$out)

# nice to find the corresponding rows
d.pizza[Outlier(d.pizza$temperature, value=FALSE, na.rm=TRUE), ]


# outliers for the each driver
tapply(d.pizza$temperature, d.pizza$driver, Outlier, na.rm=TRUE)

# the same as:
boxplot(temperature ~ driver, d.pizza)$out
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }
