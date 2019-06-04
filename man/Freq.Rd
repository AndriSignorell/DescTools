\name{Freq}
\alias{Freq}
\alias{print.Freq}
\title{
Frequency Table for a Single Variable
}
\description{
Calculates absolute and relative frequencies of a vector x. Continuous (numeric) variables
will be cut using the same logic as used by the function \code{\link{hist}}.
Categorical variables will be aggregated by \code{\link{table}}. The result will contain single and cumulative frequencies for both, absolute values and percentages.

}
\usage{
Freq(x, breaks = hist(x, plot = FALSE)$breaks, include.lowest = TRUE,
     ord = c("level", "desc", "asc", "name"),
     useNA = c("no", "ifany", "always"), ...)

\method{print}{Freq}(x, digits = NULL, ...)

}

\arguments{
  \item{x}{the variable to be described, can be any atomic type.
}
  \item{breaks}{either a numeric vector of two or more cut points or a single number (greater than or equal to 2)
    giving the number of intervals into which x is to be cut. Default taken from the function \kbd{hist()}.
    This is ignored if x is not of numeric type.
}
  \item{include.lowest}{logical, indicating if an x[i] equal to the lowest (or highest, for \code{right = FALSE}) \code{"breaks"} value should be included. Ignored if x is not of numeric type.
}
  \item{ord}{how should the result be ordered? Default is \kbd{"level"}, other choices are 'by frequency' (\kbd{"descending"} or \kbd{"ascending"})
    or 'by name of the levels' (\kbd{"name"}). The argument can be abbreviated. This is ignored if x is numeric.
}
  \item{useNA}{one out of \code{"no"}, \code{"ifany"}, \code{"always"}. Defines whether to include extra \code{NA} levels in the table.
   Defaults to \code{"no"} which is the \code{\link{table}()} default too.
}
  \item{digits}{integer, determining the number of digits used to format the relative frequencies.
}

  \item{\dots}{further arguments are passed to the function \code{\link{cut}()}. Use \code{dig.lab} to control the format of numeric group names. Use the argument \code{right} to define if the intervals should be closed on the right (and open on the left) or vice versa. \cr
  In \code{print.Freq} the dots are not used.
}
}
\details{
If \code{breaks} is specified as a single number, the range of the data is divided into breaks pieces of equal length,
and then the outer limits are moved away by 0.1\% of the range to ensure that the extreme values both fall
within the break intervals.
(If \code{x} is a constant vector, equal-length intervals are created that cover the single value.)
}
\value{an object of type \code{"Freq"}, which is basically a data.frame with 5 columns (earning a specific print routine), containing the following components:
  \item{level}{ factor. The levels of the grouping variable.}
  \item{freq}{  integer. The absolute frequencies. }
  \item{perc}{ numeric. The relative frequencies (percent).}
  \item{cumfreq}{integer. The cumulative sum of the absolute frequencies.}
  \item{cumperc}{numeric. The cumulative sum of the relative frequencies.}

}
\author{Andri Signorell <andri@signorell.net>
}

\seealso{
\code{\link{PercTable}}, \code{\link{cut}}, \code{\link{hist}}, \code{\link{cumsum}}, \code{\link{table}},
\code{\link{prop.table}}
}
\examples{
data(d.pizza)

# result is a data.frame
d.freq <- Freq(d.pizza$price)
d.freq

# it is printed by default with 3 digits for the percent values,
# but the number of digits can be defined in the print function
print(d.freq, digits=5)

# sorted by frequency
Freq(d.pizza$driver, ord="desc")

# sorted by name, including NAs
Freq(d.pizza$driver, ord="name", useNA="ifany")

# percentages and cumulative frequencies for a vector of count data
Freq(as.table(c(2,4,12,8)))
}
\keyword{ univar }

