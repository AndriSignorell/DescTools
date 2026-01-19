\name{Gmean}
\alias{Gmean}
\alias{Gsd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Geometric Mean and Standard Deviation
%%  ~~function to do ... ~~
}
\description{Calculates the geometric mean, its confidence interval and the geometric standard deviation of a vector x.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Gmean(x, conf.level = NA, sides = c("two.sided","left","right"), 
      method = c("classic", "boot"), na.rm = FALSE, ...)

Gsd(x, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a positive numeric vector. An object which is not a vector is coerced (if possible) by as.vector.
%%     ~~Describe \code{x} here~~
}
  \item{conf.level}{confidence level of the interval. Default is \code{NA}.
%%     ~~Describe \code{conf.level} here~~
}
\item{sides}{a character string specifying the side of the confidence interval, must be one of \code{"two.sided"} (default), \code{"left"} or \code{"right"}. You can specify just the initial letter. \code{"left"} would be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.}


  \item{method}{a vector of character strings representing the type of intervals required. The value should be any subset of the values \code{"classic"}, \code{"boot"}.
  See \code{\link[boot]{boot.ci}}.
%%     ~~Describe \code{x} here~~
}

  \item{na.rm}{logical, indicating whether \code{NA} values should be stripped before the computation proceeds. Defaults to \code{FALSE}.
%%     ~~Describe \code{na.rm} here~~
}
 \item{...}{further arguments are passed to the \code{\link[boot]{boot}} function. Supported arguments are \code{type} (\code{"norm"}, \code{"basic"}, \code{"stud"}, \code{"perc"}, \code{"bca"}), \code{parallel} and the number of bootstrap replicates \code{R}. If not defined those will be set to their defaults, being \code{"basic"} for \code{type},  option \code{"boot.parallel"} (and if that is not set, \code{"no"}) for \code{parallel}
 and \code{999} for \code{R}.%%     ~~Describe \code{x} here~~
}
}
\details{
The geometric mean is defined as:
\deqn{\sqrt[n]{x_{1}\cdot x_{2}\cdot x_{3} \ldots \cdot x_{n}}}{(x_1 * x_2 * ... * x_n)^(1/n)}

The geometric mean and geometric standard deviation are restricted to positive inputs (because otherwise the answer can have an imaginary component). Hence if any argument is negative, the result will be \code{NA}. If any argument is zero, then the geometric mean is zero.\cr
For strict positive values the geometric mean is computed as \code{exp(MeanCI(log(x)))}.

\bold{Considerations (Roenfeldt 2018)} \verb{     } "The calculation of the geometric mean requires that all values are non-zero and positive. So what should you do if you have data that do not meet this requirement? If you have values that equal zero, you have a few options:
\itemize{
\item Adjust your scale so that you add 1 to every number in the data set, and then subtract 1 from the resulting geometric mean.
\item Ignore zeros or missing data in your calculations.
\item Convert zeros to a very small number (often called "below the detection limit") that is less than the next smallest number in the data set.
}

If you have negative numbers, you will need to convert those numbers to a positive value before calculating the geometric mean. You can then assign the resulting geometric mean a negative value. If 
your data set contains both positive and negative values, you will have to separate them and find the 
geometric means for each group, and you can then find the weighted average of their individual 
geometric means to find the total geometric mean for the full data set.
If none of these options appeals to you, you are not alone! There is controversy among statisticians about 
what is the best method for dealing with these values. You may want to calculate several types of 
averages and decide what makes the most sense for you and the results you are trying to report."}
\value{a numeric value.
}
\references{Snedecor, G. W., Cochran, W. G. Cochran (1989) Statistical Methods, 8th ed. Ames, \emph{IA: Iowa State University Press }

Roenfeldt K. (2018) Better than Average: Calculating Geometric Means Using SAS, 
Henry M. Jackson Foundation for the Advancement of Military Medicine, 
\url{https://www.lexjansen.com/wuss/2018/56_Final_Paper_PDF.pdf}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}

\seealso{\code{\link{mean}}, \code{\link{Hmean}}

}
\examples{
x <- runif(5)
Gmean(x)

m <- matrix(runif(50), nrow = 10)
apply(m, 2, Gmean)

sapply(as.data.frame(m), Gmean)

# ......................................................
# example in https://www.stata.com/manuals13/rameans.pdf
x <- c(5,4,-4,-5,0,0,NA,7)

# positives only
Gmean(x[x>0], na.rm=TRUE, conf.level=0.95)

# add 5 to original values and remove zeros
Gmean(NAIfZero(x+5), na.rm=TRUE, conf.level = 0.95)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }

