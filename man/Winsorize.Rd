% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Winsorize.R
\name{Winsorize}
\alias{Winsorize}
\title{Winsorize (Replace Extreme Values by Less Extreme Ones)}
\usage{
Winsorize(x, val = quantile(x, probs = c(0.05, 0.95), na.rm = FALSE))
}
\arguments{
\item{x}{a numeric vector to be winsorized.}

\item{val}{the low border, all values being lower than this will be
replaced by this value.  The default is set to the 5\%-quantile of x.}
}
\value{
A vector of the same length as the original data \code{x} containing
the winsorized data.
}
\description{
Winsorizing a vector means that a predefined quantum of the smallest and/or
the largest values are replaced by less extreme values. Thereby the
substitute values are the most extreme retained values.
}
\details{
The winsorized vector is obtained by

\deqn{g(x) =
  \left\{\begin{array}{ll}
    -c &\textup{for } x \le c\\
    x  &\textup{for } |x| < c\\
    c  &\textup{for } x \ge c
    \end{array}\right.
  }{g(x)=-c, if x\le -c,
  x, |x| < c, x, if x \ge c, c }

You may also want to consider standardizing (possibly robustly) the data
before you perform a winsorization.
}
\examples{


library(DescTools)

## generate data
set.seed(9128)
x <- round(runif(100) * 100, 1)

(d.frm <- DescTools::Sort(data.frame(
  x, 
  default   = Winsorize(x), 
  quantile  = Winsorize(x, quantile(x, probs=c(0.1, 0.8), na.rm = FALSE)), 
  fixed_val = Winsorize(x, val=c(15, 85)),
  fixed_n   = Winsorize(x, val=c(Small(x, k=3)[3], Large(x, k=3)[1])),
  closest   = Winsorize(x, val=unlist(Closest(x, c(30, 70)))) 
)))[c(1:10, 90:100), ]

# use Large and Small, if a fix number of values should be winsorized (here k=3)

PlotLinesA(SetNames(d.frm, rownames=NULL), lwd=2, col=Pal("Tibco"), 
           main="Winsorized Vector")
z <- 0:10
# twosided (default):
Winsorize(z, val=c(2,8))

# onesided:
# ... replace all values > 8 with 8
Winsorize(z, val=c(min(z), 8))
# ... replace all values < 4 with 4
Winsorize(z, val=c(4, max(z)))

}
\seealso{
\code{\link[robustHD]{winsorize}} from the package \code{robustHD} contains
an option to winsorize multivariate data

\code{\link{scale}}, \code{\link{RobScale}}
}
\author{
Andri Signorell \href{mailto:andri@signorell.net}{andri@signorell.net}
}
\keyword{robust}
\keyword{univar}
