\name{Lambda}
\alias{Lambda}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Goodman Kruskal Lambda
%%  ~~function to do ... ~~
}
\description{Calculate symmetric and asymmetric Goodman Kruskal lambda and their confidence intervals. Lamdba is a measure of proportional reduction in error in cross tabulation analysis. For any sample with a nominal independent variable and dependent variable (or ones that can be treated nominally), it indicates the extent to which the modal categories and frequencies for each value of the independent variable differ from the overall modal category and frequency, i.e. for all values of the independent variable together
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Lambda(x, y = NULL, direction = c("symmetric", "row", "column"), conf.level = NA, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector, a matrix or a table.
%%     ~~Describe \code{x} here~~
}
  \item{y}{\code{NULL} (default) or a vector with compatible dimensions to x. If y is provided, \code{table(x, y, ...)} is calculated.
%%     ~~Describe \code{y} here~~
}
 \item{direction}{type of lambda. Can be one out of \code{"symmetric"} (default), \code{"row"}, \code{"column"} (abbreviations are allowed).
  If direction is set to \code{"row"} then Lambda(R|C) (column dependent) will be reported. See details.
%%     ~~Describe \code{x} here~~
}
  \item{conf.level}{confidence level for the returned confidence interval, restricted to lie between 0 and 1.
%%     ~~Describe \code{conf.level} here~~
}
  \item{\dots}{further arguments are passed to the function \code{\link{table}}, allowing i.e. to set \cr \code{useNA = c("no", "ifany", "always")}.
%%     ~~Describe \code{\dots} here~~
}
}
\details{Asymmetric lambda is interpreted as the probable improvement in predicting the column variable Y given knowledge of the row variable X.\cr
The nondirectional lambda is the average of the two asymmetric lambdas, Lambda(C|R) and Lambda(R|C).
Lambda (asymmetric and symmetric) has a scale ranging from 0 to 1.
\cr\cr
Data can be passed to the function either as matrix or data.frame in \code{x}, or as two numeric vectors \code{x} and \code{y}. In the latter case \code{table(x, y, ...)} is calculated. Thus \code{NA}s are handled the same way as \code{\link{table}} does. Note that tables are by default calculated \bold{without} NAs (which breaks the package's law to in general not omit NAs silently). The specific argument \code{useNA} can be passed via the ... argument.\cr
\code{\link{PairApply}} can be used to calculate pairwise lambdas.
%%  ~~ If necessary, more details than the description above ~~
}
\value{if no confidence intervals are requested:
the estimate as numeric value\cr\cr
else a named numeric vector with 3 elements
\item{lambda}{estimate}
\item{lwr.ci}{lower confidence interval}
\item{upr.ci}{upper confidence interval}
}

\references{
Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley & Sons

Goodman, L. A., Kruskal W. H. (1979) Measures of Association for Cross Classifications. New
York: Springer-Verlag (contains articles appearing in \emph{J. Amer. Statist. Assoc.} in 1954,
1959, 1963, 1972).\cr
http://www.nssl.noaa.gov/users/brooks/public_html/feda/papers/goodmankruskal1.pdf (might be outdated)

Liebetrau, A. M. (1983) \emph{Measures of Association}, Sage University Papers Series on Quantitative Applications in the Social Sciences, 07-004. Newbury Park, CA: Sage, pp. 17--24
%% ~put references to the literature/web site here ~
}
\author{Andri Signorell <andri@signorell.net> based on code from Antti Arppe <antti.arppe@helsinki.fi>, \cr Nanina Anderegg (confidence interval symmetric lambda)
}

\seealso{
\code{\link{GoodmanKruskalTau}}, \code{\link{GoodmanKruskalGamma}}, \code{\link{KendallTauA}}, \code{\link{KendallTauB}}, \code{\link{StuartTauC}}, \code{\link{SomersDelta}}, \code{\link{cor}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# example from Goodman Kruskal (1954)
m <- as.table(cbind(c(1768,946,115), c(807,1387,438), c(189,746,288), c(47,53,16)))
dimnames(m) <- list(paste("A", 1:3), paste("B", 1:4))
m

# direction default is "symmetric"
Lambda(m)
Lambda(m, conf.level=0.95)

Lambda(m, direction="row")
Lambda(m, direction="column")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }
