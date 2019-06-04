\name{UncertCoef}
\alias{UncertCoef}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Uncertainty Coefficient
%%  ~~function to do ... ~~
}
\description{
The uncertainty coefficient U(C|R) measures the proportion of uncertainty (entropy) in the column
variable Y that is explained by the row variable X. The function has interfaces for a table, a matrix, a data.frame and for single vectors.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
UncertCoef(x, y = NULL, direction = c("symmetric", "row", "column"), 
           conf.level = NA, p.zero.correction = 1/sum(x)^2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector, a factor, matrix or data frame. 
%%     ~~Describe \code{x} here~~
}
  \item{y}{\code{NULL} (default) or a vector, an ordered factor, matrix or data frame with compatible dimensions to x. 
%%     ~~Describe \code{y} here~~
}
 \item{direction}{direction of the calculation. Can be \code{"row"} (default) or \code{"column"}, where
  \code{"row"} calculates UncertCoef (R|C) ("column dependent").
%%     ~~Describe \code{x} here~~
}
\item{conf.level}{confidence level of the interval. If set to \code{NA} (which is the default) no confidence interval will be calculated. 
%%     ~~Describe \code{conf.level} here~~
}
\item{p.zero.correction}{slightly nudge zero values so that their logarithm can be calculated %%     ~~Describe \code{conf.level} here~~
}
  \item{\dots}{further arguments are passed to the function \code{\link{table}}, allowing i.e. to set useNA. This refers only to the vector interface.
%%     ~~Describe \code{\dots} here~~
}
}
\details{The uncertainty coefficient is computed as \deqn{U(C|R) = \frac{H(X) + H(Y) - H(XY)}{H(Y)} } and 
ranges from [0, 1]. \cr
%%  ~~ If necessary, more details than the description above ~~
}
\value{Either a single numeric value, if no confidence interval is required, \cr
or a vector with 3 elements for estimate, lower and upper confidence intervall. 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Theil, H. (1972), \emph{Statistical Decomposition Analysis}, Amsterdam: North-Holland Publishing Company.
%% ~put references to the literature/web site here ~
}
\author{Andri Signorell <andri@signorell.net> strongly based on code from Antti Arppe <antti.arppe@helsinki.fi>
%%  ~~who you are~~
}

\seealso{\code{\link{Entropy}}, \code{\link{Lambda}}, \code{\link{Assocs}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# example from Goodman Kruskal (1954)

m <- as.table(cbind(c(1768,946,115), c(807,1387,438), c(189,746,288), c(47,53,16)))
dimnames(m) <- list(paste("A", 1:3), paste("B", 1:4))
m

# direction default is "symmetric"
UncertCoef(m)
UncertCoef(m, conf.level=0.95)

UncertCoef(m, direction="row")
UncertCoef(m, direction="column")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
