\name{PairApply}
\alias{PairApply}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pairwise Calculations
%%  ~~function to do ... ~~
}
\description{Implements a logic to run pairwise calculations on the columns of a data.frame or a matrix.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PairApply(x, FUN = NULL, ..., symmetric = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a list, a data.frame or a matrix with columns to be processed pairwise.
%%     ~~Describe \code{x} here~~
}
   \item{FUN}{a function to be calculated. It is assumed, that the first 2 arguments denominate x and y.
%%     ~~Describe \code{FUN} here~~
}
  \item{\dots}{the dots are passed to FUN.
%%     ~~Describe \code{\dots} here~~
}
\item{symmetric}{logical. Does the function yield the same result for FUN(x, y) and FUN(y, x)? \cr
 If \code{TRUE} just the lower triangular matrix is calculated and transposed. Default is FALSE.
%%     ~~Describe \code{x} here~~
}
}
\details{This code is based on the logic of \code{cor()} and extended for asymmetric functions.
%%  ~~ If necessary, more details than the description above ~~
}
\seealso{\code{\link{outer}}, \code{\link{CombPairs}}, \code{\link{pairwise.table}} }
\value{a matrix with the results of FUN.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\examples{
PairApply(d.diamonds[,c("colour","clarity","cut","polish")], FUN = CramerV, 
          symmetric=TRUE)

# user defined functions are ok as well
PairApply(d.diamonds[,c("clarity","cut","polish","symmetry")], 
  FUN = function(x,y) wilcox.test(as.numeric(x), as.numeric(y))$p.value, symmetric=TRUE)

# asymetric measure
PairApply(d.diamonds[,c("colour", "clarity", "cut", "polish")], 
  FUN = Lambda, direction = "row")

# ... compare to:
Lambda(x=d.diamonds$colour, y=d.diamonds$clarity, direction="row")  
Lambda(x=d.diamonds$colour, y=d.diamonds$clarity, direction="column")  


# the data.frame
dfrm <- d.diamonds[, c("colour","clarity","cut","polish")]
PairApply(dfrm, FUN = CramerV, symmetric=TRUE)

# the same as matrix (columnwise)
m <- as.matrix(dfrm)
PairApply(m, FUN = CramerV, symmetric=TRUE)

# ... and the list interface
lst <- as.list(dfrm)
PairApply(lst, FUN = CramerV, symmetric=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip}

